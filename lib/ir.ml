(* Typed intermediate representation.  Owns the data structures the rest
   of the pipeline produces (Typecheck) and consumes (Codegen): the type
   domain, function/struct signatures, and the typed AST that mirrors
   Ast but carries `.ty` on every expression node.

   This module is pure — no validation, no Error.failf in the normal
   flow.  Validators (Typecheck) build values of these types; Codegen
   pattern-matches and emits.  Keeping it free of validation logic
   means future passes (monomorphization for generics, defer lowering,
   alternate backends) can consume Ir without dragging in the
   typechecker. *)

type int_kind = { signed : bool; width : Ast.int_width }

type typ =
  | TInt of int_kind
  | TCInt of { signed : bool }         (* `c_int` / `c_uint` — maps to the
                                          target C compiler's native `int`,
                                          width and all.  Used at FFI
                                          boundaries where the C side has
                                          a plain `int`/`unsigned int`
                                          parameter or return type.  No
                                          implicit conversion to/from
                                          regular TInt (different ABI), use
                                          `as` cast. *)
  | TCShort of { signed : bool }       (* c_short / c_ushort *)
  | TCLong of { signed : bool }        (* c_long / c_ulong *)
  | TCChar                             (* c_char — plain char *)
  | TCSChar                            (* c_schar — explicitly signed *)
  | TCUChar                            (* c_uchar *)
  | TCVoid                             (* c_void — only under TPtr *)
  | TBool
  | TString
  | TTuple of typ list
  | TStruct of string list             (* absolute path: e.g. ["foo"; "Point"] *)
  | TExtAlias of string                (* `extern type Foo;` — raw C type
                                          alias.  Codegen emits the name
                                          directly (no `struct` prefix);
                                          header'a lub stub provides the
                                          definition.  Use case: `LONG`,
                                          `APTR`, `ULONG` z AmigaOS
                                          `<exec/types.h>`. *)
  | TExtStruct of string               (* `extern struct Foo;` — opaque,
                                          top-level only, raw C name with
                                          no `ex_` prefix.  Only legal
                                          shape is `*TExtStruct` (pointer);
                                          codegen emits `struct <name>`
                                          where the user wrote it and a
                                          single `struct <name>;` forward
                                          decl up top.  No definition is
                                          ever generated — the type lives
                                          on the C side. *)
  | TEnum of string list               (* absolute path: e.g. ["geom"; "Shape"] *)
  | TStructApp of { path : string list; args : typ list }
  | TEnumApp of { path : string list; args : typ list }
                                       (* A generic struct/enum applied to
                                          type arguments that still contain
                                          free `TVar`s — i.e. not yet
                                          monomorphizable (e.g. `Pair<A, B>`
                                          in a generic `impl`/fn skeleton).
                                          The instant every arg is concrete,
                                          `resolve_type_ann` normalises it to
                                          a flat instance `TStruct`/`TEnum`
                                          via Mono.  So these only ever hold
                                          a partially-free application, are
                                          never `is_concrete`, and never reach
                                          codegen — exactly like `TVar`. *)
  | TPtr of typ                        (* `*T` *)
  | TFnPtr of { params : typ list; ret : typ option }
                                       (* `fn(T1, T2) -> R` — function
                                          pointer.  None ret = void.
                                          Codegen emits the awkward C
                                          fn-ptr form (ret followed by
                                          parenthesised name) via a
                                          dedicated helper that knows
                                          the binder name. *)
  | TNullPtr                           (* type of `null` literal — compatible
                                          with any TPtr; never reaches codegen
                                          as a declaration type *)
  | TVar of string                     (* generic type parameter (`T`, `U`).
                                          Lives until monomorphization, then
                                          is substituted with a concrete typ.
                                          Codegen never sees TVar — anything
                                          generic is monomorphized first. *)

(* Default integer type — what `int` and bare integer literals reduce to. *)
let t_i32 = TInt { signed = true; width = Ast.W32 }

let int_width_bits = function Ast.W8 -> 8 | Ast.W16 -> 16 | Ast.W32 -> 32

(* Does literal value [n] fit into an integer type of given signedness/width?
   OCaml's int is 63-bit on a 64-bit host, so all our ranges fit safely. *)
(* Predicate covering every integer-shaped type — including the c_*
   family and `extern type` aliases.  Used by `as`-cast validation
   (which lets any int-shaped type cross to any other) and by
   bare-literal coercion at FFI sites.  Add new int-like variants
   here so the rest of the pipeline picks them up automatically. *)
let is_int_like = function
  | TInt _ | TCInt _ | TCShort _ | TCLong _
  | TCChar | TCSChar | TCUChar | TExtAlias _ -> true
  | _ -> false

(* Pointer-shaped types — `*T`, the polymorphic null literal, and
   `str` (which is `const char *` in C).  Used by `as`-cast validation
   to allow ptr↔ptr casts on top of the int↔int ones; including TString
   makes FFI to AmigaOS-style `APTR`/`*c_char` ergonomic ("hello" cast
   to *c_uchar etc.). *)
let is_ptr = function
  | TPtr _ | TNullPtr | TString -> true
  | _ -> false

let int_fits n typ =
  match typ with
  | TInt { signed = true; width = Ast.W8 } -> n >= -128 && n <= 127
  | TInt { signed = false; width = Ast.W8 } -> n >= 0 && n <= 255
  | TInt { signed = true; width = Ast.W16 } -> n >= -32768 && n <= 32767
  | TInt { signed = false; width = Ast.W16 } -> n >= 0 && n <= 65535
  | TInt { signed = true; width = Ast.W32 } ->
      n >= -2147483648 && n <= 2147483647
  | TInt { signed = false; width = Ast.W32 } -> n >= 0 && n <= 4294967295
  (* c_int width is target-dependent at the C level, but in practice
     both host gcc and amiga-gcc land on 32-bit `int` — treat the
     fitting bounds as i32/u32 for literal coercion at FFI sites. *)
  | TCInt { signed = true } -> n >= -2147483648 && n <= 2147483647
  | TCInt { signed = false } -> n >= 0 && n <= 4294967295
  (* c_short / c_ushort: 16-bit C standard guarantees. *)
  | TCShort { signed = true } -> n >= -32768 && n <= 32767
  | TCShort { signed = false } -> n >= 0 && n <= 65535
  (* c_long / c_ulong: ≥32-bit per C standard.  We accept any
     OCaml-int-fitting literal (i.e. 63-bit on a 64-bit host) — the
     C compiler will catch overflow against the actual platform
     `long` width on the target.  Tightening to 32-bit would force
     awkward casts on 64-bit hosts where `long` is 64 bits. *)
  | TCLong { signed = true } -> true
  | TCLong { signed = false } -> n >= 0
  (* c_char: signedness implementation-defined; accept the union. *)
  | TCChar -> n >= -128 && n <= 255
  | TCSChar -> n >= -128 && n <= 127
  | TCUChar -> n >= 0 && n <= 255
  (* `extern type T` is an opaque C alias — could be any integer
     width or even non-integer.  We have no way to know.  Allow any
     i32-fitting literal as a pragmatic default; if the alias is for
     a non-integer the C compiler catches the misuse. *)
  | TExtAlias _ -> n >= -2147483648 && n <= 2147483647
  | _ -> false

type fn_sig = {
  param_tys : typ list;
  ret_ty : typ option;
  mangled : string;            (* C-level name (e.g. "ex_foo" or "foo__bar") *)
  fn_pub : bool;
  fn_tparams : string list;     (* generic type parameters; [] for mono *)
  fn_variadic : bool;           (* C-style varargs after [param_tys] *)
}

(* Struct signatures share the same module-aware resolution as functions —
   the registered path is the struct's absolute location. *)
type struct_sig = {
  sname_path : string list;     (* full path including struct name, e.g. ["foo"; "Point"] *)
  sfields_ty : (string * typ) list;
  sis_pub : bool;
  stparams : string list;       (* generic type parameters; [] for mono *)
  sinstance_args : typ list option;
                                 (* None for skeletons; Some args for
                                    monomorphic instances generated at
                                    use sites.  Used by bidirectional
                                    typing to recover bindings from
                                    an expected `TStruct inst_path`. *)
  sis_debug : bool;             (* `@debug` — codegen synthesizes a
                                   Rust-Debug-style printer for this type
                                   and `print(v: TStruct path)` dispatches
                                   to it.  MVP: only mono structs. *)
}

(* Enum signatures: variant order is preserved to give each variant a
   stable C tag value (tag = list index in `evariants`).  Field
   payload — empty for unit variants, synthetic `_0`/`_1`/... names
   for tuple variants, user-given names for struct variants.  Codegen
   reads `vsfields` directly so the same emission path serves all
   three forms; `vsis_struct` is kept only for type-checking the
   construction syntax (struct vs tuple). *)
type variant_sig = {
  vsname : string;
  vsfields : (string * typ) list;
  vsis_struct : bool;
}

type enum_sig = {
  ename_path : string list;     (* absolute path: e.g. ["geom"; "Shape"] *)
  evariants : variant_sig list;
  eis_pub : bool;
  etparams : string list;       (* generic type parameters; [] for mono *)
  einstance_args : typ list option;
                                 (* None for skeletons; Some args for
                                    monomorphic instances. *)
  eis_must_use : bool;          (* `@must_use` on the enum decl — propagates
                                   to monomorphic instances so the linter
                                   flags discarded values of
                                   `Result<int, str>` etc. *)
  eis_debug : bool;             (* `@debug` — codegen synthesizes a
                                   Rust-Debug-style printer that switches
                                   on the tag and renders each variant.
                                   MVP: only mono enums. *)
}

(* Mangle a function name with its module path.  Top-level (path = []) gets
   the `ex_` prefix so emitted symbols never collide with C stdlib builtins
   (gcc warns about builtin-declaration-mismatch even when our top-level
   fn is `static`).  Inside a module, names join with "__"; the module
   prefix already keeps mod-internal symbols away from stdlib names. *)
let mangle path name =
  match path with
  | [] -> "ex_" ^ name
  | _ -> String.concat "__" path ^ "__" ^ name

let rec type_of_ann = function
  | Ast.TyInt { signed; width } -> TInt { signed; width }
  | Ast.TyCInt { signed } -> TCInt { signed }
  | Ast.TyCShort { signed } -> TCShort { signed }
  | Ast.TyCLong { signed } -> TCLong { signed }
  | Ast.TyCChar -> TCChar
  | Ast.TyCSChar -> TCSChar
  | Ast.TyCUChar -> TCUChar
  | Ast.TyCVoid -> TCVoid
  | Ast.TyStr -> TString
  | Ast.TyBool -> TBool
  | Ast.TyTuple ts -> TTuple (List.map type_of_ann ts)
  | Ast.TyStruct { path; args = _ } -> TStruct path
  | Ast.TyPtr t -> TPtr (type_of_ann t)
  | Ast.TySelf ->
      failwith "internal: TySelf reached type_of_ann — the parser should \
                have substituted the impl target"
  | Ast.TyFnPtr { params; ret } ->
      TFnPtr { params = List.map type_of_ann params;
               ret = Option.map type_of_ann ret }

let int_typ_name signed width =
  let prefix = if signed then "i" else "u" in
  let bits = match width with Ast.W8 -> "8" | Ast.W16 -> "16" | Ast.W32 -> "32" in
  prefix ^ bits

(* User-facing rendering of a type — used in error messages and bug
   reports.  Reproduces the source syntax: `c_int`, `*T`, `(T1, T2)`,
   `fn(T) -> R`, etc.  See `mangle_typ` for the C-identifier-safe form
   used to build emitted C symbol names. *)
let rec typ_name = function
  | TInt { signed; width } -> int_typ_name signed width
  | TCInt { signed = true } -> "c_int"
  | TCInt { signed = false } -> "c_uint"
  | TCShort { signed = true } -> "c_short"
  | TCShort { signed = false } -> "c_ushort"
  | TCLong { signed = true } -> "c_long"
  | TCLong { signed = false } -> "c_ulong"
  | TCChar -> "c_char"
  | TCSChar -> "c_schar"
  | TCUChar -> "c_uchar"
  | TCVoid -> "c_void"
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts -> "(" ^ String.concat ", " (List.map typ_name ts) ^ ")"
  | TStruct path -> String.concat "::" path
  | TExtStruct n -> n
  | TExtAlias n -> n
  | TEnum path -> String.concat "::" path
  | TStructApp { path; args } | TEnumApp { path; args } ->
      String.concat "::" path
      ^ "<" ^ String.concat ", " (List.map typ_name args) ^ ">"
  | TPtr t -> "*" ^ typ_name t
  | TFnPtr { params; ret } ->
      let ps = String.concat ", " (List.map typ_name params) in
      let r = match ret with None -> "" | Some t -> " -> " ^ typ_name t in
      "fn(" ^ ps ^ ")" ^ r
  | TNullPtr -> "*<null>"
  | TVar n -> n

(* Equality used for type-match decisions in let/return/arg/field/assign
   sites.  Plain `=` would reject `TNullPtr` against any concrete `TPtr T`;
   here we treat the null literal as polymorphic over pointer types. *)
let rec typ_eq a b =
  match a, b with
  | TNullPtr, TPtr _ | TPtr _, TNullPtr -> true
  | TNullPtr, TNullPtr -> true
  | TPtr a, TPtr b -> typ_eq a b
  | TTuple xs, TTuple ys ->
      List.length xs = List.length ys && List.for_all2 typ_eq xs ys
  | _ -> a = b

(* Mangle a type to a C-identifier-safe string used as a unique key for
   tuple-struct dedup and as the C type name for named structs.  Tuples are
   anonymous (`tup<n>_T1_T2`); `tuple_struct_name` adds the `ex_` prefix so
   the emitted C struct sits in the same namespace as user fns/structs.

   Distinct from `typ_name`: this drops underscores from C primitive
   names (`cint` not `c_int`), prefixes pointers (`ptr_T`), and
   serialises fn-ptrs/tuples as identifier-safe shapes — none of the
   `*`/`::`/`,`/`->` punctuation `typ_name` emits would be legal in
   a C identifier. *)
let rec mangle_typ = function
  | TInt { signed; width } -> int_typ_name signed width
  | TCInt { signed = true } -> "cint"
  | TCInt { signed = false } -> "cuint"
  | TCShort { signed = true } -> "cshort"
  | TCShort { signed = false } -> "cushort"
  | TCLong { signed = true } -> "clong"
  | TCLong { signed = false } -> "culong"
  | TCChar -> "cchar"
  | TCSChar -> "cschar"
  | TCUChar -> "cuchar"
  | TCVoid -> "cvoid"
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts ->
      Printf.sprintf "tup%d_%s" (List.length ts)
        (String.concat "_" (List.map mangle_typ ts))
  | TStruct path | TEnum path ->
      (match List.rev path with
       | [] -> failwith "empty named-type path"
       | n :: rest -> mangle (List.rev rest) n)
  | TStructApp _ | TEnumApp _ ->
      (* Always carries a free TVar, so it's never concrete and should
         have been normalised to a flat instance before any C name was
         needed — same contract as TVar. *)
      failwith "internal: TStructApp/TEnumApp reached mangle_typ — \
                monomorphization should have normalised it"
  | TExtStruct n -> n              (* raw — opaque struct lives in C namespace *)
  | TExtAlias n -> n               (* raw — opaque type alias lives in C namespace *)
  | TPtr t -> "ptr_" ^ mangle_typ t
  | TFnPtr { params; ret } ->
      let ps = String.concat "_" (List.map mangle_typ params) in
      let r = match ret with None -> "void" | Some t -> mangle_typ t in
      Printf.sprintf "fn%d_%s_to_%s" (List.length params) ps r
  | TVar n ->
      (* TVar should be substituted away by monomorphization before any
         consumer asks for a C name.  If we see one here it's a compiler
         bug, not user error. *)
      failwith ("internal: TVar '" ^ n ^ "' reached mangle_typ — \
                 monomorphization missed an instantiation")
  | TNullPtr -> failwith "TNullPtr should never be mangled"

let tuple_struct_name ts = "ex_" ^ mangle_typ (TTuple ts)

(* User-facing type rendering for `type_name(expr)` and error messages.
   Delegates to [typ_name] for everything except generic enum/struct
   instances, which [typ_name] would show in mangled form
   (`Result_i32_str`); here we consult the [structs] / [enums] indexes
   for the matching sig and reconstruct `Result<i32, str>` from the saved
   `einstance_args` / `sinstance_args`.  Pure over the indexes so both
   Typecheck (compile-time `++` folding of `type_name`) and Codegen can
   share it. *)
let rec render_typ_user_facing ~structs ~enums t =
  match t with
  | TEnum path ->
      (match List.find_opt (fun (e : enum_sig) -> e.ename_path = path) enums with
       | Some { einstance_args = Some args; _ } when args <> [] ->
           render_named_with_args ~structs ~enums path args
       | _ -> typ_name t)
  | TStruct path ->
      (match List.find_opt (fun (s : struct_sig) -> s.sname_path = path) structs with
       | Some { sinstance_args = Some args; _ } when args <> [] ->
           render_named_with_args ~structs ~enums path args
       | _ -> typ_name t)
  | TPtr inner -> "*" ^ render_typ_user_facing ~structs ~enums inner
  | TTuple ts ->
      "(" ^ String.concat ", "
              (List.map (render_typ_user_facing ~structs ~enums) ts) ^ ")"
  | _ -> typ_name t
and render_named_with_args ~structs ~enums path args =
  let last = List.nth path (List.length path - 1) in
  let suffix = "_" ^ String.concat "_" (List.map mangle_typ args) in
  let last_len = String.length last in
  let suf_len = String.length suffix in
  let base =
    if last_len > suf_len
       && String.sub last (last_len - suf_len) suf_len = suffix
    then String.sub last 0 (last_len - suf_len)
    else last
  in
  let prefix =
    let rec init = function [] | [_] -> [] | x :: rest -> x :: init rest in
    init path
  in
  let qualified = String.concat "::" (prefix @ [base]) in
  let arg_strs = List.map (render_typ_user_facing ~structs ~enums) args in
  qualified ^ "<" ^ String.concat ", " arg_strs ^ ">"

(* Bottom-up tree rebuilder: structural constructors (TPtr / TTuple /
   TFnPtr) recurse internally; [f] is applied at every leaf type and
   returns its replacement.  Use this when a transformation only
   touches leaves and leaves the shape intact — see `subst_typ` for
   the canonical instance.  Adding a new structural constructor in
   the future requires extending this helper (and `type_for_all`)
   only — single editing point for the whole pipeline. *)
let rec type_map ~f = function
  | TPtr inner -> TPtr (type_map ~f inner)
  | TTuple ts -> TTuple (List.map (type_map ~f) ts)
  | TFnPtr { params; ret } ->
      TFnPtr { params = List.map (type_map ~f) params;
               ret = Option.map (type_map ~f) ret }
  | TStructApp { path; args } ->
      TStructApp { path; args = List.map (type_map ~f) args }
  | TEnumApp { path; args } ->
      TEnumApp { path; args = List.map (type_map ~f) args }
  | leaf -> f leaf

(* Conjunction fold over a type tree: [f] runs on each leaf, structural
   constructors short-circuit through &&.  Use for predicate questions
   like "is this type concrete" or "does this type mention X". *)
let rec type_for_all ~f = function
  | TPtr inner -> type_for_all ~f inner
  | TTuple ts -> List.for_all (type_for_all ~f) ts
  | TFnPtr { params; ret } ->
      List.for_all (type_for_all ~f) params
      && (match ret with Some t -> type_for_all ~f t | None -> true)
  | TStructApp { args; _ } | TEnumApp { args; _ } ->
      List.for_all (type_for_all ~f) args
  | leaf -> f leaf

(* True when [t]'s type tree has more than [limit] nodes.  Short-circuits
   the moment the limit is passed, so the traversal is O(min(nodes,
   limit)) even on a pathologically large type — runaway fn
   monomorphization (`fn f<T>(x: T) { f((x, x)); }`) builds
   exponentially-sized tuple types, and the guard must inspect them
   without itself blowing up. *)
let typ_size_exceeds limit t =
  let count = ref 0 in
  let rec go t =
    incr count;
    if !count > limit then raise Exit;
    match t with
    | TPtr inner -> go inner
    | TTuple ts -> List.iter go ts
    | TFnPtr { params; ret } -> List.iter go params; Option.iter go ret
    | _ -> ()
  in
  (try go t; false with Exit -> true)

(* Substitute every `TVar n` in [ty] using the [bindings] association.
   Variables not present in [bindings] are left as-is — partial
   substitution is the common case for nested generic decls. *)
let subst_typ bindings =
  type_map ~f:(function
    | TVar n as t ->
        (match List.assoc_opt n bindings with
         | Some replacement -> replacement
         | None -> t)
    | t -> t)

(* True when [ty] is monomorphic — no `TVar` anywhere in the tree.
   Codegen filters generic decls (those containing TVar) out of the
   per-program emission until the monomorphizer materialises concrete
   instantiations. *)
let is_concrete =
  type_for_all ~f:(function TVar _ -> false | _ -> true)

(* Typed AST — mirrors Ast.expr/stmt but every node carries the type
   computed by elaboration in `.ty`.  Codegen reads the type directly,
   never re-runs `type_of`. *)
type texpr_node =
  | TIntLit of int
  | TBoolLit of bool
  | TNullLit
  | TStringLit of string
  | TVar of string
  | TFnRef of string                    (* reference to a fn by C-side
                                           name — codegen emits the
                                           raw identifier in expression
                                           position; C autoconverts to
                                           function pointer. *)
  | TNeg of texpr
  | TBitNot of texpr                    (* `~e` — bitwise complement *)
  | TBinOp of Ast.binop * texpr * texpr
  | TCall of { mangled : string; args : texpr list }
  | TBuiltinCall of { name : string; args : texpr list }
  | TIndirectCall of { fn_expr : texpr; args : texpr list }
                                        (* call through an arbitrary fn-ptr
                                           expression — used when `recv.field`
                                           is a fn-ptr field rather than a
                                           method.  Codegen emits
                                           `(<fn_expr>)(args)`; C resolves
                                           the call through the auto-deref'd
                                           pointer.  Direct fn-ptr-local
                                           calls still go through TCall (the
                                           local name doubles as the call
                                           target). *)
  | TCast of texpr * Ast.type_ann
  | TTupleLit of texpr list
  | TStructLit of { sname_path : string list;
                    fields : (string * texpr) list;
                    base : texpr option }
  | TFieldAccess of { target : texpr; field : string }
  | TRef of texpr
  | TDeref of texpr
  | TNew of { sname_path : string list;
              fields : (string * texpr) list;
              base : texpr option }
  | TEnumLit of { ename_path : string list;
                  variant : string;
                  tag : int;            (* index in the enum's variant list *)
                  args : (string * texpr) list }
                                        (* empty for unit; field names
                                           come from variant_sig.vsfields,
                                           so codegen emits
                                           `data.<variant>.<name> = e` *)
  | TMatch of { scrutinee : texpr;
                ename_path : string list;
                arms : tmatch_arm list }
  | TIfExpr of { cond : texpr; then_val : texpr; else_val : texpr }
                                        (* `if c { a } else { b }` used as a
                                           value.  Block-shaped like TMatch:
                                           never emitted inline by gen_expr —
                                           value positions route through
                                           emit_value_into_temp, nested uses
                                           are lifted to a `__lift` temp.
                                           Both branches yield a value of the
                                           expression's type. *)
  | TSizeOf of typ                      (* `size_of(T)` — codegen emits
                                           `sizeof(<c_type_prefix t>)`.
                                           For instances of generic fns
                                           the typ here is concrete after
                                           subst_typ runs in resolve_type_ann. *)

and tmatch_arm = {
  tpat : tpattern;
  tbody : texpr;
  tdiverges : bool;             (* if true, codegen emits `return tbody;`
                                   instead of assigning to the match's
                                   `__exile_ret` slot.  Used by the `try`
                                   desugar to early-return Err/None from
                                   the enclosing fn.  Diverging arms are
                                   skipped when computing the match's
                                   overall result type. *)
  tarm_pos : Pos.t;
}

and tpattern =
  | TPWildcard
  | TPVar of string
  | TPVariant of { variant : string; tag : int;
                   binds : (string * tpattern) list }
                                        (* field name + sub-pattern;
                                           tuple form uses synthetic
                                           `_0`/`_1`/... names *)

and texpr = {
  e : texpr_node;
  ty : typ;
  pos : Pos.t;
}

type tstmt =
  | TLet of { name : string; value : texpr; pos : Pos.t }
  | TLetTuple of { names : string list; value : texpr; pos : Pos.t }
  | TAssign of { path : string list; value : texpr; pos : Pos.t }
                                          (* Single-segment path = local
                                             variable; multi-segment = qualified
                                             reference to an `extern var`. *)
  | TAssignField of { target : texpr; field : string; value : texpr;
                      pos : Pos.t }
  | TAssignDeref of { target : texpr; value : texpr; pos : Pos.t }
  | TReturn of { value : texpr option; pos : Pos.t }
                                          (* None = bare `return;` from a
                                             void fn (main's bare return is
                                             desugared to `Some 0` in
                                             typecheck) *)
  | TExprStmt of texpr
  | TIf of { cond : texpr; then_body : tstmt list; else_body : tstmt list }
  | TWhile of { cond : texpr; body : tstmt list }
  | TDefer of { body : tstmt list; pos : Pos.t }

(* Structural traversal primitives for the typed AST.  Every consumer
   that used to handwrite a per-constructor match (`collect_tuple_types_of`,
   `uses_heap_of` in Typecheck; `reads_in_expr`, `lets_in_stmts`,
   must-use walker in Lint) now goes through one of these.  Adding a
   new texpr/tstmt constructor requires extending only the *_children
   functions below. *)

let texpr_children (te : texpr) : texpr list =
  match te.e with
  | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _
  | TVar _ | TFnRef _ | TSizeOf _ -> []
  | TNeg sub | TBitNot sub | TRef sub | TDeref sub | TCast (sub, _) -> [sub]
  | TBinOp (_, l, r) -> [l; r]
  | TCall { args; _ } | TBuiltinCall { args; _ } -> args
  | TIndirectCall { fn_expr; args } -> fn_expr :: args
  | TTupleLit es -> es
  | TStructLit { fields; base; _ } | TNew { fields; base; _ } ->
      List.map snd fields @ Option.to_list base
  | TFieldAccess { target; _ } -> [target]
  | TEnumLit { args; _ } -> List.map snd args
  | TMatch { scrutinee; arms; _ } ->
      scrutinee :: List.map (fun a -> a.tbody) arms
  | TIfExpr { cond; then_val; else_val } -> [cond; then_val; else_val]

let rec iter_texpr f e =
  f e;
  List.iter (iter_texpr f) (texpr_children e)

let rec exists_texpr p e =
  p e || List.exists (exists_texpr p) (texpr_children e)

let rec fold_texpr f acc e =
  let acc = f acc e in
  List.fold_left (fold_texpr f) acc (texpr_children e)

(* Immediate sub-statements: bodies of TIf/TWhile/TDefer. *)
let tstmt_substmts = function
  | TIf { then_body; else_body; _ } -> then_body @ else_body
  | TWhile { body; _ } | TDefer { body; _ } -> body
  | TLet _ | TLetTuple _ | TAssign _ | TAssignField _
  | TAssignDeref _ | TReturn _ | TExprStmt _ -> []

(* Exprs that live DIRECTLY in [s] — cond, value, target.  Does NOT
   include exprs nested in sub-stmts; compose with iter_texpr / fold_texpr
   on each entry for deep traversal of an entire fn body. *)
let tstmt_own_exprs = function
  | TLet { value; _ } | TLetTuple { value; _ }
  | TAssign { value; _ } | TExprStmt value -> [value]
  | TReturn { value; _ } -> Option.to_list value
  | TAssignField { target; value; _ }
  | TAssignDeref { target; value; _ } -> [target; value]
  | TIf { cond; _ } | TWhile { cond; _ } -> [cond]
  | TDefer _ -> []

let rec iter_tstmt f s =
  f s;
  List.iter (iter_tstmt f) (tstmt_substmts s)

let rec exists_tstmt p s =
  p s || List.exists (exists_tstmt p) (tstmt_substmts s)

let rec fold_tstmt f acc s =
  let acc = f acc s in
  List.fold_left (fold_tstmt f) acc (tstmt_substmts s)

(* Per-function payload that codegen consumes — original Ast.func for
   the user-side trivia (param names, fn name, pos), the resolved C
   name, parameter and return types pre-resolved against the surrounding
   scope (so mod-local struct names round-trip to absolute paths in C),
   the elaborated body, and the hoisted let-decl list. *)
type tfunc = {
  tf_path : string list;
  tf_func : Ast.func;
  tf_mangled : string;
  tf_param_tys : typ list;          (* one per tf_func.params, scope-resolved *)
  tf_ret_ty : typ option;           (* mirrors tf_func.ret_ty, scope-resolved *)
  tf_body : tstmt list;
  tf_lets : (string * typ) list;
  tf_origin_pos : Pos.t option;     (* call site that triggered this instance,
                                       for generic-fn instances only.  None
                                       for skeleton tfuncs and originally-mono
                                       fns (lint falls back to tf_func.pos). *)
}

(* Whole-program checked + elaborated view that codegen consumes. *)
type tprogram = {
  tp_funcs : tfunc list;
  tp_struct_decls : (string list * Ast.struct_decl) list;
  tp_struct_index : struct_sig list;
  tp_enum_index : enum_sig list;
  tp_global : (string list * string * fn_sig) list;
  tp_modules : (string list * bool) list;
  tp_uses_heap : bool;
  tp_tuple_types : (string * typ) list;
  tp_fnptr_types : (string * typ) list;  (* unique TFnPtr types used
                                            in the program; codegen
                                            emits one `typedef` per
                                            entry so use-sites can
                                            reference an alias name
                                            instead of inlining the
                                            awkward C fn-ptr syntax *)
  tp_c_includes : string list;        (* `@c_include("...")` paths in
                                         source order *)
  tp_ext_consts : (string * typ) list; (* `extern const NAME: T;` —
                                         resolved-type pairs.  Codegen
                                         emits `extern const <type> NAME;`
                                         in the forward-decl block. *)
  tp_ext_vars : (string * typ) list;   (* `extern var NAME: T;` —
                                         mutable global counterpart of
                                         tp_ext_consts.  Codegen emits
                                         `extern <type> NAME;` (no const). *)
  tp_consts : (string * string) list;  (* user `const NAME: T = e;` folded
                                          to (mangled C name, literal value);
                                          codegen emits `#define <name>
                                          <value>` so use sites (already
                                          mangled TVars) and array sizes
                                          resolve at the C level. *)
}
