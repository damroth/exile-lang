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
  | TPtr of typ                        (* `*T` *)
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

let int_typ_name signed width =
  let prefix = if signed then "i" else "u" in
  let bits = match width with Ast.W8 -> "8" | Ast.W16 -> "16" | Ast.W32 -> "32" in
  prefix ^ bits

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
  | TPtr t -> "*" ^ typ_name t
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
   the emitted C struct sits in the same namespace as user fns/structs. *)
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
  | TExtStruct n -> n              (* raw — opaque struct lives in C namespace *)
  | TExtAlias n -> n               (* raw — opaque type alias lives in C namespace *)
  | TPtr t -> "ptr_" ^ mangle_typ t
  | TVar n ->
      (* TVar should be substituted away by monomorphization before any
         consumer asks for a C name.  If we see one here it's a compiler
         bug, not user error. *)
      failwith ("internal: TVar '" ^ n ^ "' reached mangle_typ — \
                 monomorphization missed an instantiation")
  | TNullPtr -> failwith "TNullPtr should never be mangled"

let tuple_struct_name ts = "ex_" ^ mangle_typ (TTuple ts)

(* Substitute every `TVar n` in [ty] using the [bindings] association.
   Variables not present in [bindings] are left as-is — partial
   substitution is the common case for nested generic decls. *)
let rec subst_typ bindings = function
  | TVar n as t ->
      (match List.assoc_opt n bindings with
       | Some replacement -> replacement
       | None -> t)
  | TPtr inner -> TPtr (subst_typ bindings inner)
  | TTuple ts -> TTuple (List.map (subst_typ bindings) ts)
  | (TInt _ | TCInt _ | TCShort _ | TCLong _
    | TCChar | TCSChar | TCUChar | TCVoid
    | TBool | TString | TStruct _ | TExtStruct _
    | TExtAlias _ | TEnum _ | TNullPtr) as t -> t

(* True when [ty] is monomorphic — no `TVar` anywhere in the tree.
   Codegen filters generic decls (those containing TVar) out of the
   per-program emission until the monomorphizer materialises concrete
   instantiations. *)
let rec is_concrete = function
  | TVar _ -> false
  | TPtr inner -> is_concrete inner
  | TTuple ts -> List.for_all is_concrete ts
  | TInt _ | TCInt _ | TCShort _ | TCLong _
  | TCChar | TCSChar | TCUChar | TCVoid
  | TBool | TString | TStruct _ | TExtStruct _
  | TExtAlias _ | TEnum _ | TNullPtr -> true

(* Typed AST — mirrors Ast.expr/stmt but every node carries the type
   computed by elaboration in `.ty`.  Codegen reads the type directly,
   never re-runs `type_of`. *)
type texpr_node =
  | TIntLit of int
  | TBoolLit of bool
  | TNullLit
  | TStringLit of string
  | TVar of string
  | TNeg of texpr
  | TBinOp of Ast.binop * texpr * texpr
  | TCall of { mangled : string; args : texpr list }
  | TBuiltinCall of { name : string; args : texpr list }
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
  | TAssign of { name : string; value : texpr; pos : Pos.t }
  | TAssignField of { target : texpr; field : string; value : texpr;
                      pos : Pos.t }
  | TAssignDeref of { target : texpr; value : texpr; pos : Pos.t }
  | TReturn of { value : texpr; pos : Pos.t }
  | TExprStmt of texpr
  | TIf of { cond : texpr; then_body : tstmt list; else_body : tstmt list }
  | TWhile of { cond : texpr; body : tstmt list }
  | TDefer of { body : tstmt list; pos : Pos.t }

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
  tp_c_includes : string list;        (* `@c_include("...")` paths in
                                         source order *)
  tp_ext_consts : (string * typ) list; (* `extern const NAME: T;` —
                                         resolved-type pairs.  Codegen
                                         emits `extern <type> NAME;` in
                                         the forward-decl block. *)
}
