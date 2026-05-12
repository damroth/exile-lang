(* Semantic / type-checking layer.  Validates an Ast.program and produces
   the typed Ir.program that codegen consumes.  The typed-AST data
   structures and pure type-domain utilities (typ, mangle, mangle_typ,
   typ_eq, tuple_struct_name, ...) live in `Ir`; this module owns the
   validation rules: lookups with visibility, builtin signatures,
   expression typing, and elaboration. *)

open Ir

(* Resolution context: every function and every struct in the program
   (keyed by module path and name), plus a flat list of every module with
   its pub flag, plus the path of the function we are currently emitting
   code for.  Local references (path = [name]) resolve within the current
   scope; qualified ones (path > 1) resolve from the root with per-segment
   visibility checks.  Mono-instance accumulator threads through
   `instances` (state + helpers live in `Mono`); the enclosing fn's
   return type rides on `ret_ty` so `try` can validate and construct
   the early-return value. *)

type fn_ctx = {
  global : (string list * string * fn_sig) list;
  structs : struct_sig list;
  enums : enum_sig list;
  modules : (string list * bool) list;   (* full path -> is_pub *)
  scope : string list;
  tparams : string list;                 (* generic type params in scope:
                                            ["T"; "U"] inside a generic
                                            decl's body, [] otherwise *)
  tvar_bindings : (string * typ) list;   (* substitute these into every
                                            type produced by resolve_type_ann.
                                            Empty for skeleton elab; populated
                                            with concrete bindings when
                                            re-elaborating a generic fn
                                            instance.  TVars matching keys
                                            in here are replaced after
                                            resolve so body code mentioning
                                            T sees the instance's concrete
                                            type. *)
  instances : Mono.state;
  ext_structs : string list;             (* names of `extern struct Foo`
                                            decls (opaque or exposed) —
                                            resolve_type_ann uses this to
                                            map a single-segment path to
                                            TExtStruct.  Field lookup goes
                                            through ext_struct_fields. *)
  ext_struct_fields : (string * (string * typ) list) list;
                                          (* For exposed extern struct
                                             decls (`extern struct Foo { f:
                                             T, ... }`): name → resolved
                                             fields.  Opaque structs are
                                             absent from this list.
                                             FieldAccess on a TExtStruct
                                             consults this for field
                                             types; absence = opaque
                                             access rejected. *)
  ext_types : string list;               (* names of `extern type Foo;`
                                            aliases — resolve_type_ann
                                            maps a single-segment path
                                            to TExtAlias *)
  ext_consts : (string * typ) list;      (* `extern const NAME: T;` —
                                            looked up in Var expr after
                                            local env miss; codegen
                                            emits raw NAME at use sites *)
  ext_vars : (string * typ) list;        (* `extern var NAME: T;` —
                                            mutable global counterpart of
                                            ext_consts.  Looked up in Var
                                            expr (read) and in Assign
                                            (write).  Codegen emits raw
                                            NAME at use sites. *)
  ret_ty : typ option;                   (* enclosing fn's return type;
                                            None for void.  Used by `try`
                                            to validate / construct the
                                            early-return Err/None value. *)
  fn_asts : (string * (string list * Ast.func)) list;
                                          (* mangled C name → (parent path,
                                             original AST).  Lets generic
                                             fn-instance bodies be re-elaborated
                                             from the source AST after
                                             monomorphization picks args. *)
  aliases : (string list * string * string list * Pos.t) list;
                                          (* `pub use foo::bar;` re-exports.
                                             Each: (scope, local_name, target,
                                             decl_pos).  Lookup at scope for
                                             local_name redirects to target
                                             path resolved
                                             against the same scope. *)
}

let rec is_prefix xs ys =
  match xs, ys with
  | [], _ -> true
  | _, [] -> false
  | x :: xs', y :: ys' -> x = y && is_prefix xs' ys'

(* Drop the last segment of a module path; [] -> []. *)
let parent_path = function
  | [] -> []
  | xs -> List.rev (List.tl (List.rev xs))

(* Walk up the current scope, trying [resolve] at each ancestor level.
   [path] is split into a relative [(mod_path, name)] pair; for each
   prefix we try [resolve (prefix @ mod_path) name].  Used by both fn
   and struct lookup, which only differ in [resolve].

   Re-exports (`pub use foo::bar;`) are checked at each ancestor too:
   if the lookup name matches an alias declared in this scope, retry
   the resolution with the alias's target path.  Cycles through aliases
   are guarded by a visited-paths set. *)
let walk_scope_up ~resolve ctx (path : string list) =
  let (suggested_mod, name) =
    match List.rev path with
    | [] -> failwith "empty path"
    | n :: rest -> (List.rev rest, n)
  in
  let try_alias prefix lookup_name =
    (* Single-segment lookups (e.g. `Window`) at scope `prefix` may
       redirect through `pub use foo::bar;` declared at `prefix`.
       Returns target path if matched. *)
    if suggested_mod <> [] then None
    else
      List.find_map (fun (alias_scope, local_name, target_path, _) ->
        if alias_scope = prefix && local_name = lookup_name
        then Some target_path
        else None)
        ctx.aliases
  in
  let visited = ref [] in
  let rec walk prefix =
    let key = (prefix, suggested_mod, name) in
    if List.mem key !visited then None
    else begin
      visited := key :: !visited;
      match resolve (prefix @ suggested_mod) name with
      | Some r -> Some r
      | None ->
          (match try_alias prefix name with
           | Some target ->
               (* Recurse with the alias target as the new path; keep
                  walking from the same prefix so the target resolves
                  against scopes visible from the alias declaration. *)
               let (alias_mod, alias_name) =
                 match List.rev target with
                 | n :: rest -> (List.rev rest, n)
                 | [] -> ([], "")
               in
               (match resolve (prefix @ alias_mod) alias_name with
                | Some r -> Some r
                | None ->
                    (match prefix with
                     | [] -> None
                     | _ -> walk (parent_path prefix)))
           | None ->
               (match prefix with
                | [] -> None
                | _ -> walk (parent_path prefix)))
    end
  in
  walk ctx.scope

(* Resolve a call path to a function.  Walks the current scope from the
   deepest ancestor down to the root, trying [prefix @ suggested_mod] at
   each level. *)
let lookup_fn ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_map
      (fun (p, n, s) ->
        if p = mod_path && n = name then Some (mod_path, s) else None)
      ctx.global)

let lookup_struct ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_opt
      (fun s -> s.sname_path = mod_path @ [name])
      ctx.structs)

let lookup_enum ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_opt
      (fun e -> e.ename_path = mod_path @ [name])
      ctx.enums)

(* Look up a struct by its absolute path among the program's skeleton
   structs and any monomorphic instances generated so far.  Used
   after type_of has produced `TStruct path` (path may be mangled). *)
let resolve_struct_by_path ctx path =
  match Mono.find_struct ctx.instances path with
  | Some is -> Some is
  | None ->
      List.find_opt (fun s -> s.sname_path = path) ctx.structs

let resolve_enum_by_path ctx path =
  match Mono.find_enum ctx.instances path with
  | Some ie -> Some ie
  | None ->
      List.find_opt (fun e -> e.ename_path = path) ctx.enums

(* `extern struct Foo;` is opaque — exile knows the name but not the
   layout, so the only legal shape is a pointer.  Same goes for the
   `c_void` type: there is no value of type `void` in C, only `void
   *`.  This walks a resolved type and rejects bare `TExtStruct` /
   `TCVoid` outside any `TPtr` wrapper.  Anywhere under `TPtr` is
   fine; tuples are recursed into. *)
(* Walk a resolved type and reject bare `TExtStruct` (opaque) / `TCVoid`
   outside any `TPtr` wrapper.  Exposed extern struct names live in
   [~exposed]; they're treated as ordinary value types with a known
   layout (matching the C header pulled in via @c_include). *)
let rec forbid_naked_opaque ?(exposed = []) pos = function
  | TExtStruct n when not (List.mem n exposed) ->
      Error.failf pos
        "opaque type '%s' can only be used through a pointer (`*%s`) — \
         exile doesn't know its layout (use `extern struct %s { ... }` \
         to expose fields)" n n n
  | TExtStruct _ -> ()
  | TCVoid ->
      Error.failf pos
        "'c_void' has no values — only `*c_void` is usable as a type"
  | TPtr _ -> ()
  | TTuple ts -> List.iter (forbid_naked_opaque ~exposed pos) ts
  | _ -> ()

(* Find a variant by name in an enum's variant list, returning its
   (tag, variant_sig).  Tag is the index — codegen reads positions
   from the same list, so this is the canonical numbering. *)
let find_variant (e : enum_sig) name : (int * variant_sig) option =
  let rec walk i = function
    | [] -> None
    | (vs : variant_sig) :: _ when vs.vsname = name -> Some (i, vs)
    | _ :: rest -> walk (i + 1) rest
  in
  walk 0 e.evariants

(* Scope-aware analogue of `Ir.type_of_ann`: rewrites `TyStruct path`
   so the resulting `TStruct` carries the *absolute* struct path,
   resolved against the surrounding scope.  Without this, a fn or
   field declared as `: Point` inside `mod foo` would carry the
   relative path `["Point"]`, while values of the same type carry
   the absolute `["foo"; "Point"]` — and `typ_eq` would reject the
   match.

   On unresolved struct names we fall back to the raw path: downstream
   checks (lookup at use site, or the `typ_eq` mismatch) emit a clearer
   contextual error than we could here without a `Pos.t`. *)
let rec resolve_type_ann_raw ctx ann =
  match ann with
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
  | Ast.TyTuple ts -> TTuple (List.map (resolve_type_ann_raw ctx) ts)
  | Ast.TyPtr t -> TPtr (resolve_type_ann_raw ctx t)
  | Ast.TyFnPtr { params; ret } ->
      TFnPtr { params = List.map (resolve_type_ann_raw ctx) params;
               ret = Option.map (resolve_type_ann_raw ctx) ret }
  | Ast.TyStruct { path; args = [] } ->
      (* Non-generic case: tparam reference / extern type / extern
         struct / struct / enum / fallback.  ext_types / ext_structs
         are flat (single C symbol name); qualified paths like
         `raw::ULONG` accept the path as long as the last segment
         matches.  Tparam ref is single-segment only. *)
      let last = match List.rev path with n :: _ -> n | [] -> "" in
      (match path with
       | [n] when List.mem n ctx.tparams -> TVar n
       | _ when List.mem last ctx.ext_types -> TExtAlias last
       | _ when List.mem last ctx.ext_structs -> TExtStruct last
       | _ ->
           (match lookup_struct ctx path with
            | Some s -> TStruct s.sname_path
            | None ->
                (match lookup_enum ctx path with
                 | Some e -> TEnum e.ename_path
                 | None -> TStruct path)))
  | Ast.TyStruct { path; args } ->
      (* Generic application `Foo<T1, T2>`: resolve each arg, look up
         the (still-skeletal) generic decl, substitute in its fields,
         and register a monomorphic instance under a mangled path.
         Subsequent uses of the same `Foo<T1, T2>` find the cached
         instance instead of re-substituting. *)
      let resolved_args = List.map (resolve_type_ann_raw ctx) args in
      (* Reject substitutions that still contain TVars from the caller
         scope — a properly monomorphic instance has no free type
         variables.  When we eventually add generic fns / inference
         this restriction relaxes. *)
      List.iter (fun t ->
        if not (is_concrete t) then
          Error.failf Pos.zero
            "generic argument for '%s' must be a concrete type, got %s"
            (String.concat "::" path) (typ_name t))
        resolved_args;
      let check_arity ~name ~tparams =
        let expected = List.length tparams in
        let got = List.length resolved_args in
        if expected <> got then
          Error.failf Pos.zero
            "type '%s' expects %d generic argument(s), got %d"
            name expected got
      in
      (match lookup_struct ctx path with
       | Some s ->
           check_arity ~name:(String.concat "::" s.sname_path)
             ~tparams:s.stparams;
           let inst = Mono.instantiate_struct ctx.instances s resolved_args in
           TStruct inst.sname_path
       | None ->
           (match lookup_enum ctx path with
            | Some e ->
                check_arity ~name:(String.concat "::" e.ename_path)
                  ~tparams:e.etparams;
                let inst = Mono.instantiate_enum ctx.instances e resolved_args in
                TEnum inst.ename_path
            | None ->
                Error.failf Pos.zero
                  "unknown generic type '%s'"
                  (String.concat "::" path)))

(* Public entry point.  Wraps `resolve_type_ann_raw` with a final
   `subst_typ ctx.tvar_bindings` step so generic-instance bodies see
   concrete types where the source mentions the fn's type parameters.
   Skeleton elaboration (`tvar_bindings = []`) leaves the result
   unchanged. *)
let resolve_type_ann ctx ann =
  subst_typ ctx.tvar_bindings (resolve_type_ann_raw ctx ann)

(* Recognise an integer literal expression — bare or negated — for type-fitting
   checks at let-binding sites. *)
let expr_int_lit = function
  | Ast.IntLit (n, _) -> Some n
  | Ast.Neg (Ast.IntLit (n, _), _) -> Some (-n)
  | _ -> None

(* True when [expr] is an integer literal that fits into [target] (a TInt).
   Used at assignment / field-init sites to allow `let x: i8 = 5;` even
   though `5` is `i32` by default. *)
let int_lit_fits expr target =
  match expr_int_lit expr with
  | Some n when is_int_like target -> int_fits n target
  | _ -> false

(* First duplicate key in [xs] under [key], or None.  Replaces the
   ad-hoc O(n²) `List.exists` loops scattered around the typechecker. *)
let find_dup ~key xs =
  let rec loop seen = function
    | [] -> None
    | x :: rest ->
        let k = key x in
        if List.mem k seen then Some k else loop (k :: seen) rest
  in
  loop [] xs

(* C89 reserved words.  Exile identifiers that survive into the generated C
   without mangling (locals, parameters) must not collide.  Top-level fn
   names get the `ex_` prefix from mangle, so they are also safe — but the
   check still rejects them up front for a clearer error. *)
let c_keywords = [
  "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do";
  "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if";
  "int"; "long"; "register"; "return"; "short"; "signed"; "sizeof";
  "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"; "void";
  "volatile"; "while"
]

let check_c_ident pos kind name =
  if List.mem name c_keywords then
    Error.failf pos "%s '%s' is a reserved C keyword" kind name

(* Compile-time builtin signatures.  The codegen layer carries a parallel
   table of emitters keyed by name; this module owns only the type-checking
   side.  Builtins are looked up by single-segment paths — no module
   qualification. *)
type builtin_sig = {
  bname : string;
  bcheck : ctx:fn_ctx -> pos:Pos.t -> arg_tys:typ list -> allow_void:bool -> typ;
}

(* True when [path] names a struct/enum whose `@debug` printer the codegen
   will synthesize.  Drives the `print(v)` dispatch on aggregate types. *)
let struct_is_debug (ctx : fn_ctx) path =
  List.exists (fun (s : struct_sig) ->
    s.sname_path = path && s.sis_debug) ctx.structs
let enum_is_debug (ctx : fn_ctx) path =
  List.exists (fun (e : enum_sig) ->
    e.ename_path = path && e.eis_debug) ctx.enums

let builtin_print = {
  bname = "print";
  bcheck = (fun ~ctx ~pos ~arg_tys ~allow_void:_ ->
    match arg_tys with
    | [ TTuple _ ] ->
        Error.failf pos
          "cannot print a tuple; destructure with 'let (...)' first"
    | [ TStruct path ] when struct_is_debug ctx path -> t_i32
    | [ TStruct path ] ->
        Error.failf pos
          "cannot print a struct value (%s); print individual fields, \
           or mark the struct with `@debug`"
          (String.concat "::" path)
    | [ TPtr _ as t ] ->
        Error.failf pos
          "cannot print a pointer value (%s); deref or print a field"
          (typ_name t)
    | [ TNullPtr ] ->
        Error.failf pos "cannot print 'null'"
    | [ TEnum path ] when enum_is_debug ctx path -> t_i32
    | [ TEnum path ] ->
        Error.failf pos
          "cannot print an enum value (%s); match on it and print per variant, \
           or mark the enum with `@debug`"
          (String.concat "::" path)
    | [ TExtAlias n ] ->
        Error.failf pos
          "cannot print an 'extern type' value (%s); cast to a known \
           type first (e.g. `as int`)" n
    | [ TExtStruct n ] ->
        Error.failf pos
          "cannot print an 'extern struct' value (%s); only pointers \
           to opaque types are usable" n
    | [ (TCShort _ | TCLong _ | TCChar | TCSChar | TCUChar) as t ] ->
        Error.failf pos
          "cannot directly print a %s value; cast to a known type \
           first (e.g. `as int`)" (typ_name t)
    | [_] -> t_i32
    | tys ->
        Error.failf pos "print() takes exactly one argument, got %d"
          (List.length tys));
}

let builtin_free = {
  bname = "free";
  bcheck = (fun ~ctx:_ ~pos ~arg_tys ~allow_void ->
    match arg_tys with
    | [ TPtr _ ] when allow_void -> t_i32  (* placeholder, caller discards *)
    | [ TPtr _ ] ->
        Error.failf pos "'free' returns void, cannot use as a value"
    | [ other ] ->
        Error.failf pos "'free' expects a pointer, got %s" (typ_name other)
    | tys ->
        Error.failf pos "free() takes exactly one argument, got %d"
          (List.length tys));
}

let builtins = [ builtin_print; builtin_free ]

let lookup_builtin = function
  | [ name ] -> List.find_opt (fun b -> b.bname = name) builtins
  | _ -> None

(* `Foo::Bar(args)` parses as `Call(["Foo"; "Bar"], args)` — the parser
   can't tell a fn call from an enum constructor.  `elab_expr` uses this
   helper to rewrite Call into EnumLit when the path resolves to an
   enum + variant. *)
let rewrite_call_as_enum_lit ctx path args pos =
  match path with
  | _ :: _ :: _ ->
      let (init, last) =
        let rev = List.rev path in
        (List.rev (List.tl rev), List.hd rev)
      in
      (match lookup_enum ctx init with
       | Some _ ->
           Some (Ast.EnumLit { tname = init; variant = last;
                               args = Ast.EATuple args; pos })
       | None -> None)
  | _ -> None

(* Mirror of `rewrite_call_as_enum_lit` for struct-variant construction:
   `Foo::Triangle { base: 1, height: 2 }` parses as a `StructLit` (the
   parser can't tell a struct from an enum struct-variant); typecheck
   redirects to `EnumLit (EAStruct fields)` when the path resolves to
   an enum + variant.  Functional update (`..base`) on enum variants
   isn't supported — error if base is set. *)
let rewrite_struct_lit_as_enum_lit ctx tname fields base pos =
  match tname with
  | _ :: _ :: _ ->
      let (init, last) =
        let rev = List.rev tname in
        (List.rev (List.tl rev), List.hd rev)
      in
      (match lookup_enum ctx init with
       | Some _ ->
           if base <> None then
             Error.failf pos
               "'..base' functional update is not supported on enum variants";
           Some (Ast.EnumLit { tname = init; variant = last;
                               args = Ast.EAStruct fields; pos })
       | None -> None)
  | _ -> None

(* Generic-call dispatch: if the resolved fn is generic, infer its
   tparams from the actual arg types (and from the surrounding expected
   type via a bidirectional seed pair `(skel.ret_ty, expected)`),
   instantiate, and return the instance's mangled name + concrete sig.
   For mono fns returns the skeleton's mangled name + sig unchanged.

   Caller is responsible for visibility + arity + arg-type checks; this
   helper owns only the inference / instantiation step.  When
   inference fails (under-determined T), `Mono.infer_tparams` raises
   with a hint to add a let / return type annotation. *)
let resolve_call_dispatch ~pos ~expected ctx ~resolved_mod ~arg_tys
    (skel : fn_sig) : string * typ list * typ option =
  if skel.fn_tparams = [] then
    (skel.mangled, skel.param_tys, skel.ret_ty)
  else begin
    let n_fixed = List.length skel.param_tys in
    let take n xs =
      let rec loop n acc = function
        | _ when n <= 0 -> List.rev acc
        | [] -> List.rev acc
        | x :: rest -> loop (n - 1) (x :: acc) rest
      in
      loop n [] xs
    in
    let inf_pairs = List.combine skel.param_tys (take n_fixed arg_tys) in
    let seed_pairs =
      match expected, skel.ret_ty with
      | Some exp, Some r -> [(r, exp)]
      | _ -> []
    in
    let inferred =
      Mono.infer_tparams ~pos skel.fn_tparams (seed_pairs @ inf_pairs)
    in
    let bindings = List.combine skel.fn_tparams inferred in
    let func =
      match List.assoc_opt skel.mangled ctx.fn_asts with
      | Some (_, f) -> f
      | None ->
          Error.failf pos
            "internal: missing AST for generic fn '%s'" skel.mangled
    in
    let inst =
      Mono.instantiate_fn ctx.instances
        ~path:resolved_mod ~func ~skel ~bindings ~origin_pos:pos
    in
    (inst.mangled, inst.param_tys, inst.ret_ty)
  end


(* Elaborate Ast.expr → texpr.  Each typed node carries its type in
   `.ty`, so codegen never has to re-run typing.  Single source of
   truth for both type computation and tree elaboration — used to
   coexist with a separate `type_of` function which produced
   O(N²) elab cost on deeply-nested expressions; that function and
   its helpers (binop_operand_types, validate_struct_lit, desugar_orelse)
   are gone, with their validation logic inlined per-case here. *)
let rec elab_expr ?(allow_void = false) ?expected ctx env e : texpr =
  let pos = Ast.expr_pos e in
  match e with
  | Ast.IntLit (n, _) -> { e = TIntLit n; ty = t_i32; pos }
  | Ast.BoolLit (b, _) -> { e = TBoolLit b; ty = TBool; pos }
  | Ast.NullLit _ -> { e = TNullLit; ty = TNullPtr; pos }
  | Ast.StringLit (s, _) -> { e = TStringLit s; ty = TString; pos }
  | Ast.Neg (sub, neg_pos) ->
      let sub' = elab_expr ctx env sub in
      if not (is_int_like sub'.ty) then
        Error.failf neg_pos "negation '-' requires an integer, got %s"
          (typ_name sub'.ty);
      { e = TNeg sub'; ty = sub'.ty; pos }
  | Ast.BinOp (op, l, r, _) ->
      (* Smart literal coercion: when one operand is an integer
         literal, elab the other first so the literal can adopt a
         matching TInt width.  Without this, `let x: i16 = 5 + y`
         (y: i16) would build the literal as t_i32 and fail the
         later equality check. *)
      let elab_lit n other_ty other_pos =
        let lit_ty =
          match other_ty with
          | TInt _ when int_fits n other_ty -> other_ty
          | _ -> t_i32
        in
        { e = TIntLit n; ty = lit_ty; pos = other_pos }
      in
      let (l', r') =
        match l, r with
        | Ast.IntLit (n, lp), _ ->
            let r' = elab_expr ctx env r in
            (elab_lit n r'.ty lp, r')
        | _, Ast.IntLit (n, rp) ->
            let l' = elab_expr ctx env l in
            (l', elab_lit n l'.ty rp)
        | _ -> (elab_expr ctx env l, elab_expr ctx env r)
      in
      let name = Ast.binop_name op in
      let need_int_operands () =
        if not (is_int_like l'.ty && is_int_like r'.ty) then
          Error.failf pos
            "operator '%s' requires integer operands, got %s and %s"
            name (typ_name l'.ty) (typ_name r'.ty)
      in
      (* For TInt × TInt with same signedness but mixed width, the
         result takes the wider operand's type (C-style promotion).
         Any other type combination is rejected — `as` casts are the
         only way to cross sign or kind boundaries (c_int ↔ TInt,
         signed ↔ unsigned). *)
      let promote_int_widen () =
        match l'.ty, r'.ty with
        | TInt a, TInt b when a.signed = b.signed ->
            if int_width_bits a.width >= int_width_bits b.width
            then TInt a else TInt b
        | _ ->
            Error.failf pos
              "operator '%s' between incompatible types %s and %s"
              name (typ_name l'.ty) (typ_name r'.ty)
      in
      let result_t =
        match op with
        | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div ->
            need_int_operands ();
            if typ_eq l'.ty r'.ty then l'.ty
            else promote_int_widen ()
        | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq ->
            need_int_operands ();
            (if not (typ_eq l'.ty r'.ty) then
               ignore (promote_int_widen ()));
            TBool
        | Ast.EqEq | Ast.NotEq ->
            if not (typ_eq l'.ty r'.ty) then
              Error.failf pos
                "equality '%s' between incompatible types %s and %s"
                name (typ_name l'.ty) (typ_name r'.ty);
            TBool
      in
      { e = TBinOp (op, l', r'); ty = result_t; pos }
  | Ast.Cast (sub, ann, cast_pos) ->
      let sub' = elab_expr ctx env sub in
      let tgt = resolve_type_ann ctx ann in
      if is_int_like sub'.ty && is_int_like tgt then ()
      else if is_ptr sub'.ty && is_ptr tgt then ()
      else
        Error.failf cast_pos
          "cannot cast %s to %s (only integer-to-integer or \
           pointer-to-pointer casts supported)"
          (typ_name sub'.ty) (typ_name tgt);
      { e = TCast (sub', ann); ty = tgt; pos }
  | Ast.Ref (sub, _) ->
      let sub' = elab_expr ctx env sub in
      { e = TRef sub'; ty = TPtr sub'.ty; pos }
  | Ast.Deref (sub, deref_pos) ->
      let sub' = elab_expr ctx env sub in
      let ty =
        match sub'.ty with
        | TPtr t -> t
        | TNullPtr -> Error.failf deref_pos "cannot deref 'null'"
        | other ->
            Error.failf deref_pos "deref '*' requires a pointer, got %s"
              (typ_name other)
      in
      { e = TDeref sub'; ty; pos }
  | Ast.TupleLit (es, _) ->
      let es' = List.map (elab_expr ctx env) es in
      let ty = TTuple (List.map (fun (e : texpr) -> e.ty) es') in
      { e = TTupleLit es'; ty; pos }
  | Ast.SizeOf (ann, _) ->
      (* `size_of(T)` yields a c_uint constant. *)
      let t = resolve_type_ann ctx ann in
      { e = TSizeOf t; ty = TCInt { signed = false }; pos }
  | Ast.Var (n, var_pos) ->
      (* Local / extern const / extern var → TVar; top-level fn name →
         TFnRef (auto-converts to fn-ptr in C).  Matches the lookup order
         used by type_of's Var case for consistency. *)
      (match List.assoc_opt n env with
       | Some t -> { e = TVar n; ty = t; pos }
       | None ->
           (match List.assoc_opt n ctx.ext_consts with
            | Some t -> { e = TVar n; ty = t; pos }
            | None ->
                (match List.assoc_opt n ctx.ext_vars with
                 | Some t -> { e = TVar n; ty = t; pos }
                 | None ->
                     (match lookup_fn ctx [n] with
                      | Some (_, { mangled; param_tys; ret_ty; _ }) ->
                          { e = TFnRef mangled;
                            ty = TFnPtr { params = param_tys; ret = ret_ty };
                            pos }
                      | None ->
                          Error.failf var_pos "undefined variable '%s'" n))))
  | Ast.Orelse (value, default, or_pos) ->
      (* `value orelse default` is a 2-arm match over Option/Result.
         We build the TMatch directly from elab'd children to avoid
         a desugar→re-elab roundtrip (which used to be a second
         traversal of `value`). *)
      let tvalue = elab_expr ctx env value in
      let (ename_path, ok_name, err_name, err_binds) =
        match tvalue.ty with
        | TEnum p when Mono.is_instance_of ["Option"] p ->
            (p, "Some", "None", [])
        | TEnum p when Mono.is_instance_of ["Result"] p ->
            (p, "Ok", "Err", [ ("_0", TPWildcard) ])
        | other ->
            Error.failf or_pos
              "'orelse' requires an Option or Result value, got %s"
              (typ_name other)
      in
      let e_sig =
        match resolve_enum_by_path ctx ename_path with
        | Some e -> e
        | None -> Error.failf or_pos
                    "internal: Option/Result instance missing"
      in
      let (ok_tag, ok_vsig) =
        match find_variant e_sig ok_name with
        | Some r -> r
        | None -> Error.failf or_pos
                    "internal: '%s' missing on '%s'"
                    ok_name (String.concat "::" ename_path)
      in
      let (err_tag, _) =
        match find_variant e_sig err_name with
        | Some r -> r
        | None -> Error.failf or_pos
                    "internal: '%s' missing on '%s'"
                    err_name (String.concat "::" ename_path)
      in
      let ok_payload_ty =
        match ok_vsig.vsfields with
        | [(_, t)] -> t
        | _ -> Error.failf or_pos
                 "internal: Ok/Some payload shape"
      in
      let bind_name = "__orelse_v" in
      let ok_arm = {
        tpat = TPVariant {
          variant = ok_name; tag = ok_tag;
          binds = [ ("_0", TPVar bind_name) ];
        };
        tbody = { e = TVar bind_name; ty = ok_payload_ty; pos = or_pos };
        tdiverges = false;
        tarm_pos = or_pos;
      } in
      let default_env = (bind_name, ok_payload_ty) :: env in
      let _ = default_env in
      let tdefault =
        elab_expr ~allow_void ?expected ctx env default
      in
      if not (typ_eq tdefault.ty ok_payload_ty)
         && not (int_lit_fits default ok_payload_ty)
      then
        Error.failf or_pos
          "orelse arms have inconsistent types: %s vs %s"
          (typ_name ok_payload_ty) (typ_name tdefault.ty);
      let err_arm = {
        tpat = TPVariant {
          variant = err_name; tag = err_tag; binds = err_binds;
        };
        tbody = tdefault;
        tdiverges = false;
        tarm_pos = or_pos;
      } in
      { e = TMatch {
          scrutinee = tvalue; ename_path; arms = [ ok_arm; err_arm ];
        };
        ty = ok_payload_ty; pos }
  | Ast.Match { scrutinee; arms; pos = match_pos } ->
      if arms = [] then
        Error.failf match_pos "'match' must have at least one arm";
      let tscrut = elab_expr ctx env scrutinee in
      let ename_path =
        match tscrut.ty with
        | TEnum p -> p
        | other ->
            Error.failf match_pos
              "'match' requires an enum value, got %s" (typ_name other)
      in
      let e_sig =
        match resolve_enum_by_path ctx ename_path with
        | Some e -> e
        | None ->
            Error.failf match_pos
              "internal: enum '%s' missing from index"
              (String.concat "::" ename_path)
      in
      let lower_subpat = function
        | Ast.PWildcard _ -> TPWildcard
        | Ast.PVar (n, _) -> TPVar n
        | Ast.PVariant { pos = ppos; _ } ->
            Error.failf ppos
              "nested variant patterns are not yet supported"
      in
      let tarms =
        List.map (fun (a : Ast.match_arm) ->
          let (tpat, arm_env) =
            match a.pat with
            | Ast.PWildcard _ -> (TPWildcard, env)
            | Ast.PVar (n, _) -> (TPVar n, (n, tscrut.ty) :: env)
            | Ast.PVariant { tname; variant; binds; pos = ppos } ->
                (* Pattern's enum must match (or be an instance of) the
                   scrutinee's enum. *)
                let resolved =
                  match lookup_enum ctx tname with
                  | Some e' -> e'.ename_path
                  | None ->
                      Error.failf ppos "unknown enum '%s' in pattern"
                        (String.concat "::" tname)
                in
                if not (Mono.is_instance_of resolved ename_path) then
                  Error.failf ppos
                    "pattern matches '%s' but scrutinee has type '%s'"
                    (String.concat "::" resolved)
                    (String.concat "::" ename_path);
                let (tag, vsig) =
                  match find_variant e_sig variant with
                  | Some r -> r
                  | None ->
                      Error.failf ppos "enum '%s' has no variant '%s'"
                        (String.concat "::" ename_path) variant
                in
                (* Validate bind syntax against variant kind, then build
                   ordered (name, sub-pattern) pairs. *)
                let ordered_binds =
                  match binds, vsig.vsis_struct with
                  | Ast.PBTuple ps, false ->
                      let expected_n = List.length vsig.vsfields in
                      let got = List.length ps in
                      if expected_n <> got then
                        Error.failf ppos
                          "variant '%s' has %d field(s), pattern binds %d"
                          variant expected_n got;
                      List.map2 (fun (n, _) p -> (n, p)) vsig.vsfields ps
                  | Ast.PBStruct entries, true ->
                      let expected_names = List.map fst vsig.vsfields in
                      let got_names = List.map fst entries in
                      (match find_dup ~key:Fun.id got_names with
                       | Some n ->
                           Error.failf ppos
                             "duplicate field '%s' in pattern" n
                       | None -> ());
                      List.iter (fun (n, _) ->
                        if not (List.mem n expected_names) then
                          Error.failf ppos
                            "variant '%s' has no field '%s'" variant n)
                        entries;
                      List.map (fun n ->
                        match List.assoc_opt n entries with
                        | Some p -> (n, p)
                        | None -> (n, Ast.PWildcard ppos))
                        expected_names
                  | Ast.PBTuple _, true ->
                      Error.failf ppos
                        "variant '%s' is a struct variant; \
                         match it with '{ field: pat }', not '(...)'"
                        variant
                  | Ast.PBStruct _, false ->
                      Error.failf ppos
                        "variant '%s' is a tuple variant; \
                         match it with '(...)', not '{ field: pat }'"
                        variant
                in
                let tbinds =
                  List.map (fun (n, p) -> (n, lower_subpat p))
                    ordered_binds
                in
                (* Duplicate bind name check across the pattern. *)
                let bind_names =
                  List.filter_map (fun (_, p) ->
                    match p with Ast.PVar (n, _) -> Some n | _ -> None)
                    ordered_binds
                in
                (match find_dup ~key:Fun.id bind_names with
                 | Some n ->
                     Error.failf ppos
                       "duplicate bind name '%s' in pattern" n
                 | None -> ());
                let env' =
                  List.fold_left2 (fun acc (_, bp) (_, ft) ->
                    match bp with
                    | Ast.PVar (n, _) -> (n, ft) :: acc
                    | _ -> acc)
                    env ordered_binds vsig.vsfields
                in
                (TPVariant { variant; tag; binds = tbinds }, env')
          in
          let tbody = elab_expr ~allow_void ctx arm_env a.body in
          { tpat; tbody; tdiverges = false; tarm_pos = a.arm_pos })
          arms
      in
      (* Exhaustiveness: every variant must be reached.  A wildcard or
         bare-bind pattern covers all remaining variants. *)
      let has_catchall =
        List.exists (fun (a : Ast.match_arm) ->
          match a.pat with
          | Ast.PWildcard _ | Ast.PVar _ -> true
          | _ -> false)
          arms
      in
      if not has_catchall then begin
        let covered =
          List.filter_map (fun (a : Ast.match_arm) ->
            match a.pat with
            | Ast.PVariant { variant; _ } -> Some variant
            | _ -> None)
            arms
        in
        let missing =
          List.filter
            (fun (vs : variant_sig) -> not (List.mem vs.vsname covered))
            e_sig.evariants
        in
        if missing <> [] then
          Error.failf match_pos
            "non-exhaustive 'match': variant(s) %s not covered \
             (add an arm or '_')"
            (String.concat ", "
               (List.map (fun (vs : variant_sig) -> vs.vsname) missing))
      end;
      (* All non-diverging arm bodies must agree on a type — pick the
         first as the witness. *)
      let non_div =
        List.filter_map (fun (a : tmatch_arm) ->
          if a.tdiverges then None else Some a.tbody)
          tarms
      in
      let result_ty =
        match non_div with
        | [] -> t_i32   (* all diverge — placeholder; outer caller discards *)
        | b0 :: rest ->
            List.iter (fun (b : texpr) ->
              if not (typ_eq b.ty b0.ty) then
                Error.failf match_pos
                  "match arms have inconsistent types: %s vs %s"
                  (typ_name b0.ty) (typ_name b.ty))
              rest;
            b0.ty
      in
      { e = TMatch { scrutinee = tscrut; ename_path; arms = tarms };
        ty = result_ty; pos }
  | Ast.Try (value, try_pos) ->
      (* Lower to a TMatch with one yielding Ok/Some arm and one
         diverging Err/None arm.  The diverging arm carries the
         early-return value (an EnumLit on the *outer* fn's ret
         instance); codegen emits `return tbody;` for it.  ret_ty
         must be set and match the inner enum (Option↔Option,
         Result with same E↔Result with same E). *)
      let tinner = elab_expr ctx env value in
      let inner_path =
        match tinner.ty with
        | TEnum p -> p
        | _ ->
            Error.failf try_pos
              "'try' requires Option or Result, got %s" (typ_name tinner.ty)
      in
      let inner_skel =
        if Mono.is_instance_of ["Option"] inner_path then ["Option"]
        else if Mono.is_instance_of ["Result"] inner_path then ["Result"]
        else
          Error.failf try_pos
            "'try' requires Option or Result, got %s" (typ_name tinner.ty)
      in
      let outer_path =
        match ctx.ret_ty with
        | Some (TEnum p) when Mono.is_instance_of inner_skel p -> p
        | Some other ->
            Error.failf try_pos
              "'try' on %s value but enclosing fn returns %s — they must \
               share the same Option/Result shape"
              (typ_name tinner.ty) (typ_name other)
        | None ->
            Error.failf try_pos
              "'try' is only allowed in fns that return Option or Result \
               (this fn has no return type)"
      in
      let inner_e =
        match resolve_enum_by_path ctx inner_path with
        | Some e -> e
        | None -> Error.failf try_pos "internal: inner enum missing"
      in
      let outer_e =
        match resolve_enum_by_path ctx outer_path with
        | Some e -> e
        | None -> Error.failf try_pos "internal: outer enum missing"
      in
      let find_v e name =
        match find_variant e name with
        | Some r -> r
        | None -> Error.failf try_pos "internal: variant '%s' missing" name
      in
      let (ok_name, err_name) =
        if inner_skel = ["Option"] then ("Some", "None")
        else ("Ok", "Err")
      in
      let (ok_tag, ok_vs) = find_v inner_e ok_name in
      let (err_tag_in, err_vs_in) = find_v inner_e err_name in
      let (outer_err_tag, outer_err_vs) = find_v outer_e err_name in
      let payload_compat (in_fs : (string * typ) list)
                         (out_fs : (string * typ) list) =
        List.length in_fs = List.length out_fs
        && List.for_all2 (fun (_, a) (_, b) -> typ_eq a b) in_fs out_fs
      in
      if not (payload_compat err_vs_in.vsfields outer_err_vs.vsfields) then
        Error.failf try_pos
          "'try' Err/None payload mismatch: inner %s vs outer %s"
          (typ_name (TEnum inner_path)) (typ_name (TEnum outer_path));
      let ok_payload_ty =
        match ok_vs.vsfields with
        | [(_, t)] -> t
        | _ -> Error.failf try_pos "internal: Ok/Some payload shape"
      in
      let try_bind = "__try_v" in
      let ok_arm_pat =
        TPVariant {
          variant = ok_name;
          tag = ok_tag;
          binds = [ ("_0", TPVar try_bind) ];
        }
      in
      let ok_body = { e = TVar try_bind; ty = ok_payload_ty; pos = try_pos } in
      let ok_arm = {
        tpat = ok_arm_pat; tbody = ok_body;
        tdiverges = false; tarm_pos = try_pos;
      } in
      let try_err_bind = "__try_e" in
      let (err_arm_pat, outer_err_args) =
        if inner_skel = ["Option"] then
          (TPVariant { variant = err_name; tag = err_tag_in; binds = [] }, [])
        else
          (TPVariant {
              variant = err_name;
              tag = err_tag_in;
              binds = [ ("_0", TPVar try_err_bind) ];
            },
           let e_ty =
             match err_vs_in.vsfields with
             | [(_, t)] -> t
             | _ -> Error.failf try_pos "internal: Err payload shape"
           in
           [ ("_0", { e = TVar try_err_bind; ty = e_ty; pos = try_pos }) ])
      in
      let err_body = {
        e = TEnumLit {
          ename_path = outer_path;
          variant = err_name;
          tag = outer_err_tag;
          args = outer_err_args;
        };
        ty = TEnum outer_path;
        pos = try_pos;
      } in
      let err_arm = {
        tpat = err_arm_pat; tbody = err_body;
        tdiverges = true; tarm_pos = try_pos;
      } in
      let node = TMatch {
        scrutinee = tinner;
        ename_path = inner_path;
        arms = [ ok_arm; err_arm ];
      } in
      { e = node; ty = ok_payload_ty; pos }
  | (Ast.StructLit { tname; fields; base; pos = lit_pos } as raw_lit)
  | (Ast.New { tname; fields; base; pos = lit_pos } as raw_lit) ->
      (* Enum struct-variant ctor: parser emits StructLit for
         `Foo::V { f: e }`; if the path resolves to an enum variant,
         re-elab via the EnumLit branch. *)
      let as_struct_lit = match raw_lit with
        | Ast.StructLit _ -> true | _ -> false
      in
      (match if as_struct_lit
             then rewrite_struct_lit_as_enum_lit ctx tname fields base lit_pos
             else None with
       | Some e -> elab_expr ~allow_void ?expected ctx env e
       | None ->
           let display = String.concat "::" tname in
           let s = match lookup_struct ctx tname with
             | Some s -> s
             | None -> Error.failf lit_pos "unknown struct '%s'" display
           in
           let parent = parent_path s.sname_path in
           if (not s.sis_pub) && not (is_prefix parent ctx.scope) then
             Error.failf lit_pos "struct '%s' is private to module '%s'"
               display
               (if parent = [] then "<root>"
                else String.concat "::" parent);
           (match find_dup ~key:fst fields with
            | Some n ->
                Error.failf lit_pos
                  "duplicate field '%s' in struct literal '%s'" n display
            | None -> ());
           let provided = List.map fst fields in
           let expected_names = List.map fst s.sfields_ty in
           let missing =
             List.filter (fun n -> not (List.mem n provided)) expected_names
           in
           let extra =
             List.filter (fun n -> not (List.mem n expected_names)) provided
           in
           if extra <> [] then
             Error.failf lit_pos
               "struct literal '%s' has unknown field(s): %s"
               display (String.concat ", " extra);
           (* Elab fields (preserve source order) and optional base. *)
           let tfields =
             List.map (fun (fn, fe) -> (fn, elab_expr ctx env fe)) fields
           in
           let tbase = Option.map (elab_expr ctx env) base in
           (* Generic struct instantiation: infer tparams from elab'd
              field types, build a fresh monomorphic struct_sig. *)
           let s_concrete =
             if s.stparams = [] then s
             else begin
               if tbase <> None then
                 Error.failf lit_pos
                   "'..base' on a generic struct '%s' is not yet supported \
                    (annotate the value's type and re-create the literal)"
                   display;
               if missing <> [] then
                 Error.failf lit_pos
                   "struct literal '%s' missing field(s): %s"
                   display (String.concat ", " missing);
               let pairs =
                 List.map (fun (fn, (fe : texpr)) ->
                   let decl_t = List.assoc fn s.sfields_ty in
                   (decl_t, fe.ty))
                   tfields
               in
               let inferred =
                 Mono.infer_tparams ~pos:lit_pos s.stparams pairs
               in
               Mono.instantiate_struct ctx.instances s inferred
             end
           in
           (match tbase with
            | None ->
                if missing <> [] && s.stparams = [] then
                  Error.failf lit_pos
                    "struct literal '%s' missing field(s): %s"
                    display (String.concat ", " missing)
            | Some be ->
                (match be.ty with
                 | TStruct path when path = s_concrete.sname_path -> ()
                 | _ ->
                     Error.failf lit_pos
                       "'..base' in struct literal '%s' expects a value \
                        of type %s, got %s"
                       display display (typ_name be.ty)));
           (* Per-field type check against the concrete struct_sig.
              int_lit_fits stays on the original Ast literal so the
              compatibility check matches the existing behavior. *)
           List.iter2 (fun (fn, fe) (fn', (te : texpr)) ->
             assert (fn = fn');
             let fty = List.assoc fn s_concrete.sfields_ty in
             if not (typ_eq te.ty fty) && not (int_lit_fits fe fty) then
               Error.failf lit_pos
                 "field '%s' of struct '%s': expected %s, got %s"
                 fn display (typ_name fty) (typ_name te.ty))
             fields tfields;
           let sname_path = s_concrete.sname_path in
           let result_node =
             if as_struct_lit
             then TStructLit { sname_path; fields = tfields; base = tbase }
             else TNew { sname_path; fields = tfields; base = tbase }
           in
           let result_ty =
             let st = TStruct sname_path in
             if as_struct_lit then st else TPtr st
           in
           { e = result_node; ty = result_ty; pos })
  | Ast.EnumLit { tname; variant; args; pos = lit_pos } ->
      (* Fall-back BEFORE enum lookup: qualified `raw::DOSBase` with no
         payload is a reference to an extern var/const, not a unit
         variant.  Last segment is the C symbol name; ext_* tables are
         flat. *)
      let extern_global = match args with
        | Ast.EATuple [] when tname <> [] ->
            (match List.assoc_opt variant ctx.ext_vars with
             | Some t -> Some t
             | None -> List.assoc_opt variant ctx.ext_consts)
        | _ -> None
      in
      (match extern_global with
       | Some t -> { e = TVar variant; ty = t; pos }
       | None ->
           let e_sig = match lookup_enum ctx tname with
             | Some e -> e
             | None ->
                 Error.failf lit_pos "unknown enum '%s'"
                   (String.concat "::" tname)
           in
           let v = match List.find_opt
                     (fun (vs : variant_sig) -> vs.vsname = variant)
                     e_sig.evariants with
             | Some v -> v
             | None ->
                 Error.failf lit_pos "enum '%s' has no variant '%s'"
                   (String.concat "::" e_sig.ename_path) variant
           in
           let display =
             String.concat "::" e_sig.ename_path ^ "::" ^ variant
           in
           (* Pre-elab args once; subsequent generic inference and
              type-checking read .ty directly. *)
           let elab_args =
             match args with
             | Ast.EATuple es -> `Tuple (List.map (elab_expr ctx env) es, es)
             | Ast.EAStruct fs ->
                 `Struct (List.map (fun (n, e) ->
                            (n, elab_expr ctx env e, e)) fs)
           in
           (* Generic enum: infer tparams from elab'd payload types,
              instantiate, then validate against the concrete shape. *)
           let final_enum =
             if e_sig.etparams = [] then e_sig
             else begin
               let pairs =
                 match elab_args with
                 | `Tuple (telabs, _) when not v.vsis_struct ->
                     if List.length telabs <> List.length v.vsfields then []
                     else
                       List.map2 (fun (_, decl_t) (te : texpr) ->
                         (decl_t, te.ty))
                         v.vsfields telabs
                 | `Struct fs when v.vsis_struct ->
                     List.filter_map (fun (n, (te : texpr), _) ->
                       match List.assoc_opt n v.vsfields with
                       | Some decl_t -> Some (decl_t, te.ty)
                       | None -> None)
                       fs
                 | _ -> []
               in
               let seed =
                 match expected with
                 | Some (TEnum exp_path)
                   when Mono.is_instance_of e_sig.ename_path exp_path ->
                     (match resolve_enum_by_path ctx exp_path with
                      | Some exp_inst ->
                          (match exp_inst.einstance_args with
                           | Some inst_args ->
                               List.combine e_sig.etparams inst_args
                           | None -> [])
                      | None -> [])
                 | _ -> []
               in
               let inferred =
                 Mono.infer_tparams ~pos:lit_pos ~seed e_sig.etparams pairs
               in
               Mono.instantiate_enum ctx.instances e_sig inferred
             end
           in
           let (tag, v_used) =
             match find_variant final_enum variant with
             | Some r -> r
             | None ->
                 Error.failf lit_pos
                   "internal: variant '%s' missing from enum '%s' \
                    after instantiation"
                   variant (String.concat "::" final_enum.ename_path)
           in
           let result_path = final_enum.ename_path in
           (* Validate args against the concrete v_used + build TEnumLit
              args list. *)
           let targs =
             match elab_args with
             | `Tuple (telabs, raws) ->
                 if v_used.vsis_struct then
                   Error.failf lit_pos
                     "variant '%s' is a struct variant; construct it \
                      with '{ field: ... }', not with '(...)'" display;
                 let expected_n = List.length v_used.vsfields in
                 let got = List.length telabs in
                 if expected_n <> got then
                   Error.failf lit_pos
                     "variant '%s' takes %d argument(s), got %d"
                     display expected_n got;
                 List.iteri (fun i ((_, exp), (te : texpr)) ->
                   if not (typ_eq exp te.ty)
                      && not (int_lit_fits (List.nth raws i) exp)
                   then
                     Error.failf lit_pos
                       "argument %d of '%s': expected %s, got %s"
                       (i + 1) display (typ_name exp) (typ_name te.ty))
                   (List.combine v_used.vsfields telabs);
                 List.map2 (fun (n, _) te -> (n, te))
                   v_used.vsfields telabs
             | `Struct fs ->
                 if not v_used.vsis_struct then
                   Error.failf lit_pos
                     "variant '%s' is a tuple variant; construct it \
                      with '(...)', not with '{ field: ... }'" display;
                 let expected_names = List.map fst v_used.vsfields in
                 let got_names = List.map (fun (n, _, _) -> n) fs in
                 (match find_dup ~key:Fun.id got_names with
                  | Some n ->
                      Error.failf lit_pos
                        "duplicate field '%s' in '%s' construction"
                        n display
                  | None -> ());
                 List.iter (fun (n, _, _) ->
                   if not (List.mem n expected_names) then
                     Error.failf lit_pos
                       "variant '%s' has no field '%s'" display n) fs;
                 List.iter (fun n ->
                   if not (List.exists (fun (m, _, _) -> m = n) fs) then
                     Error.failf lit_pos
                       "missing field '%s' in '%s' construction" n display)
                   expected_names;
                 List.iter (fun (n, (te : texpr), raw) ->
                   let exp = List.assoc n v_used.vsfields in
                   if not (typ_eq exp te.ty) && not (int_lit_fits raw exp)
                   then
                     Error.failf lit_pos
                       "field '%s' of '%s': expected %s, got %s"
                       n display (typ_name exp) (typ_name te.ty)) fs;
                 (* Emit args in variant's field-declaration order. *)
                 List.map (fun (n, _) ->
                   let (_, te, _) =
                     List.find (fun (m, _, _) -> m = n) fs
                   in
                   (n, te))
                   v_used.vsfields
           in
           { e = TEnumLit { ename_path = result_path; variant; tag;
                            args = targs };
             ty = TEnum result_path; pos })
  | Ast.Call (path, args, call_pos) ->
      (* Enum-ctor dispatch first: a Call whose path resolves to an
         enum variant is rewritten to an EnumLit and elab'd again. *)
      (match rewrite_call_as_enum_lit ctx path args call_pos with
       | Some lowered -> elab_expr ~allow_void ?expected ctx env lowered
       | None ->
           let targs = List.map (elab_expr ctx env) args in
           let arg_tys = List.map (fun (a : texpr) -> a.ty) targs in
           (match lookup_builtin path with
            | Some b ->
                let result_ty =
                  b.bcheck ~ctx ~pos:call_pos ~arg_tys ~allow_void
                in
                let name =
                  match path with [n] -> n | _ -> assert false
                in
                { e = TBuiltinCall { name; args = targs };
                  ty = result_ty; pos }
            | None ->
                (* Indirect call through a fn-ptr value (local / extern
                   const / extern var).  Single-segment path only. *)
                let fnptr_local =
                  match path with
                  | [n] ->
                      (match List.assoc_opt n env with
                       | Some (TFnPtr { params; ret }) ->
                           Some (n, params, ret)
                       | _ ->
                           (match List.assoc_opt n ctx.ext_consts with
                            | Some (TFnPtr { params; ret }) ->
                                Some (n, params, ret)
                            | _ ->
                                (match List.assoc_opt n ctx.ext_vars with
                                 | Some (TFnPtr { params; ret }) ->
                                     Some (n, params, ret)
                                 | _ -> None)))
                  | _ -> None
                in
                (match fnptr_local with
                 | Some (n, params, ret) ->
                     let expected_n = List.length params in
                     let got = List.length args in
                     if expected_n <> got then
                       Error.failf call_pos
                         "function pointer '%s' expects %d argument(s), \
                          got %d"
                         n expected_n got;
                     List.iteri (fun i (exp, act) ->
                       if not (typ_eq exp act)
                          && not (int_lit_fits (List.nth args i) exp)
                       then
                         Error.failf call_pos
                           "argument %d of '%s': expected %s, got %s"
                           (i + 1) n (typ_name exp) (typ_name act))
                       (List.combine params arg_tys);
                     let result_ty =
                       match ret with
                       | Some t -> t
                       | None when allow_void -> t_i32
                       | None ->
                           Error.failf call_pos
                             "'%s' returns void, cannot use as a value" n
                     in
                     { e = TCall { mangled = n; args = targs };
                       ty = result_ty; pos }
                 | None ->
                     let display = String.concat "::" path in
                     (match lookup_fn ctx path with
                      | None ->
                          Error.failf call_pos "unknown function '%s'" display
                      | Some (resolved_mod,
                              ({ fn_pub; fn_variadic; _ } as skel)) ->
                          let (mangled, param_tys, ret_ty) =
                            resolve_call_dispatch ~pos:call_pos ~expected ctx
                              ~resolved_mod ~arg_tys skel
                          in
                          (* Qualified call: each module segment must be
                             visible from the current scope.  Walks the
                             resolved fn's module path (where we actually
                             found the fn). *)
                          (match path with
                           | [_] -> ()
                           | _ ->
                               let rec walk_segments parent = function
                                 | [] -> ()
                                 | seg :: rest ->
                                     let mod_path = parent @ [seg] in
                                     let pub =
                                       match List.assoc_opt mod_path
                                               ctx.modules with
                                       | Some b -> b
                                       | None ->
                                           Error.failf call_pos
                                             "unknown module '%s'"
                                             (String.concat "::" mod_path)
                                     in
                                     if (not pub)
                                        && not (is_prefix parent ctx.scope)
                                     then
                                       Error.failf call_pos
                                         "module '%s' is private (not \
                                          visible from '%s')"
                                         (String.concat "::" mod_path)
                                         (if ctx.scope = [] then "<root>"
                                          else String.concat "::" ctx.scope);
                                     walk_segments mod_path rest
                               in
                               walk_segments [] resolved_mod;
                               if (not fn_pub) && ctx.scope <> resolved_mod
                               then
                                 Error.failf call_pos
                                   "function '%s' is private to module '%s'"
                                   display
                                   (String.concat "::" resolved_mod));
                          let expected_n = List.length param_tys in
                          let got = List.length args in
                          let arity_ok =
                            if fn_variadic then got >= expected_n
                            else got = expected_n
                          in
                          if not arity_ok then
                            Error.failf call_pos
                              (if fn_variadic
                               then "function '%s' expects at least %d \
                                     argument(s), got %d"
                               else "function '%s' expects %d argument(s), \
                                     got %d")
                              display expected_n got;
                          (* Variadic extras pass unchecked. *)
                          let fixed_arg_tys =
                            let rec take n xs =
                              if n <= 0 then []
                              else match xs with
                                | [] -> []
                                | x :: rest -> x :: take (n - 1) rest
                            in
                            take expected_n arg_tys
                          in
                          List.iteri (fun i (exp, act) ->
                            if not (typ_eq exp act)
                               && not (int_lit_fits (List.nth args i) exp)
                            then
                              Error.failf call_pos
                                "argument %d of '%s': expected %s, got %s"
                                (i + 1) display (typ_name exp) (typ_name act))
                            (List.combine param_tys fixed_arg_tys);
                          let result_ty =
                            match ret_ty with
                            | Some t -> t
                            | None when allow_void -> t_i32
                            | None ->
                                Error.failf call_pos
                                  "'%s' returns void, cannot use as a value"
                                  display
                          in
                          { e = TCall { mangled; args = targs };
                            ty = result_ty; pos }))))
  | Ast.MethodCall { receiver; name; args; pos = mc_pos } ->
      let trecv = elab_expr ctx env receiver in
      let struct_path =
        match trecv.ty with
        | TStruct p -> p
        | TPtr (TStruct p) -> p
        | other ->
            Error.failf mc_pos
              "method call '.%s()' requires a struct value or pointer to \
               struct, got %s"
              name (typ_name other)
      in
      let mpath = struct_path @ [name] in
      let display = String.concat "::" mpath in
      let targs = List.map (elab_expr ctx env) args in
      let arg_tys = List.map (fun (a : texpr) -> a.ty) targs in
      let fnptr_field =
        match resolve_struct_by_path ctx struct_path with
        | Some s ->
            (match List.assoc_opt name s.sfields_ty with
             | Some (TFnPtr { params; ret }) -> Some (params, ret)
             | _ -> None)
        | None -> None
      in
      (match lookup_fn ctx mpath with
       | None ->
           (match fnptr_field with
            | Some (params, ret) ->
                let expected_n = List.length params in
                let got = List.length args in
                if expected_n <> got then
                  Error.failf mc_pos
                    "fn-pointer field '%s.%s' expects %d argument(s), got %d"
                    (String.concat "::" struct_path) name expected_n got;
                List.iteri (fun i (exp, act) ->
                  if not (typ_eq exp act)
                     && not (int_lit_fits (List.nth args i) exp)
                  then
                    Error.failf mc_pos
                      "argument %d of '%s.%s': expected %s, got %s"
                      (i + 1) (String.concat "::" struct_path) name
                      (typ_name exp) (typ_name act))
                  (List.combine params arg_tys);
                let result_ty =
                  match ret with
                  | Some t -> t
                  | None when allow_void -> t_i32
                  | None ->
                      Error.failf mc_pos
                        "'%s.%s' returns void, cannot use as a value"
                        (String.concat "::" struct_path) name
                in
                let field_ty =
                  match resolve_struct_by_path ctx struct_path with
                  | Some s ->
                      (match List.assoc_opt name s.sfields_ty with
                       | Some t -> t
                       | None -> assert false)
                  | None -> assert false
                in
                let fn_expr = {
                  e = TFieldAccess { target = trecv; field = name };
                  ty = field_ty; pos = trecv.pos;
                } in
                { e = TIndirectCall { fn_expr; args = targs };
                  ty = result_ty; pos }
            | None ->
                Error.failf mc_pos "no method '%s' on type '%s'"
                  name (String.concat "::" struct_path))
       | Some (resolved_mod, ({ fn_pub; _ } as skel)) ->
           let rec walk_segments parent = function
             | [] -> ()
             | seg :: rest ->
                 let mod_path = parent @ [seg] in
                 (match List.assoc_opt mod_path ctx.modules with
                  | Some pub ->
                      if (not pub) && not (is_prefix parent ctx.scope) then
                        Error.failf mc_pos
                          "type '%s' is private (not visible from '%s')"
                          (String.concat "::" mod_path)
                          (if ctx.scope = [] then "<root>"
                           else String.concat "::" ctx.scope)
                  | None -> ());
                 walk_segments mod_path rest
           in
           walk_segments [] resolved_mod;
           if (not fn_pub) && ctx.scope <> resolved_mod then
             Error.failf mc_pos "method '%s' is private to '%s'"
               name (String.concat "::" resolved_mod);
           let arg_tys_for_dispatch = trecv.ty :: arg_tys in
           let (mangled, inst_param_tys, ret_ty) =
             resolve_call_dispatch ~pos:mc_pos ~expected ctx
               ~resolved_mod ~arg_tys:arg_tys_for_dispatch skel
           in
           let expected_args = List.length inst_param_tys - 1 in
           let got_args = List.length args in
           if expected_args <> got_args then
             Error.failf mc_pos
               "method '%s' takes %d argument(s), got %d"
               display expected_args got_args;
           (match inst_param_tys with
            | self_ty :: rest_params ->
                List.iteri (fun i (exp, act) ->
                  if not (typ_eq exp act)
                     && not (int_lit_fits (List.nth args i) exp)
                  then
                    Error.failf mc_pos
                      "argument %d of '%s': expected %s, got %s"
                      (i + 1) display (typ_name exp) (typ_name act))
                  (List.combine rest_params arg_tys);
                let result_ty =
                  match ret_ty with
                  | Some t -> t
                  | None when allow_void -> t_i32
                  | None ->
                      Error.failf mc_pos
                        "'%s' returns void, cannot use as a value" display
                in
                (* Auto-ref / auto-deref: align receiver shape with the
                   method's self-param shape. *)
                let trecv_adj =
                  match self_ty, trecv.ty with
                  | TStruct _, TStruct _ -> trecv
                  | TPtr _, TPtr _ -> trecv
                  | TPtr _ as pt, TStruct _ ->
                      { e = TRef trecv; ty = pt; pos = trecv.pos }
                  | TStruct _ as st, TPtr _ ->
                      { e = TDeref trecv; ty = st; pos = trecv.pos }
                  | _ -> assert false
                in
                { e = TCall { mangled; args = trecv_adj :: targs };
                  ty = result_ty; pos }
            | [] -> assert false   (* methods always have self in registry *)))
  | Ast.FieldAccess (target, fname, fa_pos) ->
      (* `.field` auto-derefs one level of pointer-to-struct.  Works
         for ordinary structs (consulting struct_index) and for exposed
         extern structs (consulting ext_struct_fields). *)
      let target' = elab_expr ctx env target in
      let field_ty =
        match target'.ty with
        | TStruct p | TPtr (TStruct p) ->
            let s = match resolve_struct_by_path ctx p with
              | Some s -> s
              | None -> Error.failf fa_pos "unknown struct '%s'"
                          (String.concat "::" p)
            in
            (match List.assoc_opt fname s.sfields_ty with
             | Some t -> t
             | None ->
                 Error.failf fa_pos "struct '%s' has no field '%s'"
                   (String.concat "::" p) fname)
        | TExtStruct n | TPtr (TExtStruct n) ->
            (match List.assoc_opt n ctx.ext_struct_fields with
             | None ->
                 Error.failf fa_pos
                   "field access '.%s' on opaque type '%s' — declare \
                    fields with `extern struct %s { ... }` to access them"
                   fname n n
             | Some fs ->
                 (match List.assoc_opt fname fs with
                  | Some t -> t
                  | None ->
                      Error.failf fa_pos "extern struct '%s' has no field '%s'"
                        n fname))
        | other ->
            Error.failf fa_pos
              "field access '.%s' requires a struct value or pointer to \
               struct, got %s"
              fname (typ_name other)
      in
      { e = TFieldAccess { target = target'; field = fname };
        ty = field_ty; pos }

(* Single-walk variant of the old `collect_lets`: it both validates the
   body (mirroring the per-stmt type checks that lived there) and produces
   the elaborated `tstmt list`, alongside the hoisted let-decl list
   that the function-top declarations need.  Replaces `collect_lets` —
   `check_program` calls this once per function. *)
let elab_body ?(ret_ty : typ option = None) ctx param_env stmts
    : (string * typ) list * tstmt list =
  let decls = ref [] in
  let add_decl name t pos =
    check_c_ident pos "variable" name;
    if List.mem_assoc name param_env then
      Error.failf pos "variable '%s' shadows a parameter" name;
    if List.mem_assoc name !decls then
      Error.failf pos "variable '%s' already declared in this function" name;
    decls := (name, t) :: !decls
  in
  let rec walk env = function
    | [] -> (env, [])
    | s :: rest ->
        let (env', ts) = walk_stmt env s in
        let (final_env, rest_ts) = walk env' rest in
        (final_env, ts :: rest_ts)
  and walk_stmt env stmt : (string * typ) list * tstmt =
    match stmt with
    | Ast.Let { name; value; ty_ann; pos } ->
        (* When the let has a type annotation, resolve it first so we
           can pass it as the expected type to elab_expr — that lets
           generic ctors like `Result::Ok(n)` infer all their tparams
           even when the payload alone doesn't determine them. *)
        let expected =
          match ty_ann with
          | Some ann -> Some (resolve_type_ann ctx ann)
          | None -> None
        in
        Option.iter
          (forbid_naked_opaque ~exposed:(List.map fst ctx.ext_struct_fields) pos)
          expected;
        let tvalue = elab_expr ?expected ctx env value in
        let t_inferred = tvalue.ty in
        let t_actual =
          match expected with
          | None ->
              (match t_inferred with
               | TNullPtr ->
                   Error.failf pos
                     "cannot infer pointer type for 'null'; add a type \
                      annotation like 'let %s: *T = null;'" name
               | _ -> t_inferred)
          | Some t_ann ->
              (match expr_int_lit value, t_ann with
               | Some n, TInt _ when int_fits n t_ann -> t_ann
               | Some n, TInt { signed = false; _ } when n < 0 ->
                   Error.failf pos
                     "negative literal %d cannot fit in %s" n (typ_name t_ann)
               | Some n, TInt _ ->
                   Error.failf pos
                     "literal %d does not fit in %s" n (typ_name t_ann)
               | _ ->
                   if typ_eq t_ann t_inferred then t_ann
                   else
                     Error.failf pos
                       "variable '%s' declared as %s but initializer has type %s"
                       name (typ_name t_ann) (typ_name t_inferred))
        in
        add_decl name t_actual pos;
        ((name, t_actual) :: env, TLet { name; value = tvalue; pos })
    | Ast.LetTuple { names; value; pos } ->
        let tvalue = elab_expr ctx env value in
        let elem_tys =
          match tvalue.ty with
          | TTuple ts -> ts
          | other ->
              Error.failf pos
                "destructuring 'let (...)' expects a tuple value, got %s"
                (typ_name other)
        in
        let n_names = List.length names in
        let n_elems = List.length elem_tys in
        if n_names <> n_elems then
          Error.failf pos
            "destructuring 'let (...)' has %d names but value is a %d-tuple"
            n_names n_elems;
        let pairs = List.combine names elem_tys in
        (match find_dup ~key:fst pairs with
         | Some n -> Error.failf pos "duplicate name '%s' in 'let (...)'" n
         | None -> ());
        List.iter (fun (n, ty) -> add_decl n ty pos) pairs;
        (List.rev_append pairs env,
         TLetTuple { names; value = tvalue; pos })
    | Ast.Assign { path; value; pos } ->
        let display = String.concat "::" path in
        let target_ty =
          match path with
          | [name] ->
              (match List.assoc_opt name env with
               | Some t -> Some t
               | None ->
                   (* Single-segment can also be an unqualified extern var
                      (e.g. when bare `DOSBase` is in scope through the
                      flat ext_vars list). *)
                   List.assoc_opt name ctx.ext_vars)
          | _ ->
              (* Qualified path: must resolve to an extern var.  Today
                 ext_vars is flat (last segment is the C symbol name);
                 require the last segment to match an entry. *)
              (match List.rev path with
               | last :: _ -> List.assoc_opt last ctx.ext_vars
               | [] -> None)
        in
        (match target_ty with
         | None ->
             let last_name = match List.rev path with
               | n :: _ -> n
               | [] -> ""
             in
             if List.mem_assoc last_name ctx.ext_consts then
               Error.failf pos
                 "cannot assign to '%s' — it's an `extern const` (use \
                  `extern var` for mutable globals)" display
             else
               Error.failf pos "assignment to undefined variable '%s'" display
         | Some _ -> ());
        let tvalue = elab_expr ctx env value in
        (env, TAssign { path; value = tvalue; pos })
    | Ast.AssignField { target; field; value; pos } ->
        let ttarget = elab_expr ctx env target in
        let fty =
          match ttarget.ty with
          | TStruct p | TPtr (TStruct p) ->
              let s = match resolve_struct_by_path ctx p with
                | Some s -> s
                | None ->
                    Error.failf pos "unknown struct '%s'"
                      (String.concat "::" p)
              in
              (match List.assoc_opt field s.sfields_ty with
               | Some t -> t
               | None ->
                   Error.failf pos "struct '%s' has no field '%s'"
                     (String.concat "::" p) field)
          | TExtStruct n | TPtr (TExtStruct n) ->
              (match List.assoc_opt n ctx.ext_struct_fields with
               | None ->
                   Error.failf pos
                     "assignment to '.%s' on opaque type '%s' — \
                      declare fields with `extern struct %s { ... }`"
                     field n n
               | Some fs ->
                   (match List.assoc_opt field fs with
                    | Some t -> t
                    | None ->
                        Error.failf pos
                          "extern struct '%s' has no field '%s'"
                          n field))
          | other ->
              Error.failf pos
                "assignment to field '.%s' requires a struct value or \
                 pointer to struct, got %s"
                field (typ_name other)
        in
        let tvalue = elab_expr ctx env value in
        if not (typ_eq tvalue.ty fty) && not (int_lit_fits value fty) then
          Error.failf pos
            "field '%s': expected %s, got %s"
            field (typ_name fty) (typ_name tvalue.ty);
        (env, TAssignField { target = ttarget; field;
                                  value = tvalue; pos })
    | Ast.AssignDeref { target; value; pos } ->
        let ttarget = elab_expr ctx env target in
        let inner =
          match ttarget.ty with
          | TPtr t -> t
          | other ->
              Error.failf pos
                "assignment through '*' requires a pointer, got %s"
                (typ_name other)
        in
        let tvalue = elab_expr ctx env value in
        if not (typ_eq tvalue.ty inner) && not (int_lit_fits value inner) then
          Error.failf pos
            "deref assignment: expected %s, got %s"
            (typ_name inner) (typ_name tvalue.ty);
        (env, TAssignDeref { target = ttarget; value = tvalue; pos })
    | Ast.Return (e, pos) ->
        let tvalue = elab_expr ?expected:ret_ty ctx env e in
        (* The bidirectional `expected` seeds inner inference (literal
           widths, generic params, tuple/struct shape) but does not
           constrain the result — the equality check enforces the
           contract at the source pos. *)
        (match ret_ty with
         | Some expected_ty ->
             if not (typ_eq tvalue.ty expected_ty) then
               Error.failf pos
                 "return: expected %s, got %s"
                 (typ_name expected_ty) (typ_name tvalue.ty)
         | None -> ());
        (env, TReturn { value = tvalue; pos })
    | Ast.ExprStmt e ->
        let tvalue = elab_expr ~allow_void:true ctx env e in
        (env, TExprStmt tvalue)
    | Ast.If { cond; then_body; else_body } ->
        let tcond = elab_expr ctx env cond in
        let (_, t_then) = walk env then_body in
        let (_, t_else) = walk env else_body in
        (param_env @ List.rev !decls,
         TIf { cond = tcond; then_body = t_then; else_body = t_else })
    | Ast.While { cond; body } ->
        let tcond = elab_expr ctx env cond in
        let (_, tbody) = walk env body in
        (param_env @ List.rev !decls,
         TWhile { cond = tcond; body = tbody })
    | Ast.Defer { body; pos } ->
        let (_, tbody) = walk env body in
        (env, TDefer { body = tbody; pos })
  in
  let (_, tstmts) = walk param_env stmts in
  (* Post-elab: lift block-shaped sub-expressions (TStructLit, TTupleLit,
     TNew, TEnumLit, TMatch) that appear in positions where codegen
     emits via `gen_expr` (which can't handle them) into preceding
     `__lift_N` let-bindings.  Top-level value of let / return / assign
     stays block-shaped — codegen already handles it via
     `emit_value_into_temp`.  Match arm bodies are NOT walked: lifting
     a sub-expression of an arm body would hoist its evaluation above
     the match, breaking conditional semantics. *)
  let lift_counter = ref 0 in
  let fresh_lift () =
    let n = !lift_counter in
    incr lift_counter;
    Printf.sprintf "__lift_%d" n
  in
  let is_block (e : texpr) =
    match e.e with
    | TStructLit _ | TTupleLit _ | TNew _ | TEnumLit _ | TMatch _ -> true
    | _ -> false
  in
  let rec walk_expr ~allow_top (te : texpr) : texpr * tstmt list =
    let walked, prelude = walk_subs te in
    if is_block walked && not allow_top then
      let n = fresh_lift () in
      decls := (n, walked.ty) :: !decls;
      let lift_let = TLet { name = n; value = walked; pos = walked.pos } in
      ({ te with e = TVar n }, prelude @ [ lift_let ])
    else
      (walked, prelude)
  and walk_subs (te : texpr) : texpr * tstmt list =
    let map_args args =
      let pairs = List.map (walk_expr ~allow_top:false) args in
      (List.map fst pairs, List.concat_map snd pairs)
    in
    let map_fields fields =
      let pairs =
        List.map (fun (n, e) ->
          let (e', p) = walk_expr ~allow_top:false e in
          ((n, e'), p)) fields
      in
      (List.map fst pairs, List.concat_map snd pairs)
    in
    let map_opt = function
      | None -> (None, [])
      | Some e ->
          let (e', p) = walk_expr ~allow_top:false e in
          (Some e', p)
    in
    match te.e with
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _
    | TFnRef _ | TSizeOf _ ->
        (te, [])
    | TNeg sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TNeg sub' }, p)
    | TBinOp (op, l, r) ->
        let (l', pl) = walk_expr ~allow_top:false l in
        let (r', pr) = walk_expr ~allow_top:false r in
        ({ te with e = TBinOp (op, l', r') }, pl @ pr)
    | TCall { mangled; args } ->
        let (args', p) = map_args args in
        ({ te with e = TCall { mangled; args = args' } }, p)
    | TBuiltinCall { name; args } ->
        let (args', p) = map_args args in
        ({ te with e = TBuiltinCall { name; args = args' } }, p)
    | TIndirectCall { fn_expr; args } ->
        let (fn_expr', pf) = walk_expr ~allow_top:false fn_expr in
        let (args', pa) = map_args args in
        ({ te with e = TIndirectCall { fn_expr = fn_expr'; args = args' } },
         pf @ pa)
    | TCast (sub, ann) ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TCast (sub', ann) }, p)
    | TFieldAccess { target; field } ->
        let (t', p) = walk_expr ~allow_top:false target in
        ({ te with e = TFieldAccess { target = t'; field } }, p)
    | TRef sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TRef sub' }, p)
    | TDeref sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TDeref sub' }, p)
    | TTupleLit es ->
        let (es', p) = map_args es in
        ({ te with e = TTupleLit es' }, p)
    | TStructLit { sname_path; fields; base } ->
        let (fields', pf) = map_fields fields in
        let (base', pb) = map_opt base in
        ({ te with e = TStructLit { sname_path; fields = fields';
                                    base = base' } }, pf @ pb)
    | TNew { sname_path; fields; base } ->
        let (fields', pf) = map_fields fields in
        let (base', pb) = map_opt base in
        ({ te with e = TNew { sname_path; fields = fields';
                              base = base' } }, pf @ pb)
    | TEnumLit { ename_path; variant; tag; args } ->
        let (args', p) = map_fields args in
        ({ te with e = TEnumLit { ename_path; variant; tag;
                                  args = args' } }, p)
    | TMatch { scrutinee; ename_path; arms } ->
        let (scr', p) = walk_expr ~allow_top:false scrutinee in
        ({ te with e = TMatch { scrutinee = scr'; ename_path; arms } }, p)
  in
  let rec lift_stmts stmts = List.concat_map lift_stmt stmts
  and lift_stmt = function
    | TLet { name; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TLet { name; value = v'; pos } ]
    | TLetTuple { names; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TLetTuple { names; value = v'; pos } ]
    | TAssign { path; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TAssign { path; value = v'; pos } ]
    | TAssignField { target; field; value; pos } ->
        let (t', pt) = walk_expr ~allow_top:false target in
        let (v', pv) = walk_expr ~allow_top:false value in
        pt @ pv @ [ TAssignField { target = t'; field; value = v'; pos } ]
    | TAssignDeref { target; value; pos } ->
        let (t', pt) = walk_expr ~allow_top:false target in
        let (v', pv) = walk_expr ~allow_top:false value in
        pt @ pv @ [ TAssignDeref { target = t'; value = v'; pos } ]
    | TReturn { value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TReturn { value = v'; pos } ]
    | TExprStmt e ->
        (* Top-level Match in expr-stmt position is handled by
           emit_match_stmt; everything else must be simple. *)
        let allow_top = match e.e with TMatch _ -> true | _ -> false in
        let (e', p) = walk_expr ~allow_top e in
        p @ [ TExprStmt e' ]
    | TIf { cond; then_body; else_body } ->
        let (c', p) = walk_expr ~allow_top:false cond in
        p @ [ TIf { cond = c'; then_body = lift_stmts then_body;
                    else_body = lift_stmts else_body } ]
    | TWhile { cond; body } ->
        let (c', p) = walk_expr ~allow_top:false cond in
        (* If `cond` was block-shaped, the lift evaluates it once before
           the loop — subsequent iterations re-use the temp.  In
           practice cond is bool-typed, so block-shaped conds are
           rare; document this limitation if it bites. *)
        p @ [ TWhile { cond = c'; body = lift_stmts body } ]
    | TDefer { body; pos } ->
        [ TDefer { body = lift_stmts body; pos } ]
  in
  let lifted = lift_stmts tstmts in
  (List.rev !decls, lifted)

(* Result of one walk over the program tree: every function (with its
   absolute module path and mangled C name), every struct, and every
   module (with its pub flag).  Order matches source order at each
   nesting level — preserves the user-visible declaration order in the
   emitted C. *)
type flat = {
  funcs : (string list * Ast.func * string) list;
  structs : (string list * Ast.struct_decl) list;
  ext_structs : Ast.extern_struct list;   (* always top-level, raw C names *)
  ext_types : Ast.extern_type list;       (* `extern type Foo;` aliases *)
  ext_consts : Ast.extern_const list;     (* `extern const NAME: T;` *)
  ext_vars : Ast.extern_var list;         (* `extern var NAME: T;` —
                                             mutable global, l-value *)
  enums : (string list * Ast.enum_decl) list;
  modules : (string list * bool) list;
  c_includes : string list;               (* `@c_include("...")` paths *)
  (* Each `impl` block keeps its enclosing module path; target struct
     resolution (relative-to-scope, ancestor walk-up) happens later
     once the struct index is built. *)
  impls : (string list * Ast.impl_block) list;
  (* `pub use foo::bar;` re-exports.  Each entry is
     (scope, local_name, target_relative_path, decl_pos).  Lookup at
     `scope` for `local_name` redirects to `target_relative_path`
     resolved against the same scope; `decl_pos` points at the `pub
     use` statement and is used by `validate_aliases` to anchor
     unresolved-target errors at the decl site rather than at first
     usage.  Multi-segment local_name not supported (the parser only
     emits single-segment Use names today). *)
  aliases : (string list * string * string list * Pos.t) list;
}

let flatten_items program =
  let funcs = ref [] in
  let structs = ref [] in
  let ext_structs = ref [] in
  let ext_types = ref [] in
  let ext_consts = ref [] in
  let ext_vars = ref [] in
  let enums = ref [] in
  let modules = ref [] in
  let impls = ref [] in
  let c_includes = ref [] in
  (* Uniform "must be at top level" reject — `extern struct/type/const`
     and `@c_include` all share the same constraint with the same
     wording.  Path captured by walk and threaded in. *)
  let in_raw_module path = match List.rev path with
    | "raw" :: _ -> true
    | _ -> false
  in
  let require_raw ~current_path ~kind ~name ~pos =
    if not (in_raw_module current_path) then
      Error.failf pos
        "'extern %s %s' must live inside a `mod raw { ... }` block \
         (FFI hygiene rule); wrap with `mod raw { ... }` and call as \
         `raw::%s` or import via `use raw::*;`"
        kind name name
  in
  let aliases = ref [] in
  let rec walk path items =
    List.iter
      (fun item -> match item with
        | Ast.Function (f : Ast.func) ->
            if f.is_extern then
              require_raw ~current_path:path ~kind:"fn"
                ~name:f.name ~pos:f.pos;
            (* extern fn uses its C-side name (set via `as` rename or
               default to f.name); main stays unprefixed; everything
               else gets ex_/mod__. *)
            let m =
              if f.is_extern then f.c_name
              else if f.name = "main" then "main"
              else mangle path f.name
            in
            funcs := (path, f, m) :: !funcs
        | Ast.Struct s ->
            structs := (path, s) :: !structs
        | Ast.ExternStruct es ->
            require_raw ~current_path:path ~kind:"struct"
              ~name:es.Ast.esname ~pos:es.Ast.espos;
            ext_structs := es :: !ext_structs
        | Ast.ExternType et ->
            require_raw ~current_path:path ~kind:"type"
              ~name:et.Ast.xtname ~pos:et.Ast.xtpos;
            ext_types := et :: !ext_types
        | Ast.ExternConst ec ->
            require_raw ~current_path:path ~kind:"const"
              ~name:ec.Ast.ecname ~pos:ec.Ast.ecpos;
            ext_consts := ec :: !ext_consts
        | Ast.ExternVar ev ->
            require_raw ~current_path:path ~kind:"var"
              ~name:ev.Ast.evname ~pos:ev.Ast.evpos;
            ext_vars := ev :: !ext_vars
        | Ast.Enum e ->
            enums := (path, e) :: !enums
        | Ast.Module m ->
            let mod_path = path @ [m.Ast.mname] in
            modules := (mod_path, m.Ast.mis_pub) :: !modules;
            walk mod_path m.Ast.mitems
        | Ast.Impl ib ->
            impls := (path, ib) :: !impls
        | Ast.CInclude { path = inc_path; pos } ->
            if path <> [] then
              Error.failf pos
                "'@c_include' must be at top level, not inside a module";
            c_includes := inc_path :: !c_includes
        | Ast.Use { path = use_path; is_pub; pos; is_wildcard = false }
            when is_pub ->
            (* `pub use foo::bar;` — single-name re-export.  Record as
               (scope = current walk path, local_name = last segment of
               use_path, target = full use_path).  Lookup at this scope
               for `local_name` will redirect to `use_path` resolved
               against the same scope. *)
            (match List.rev use_path with
             | local_name :: _ ->
                 aliases := (path, local_name, use_path, pos) :: !aliases
             | [] ->
                 Error.failf pos "internal: empty 'pub use' path")
        | Ast.Use { pos; _ } ->
            Error.failf pos
              "internal: 'use' declaration reached codegen unresolved \
               (loader pass missing?)")
      items
  in
  walk [] program;
  { funcs = List.rev !funcs;
    structs = List.rev !structs;
    ext_structs = List.rev !ext_structs;
    ext_types = List.rev !ext_types;
    ext_consts = List.rev !ext_consts;
    ext_vars = List.rev !ext_vars;
    c_includes = List.rev !c_includes;
    enums = List.rev !enums;
    modules = List.rev !modules;
    impls = List.rev !impls;
    aliases = List.rev !aliases }

(* Build the global function index: every function with its module path,
   exile-side name, and signature.  main() is excluded — it is not callable. *)
let build_global_index ~instances ~ext_structs ~ext_types ~ext_consts ~ext_struct_fields ~struct_index ~enum_index ~modules ~aliases flat =
  List.filter_map
    (fun (p, (f : Ast.func), mangled) ->
      if f.name = "main" then None
      else
        let ctx = {
          global = []; structs = struct_index; enums = enum_index;
          modules; scope = p; tparams = f.tparams;
          tvar_bindings = []; fn_asts = []; aliases;
          ext_vars = []; ext_struct_fields;
          instances; ext_structs; ext_types; ext_consts; ret_ty = None;
        } in
        Some
          (p, f.name,
           { param_tys =
               List.map (fun pp -> resolve_type_ann ctx pp.Ast.pty) f.params;
             ret_ty = Option.map (resolve_type_ann ctx) f.ret_ty;
             mangled;
             fn_pub = f.is_pub;
             fn_tparams = f.tparams;
             fn_variadic = f.is_variadic }))
    flat

(* Build the struct registry from the flattened struct declarations.
   Two-pass: first collect every struct's absolute path with empty
   fields, then resolve each declaration's field types against that
   skeleton (and against the enum index, which has no struct deps).
   Two passes are necessary because field types can refer to other
   structs declared in any order, and `resolve_type_ann` needs to see
   them all to rewrite relative paths to absolute. *)
let build_struct_index ~instances ~ext_structs ~ext_types ~ext_consts ~modules ~enums struct_flat =
  let skeleton =
    List.map
      (fun (p, (s : Ast.struct_decl)) ->
        { sname_path = p @ [s.sname];
          sfields_ty = [];
          sis_pub = s.sis_pub;
          stparams = s.stparams;
          sinstance_args = None;
          sis_debug = s.sis_debug })
      struct_flat
  in
  List.map2
    (fun (p, (s : Ast.struct_decl)) skel ->
      let ctx = {
        global = []; structs = skeleton; enums;
        modules; scope = p; tparams = s.stparams;
        tvar_bindings = []; fn_asts = []; aliases = [];
        ext_vars = []; ext_struct_fields = [];
        instances; ext_structs; ext_types; ext_consts; ret_ty = None;
      } in
      { skel with
        sfields_ty =
          List.map (fun (n, t) -> (n, resolve_type_ann ctx t)) s.sfields })
    struct_flat skeleton

(* Build the enum registry.  Like `build_struct_index` we go two-pass:
   first collect every enum's absolute path with empty variants (so
   payload types in any enum can refer to any other enum), then
   resolve each variant's `vfields` against the struct + enum
   skeleton.  Tuple variants synthesise `_0`/`_1`/... names so all
   three forms look the same to codegen; struct variants keep their
   user-given names.  `vsis_struct` lets the constructor type-check
   reject `Foo::V(args)` for a struct variant and vice versa. *)
let build_enum_index ~instances ~ext_structs ~ext_types ~ext_consts ~modules ~struct_index enum_flat =
  let skeleton =
    List.map
      (fun (p, (e : Ast.enum_decl)) ->
        let variants =
          List.map (fun (v : Ast.enum_variant) ->
            { vsname = v.vname; vsfields = []; vsis_struct = false })
            e.evariants
        in
        { ename_path = p @ [e.ename]; evariants = variants;
          eis_pub = e.eis_pub; etparams = e.etparams;
          einstance_args = None;
          eis_must_use = e.emust_use;
          eis_debug = e.eis_debug })
      enum_flat
  in
  List.map2
    (fun (p, (e : Ast.enum_decl)) skel ->
      let ctx = {
        global = []; structs = struct_index; enums = skeleton;
        modules; scope = p; tparams = e.etparams;
        tvar_bindings = []; fn_asts = []; aliases = [];
        ext_vars = []; ext_struct_fields = [];
        instances; ext_structs; ext_types; ext_consts; ret_ty = None;
      } in
      let variants =
        List.map (fun (v : Ast.enum_variant) ->
          let (vsfields, vsis_struct) =
            match v.vkind with
            | Ast.VUnit -> ([], false)
            | Ast.VTuple tys ->
                (List.mapi (fun i t ->
                   ("_" ^ string_of_int i, resolve_type_ann ctx t)) tys,
                 false)
            | Ast.VStruct fs ->
                (List.map (fun (n, t) -> (n, resolve_type_ann ctx t)) fs,
                 true)
          in
          { vsname = v.vname; vsfields; vsis_struct })
          e.evariants
      in
      { skel with evariants = variants })
    enum_flat skeleton

(* Walk a typed function body looking for tuple types in use, deduplicating
   by mangled name; codegen later emits one C struct per unique shape.
   Reads the `.ty` field on each typed expression — no `type_of` dispatch
   needed. *)
let collect_tuple_types_of tfuncs =
  let tup_seen = ref [] in
  let fnptr_seen = ref [] in
  let add_tuple t =
    let name = mangle_typ t in
    if not (List.exists (fun (n, _) -> n = name) !tup_seen) then
      tup_seen := (name, t) :: !tup_seen
  in
  let add_fnptr t =
    let name = mangle_typ t in
    if not (List.exists (fun (n, _) -> n = name) !fnptr_seen) then
      fnptr_seen := (name, t) :: !fnptr_seen
  in
  let rec walk_typ t =
    match t with
    | TTuple ts -> add_tuple t; List.iter walk_typ ts
    | TFnPtr { params; ret } ->
        add_fnptr t;
        List.iter walk_typ params;
        Option.iter walk_typ ret
    | TPtr inner -> walk_typ inner
    | _ -> ()
  in
  let walk_typ_ann ann = walk_typ (type_of_ann ann) in
  let rec walk_texpr (te : texpr) =
    walk_typ te.ty;
    match te.e with
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _
    | TFnRef _ -> ()
    | TSizeOf t -> walk_typ t
    | TNeg sub | TRef sub | TDeref sub | TCast (sub, _) -> walk_texpr sub
    | TBinOp (_, l, r) -> walk_texpr l; walk_texpr r
    | TCall { args; _ } | TBuiltinCall { args; _ } -> List.iter walk_texpr args
    | TIndirectCall { fn_expr; args } ->
        walk_texpr fn_expr; List.iter walk_texpr args
    | TTupleLit es -> List.iter walk_texpr es
    | TStructLit { fields; base; _ } | TNew { fields; base; _ } ->
        List.iter (fun (_, fe) -> walk_texpr fe) fields;
        Option.iter walk_texpr base
    | TFieldAccess { target; _ } -> walk_texpr target
    | TEnumLit { args; _ } ->
        List.iter (fun (_, fe) -> walk_texpr fe) args
    | TMatch { scrutinee; arms; _ } ->
        walk_texpr scrutinee;
        List.iter (fun a -> walk_texpr a.tbody) arms
  in
  let rec walk_tstmt = function
    | TLet { value; _ } | TLetTuple { value; _ }
    | TAssign { value; _ } | TReturn { value; _ }
    | TExprStmt value -> walk_texpr value
    | TAssignField { target; value; _ }
    | TAssignDeref { target; value; _ } ->
        walk_texpr target; walk_texpr value
    | TIf { cond; then_body; else_body } ->
        walk_texpr cond;
        List.iter walk_tstmt then_body;
        List.iter walk_tstmt else_body
    | TWhile { cond; body } ->
        walk_texpr cond; List.iter walk_tstmt body
    | TDefer { body; _ } -> List.iter walk_tstmt body
  in
  List.iter
    (fun tf ->
      Option.iter walk_typ_ann tf.tf_func.Ast.ret_ty;
      List.iter (fun (p : Ast.param) -> walk_typ_ann p.pty) tf.tf_func.params;
      List.iter walk_tstmt tf.tf_body)
    tfuncs;
  (List.rev !tup_seen, List.rev !fnptr_seen)

(* Detect heap usage by scanning the typed bodies for `TNew` expressions or
   builtin `free(p)` calls — both are emitted in C only when one of them is
   present, so codegen can conditionally include `<stdlib.h>`. *)
let uses_heap_of tfuncs =
  let rec walk_texpr (te : texpr) =
    match te.e with
    | TNew _ -> true
    | TBuiltinCall { name = "free"; _ } -> true
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _
    | TFnRef _ | TSizeOf _ -> false
    | TNeg sub | TRef sub | TDeref sub | TCast (sub, _) -> walk_texpr sub
    | TBinOp (_, l, r) -> walk_texpr l || walk_texpr r
    | TCall { args; _ } | TBuiltinCall { args; _ } ->
        List.exists walk_texpr args
    | TIndirectCall { fn_expr; args } ->
        walk_texpr fn_expr || List.exists walk_texpr args
    | TTupleLit es -> List.exists walk_texpr es
    | TStructLit { fields; base; _ } ->
        List.exists (fun (_, fe) -> walk_texpr fe) fields
        || (match base with Some b -> walk_texpr b | None -> false)
    | TFieldAccess { target; _ } -> walk_texpr target
    | TEnumLit { args; _ } ->
        List.exists (fun (_, fe) -> walk_texpr fe) args
    | TMatch { scrutinee; arms; _ } ->
        walk_texpr scrutinee
        || List.exists (fun a -> walk_texpr a.tbody) arms
  in
  let rec walk_tstmt = function
    | TLet { value; _ } | TLetTuple { value; _ }
    | TAssign { value; _ } | TReturn { value; _ }
    | TExprStmt value -> walk_texpr value
    | TAssignField { target; value; _ }
    | TAssignDeref { target; value; _ } ->
        walk_texpr target || walk_texpr value
    | TIf { cond; then_body; else_body } ->
        walk_texpr cond
        || List.exists walk_tstmt then_body
        || List.exists walk_tstmt else_body
    | TWhile { cond; body } ->
        walk_texpr cond || List.exists walk_tstmt body
    | TDefer { body; _ } -> List.exists walk_tstmt body
  in
  List.exists (fun tf -> List.exists walk_tstmt tf.tf_body) tfuncs

(* Resolve `impl` blocks against the struct registry, validate each method
   (self-param shape, name clash with fields, dup methods across blocks),
   and lower them to ordinary fn entries plus virtual-module entries.

   Lowering: a method on `Foo` becomes a regular fn in the global index
   under path = absolute struct path, with mangled name `Foo__method`
   (or `mod__Foo__method` for `Foo` inside a module).  The struct's
   absolute path is registered as a virtual module so qualified call
   visibility walks (`Foo::method(p, ...)`) resolve naturally. *)
let expand_impls ~instances ~ext_structs ~ext_types ~ext_consts flat struct_index enum_index modules =
  let resolved =
    List.map
      (fun (parent_path, ib) ->
        let ctx = {
          global = []; structs = struct_index; enums = enum_index;
          modules; scope = parent_path; tparams = [];
          tvar_bindings = []; fn_asts = []; aliases = [];
          ext_vars = []; ext_struct_fields = [];
          instances; ext_structs; ext_types; ext_consts; ret_ty = None;
        } in
        let s =
          match lookup_struct ctx ib.Ast.itarget with
          | Some s -> s
          | None ->
              Error.failf ib.Ast.ipos
                "unknown struct '%s' in 'impl' block"
                (String.concat "::" ib.Ast.itarget)
        in
        let target_path = s.sname_path in
        let field_names = List.map fst s.sfields_ty in
        let in_block_seen = ref [] in
        List.iter
          (fun (m : Ast.func) ->
            if List.mem m.name field_names then
              Error.failf m.pos
                "method name '%s' clashes with a field on '%s'"
                m.name (String.concat "::" target_path);
            if List.mem m.name !in_block_seen then
              Error.failf m.pos
                "method '%s' already defined in this 'impl' block" m.name;
            in_block_seen := m.name :: !in_block_seen;
            (* When the first param is named `self`, its annotation must
               match `Self` or `*Self`; any other type is a configuration
               error.  Other names for the receiver are allowed (and the
               method becomes a static method — no auto-ref/-deref later). *)
            (match m.params with
             | { pname = "self"; pty = ann } :: _ ->
                 let self_t = resolve_type_ann ctx ann in
                 (match self_t with
                  | TStruct p when p = target_path -> ()
                  | TPtr (TStruct p) when p = target_path -> ()
                  | _ ->
                      Error.failf m.pos
                        "first parameter 'self' must have type '%s' or '*%s', \
                         got %s"
                        (String.concat "::" target_path)
                        (String.concat "::" target_path)
                        (typ_name self_t))
             | _ -> ()))
          ib.Ast.iitems;
        (target_path, s.sis_pub, ib.Ast.iitems))
      flat.impls
  in
  (* Cross-block dup check: same method name on the same struct in two
     different impl blocks. *)
  let seen_methods = Hashtbl.create 16 in
  List.iter
    (fun (target_path, _, methods) ->
      List.iter
        (fun (m : Ast.func) ->
          let key = (target_path, m.name) in
          match Hashtbl.find_opt seen_methods key with
          | Some _ ->
              Error.failf m.pos
                "method '%s' on '%s' already defined in another 'impl' block"
                m.name (String.concat "::" target_path)
          | None -> Hashtbl.add seen_methods key m.pos)
        methods)
    resolved;
  let virtual_modules =
    let seen = ref [] in
    List.filter_map
      (fun (target_path, sis_pub, _) ->
        if List.mem target_path !seen then None
        else (seen := target_path :: !seen;
              Some (target_path, sis_pub)))
      resolved
  in
  let impl_funcs =
    List.concat_map
      (fun (target_path, _, methods) ->
        List.map
          (fun (m : Ast.func) ->
            let mangled = mangle target_path m.name in
            (target_path, m, mangled))
          methods)
      resolved
  in
  (impl_funcs, virtual_modules)

(* Synthetic items prepended to every program before flatten_items.  Gives
   `Option<T>` and `Result<T, E>` for free without a `use` or explicit
   declaration.  Both live at top level (path = []) so users write
   `Option::None`, `Result::Ok(x)` directly.  If a user redeclares one
   at top level we skip our copy — the user's definition wins. *)
let prelude_pos = { Pos.zero with file = "<prelude>" }

(* Mono structs synthesised by `prelude_items`.  Listed here so the
   post-typecheck DCE pass in `check_program` can drop them when no
   user code mentions the type — keeps unrelated programs (hello_world,
   etc.) from carrying an unused `struct ex_Allocator` decl.  Add a
   name when introducing a new mono prelude struct. *)
let prelude_mono_struct_names = ["Allocator"]

let prelude_items () =
  let mk_unit name = {
    Ast.vname = name; vkind = Ast.VUnit; vpos = prelude_pos;
  } in
  let mk_tuple name tys = {
    Ast.vname = name; vkind = Ast.VTuple tys; vpos = prelude_pos;
  } in
  let tvar n = Ast.TyStruct { path = [n]; args = [] } in
  let option_decl = {
    Ast.ename = "Option";
    etparams = ["T"];
    evariants = [ mk_unit "None"; mk_tuple "Some" [tvar "T"] ];
    epos = prelude_pos;
    eis_pub = true;
    etier_hint = Some "core";
    emust_use = true;
    eis_debug = false;
  } in
  let result_decl = {
    Ast.ename = "Result";
    etparams = ["T"; "E"];
    evariants = [ mk_tuple "Ok" [tvar "T"]; mk_tuple "Err" [tvar "E"] ];
    epos = prelude_pos;
    eis_pub = true;
    etier_hint = Some "core";
    emust_use = true;
    eis_debug = false;
  } in
  (* Allocator — uniform pluggable memory interface.  `state` rides on
     every call so allocators with backing data (arenas, pools) can
     reach it; stateless ones (libc) ignore.  Generic methods alloc/
     free are monomorphized per T at call site, so the typed cast +
     `size_of(T)` expand to compile-time constants in the emitted C.
     User code prefers `alloc.alloc::<Foo>()` over raw pointer maths. *)
  let cvoid_ptr = Ast.TyPtr Ast.TyCVoid in
  let cuint = Ast.TyCInt { signed = false } in
  let alloc_struct = {
    Ast.sname = "Allocator";
    stparams = [];
    sfields = [
      ("state", cvoid_ptr);
      ("alloc_fn",
       Ast.TyFnPtr { params = [cvoid_ptr; cuint]; ret = Some cvoid_ptr });
      ("free_fn",
       Ast.TyFnPtr { params = [cvoid_ptr; cvoid_ptr]; ret = None });
    ];
    spos = prelude_pos;
    sis_pub = true;
    stier_hint = Some "core";
    sis_debug = false;
  } in
  (* Method bodies use the fn-ptr-field call syntax (`self.alloc_fn(...)`)
     directly — typecheck routes it through TIndirectCall when the
     receiver's matching field is a TFnPtr. *)
  let pos = prelude_pos in
  let var n = Ast.Var (n, pos) in
  let alloc_body = [
    (* return (self.alloc_fn(self.state, size_of(T))) as *T; *)
    Ast.Return (
      Ast.Cast (
        Ast.MethodCall {
          receiver = var "self"; name = "alloc_fn";
          args = [ Ast.FieldAccess (var "self", "state", pos);
                   Ast.SizeOf (tvar "T", pos) ];
          pos;
        },
        Ast.TyPtr (tvar "T"), pos),
      pos);
  ] in
  let free_body = [
    (* self.free_fn(self.state, p as *c_void); *)
    Ast.ExprStmt (Ast.MethodCall {
      receiver = var "self"; name = "free_fn";
      args = [ Ast.FieldAccess (var "self", "state", pos);
               Ast.Cast (var "p", cvoid_ptr, pos) ];
      pos;
    });
  ] in
  let mk_method name tparams params ret body = {
    Ast.name; c_name = name; tparams; params; ret_ty = ret; body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false; pos;
  } in
  let self_param =
    { Ast.pname = "self";
      pty = Ast.TyStruct { path = ["Allocator"]; args = [] };
      preg = None }
  in
  let alloc_method =
    mk_method "alloc" ["T"] [self_param]
      (Some (Ast.TyPtr (tvar "T"))) alloc_body
  in
  let free_method =
    mk_method "free" ["T"]
      [ self_param;
        { Ast.pname = "p"; pty = Ast.TyPtr (tvar "T"); preg = None } ]
      None free_body
  in
  let alloc_impl = {
    Ast.itarget = ["Allocator"];
    iitems = [alloc_method; free_method];
    ipos = pos;
  } in
  [ Ast.Enum option_decl; Ast.Enum result_decl;
    Ast.Struct alloc_struct; Ast.Impl alloc_impl ]

(* Skip prelude items whose names collide with a user-declared top-level
   enum or struct (and skip the matching `impl` block in that case).
   Matches by name only (top-level path = []). *)
let prepend_prelude (program : Ast.program) : Ast.program =
  let user_top_enum_names =
    List.filter_map
      (fun item -> match item with
        | Ast.Enum e -> Some e.ename
        | _ -> None)
      program
  in
  let user_top_struct_names =
    List.filter_map
      (fun item -> match item with
        | Ast.Struct s -> Some s.sname
        | _ -> None)
      program
  in
  let kept =
    List.filter
      (fun item -> match item with
        | Ast.Enum e -> not (List.mem e.ename user_top_enum_names)
        | Ast.Struct s -> not (List.mem s.sname user_top_struct_names)
        | Ast.Impl ib ->
            (match ib.itarget with
             | [n] -> not (List.mem n user_top_struct_names)
             | _ -> true)
        | _ -> true)
      (prelude_items ())
  in
  kept @ program

let check_program program : tprogram =
  let mono_state = Mono.new_state () in
  let program = prepend_prelude program in
  let flat = flatten_items program in
  (* main() must be at top level, not inside a module. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
      if f.name = "main" && path <> [] then
        Error.raise_ f.pos
          "'main' must be at top level, not inside a module")
    flat.funcs;
  (* Top-level function names land in C unmangled (modulo the `ex_`
     prefix), so they must not collide with C keywords.  Mod-internal fns
     get a `mod__` prefix and are safe. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
      if path = [] then check_c_ident f.pos "function" f.name)
    flat.funcs;
  (* Param names are emitted unprefixed in C parameter lists, so they
     also need the keyword check.  (Local lets are checked inside
     elab_body.) *)
  List.iter
    (fun (_, (f : Ast.func), _) ->
      List.iter
        (fun (p : Ast.param) -> check_c_ident f.pos "parameter" p.pname)
        f.params)
    flat.funcs;
  (* Order: structs need an enum skeleton (struct fields can mention
     enums); enums need a struct skeleton (variant payloads can mention
     structs).  Build a placeholder enum skeleton (paths only) first,
     pass it to build_struct_index, then build the full enum_index
     against the now-resolved struct_index. *)
  let enum_skeleton =
    List.map (fun (p, (e : Ast.enum_decl)) ->
      let variants =
        List.map (fun (v : Ast.enum_variant) ->
          { vsname = v.vname; vsfields = []; vsis_struct = false })
          e.evariants
      in
      { ename_path = p @ [e.ename]; evariants = variants;
        eis_pub = e.eis_pub; etparams = e.etparams;
        einstance_args = None;
        eis_must_use = e.emust_use;
        eis_debug = e.eis_debug })
      flat.enums
  in
  let ext_structs =
    List.map (fun (es : Ast.extern_struct) -> es.esname) flat.ext_structs
  in
  let ext_types =
    List.map (fun (et : Ast.extern_type) -> et.xtname) flat.ext_types
  in
  let ann_only_ctx = {
    global = []; structs = []; enums = []; modules = flat.modules;
    scope = []; tparams = []; tvar_bindings = []; fn_asts = [];
    aliases = [];
    ext_vars = []; ext_struct_fields = [];
    instances = mono_state;
    ext_structs; ext_types; ext_consts = [];
    ret_ty = None;
  } in
  let ext_consts =
    List.map (fun (ec : Ast.extern_const) ->
        (ec.ecname, resolve_type_ann ann_only_ctx ec.ecty))
      flat.ext_consts
  in
  let ext_vars =
    List.map (fun (ev : Ast.extern_var) ->
        (ev.evname, resolve_type_ann ann_only_ctx ev.evty))
      flat.ext_vars
  in
  let ext_struct_fields =
    List.filter_map (fun (es : Ast.extern_struct) ->
        match es.esfields with
        | None -> None
        | Some fs ->
            Some (es.esname,
                  List.map (fun (n, t) ->
                      (n, resolve_type_ann ann_only_ctx t)) fs))
      flat.ext_structs
  in
  let struct_index =
    build_struct_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~modules:flat.modules ~enums:enum_skeleton flat.structs
  in
  let enum_index =
    build_enum_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~modules:flat.modules ~struct_index flat.enums
  in
  let (impl_funcs, virtual_modules) =
    expand_impls ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      flat struct_index enum_index flat.modules
  in
  let modules = flat.modules @ virtual_modules in
  let all_funcs = flat.funcs @ impl_funcs in
  (* Method param names also need the C-keyword check (their first param
     is `self`, which is fine; rest are user-chosen). *)
  List.iter
    (fun (_, (f : Ast.func), _) ->
      List.iter
        (fun (p : Ast.param) -> check_c_ident f.pos "parameter" p.pname)
        f.params)
    impl_funcs;
  let global =
    build_global_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~ext_struct_fields
      ~struct_index ~enum_index ~modules ~aliases:flat.aliases all_funcs
  in
  (* `pub use foo::bar;` validation.  Building the global index just
     above means every fn/struct/enum has its absolute path on file;
     we can now check each alias's target resolves from the alias's
     own scope and emit a precise error at the alias decl site
     (rather than waiting for the broken short name to surface at
     a call site, where the error confusingly says "unknown function
     'bar'" with no pointer to the bad `pub use`). *)
  List.iter (fun (scope, _local_name, target_path, decl_pos) ->
    let probe_ctx = {
      global; structs = struct_index; enums = enum_index;
      modules; scope; tparams = [];
      tvar_bindings = []; fn_asts = []; aliases = flat.aliases;
      ext_vars; ext_struct_fields;
      instances = mono_state; ext_structs; ext_types; ext_consts;
      ret_ty = None;
    } in
    let resolves =
      lookup_fn probe_ctx target_path <> None
      || lookup_struct probe_ctx target_path <> None
      || lookup_enum probe_ctx target_path <> None
    in
    if not resolves then
      Error.failf decl_pos
        "'pub use %s' refers to unknown item — no fn, struct, or enum \
         with that path is visible from this scope"
        (String.concat "::" target_path))
    flat.aliases;
  (* Side-table for re-elaborating generic fn instances after the main
     loop: keyed by skeleton mangled name, value is (parent path, AST). *)
  let fn_asts =
    List.map (fun (p, (f : Ast.func), mangled) -> (mangled, (p, f))) all_funcs
  in
  (* Build a tfunc from one fn occurrence (skeleton or instance).  For
     generic fns at the top level we still produce a tfunc carrying
     the resolved (TVar-bearing) param/ret types but skip body elab —
     skeleton bodies can contain operations (BinOp, comparisons,
     field access) that aren't well-defined on free TVars.  Codegen
     filters the skeletons out via [is_concrete]; only the instance
     tfuncs (built later from pending jobs) carry real bodies. *)
  let elab_one_fn ~tvar_bindings (path, (f : Ast.func), mangled) =
    let ctx0 = {
      global; structs = struct_index; enums = enum_index;
      modules; scope = path; tparams = f.tparams;
      tvar_bindings; fn_asts; aliases = flat.aliases;
      ext_vars; ext_struct_fields;
      instances = mono_state; ext_structs; ext_types; ext_consts;
      ret_ty = None;
    } in
    let param_tys =
      List.map (fun (p : Ast.param) -> resolve_type_ann ctx0 p.pty) f.params
    in
    let exposed_extern = List.map fst ext_struct_fields in
    List.iter
      (forbid_naked_opaque ~exposed:exposed_extern f.pos)
      param_tys;
    let ret_ty = Option.map (resolve_type_ann ctx0) f.ret_ty in
    Option.iter (forbid_naked_opaque ~exposed:exposed_extern f.pos) ret_ty;
    let ctx = { ctx0 with ret_ty } in
    let param_env =
      List.combine (List.map (fun (p : Ast.param) -> p.pname) f.params)
        param_tys
    in
    let is_skeleton = f.tparams <> [] && tvar_bindings = [] in
    let (lets, tbody) =
      if f.is_extern || is_skeleton then ([], [])
      else elab_body ~ret_ty ctx param_env f.body
    in
    { tf_path = path; tf_func = f; tf_mangled = mangled;
      tf_param_tys = param_tys; tf_ret_ty = ret_ty;
      tf_body = tbody; tf_lets = lets;
      tf_origin_pos = None }
  in
  let skeleton_tfuncs =
    List.map (elab_one_fn ~tvar_bindings:[]) all_funcs
  in
  (* Drain pending fn-instance jobs accumulated during skeleton elab.
     Each job re-runs body elaboration under a context where the fn's
     tparams substitute to concrete types.  New jobs may be queued
     during this drain (recursive generic calls); loop until the queue
     stays empty.  Mangled name on the instance tfunc is the instance's
     C-side name, set by [Mono.instantiate_fn]. *)
  let rec drain acc =
    match Mono.take_pending_fn_jobs mono_state with
    | [] -> List.rev acc
    | jobs ->
        let new_tfuncs =
          List.map (fun (job : Mono.pending_fn_job) ->
            let tf = elab_one_fn ~tvar_bindings:job.pj_bindings
              (job.pj_path, job.pj_func, job.pj_mangled)
            in
            { tf with
              tf_param_tys = job.pj_param_tys;
              tf_ret_ty = job.pj_ret_ty;
              tf_origin_pos = Some job.pj_origin_pos })
            jobs
        in
        drain (List.rev_append new_tfuncs acc)
  in
  let instance_tfuncs = drain [] in
  let tp_funcs = skeleton_tfuncs @ instance_tfuncs in
  let (tp_tuple_types, tp_fnptr_types) = collect_tuple_types_of tp_funcs in
  let tp_uses_heap = uses_heap_of tp_funcs in
  (* Drain monomorphic instances accumulated during resolve_type_ann
     into the program's indexes.  Instances accumulate in reverse
     registration order; reversing puts them in roughly the order
     users wrote them.  Codegen emits them inline with regular
     non-generic decls. *)
  let mono_structs = List.rev mono_state.inst_structs in
  let mono_enums = List.rev mono_state.inst_enums in
  let mono_inst_funcs = List.rev mono_state.inst_funcs in
  let _ = mono_inst_funcs in   (* registered fn-instance signatures live
                                  on the instance tfuncs; the global index
                                  doesn't need them since callers already
                                  emit instance mangled names directly. *)
  (* DCE for prelude mono structs.  Walk every mono tfunc's signature
     and let-types looking for `TStruct ["Allocator"]`; if no such
     reference exists, drop the struct decl AND the impl methods that
     came from the prelude.  Generic skeletons (`alloc<T>`/`free<T>`)
     mention `Allocator` in `self` only as a definition site, so they
     are excluded from the walk; their concrete instances carry the
     reference instead and ARE walked.  Prelude origin is identified
     by `tf_func.pos.file = "<prelude>"`, which is essential: a user's
     `mod Allocator { fn helper() }` registers `tf_path = ["Allocator"]`
     identical to the prelude impl methods, but its origin file differs. *)
  let rec typ_mentions target = function
    | TStruct p -> p = target
    | TPtr inner -> typ_mentions target inner
    | TTuple ts -> List.exists (typ_mentions target) ts
    | TFnPtr { params; ret } ->
        List.exists (typ_mentions target) params
        || (match ret with Some t -> typ_mentions target t | None -> false)
    | _ -> false
  in
  let tfunc_mentions target tf =
    List.exists (typ_mentions target) tf.tf_param_tys
    || (match tf.tf_ret_ty with Some t -> typ_mentions target t | None -> false)
    || List.exists (fun (_, t) -> typ_mentions target t) tf.tf_lets
  in
  let used_in_user_code path =
    List.exists
      (fun tf -> tf.tf_func.Ast.tparams = [] && tfunc_mentions path tf)
      tp_funcs
  in
  let is_from_prelude tf = tf.tf_func.Ast.pos.file = "<prelude>" in
  let struct_drop_set =
    List.filter
      (fun n -> not (used_in_user_code [n]))
      prelude_mono_struct_names
  in
  let struct_index =
    List.filter (fun s ->
      match s.sname_path with
      | [n] when List.mem n struct_drop_set -> false
      | _ -> true)
      struct_index
  in
  let tp_funcs =
    List.filter (fun tf ->
      not (is_from_prelude tf
           && match tf.tf_path with
              | [n] when List.mem n struct_drop_set -> true
              | _ -> false))
      tp_funcs
  in
  (* `@debug` field-type validation.  Every field of a `@debug` struct
     (or every payload of a `@debug` enum variant) must itself be
     printable — primitive int-like, bool, str, pointer (printed as
     address), or another `@debug` aggregate.  Catches the user trying
     to debug a type that references an opaque C handle or an
     un-debug-able aggregate. *)
  let all_structs = struct_index @ mono_structs in
  let all_enums = enum_index @ mono_enums in
  let struct_is_debug path =
    List.exists (fun s -> s.sname_path = path && s.sis_debug) all_structs
  in
  let enum_is_debug path =
    List.exists (fun e -> e.ename_path = path && e.eis_debug) all_enums
  in
  let rec field_ty_ok = function
    | TInt _ | TCInt _ | TCShort _ | TCLong _
    | TCChar | TCSChar | TCUChar | TBool | TString | TPtr _ -> true
    | TStruct p -> struct_is_debug p
    | TEnum p -> enum_is_debug p
    | TTuple ts -> List.for_all field_ty_ok ts
    | TCVoid | TExtStruct _ | TExtAlias _
    | TFnPtr _ | TNullPtr | TVar _ -> false
  in
  List.iter (fun (s : struct_sig) ->
    if s.sis_debug then
      List.iter (fun (fname, fty) ->
        if not (field_ty_ok fty) then
          let pos =
            try (List.find (fun (p, (d : Ast.struct_decl)) ->
                   p @ [d.sname] = s.sname_path) flat.structs
                 |> snd).spos
            with Not_found -> Pos.zero
          in
          Error.failf pos
            "'@debug' struct '%s': field '%s' of type %s is not debug-able \
             (mark the type `@debug`, or remove `@debug` from the struct)"
            (String.concat "::" s.sname_path) fname (typ_name fty))
        s.sfields_ty)
    all_structs;
  List.iter (fun (e : enum_sig) ->
    if e.eis_debug then
      List.iter (fun (vs : variant_sig) ->
        List.iter (fun (fname, fty) ->
          if not (field_ty_ok fty) then
            let pos =
              try (List.find (fun (p, (d : Ast.enum_decl)) ->
                     p @ [d.ename] = e.ename_path) flat.enums
                   |> snd).epos
              with Not_found -> Pos.zero
            in
            Error.failf pos
              "'@debug' enum '%s': variant '%s' payload '%s' of type %s \
               is not debug-able"
              (String.concat "::" e.ename_path) vs.vsname fname (typ_name fty))
          vs.vsfields)
        e.evariants)
    all_enums;
  { tp_funcs;
    tp_struct_decls = flat.structs;
    tp_struct_index = all_structs;
    tp_enum_index = all_enums;
    tp_global = global;
    tp_modules = modules;
    tp_uses_heap;
    tp_tuple_types;
    tp_fnptr_types;
    tp_c_includes = flat.c_includes;
    tp_ext_consts = ext_consts;
    tp_ext_vars = ext_vars }
