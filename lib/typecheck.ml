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
  instances : Mono.state;
  ext_structs : string list;             (* names of `extern struct Foo;`
                                            decls — resolve_type_ann uses
                                            this to map a single-segment
                                            path to TExtStruct *)
  ext_types : string list;               (* names of `extern type Foo;`
                                            aliases — resolve_type_ann
                                            maps a single-segment path
                                            to TExtAlias *)
  ext_consts : (string * typ) list;      (* `extern const NAME: T;` —
                                            looked up in Var expr after
                                            local env miss; codegen
                                            emits raw NAME at use sites *)
  ret_ty : typ option;                   (* enclosing fn's return type;
                                            None for void.  Used by `try`
                                            to validate / construct the
                                            early-return Err/None value. *)
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
   and struct lookup, which only differ in [resolve]. *)
let walk_scope_up ~resolve ctx (path : string list) =
  let (suggested_mod, name) =
    match List.rev path with
    | [] -> failwith "empty path"
    | n :: rest -> (List.rev rest, n)
  in
  let rec walk prefix =
    match resolve (prefix @ suggested_mod) name with
    | Some r -> Some r
    | None ->
        (match prefix with
         | [] -> None
         | _ -> walk (parent_path prefix))
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
let rec forbid_naked_opaque pos = function
  | TExtStruct n ->
      Error.failf pos
        "opaque type '%s' can only be used through a pointer (`*%s`) — \
         exile doesn't know its layout" n n
  | TCVoid ->
      Error.failf pos
        "'c_void' has no values — only `*c_void` is usable as a type"
  | TPtr _ -> ()
  | TTuple ts -> List.iter (forbid_naked_opaque pos) ts
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
let rec resolve_type_ann ctx ann =
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
  | Ast.TyTuple ts -> TTuple (List.map (resolve_type_ann ctx) ts)
  | Ast.TyPtr t -> TPtr (resolve_type_ann ctx t)
  | Ast.TyStruct { path; args = [] } ->
      (* Non-generic case: tparam reference / extern type / extern
         struct / struct / enum / fallback. *)
      (match path with
       | [n] when List.mem n ctx.tparams -> TVar n
       | [n] when List.mem n ctx.ext_types -> TExtAlias n
       | [n] when List.mem n ctx.ext_structs -> TExtStruct n
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
      let resolved_args = List.map (resolve_type_ann ctx) args in
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
  bcheck : pos:Pos.t -> arg_tys:typ list -> allow_void:bool -> typ;
}

let builtin_print = {
  bname = "print";
  bcheck = (fun ~pos ~arg_tys ~allow_void:_ ->
    match arg_tys with
    | [ TTuple _ ] ->
        Error.failf pos
          "cannot print a tuple; destructure with 'let (...)' first"
    | [ TStruct path ] ->
        Error.failf pos
          "cannot print a struct value (%s); print individual fields instead"
          (String.concat "::" path)
    | [ TPtr _ as t ] ->
        Error.failf pos
          "cannot print a pointer value (%s); deref or print a field"
          (typ_name t)
    | [ TNullPtr ] ->
        Error.failf pos "cannot print 'null'"
    | [ TEnum path ] ->
        Error.failf pos
          "cannot print an enum value (%s); match on it and print per variant"
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
  bcheck = (fun ~pos ~arg_tys ~allow_void ->
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

(* type_of returns the type of an expression.  The optional [allow_void] flag
   controls what happens when the outermost expression is a call to a void
   function: with [allow_void:true] (used by ExprStmt) the call is accepted
   and a placeholder type is returned (it will be discarded); otherwise the
   void result is rejected.  Recursive calls into operands always use the
   default — sub-expressions of arithmetic, conditions, print args, function
   args, etc. always need a real value. *)
(* `Foo::Bar(args)` parses as `Call(["Foo"; "Bar"], args)` — the parser
   can't tell a fn call from an enum constructor.  Both `type_of` and
   `elab_expr` use this helper to rewrite Call into EnumLit when the
   path resolves to an enum + variant. *)
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

(* Desugar `value orelse default` into a `match` over Option<_> / Result<_, _>:
     match value {
       | Some(__orelse_v) => __orelse_v
       | None             => default
     }
   (and `Ok(__orelse_v) / Err(_)` for Result).  The scrutinee's enum is
   detected by checking its concrete instance against the prelude
   skeleton paths.  Caller passes the typechecked scrutinee type so we
   don't typecheck `value` twice — but for the AST-level desugar we can
   afford one extra `type_of`; instantiation is idempotent. *)
let desugar_orelse ~scrutinee_ty value default pos =
  let (tname, ok_v, err_v, err_binds) =
    match scrutinee_ty with
    | TEnum p when Mono.is_instance_of ["Option"] p ->
        (["Option"], "Some", "None", Ast.PBTuple [])
    | TEnum p when Mono.is_instance_of ["Result"] p ->
        (["Result"], "Ok", "Err", Ast.PBTuple [ Ast.PWildcard pos ])
    | _ ->
        Error.failf pos
          "'orelse' requires an Option or Result value, got %s"
          (typ_name scrutinee_ty)
  in
  let bind_name = "__orelse_v" in
  let ok_arm = {
    Ast.pat = Ast.PVariant {
      tname; variant = ok_v;
      binds = Ast.PBTuple [ Ast.PVar (bind_name, pos) ];
      pos };
    body = Ast.Var (bind_name, pos);
    arm_pos = pos;
  } in
  let err_arm = {
    Ast.pat = Ast.PVariant {
      tname; variant = err_v; binds = err_binds; pos };
    body = default;
    arm_pos = pos;
  } in
  Ast.Match { scrutinee = value; arms = [ ok_arm; err_arm ]; pos }

let rec type_of ?(allow_void = false) ?expected ctx env = function
  | Ast.IntLit _ -> t_i32
  | Ast.BoolLit _ -> TBool
  | Ast.StringLit _ -> TString
  | Ast.BinOp (op, l, r, _) ->
      let (lt, rt) = binop_operand_types ctx env l r in
      let result_t =
        match lt, rt with
        | TInt a, TInt b when a = b -> TInt a
        | TInt a, TInt b when a.signed = b.signed ->
            if int_width_bits a.width >= int_width_bits b.width
            then TInt a else TInt b
        | _ -> t_i32
      in
      (match op with
       | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> result_t
       | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq | Ast.EqEq | Ast.NotEq -> TBool)
  | Ast.Neg (e, pos) ->
      (match type_of ctx env e with
       | TInt _ as t -> t
       | other ->
           Error.failf pos "negation '-' requires an integer, got %s"
             (typ_name other))
  | Ast.Cast (e, ann, pos) ->
      let src = type_of ctx env e in
      let tgt = resolve_type_ann ctx ann in
      if is_int_like src && is_int_like tgt then tgt
      else
        Error.failf pos
          "cannot cast %s to %s (only integer-to-integer casts supported)"
          (typ_name src) (typ_name tgt)
  | Ast.Var (name, pos) ->
      (match List.assoc_opt name env with
       | Some t -> t
       | None ->
           (* Fall through to globals: `extern const NAME: T;` lives in
              the same name namespace as locals at use sites. *)
           (match List.assoc_opt name ctx.ext_consts with
            | Some t -> t
            | None -> Error.failf pos "undefined variable '%s'" name))
  | Ast.TupleLit (es, _) ->
      TTuple (List.map (type_of ctx env) es)
  | Ast.StructLit { tname; fields; base; pos } ->
      (* `Foo::V { f: e }` parses as a StructLit; if the path resolves
         to an enum variant, redirect to the EnumLit branch with
         struct-form args. *)
      (match rewrite_struct_lit_as_enum_lit ctx tname fields base pos with
       | Some e -> type_of ~allow_void ?expected ctx env e
       | None ->
           let s = validate_struct_lit ctx env ~tname ~fields ~base ~pos in
           TStruct s.sname_path)
  | Ast.New { tname; fields; base; pos } ->
      let s = validate_struct_lit ctx env ~tname ~fields ~base ~pos in
      TPtr (TStruct s.sname_path)
  | Ast.FieldAccess (target, fname, pos) ->
      let tt = type_of ctx env target in
      (* `.field` auto-derefs one level of pointer-to-struct.  Codegen
         emits `target->field` in that case (vs `target.field`). *)
      let path =
        match tt with
        | TStruct p -> p
        | TPtr (TStruct p) -> p
        | _ ->
            Error.failf pos
              "field access '.%s' requires a struct value or pointer to \
               struct, got %s"
              fname (typ_name tt)
      in
      let s =
        match resolve_struct_by_path ctx path with
        | Some s -> s
        | None ->
            Error.failf pos "unknown struct '%s'"
              (String.concat "::" path)
      in
      (match List.assoc_opt fname s.sfields_ty with
       | Some t -> t
       | None ->
           Error.failf pos "struct '%s' has no field '%s'"
             (String.concat "::" path) fname)
  | Ast.Ref (e, _) ->
      TPtr (type_of ctx env e)
  | Ast.Deref (e, pos) ->
      (match type_of ctx env e with
       | TPtr t -> t
       | TNullPtr ->
           Error.failf pos "cannot deref 'null'"
       | other ->
           Error.failf pos "deref '*' requires a pointer, got %s"
             (typ_name other))
  | Ast.NullLit _ -> TNullPtr
  | Ast.Call (path, args, pos) ->
      (match rewrite_call_as_enum_lit ctx path args pos with
       | Some e -> type_of ~allow_void ?expected ctx env e
       | None ->
      let arg_tys = List.map (type_of ctx env) args in
      (match lookup_builtin path with
       | Some b -> b.bcheck ~pos ~arg_tys ~allow_void
       | None ->
      let display = String.concat "::" path in
      (match lookup_fn ctx path with
       | None -> Error.failf pos "unknown function '%s'" display
       | Some (resolved_mod, { param_tys; ret_ty; fn_pub; fn_variadic; _ }) ->
           (* Qualified call (path > 1): each module segment must be visible
              from the current scope.  We walk the resolved fn's module path
              (resolved_mod), since that's where we actually found the
              function — relative or absolute. *)
           (match path with
            | [_] -> ()
            | _ ->
                let rec walk_segments parent = function
                  | [] -> ()
                  | seg :: rest ->
                      let mod_path = parent @ [seg] in
                      let pub =
                        match List.assoc_opt mod_path ctx.modules with
                        | Some b -> b
                        | None ->
                            Error.failf pos "unknown module '%s'"
                              (String.concat "::" mod_path)
                      in
                      if (not pub) && not (is_prefix parent ctx.scope) then
                        Error.failf pos
                          "module '%s' is private (not visible from '%s')"
                          (String.concat "::" mod_path)
                          (if ctx.scope = [] then "<root>"
                           else String.concat "::" ctx.scope);
                      walk_segments mod_path rest
                in
                walk_segments [] resolved_mod;
                if (not fn_pub) && ctx.scope <> resolved_mod then
                  Error.failf pos "function '%s' is private to module '%s'"
                    display (String.concat "::" resolved_mod));
           let expected = List.length param_tys in
           let got = List.length args in
           let arity_ok =
             if fn_variadic then got >= expected
             else got = expected
           in
           if not arity_ok then
             Error.failf pos
               (if fn_variadic
                then "function '%s' expects at least %d argument(s), got %d"
                else "function '%s' expects %d argument(s), got %d")
               display expected got;
           (* Type-check only the fixed prefix; variadic extras pass
              through unchecked, mirroring C semantics — caller is
              responsible for matching the format string. *)
           let fixed_arg_tys =
             let rec take n xs =
               if n <= 0 then []
               else match xs with
                 | [] -> []
                 | x :: rest -> x :: take (n - 1) rest
             in
             take expected arg_tys
           in
           List.iteri
             (fun i (exp, act) ->
               if not (typ_eq exp act)
                  && not (int_lit_fits (List.nth args i) exp)
               then
                 Error.failf pos
                   "argument %d of '%s': expected %s, got %s"
                   (i + 1) display (typ_name exp) (typ_name act))
             (List.combine param_tys fixed_arg_tys);
           (match ret_ty with
            | Some t -> t
            | None when allow_void -> t_i32   (* placeholder, caller discards *)
            | None ->
                Error.failf pos "'%s' returns void, cannot use as a value" display))))
  | Ast.MethodCall { receiver; name; args; pos } ->
      (* Method call `recv.name(args)`: resolve the method on receiver's
         struct (which lives in the global fn index under the struct's
         absolute path), validate visibility, arity, and arg types.  The
         receiver consumes one parameter slot — `args` corresponds to
         param_tys[1..]. *)
      let recv_ty = type_of ctx env receiver in
      let struct_path =
        match recv_ty with
        | TStruct p -> p
        | TPtr (TStruct p) -> p
        | _ ->
            Error.failf pos
              "method call '.%s()' requires a struct value or pointer to \
               struct, got %s"
              name (typ_name recv_ty)
      in
      let mpath = struct_path @ [name] in
      let display = String.concat "::" mpath in
      let arg_tys = List.map (type_of ctx env) args in
      (match lookup_fn ctx mpath with
       | None ->
           Error.failf pos "no method '%s' on type '%s'"
             name (String.concat "::" struct_path)
       | Some (resolved_mod, { param_tys; ret_ty; fn_pub; _ }) ->
           let rec walk_segments parent = function
             | [] -> ()
             | seg :: rest ->
                 let mod_path = parent @ [seg] in
                 (match List.assoc_opt mod_path ctx.modules with
                  | Some pub ->
                      if (not pub) && not (is_prefix parent ctx.scope) then
                        Error.failf pos
                          "type '%s' is private (not visible from '%s')"
                          (String.concat "::" mod_path)
                          (if ctx.scope = [] then "<root>"
                           else String.concat "::" ctx.scope)
                  | None -> ());
                 walk_segments mod_path rest
           in
           walk_segments [] resolved_mod;
           if (not fn_pub) && ctx.scope <> resolved_mod then
             Error.failf pos "method '%s' is private to '%s'"
               name (String.concat "::" resolved_mod);
           let expected_args = List.length param_tys - 1 in
           let got_args = List.length args in
           if expected_args <> got_args then
             Error.failf pos
               "method '%s' takes %d argument(s), got %d"
               display expected_args got_args;
           (match param_tys with
            | _self :: rest_params ->
                List.iteri
                  (fun i (exp, act) ->
                    if not (typ_eq exp act)
                       && not (int_lit_fits (List.nth args i) exp)
                    then
                      Error.failf pos
                        "argument %d of '%s': expected %s, got %s"
                        (i + 1) display (typ_name exp) (typ_name act))
                  (List.combine rest_params arg_tys)
            | [] -> assert false (* methods always have self in registry *));
           (match ret_ty with
            | Some t -> t
            | None when allow_void -> t_i32
            | None ->
                Error.failf pos "'%s' returns void, cannot use as a value" display))
  | Ast.EnumLit { tname; variant; args; pos } ->
      let e =
        match lookup_enum ctx tname with
        | Some e -> e
        | None ->
            Error.failf pos "unknown enum '%s'"
              (String.concat "::" tname)
      in
      let v =
        match List.find_opt (fun (vs : variant_sig) -> vs.vsname = variant)
                e.evariants with
        | Some v -> v
        | None ->
            Error.failf pos "enum '%s' has no variant '%s'"
              (String.concat "::" e.ename_path) variant
      in
      let display = String.concat "::" e.ename_path ^ "::" ^ variant in
      (* Generic enum: infer type args from the ctor's payload, then
         instantiate.  Unit variants of a generic enum cannot be
         inferred (no payload to read T from) — reject with a hint
         that turbofish / let-ann would be needed. *)
      let (v_used, result_path) =
        if e.etparams = [] then (v, e.ename_path)
        else begin
          let pairs =
            match args with
            | Ast.EATuple es when not v.vsis_struct ->
                if List.length es <> List.length v.vsfields then []
                else
                  List.map2 (fun (_, decl_t) e ->
                    (decl_t, type_of ctx env e))
                    v.vsfields es
            | Ast.EAStruct fs when v.vsis_struct ->
                List.filter_map (fun (n, e) ->
                  match List.assoc_opt n v.vsfields with
                  | Some decl_t -> Some (decl_t, type_of ctx env e)
                  | None -> None)
                  fs
            | _ -> []
          in
          (* Bidirectional seed: if the surrounding context expects
             a specific instance of THIS enum (`is_instance_of`),
             pull its tparam bindings from the instance's recorded
             args.  Lets ctors like `Result::Ok(n)` and unit
             variants like `Option::None` pin all tparams even when
             the payload alone can't determine them. *)
          let seed =
            match expected with
            | Some (TEnum exp_path)
              when Mono.is_instance_of e.ename_path exp_path ->
                (match resolve_enum_by_path ctx exp_path with
                 | Some exp_inst ->
                     (match exp_inst.einstance_args with
                      | Some args -> List.combine e.etparams args
                      | None -> [])
                 | None -> [])
            | _ -> []
          in
          let inferred = Mono.infer_tparams ~pos ~seed e.etparams pairs in
          let inst = Mono.instantiate_enum ctx.instances e inferred in
          let inst_v =
            List.find (fun (vs : variant_sig) -> vs.vsname = variant)
              inst.evariants
          in
          (inst_v, inst.ename_path)
        end
      in
      (match args with
       | Ast.EATuple es ->
           if v_used.vsis_struct then
             Error.failf pos
               "variant '%s' is a struct variant; construct it with \
                '{ field: ... }', not with '(...)'" display;
           let expected = List.length v_used.vsfields in
           let got = List.length es in
           if expected <> got then
             Error.failf pos
               "variant '%s' takes %d argument(s), got %d"
               display expected got;
           let arg_tys = List.map (type_of ctx env) es in
           List.iteri
             (fun i ((_, exp), act) ->
               if not (typ_eq exp act)
                  && not (int_lit_fits (List.nth es i) exp)
               then
                 Error.failf pos
                   "argument %d of '%s': expected %s, got %s"
                   (i + 1) display (typ_name exp) (typ_name act))
             (List.combine v_used.vsfields arg_tys)
       | Ast.EAStruct fs ->
           if not v_used.vsis_struct then
             Error.failf pos
               "variant '%s' is a tuple variant; construct it with \
                '(...)', not with '{ field: ... }'" display;
           let expected_names = List.map fst v_used.vsfields in
           let got_names = List.map fst fs in
           (match find_dup ~key:Fun.id got_names with
            | Some n ->
                Error.failf pos
                  "duplicate field '%s' in '%s' construction" n display
            | None -> ());
           List.iter (fun (n, _) ->
             if not (List.mem n expected_names) then
               Error.failf pos
                 "variant '%s' has no field '%s'" display n) fs;
           List.iter (fun n ->
             if not (List.mem_assoc n fs) then
               Error.failf pos
                 "missing field '%s' in '%s' construction" n display)
             expected_names;
           List.iter (fun (n, e) ->
             let exp = List.assoc n v_used.vsfields in
             let act = type_of ctx env e in
             if not (typ_eq exp act) && not (int_lit_fits e exp) then
               Error.failf pos
                 "field '%s' of '%s': expected %s, got %s"
                 n display (typ_name exp) (typ_name act)) fs);
      TEnum result_path
  | Ast.Match { scrutinee; arms; pos } ->
      let scrut_ty = type_of ctx env scrutinee in
      let ename_path =
        match scrut_ty with
        | TEnum p -> p
        | other ->
            Error.failf pos
              "'match' requires an enum value, got %s" (typ_name other)
      in
      if arms = [] then
        Error.failf pos "'match' must have at least one arm";
      let e =
        match resolve_enum_by_path ctx ename_path with
        | Some e -> e
        | None ->
            Error.failf pos "internal: enum '%s' missing from index"
              (String.concat "::" ename_path)
      in
      (* Per-arm: validate pattern matches the scrutinee's enum, collect
         bind names (with their inferred types) into an extended env,
         and type the arm body under that env.  Sub-patterns inside
         PVariant are limited to PVar / PWildcard; nested variant
         patterns are not yet supported. *)
      let arm_tys =
        List.map (fun (a : Ast.match_arm) ->
          let arm_env =
            match a.pat with
            | Ast.PWildcard _ -> env
            | Ast.PVar (n, _) -> (n, scrut_ty) :: env
            | Ast.PVariant { tname; variant; binds; pos = ppos } ->
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
                let v =
                  match List.find_opt
                          (fun (vs : variant_sig) -> vs.vsname = variant)
                          e.evariants with
                  | Some v -> v
                  | None ->
                      Error.failf ppos "enum '%s' has no variant '%s'"
                        (String.concat "::" ename_path) variant
                in
                (* Match construction syntax against the variant's
                   declared kind, then collect (name, sub_pattern)
                   pairs in field-order before reading binds. *)
                let ordered_binds =
                  match binds, v.vsis_struct with
                  | Ast.PBTuple ps, false ->
                      let expected = List.length v.vsfields in
                      let got = List.length ps in
                      if expected <> got then
                        Error.failf ppos
                          "variant '%s' has %d field(s), pattern binds %d"
                          variant expected got;
                      List.map2 (fun (n, _) p -> (n, p)) v.vsfields ps
                  | Ast.PBStruct entries, true ->
                      let expected_names = List.map fst v.vsfields in
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
                         match it with '{ field: pat }', not '(...)'" variant
                  | Ast.PBStruct _, false ->
                      Error.failf ppos
                        "variant '%s' is a tuple variant; \
                         match it with '(...)', not '{ field: pat }'" variant
                in
                let pairs =
                  List.map2 (fun (_, bp) (_, ft) ->
                    match bp with
                    | Ast.PWildcard _ -> None
                    | Ast.PVar (n, _) -> Some (n, ft)
                    | Ast.PVariant { pos = bpos; _ } ->
                        Error.failf bpos
                          "nested variant patterns are not yet supported")
                    ordered_binds v.vsfields
                in
                let names = List.filter_map (Option.map fst) pairs in
                (match find_dup ~key:Fun.id names with
                 | Some n ->
                     Error.failf ppos
                       "duplicate bind name '%s' in pattern" n
                 | None -> ());
                List.fold_left (fun acc -> function
                  | Some pair -> pair :: acc
                  | None -> acc) env pairs
          in
          type_of ~allow_void ctx arm_env a.body)
          arms
      in
      (* Exhaustiveness: every variant must be reached.  A wildcard
         pattern (or a bare bind, which is equivalent here — both match
         anything) covers all remaining variants. *)
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
            e.evariants
        in
        if missing <> [] then
          Error.failf pos
            "non-exhaustive 'match': variant(s) %s not covered (add an arm \
             or '_')"
            (String.concat ", "
               (List.map (fun (vs : variant_sig) -> vs.vsname) missing))
      end;
      (* All arm bodies must agree on a type — pick the first as the
         witness and require the rest to match it. *)
      (match arm_tys with
       | [] -> assert false
       | t0 :: rest ->
           List.iter (fun t ->
             if not (typ_eq t t0) then
               Error.failf pos
                 "match arms have inconsistent types: %s vs %s"
                 (typ_name t0) (typ_name t))
             rest;
           t0)
  | Ast.Orelse (value, default, pos) ->
      let scrutinee_ty = type_of ctx env value in
      let lowered = desugar_orelse ~scrutinee_ty value default pos in
      type_of ~allow_void ?expected ctx env lowered
  | Ast.Try (value, pos) ->
      (* type_of returns the unwrapped payload type — same as the Ok/Some
         arm of the match `try` desugars to.  Outer ret-type validation
         happens in elab_expr; here we just need the value's type so
         callers using type_of for inference (e.g. let-RHS) see T, not
         the outer Result/Option. *)
      let inner_ty = type_of ctx env value in
      let p = match inner_ty with
        | TEnum p -> p
        | _ ->
            Error.failf pos
              "'try' requires Option or Result, got %s" (typ_name inner_ty)
      in
      let ok_name =
        if Mono.is_instance_of ["Option"] p then "Some"
        else if Mono.is_instance_of ["Result"] p then "Ok"
        else
          Error.failf pos
            "'try' requires Option or Result, got %s" (typ_name inner_ty)
      in
      (match resolve_enum_by_path ctx p with
       | Some e ->
           (match List.find_opt
                    (fun (vs : variant_sig) -> vs.vsname = ok_name)
                    e.evariants with
            | Some { vsfields = [(_, t)]; _ } -> t
            | _ ->
                Error.failf pos
                  "internal: '%s' variant missing one-field payload" ok_name)
       | None ->
           Error.failf pos
             "internal: enum '%s' missing" (String.concat "::" p))

(* Resolve operand types for a BinOp.  An integer literal on either side
   adopts the other operand's int type if it fits, so `x + 5` keeps x's
   width without forcing a cast. *)
and binop_operand_types ctx env l r =
  match l, r with
  | Ast.IntLit (n, _), _ ->
      let rt = type_of ctx env r in
      let lt =
        match rt with
        | TInt _ when int_fits n rt -> rt
        | _ -> type_of ctx env l
      in
      (lt, rt)
  | _, Ast.IntLit (n, _) ->
      let lt = type_of ctx env l in
      let rt =
        match lt with
        | TInt _ when int_fits n lt -> lt
        | _ -> type_of ctx env r
      in
      (lt, rt)
  | _ ->
      (type_of ctx env l, type_of ctx env r)

(* Shared validation for struct literals (used by both `Foo { ... }` and
   `new Foo { ... }`).  Returns the struct signature so callers can build
   the right result type (TStruct vs TPtr TStruct). *)
and validate_struct_lit ctx env ~tname ~fields ~base ~pos =
  let display = String.concat "::" tname in
  let s =
    match lookup_struct ctx tname with
    | Some s -> s
    | None -> Error.failf pos "unknown struct '%s'" display
  in
  let parent = parent_path s.sname_path in
  if (not s.sis_pub) && not (is_prefix parent ctx.scope) then
    Error.failf pos "struct '%s' is private to module '%s'"
      display
      (if parent = [] then "<root>" else String.concat "::" parent);
  (match find_dup ~key:fst fields with
   | Some n ->
       Error.failf pos "duplicate field '%s' in struct literal '%s'" n display
   | None -> ());
  let provided = List.map fst fields in
  let expected = List.map fst s.sfields_ty in
  let missing = List.filter (fun n -> not (List.mem n provided)) expected in
  let extra = List.filter (fun n -> not (List.mem n expected)) provided in
  if extra <> [] then
    Error.failf pos "struct literal '%s' has unknown field(s): %s"
      display (String.concat ", " extra);
  (* Generic decl: infer type args from field values, instantiate
     a fresh monomorphic struct_sig, and validate against that.
     Functional update `..base` on a generic struct is rejected here
     for now — without explicit type args we'd need to derive them
     from the base's type. *)
  let s_concrete =
    if s.stparams = [] then s
    else begin
      if base <> None then
        Error.failf pos
          "'..base' on a generic struct '%s' is not yet supported \
           (annotate the value's type and re-create the literal)"
          display;
      if missing <> [] then
        Error.failf pos "struct literal '%s' missing field(s): %s"
          display (String.concat ", " missing);
      let pairs =
        List.map (fun (fn, fe) ->
          let decl_t = List.assoc fn s.sfields_ty in
          let act_t = type_of ctx env fe in
          (decl_t, act_t))
          fields
      in
      let inferred_args = Mono.infer_tparams ~pos s.stparams pairs in
      Mono.instantiate_struct ctx.instances s inferred_args
    end
  in
  (* For the mono case (and after instantiation for the generic case),
     proceed with the regular field-typing checks against the
     concrete struct_sig. *)
  (match base with
   | None ->
       if missing <> [] && s.stparams = [] then
         Error.failf pos "struct literal '%s' missing field(s): %s"
           display (String.concat ", " missing)
   | Some be ->
       let bt = type_of ctx env be in
       (match bt with
        | TStruct path when path = s_concrete.sname_path -> ()
        | _ ->
            Error.failf pos
              "'..base' in struct literal '%s' expects a value of \
               type %s, got %s"
              display display (typ_name bt)));
  List.iter
    (fun (fn, fe) ->
      let fty = List.assoc fn s_concrete.sfields_ty in
      let act = type_of ctx env fe in
      if not (typ_eq act fty) && not (int_lit_fits fe fty) then
        Error.failf pos
          "field '%s' of struct '%s': expected %s, got %s"
          fn display (typ_name fty) (typ_name act))
    fields;
  s_concrete

(* Elaborate Ast.expr → texpr.  Each typed node carries the result of
   `type_of` in `.ty`, so codegen never has to re-run typing.  Validation
   (already done by the time we reach elab_expr from elab_body / external
   callers) is invariant — we still call type_of internally to produce the
   ty field; on an already-validated tree it succeeds without raising. *)
let rec elab_expr ?(allow_void = false) ?expected ctx env e : texpr =
  let ty = type_of ~allow_void ?expected ctx env e in
  let pos = Ast.expr_pos e in
  let node : texpr_node =
    match e with
    | Ast.IntLit (n, _) -> TIntLit n
    | Ast.BoolLit (b, _) -> TBoolLit b
    | Ast.NullLit _ -> TNullLit
    | Ast.StringLit (s, _) -> TStringLit s
    | Ast.Var (n, _) -> TVar n
    | Ast.Neg (sub, _) -> TNeg (elab_expr ctx env sub)
    | Ast.BinOp (op, l, r, _) ->
        TBinOp (op, elab_expr ctx env l, elab_expr ctx env r)
    | Ast.Cast (sub, ann, _) -> TCast (elab_expr ctx env sub, ann)
    | Ast.TupleLit (es, _) -> TTupleLit (List.map (elab_expr ctx env) es)
    | Ast.StructLit { tname; fields; base; pos } ->
        (* Same dispatch as in `type_of`: enum struct-variant ctor
           lowers through the EnumLit elab branch. *)
        (match rewrite_struct_lit_as_enum_lit ctx tname fields base pos with
         | Some e -> (elab_expr ~allow_void ?expected ctx env e).e
         | None ->
             let s =
               match lookup_struct ctx tname with
               | Some s -> s
               | None -> assert false   (* validated upstream *)
             in
             TStructLit {
               sname_path = s.sname_path;
               fields = List.map (fun (n, fe) -> (n, elab_expr ctx env fe))
                          fields;
               base = Option.map (elab_expr ctx env) base;
             })
    | Ast.New { tname; fields; base; _ } ->
        let s =
          match lookup_struct ctx tname with
          | Some s -> s
          | None -> assert false
        in
        TNew {
          sname_path = s.sname_path;
          fields = List.map (fun (n, fe) -> (n, elab_expr ctx env fe)) fields;
          base = Option.map (elab_expr ctx env) base;
        }
    | Ast.FieldAccess (target, field, _) ->
        TFieldAccess { target = elab_expr ctx env target; field }
    | Ast.Ref (sub, _) -> TRef (elab_expr ctx env sub)
    | Ast.Deref (sub, _) -> TDeref (elab_expr ctx env sub)
    | Ast.Call (path, args, pos) ->
        (match rewrite_call_as_enum_lit ctx path args pos with
         | Some e ->
             (* Recurse so the EnumLit branch handles arg elaboration
                and tag lookup uniformly with the no-args case. *)
             (elab_expr ~allow_void ?expected ctx env e).e
         | None ->
        let targs = List.map (elab_expr ctx env) args in
        (match lookup_builtin path with
         | Some _ ->
             let name = match path with [n] -> n | _ -> assert false in
             TBuiltinCall { name; args = targs }
         | None ->
             let mangled =
               match lookup_fn ctx path with
               | Some (_, s) -> s.mangled
               | None -> assert false   (* validated upstream *)
             in
             TCall { mangled; args = targs }))
    | Ast.MethodCall { receiver; name; args; _ } ->
        let trecv = elab_expr ctx env receiver in
        let struct_path =
          match trecv.ty with
          | TStruct p -> p
          | TPtr (TStruct p) -> p
          | _ -> assert false   (* validated by type_of *)
        in
        let mpath = struct_path @ [name] in
        let (mangled, self_ty) =
          match lookup_fn ctx mpath with
          | Some (_, { mangled; param_tys = self_t :: _; _ }) ->
              (mangled, self_t)
          | _ -> assert false
        in
        (* Auto-ref / auto-deref: align receiver shape with the method's
           self-param shape.  Both directions: `Foo` → `*Foo` via TRef,
           `*Foo` → `Foo` via TDeref. *)
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
        let targs = List.map (elab_expr ctx env) args in
        TCall { mangled; args = trecv_adj :: targs }
    | Ast.EnumLit { tname = _; variant; args; _ } ->
        (* type_of (called above into `ty`) already instantiated any
           generic enum, so `ty = TEnum inst_path` points at the
           concrete monomorphic version.  Look up that instance to
           resolve the variant by name; field types are already
           substituted to concrete values there. *)
        let ename_path =
          match ty with TEnum p -> p | _ -> assert false
        in
        let e =
          match resolve_enum_by_path ctx ename_path with
          | Some e -> e
          | None -> assert false
        in
        let (tag, vsig) =
          match find_variant e variant with
          | Some r -> r
          | None -> assert false   (* validated upstream by type_of *)
        in
        let targs =
          match args with
          | Ast.EATuple es ->
              List.map2 (fun (n, _) e -> (n, elab_expr ctx env e))
                vsig.vsfields es
          | Ast.EAStruct fs ->
              List.map (fun (n, _) ->
                let e = List.assoc n fs in
                (n, elab_expr ctx env e))
                vsig.vsfields
        in
        TEnumLit { ename_path; variant; tag; args = targs }
    | Ast.Match { scrutinee; arms; _ } ->
        let tscrut = elab_expr ctx env scrutinee in
        let scrut_ty = tscrut.ty in
        let ename_path =
          match scrut_ty with
          | TEnum p -> p
          | _ -> assert false
        in
        let e =
          match resolve_enum_by_path ctx ename_path with
          | Some e -> e
          | None -> assert false
        in
        let lower_subpat = function
          | Ast.PWildcard _ -> TPWildcard
          | Ast.PVar (n, _) -> TPVar n
          | Ast.PVariant _ -> assert false (* validated upstream *)
        in
        let tarms =
          List.map (fun (a : Ast.match_arm) ->
            let (tpat, arm_env) =
              match a.pat with
              | Ast.PWildcard _ -> (TPWildcard, env)
              | Ast.PVar (n, _) -> (TPVar n, (n, scrut_ty) :: env)
              | Ast.PVariant { variant; binds; _ } ->
                  let (tag, vsig) =
                    match find_variant e variant with
                    | Some r -> r
                    | None -> assert false   (* validated upstream *)
                  in
                  (* Reduce both bind forms to a list of (field_name,
                     sub_pattern) in field-order — type_of has already
                     checked that struct/tuple syntax matches the
                     variant kind and that names line up. *)
                  let ordered_binds =
                    match binds with
                    | Ast.PBTuple ps ->
                        List.map2 (fun (n, _) p -> (n, p)) vsig.vsfields ps
                    | Ast.PBStruct entries ->
                        List.map (fun (n, _) ->
                          match List.assoc_opt n entries with
                          | Some p -> (n, p)
                          | None -> (n, Ast.PWildcard a.arm_pos))
                          vsig.vsfields
                  in
                  let tbinds =
                    List.map (fun (n, p) -> (n, lower_subpat p))
                      ordered_binds
                  in
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
        TMatch { scrutinee = tscrut; ename_path; arms = tarms }
    | Ast.Orelse (value, default, pos) ->
        let scrutinee_ty = type_of ctx env value in
        let lowered = desugar_orelse ~scrutinee_ty value default pos in
        let lifted = elab_expr ~allow_void ?expected ctx env lowered in
        lifted.e
    | Ast.Try (value, pos) ->
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
              Error.failf pos
                "'try' requires Option or Result, got %s" (typ_name tinner.ty)
        in
        let inner_skel =
          if Mono.is_instance_of ["Option"] inner_path then ["Option"]
          else if Mono.is_instance_of ["Result"] inner_path then ["Result"]
          else
            Error.failf pos
              "'try' requires Option or Result, got %s" (typ_name tinner.ty)
        in
        let outer_path =
          match ctx.ret_ty with
          | Some (TEnum p) when Mono.is_instance_of inner_skel p -> p
          | Some other ->
              Error.failf pos
                "'try' on %s value but enclosing fn returns %s — they must \
                 share the same Option/Result shape"
                (typ_name tinner.ty) (typ_name other)
          | None ->
              Error.failf pos
                "'try' is only allowed in fns that return Option or Result \
                 (this fn has no return type)"
        in
        let inner_e =
          match resolve_enum_by_path ctx inner_path with
          | Some e -> e
          | None -> Error.failf pos "internal: inner enum missing"
        in
        let outer_e =
          match resolve_enum_by_path ctx outer_path with
          | Some e -> e
          | None -> Error.failf pos "internal: outer enum missing"
        in
        let find_v e name =
          match find_variant e name with
          | Some r -> r
          | None -> Error.failf pos "internal: variant '%s' missing" name
        in
        let (ok_name, err_name) =
          if inner_skel = ["Option"] then ("Some", "None")
          else ("Ok", "Err")
        in
        let (ok_tag, ok_vs) = find_v inner_e ok_name in
        let (err_tag_in, err_vs_in) = find_v inner_e err_name in
        let (outer_err_tag, outer_err_vs) = find_v outer_e err_name in
        (* Verify Err payload types align between inner and outer.  For
           Option both have empty payloads; for Result both have a single
           field of the same E type. *)
        let payload_compat (in_fs : (string * typ) list) (out_fs : (string * typ) list) =
          List.length in_fs = List.length out_fs
          && List.for_all2 (fun (_, a) (_, b) -> typ_eq a b) in_fs out_fs
        in
        if not (payload_compat err_vs_in.vsfields outer_err_vs.vsfields) then
          Error.failf pos
            "'try' Err/None payload mismatch: inner %s vs outer %s"
            (typ_name (TEnum inner_path)) (typ_name (TEnum outer_path));
        let ok_payload_ty =
          match ok_vs.vsfields with
          | [(_, t)] -> t
          | _ -> Error.failf pos "internal: Ok/Some payload shape"
        in
        (* Yielding arm: bind Ok/Some _0 to __try_v, body is `__try_v`. *)
        let try_bind = "__try_v" in
        let ok_arm_pat =
          TPVariant {
            variant = ok_name;
            tag = ok_tag;
            binds = [ ("_0", TPVar try_bind) ];
          }
        in
        let ok_body = {
          e = TVar try_bind;
          ty = ok_payload_ty;
          pos;
        } in
        let ok_arm = {
          tpat = ok_arm_pat;
          tbody = ok_body;
          tdiverges = false;
          tarm_pos = pos;
        } in
        (* Diverging arm: bind Err _0 to __try_e (Result) or no binds
           (Option); body is `Outer::Err(__try_e)` / `Outer::None` of
           the outer fn's instance.  Codegen emits `return tbody;`. *)
        let try_err_bind = "__try_e" in
        let (err_arm_pat, outer_err_args) =
          if inner_skel = ["Option"] then
            (TPVariant { variant = err_name; tag = err_tag_in; binds = [] },
             [])
          else
            (TPVariant {
                variant = err_name;
                tag = err_tag_in;
                binds = [ ("_0", TPVar try_err_bind) ];
              },
             let e_ty =
               match err_vs_in.vsfields with
               | [(_, t)] -> t
               | _ -> Error.failf pos "internal: Err payload shape"
             in
             [ ("_0", { e = TVar try_err_bind; ty = e_ty; pos }) ])
        in
        let err_body = {
          e = TEnumLit {
            ename_path = outer_path;
            variant = err_name;
            tag = outer_err_tag;
            args = outer_err_args;
          };
          ty = TEnum outer_path;
          pos;
        } in
        let err_arm = {
          tpat = err_arm_pat;
          tbody = err_body;
          tdiverges = true;
          tarm_pos = pos;
        } in
        TMatch {
          scrutinee = tinner;
          ename_path = inner_path;
          arms = [ ok_arm; err_arm ];
        }
  in
  { e = node; ty; pos }

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
        Option.iter (forbid_naked_opaque pos) expected;
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
    | Ast.Assign { name; value; pos } ->
        if not (List.mem_assoc name env) then
          Error.failf pos "assignment to undefined variable '%s'" name;
        let tvalue = elab_expr ctx env value in
        (env, TAssign { name; value = tvalue; pos })
    | Ast.AssignField { target; field; value; pos } ->
        let ttarget = elab_expr ctx env target in
        let path =
          match ttarget.ty with
          | TStruct p -> p
          | TPtr (TStruct p) -> p
          | other ->
              Error.failf pos
                "assignment to field '.%s' requires a struct value or \
                 pointer to struct, got %s"
                field (typ_name other)
        in
        let s =
          match resolve_struct_by_path ctx path with
          | Some s -> s
          | None ->
              Error.failf pos "unknown struct '%s'"
                (String.concat "::" path)
        in
        let fty =
          match List.assoc_opt field s.sfields_ty with
          | Some t -> t
          | None ->
              Error.failf pos "struct '%s' has no field '%s'"
                (String.concat "::" path) field
        in
        let tvalue = elab_expr ctx env value in
        if not (typ_eq tvalue.ty fty) && not (int_lit_fits value fty) then
          Error.failf pos
            "field '%s' of struct '%s': expected %s, got %s"
            field (String.concat "::" path) (typ_name fty)
            (typ_name tvalue.ty);
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
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _ ->
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
    | TAssign { name; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TAssign { name; value = v'; pos } ]
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
  enums : (string list * Ast.enum_decl) list;
  modules : (string list * bool) list;
  c_includes : string list;               (* `@c_include("...")` paths *)
  (* Each `impl` block keeps its enclosing module path; target struct
     resolution (relative-to-scope, ancestor walk-up) happens later
     once the struct index is built. *)
  impls : (string list * Ast.impl_block) list;
}

let flatten_items program =
  let funcs = ref [] in
  let structs = ref [] in
  let ext_structs = ref [] in
  let ext_types = ref [] in
  let ext_consts = ref [] in
  let enums = ref [] in
  let modules = ref [] in
  let impls = ref [] in
  let c_includes = ref [] in
  (* Uniform "must be at top level" reject — `extern struct/type/const`
     and `@c_include` all share the same constraint with the same
     wording.  Path captured by walk and threaded in. *)
  let top_only ~current_path ~kind ~name ~pos =
    if current_path <> [] then
      Error.failf pos
        "'extern %s %s' must be at top level, not inside a module"
        kind name
  in
  let rec walk path items =
    List.iter
      (fun item -> match item with
        | Ast.Function (f : Ast.func) ->
            if f.is_extern && path <> [] then
              Error.failf f.pos
                "'extern fn %s' must be at top level, not inside a module"
                f.name;
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
            top_only ~current_path:path ~kind:"struct"
              ~name:es.Ast.esname ~pos:es.Ast.espos;
            ext_structs := es :: !ext_structs
        | Ast.ExternType et ->
            top_only ~current_path:path ~kind:"type"
              ~name:et.Ast.xtname ~pos:et.Ast.xtpos;
            ext_types := et :: !ext_types
        | Ast.ExternConst ec ->
            top_only ~current_path:path ~kind:"const"
              ~name:ec.Ast.ecname ~pos:ec.Ast.ecpos;
            ext_consts := ec :: !ext_consts
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
    c_includes = List.rev !c_includes;
    enums = List.rev !enums;
    modules = List.rev !modules;
    impls = List.rev !impls }

(* Build the global function index: every function with its module path,
   exile-side name, and signature.  main() is excluded — it is not callable. *)
let build_global_index ~instances ~ext_structs ~ext_types ~ext_consts ~struct_index ~enum_index ~modules flat =
  List.filter_map
    (fun (p, (f : Ast.func), mangled) ->
      if f.name = "main" then None
      else
        let ctx = {
          global = []; structs = struct_index; enums = enum_index;
          modules; scope = p; tparams = f.tparams;
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
          sinstance_args = None })
      struct_flat
  in
  List.map2
    (fun (p, (s : Ast.struct_decl)) skel ->
      let ctx = {
        global = []; structs = skeleton; enums;
        modules; scope = p; tparams = s.stparams;
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
          einstance_args = None })
      enum_flat
  in
  List.map2
    (fun (p, (e : Ast.enum_decl)) skel ->
      let ctx = {
        global = []; structs = struct_index; enums = skeleton;
        modules; scope = p; tparams = e.etparams;
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
  let seen = ref [] in
  let add t =
    let name = mangle_typ t in
    if not (List.exists (fun (n, _) -> n = name) !seen) then
      seen := (name, t) :: !seen
  in
  let rec walk_typ t =
    match t with
    | TTuple ts -> add t; List.iter walk_typ ts
    | _ -> ()
  in
  let walk_typ_ann ann = walk_typ (type_of_ann ann) in
  let rec walk_texpr (te : texpr) =
    walk_typ te.ty;
    match te.e with
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _ -> ()
    | TNeg sub | TRef sub | TDeref sub | TCast (sub, _) -> walk_texpr sub
    | TBinOp (_, l, r) -> walk_texpr l; walk_texpr r
    | TCall { args; _ } | TBuiltinCall { args; _ } -> List.iter walk_texpr args
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
  List.rev !seen

(* Detect heap usage by scanning the typed bodies for `TNew` expressions or
   builtin `free(p)` calls — both are emitted in C only when one of them is
   present, so codegen can conditionally include `<stdlib.h>`. *)
let uses_heap_of tfuncs =
  let rec walk_texpr (te : texpr) =
    match te.e with
    | TNew _ -> true
    | TBuiltinCall { name = "free"; _ } -> true
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _ -> false
    | TNeg sub | TRef sub | TDeref sub | TCast (sub, _) -> walk_texpr sub
    | TBinOp (_, l, r) -> walk_texpr l || walk_texpr r
    | TCall { args; _ } | TBuiltinCall { args; _ } ->
        List.exists walk_texpr args
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
let prelude_pos = { Pos.line = 0; col = 0; file = "<prelude>" }

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
  } in
  let result_decl = {
    Ast.ename = "Result";
    etparams = ["T"; "E"];
    evariants = [ mk_tuple "Ok" [tvar "T"]; mk_tuple "Err" [tvar "E"] ];
    epos = prelude_pos;
    eis_pub = true;
  } in
  [ Ast.Enum option_decl; Ast.Enum result_decl ]

(* Skip prelude items whose names collide with a user-declared top-level
   enum.  Matches by name only (top-level path = []). *)
let prepend_prelude (program : Ast.program) : Ast.program =
  let user_top_enum_names =
    List.filter_map
      (fun item -> match item with
        | Ast.Enum e -> Some e.ename
        | _ -> None)
      program
  in
  let kept =
    List.filter
      (fun item -> match item with
        | Ast.Enum e -> not (List.mem e.ename user_top_enum_names)
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
        einstance_args = None })
      flat.enums
  in
  let ext_structs =
    List.map (fun (es : Ast.extern_struct) -> es.esname) flat.ext_structs
  in
  let ext_types =
    List.map (fun (et : Ast.extern_type) -> et.xtname) flat.ext_types
  in
  let ext_consts =
    List.map (fun (ec : Ast.extern_const) ->
        let ctx_for_ann = {
          global = []; structs = []; enums = []; modules = flat.modules;
          scope = []; tparams = []; instances = mono_state;
          ext_structs; ext_types; ext_consts = [];
          ret_ty = None;
        } in
        (ec.ecname, resolve_type_ann ctx_for_ann ec.ecty))
      flat.ext_consts
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
      ~struct_index ~enum_index ~modules all_funcs
  in
  let tp_funcs =
    List.map
      (fun (path, (f : Ast.func), mangled) ->
        let ctx0 = {
          global; structs = struct_index; enums = enum_index;
          modules; scope = path; tparams = f.tparams;
          instances = mono_state; ext_structs; ext_types; ext_consts;
          ret_ty = None;
        } in
        let param_tys =
          List.map (fun (p : Ast.param) -> resolve_type_ann ctx0 p.pty) f.params
        in
        List.iter (forbid_naked_opaque f.pos) param_tys;
        let ret_ty = Option.map (resolve_type_ann ctx0) f.ret_ty in
        Option.iter (forbid_naked_opaque f.pos) ret_ty;
        let ctx = { ctx0 with ret_ty } in
        let param_env =
          List.combine (List.map (fun (p : Ast.param) -> p.pname) f.params)
            param_tys
        in
        let (lets, tbody) =
          if f.is_extern then ([], [])
          else elab_body ~ret_ty ctx param_env f.body
        in
        { tf_path = path; tf_func = f; tf_mangled = mangled;
          tf_param_tys = param_tys; tf_ret_ty = ret_ty;
          tf_body = tbody; tf_lets = lets })
      all_funcs
  in
  let tp_tuple_types = collect_tuple_types_of tp_funcs in
  let tp_uses_heap = uses_heap_of tp_funcs in
  (* Drain monomorphic instances accumulated during resolve_type_ann
     into the program's indexes.  Instances accumulate in reverse
     registration order; reversing puts them in roughly the order
     users wrote them.  Codegen emits them inline with regular
     non-generic decls. *)
  let mono_structs = List.rev mono_state.inst_structs in
  let mono_enums = List.rev mono_state.inst_enums in
  { tp_funcs;
    tp_struct_decls = flat.structs;
    tp_struct_index = struct_index @ mono_structs;
    tp_enum_index = enum_index @ mono_enums;
    tp_global = global;
    tp_modules = modules;
    tp_uses_heap;
    tp_tuple_types;
    tp_c_includes = flat.c_includes;
    tp_ext_consts = ext_consts }
