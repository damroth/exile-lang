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
   visibility checks. *)
type fn_ctx = {
  global : (string list * string * fn_sig) list;
  structs : struct_sig list;
  modules : (string list * bool) list;   (* full path -> is_pub *)
  scope : string list;
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
  | Ast.TyStr -> TString
  | Ast.TyBool -> TBool
  | Ast.TyTuple ts -> TTuple (List.map (resolve_type_ann ctx) ts)
  | Ast.TyPtr t -> TPtr (resolve_type_ann ctx t)
  | Ast.TyStruct path ->
      (match lookup_struct ctx path with
       | Some s -> TStruct s.sname_path
       | None -> TStruct path)

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
  match expr_int_lit expr, target with
  | Some n, TInt _ -> int_fits n target
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
let rec type_of ?(allow_void = false) ctx env = function
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
      (match src, tgt with
       | TInt _, TInt _ -> tgt
       | _ ->
           Error.failf pos
             "cannot cast %s to %s (only integer-to-integer casts supported)"
             (typ_name src) (typ_name tgt))
  | Ast.Var (name, pos) ->
      (match List.assoc_opt name env with
       | Some t -> t
       | None -> Error.failf pos "undefined variable '%s'" name)
  | Ast.TupleLit (es, _) ->
      TTuple (List.map (type_of ctx env) es)
  | Ast.StructLit { tname; fields; base; pos } ->
      let s = validate_struct_lit ctx env ~tname ~fields ~base ~pos in
      TStruct s.sname_path
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
        match lookup_struct ctx path with
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
      let arg_tys = List.map (type_of ctx env) args in
      (match lookup_builtin path with
       | Some b -> b.bcheck ~pos ~arg_tys ~allow_void
       | None ->
      let display = String.concat "::" path in
      (match lookup_fn ctx path with
       | None -> Error.failf pos "unknown function '%s'" display
       | Some (resolved_mod, { param_tys; ret_ty; fn_pub; _ }) ->
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
           if expected <> got then
             Error.failf pos "function '%s' expects %d argument(s), got %d"
               display expected got;
           List.iteri
             (fun i (exp, act) ->
               if not (typ_eq exp act) then
                 Error.failf pos
                   "argument %d of '%s': expected %s, got %s"
                   (i + 1) display (typ_name exp) (typ_name act))
             (List.combine param_tys arg_tys);
           (match ret_ty with
            | Some t -> t
            | None when allow_void -> t_i32   (* placeholder, caller discards *)
            | None ->
                Error.failf pos "'%s' returns void, cannot use as a value" display)))
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
                    if not (typ_eq exp act) then
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
  (* `..base` (functional update) fills any unspecified fields, so
     missing-field check is skipped when a base is present.  The base
     itself must be of the same struct type. *)
  (match base with
   | None ->
       if missing <> [] then
         Error.failf pos "struct literal '%s' missing field(s): %s"
           display (String.concat ", " missing)
   | Some be ->
       let bt = type_of ctx env be in
       (match bt with
        | TStruct path when path = s.sname_path -> ()
        | _ ->
            Error.failf pos
              "'..base' in struct literal '%s' expects a value of \
               type %s, got %s"
              display display (typ_name bt)));
  if extra <> [] then
    Error.failf pos "struct literal '%s' has unknown field(s): %s"
      display (String.concat ", " extra);
  List.iter
    (fun (fn, fe) ->
      let fty = List.assoc fn s.sfields_ty in
      let act = type_of ctx env fe in
      if not (typ_eq act fty) && not (int_lit_fits fe fty) then
        Error.failf pos
          "field '%s' of struct '%s': expected %s, got %s"
          fn display (typ_name fty) (typ_name act))
    fields;
  s

(* Elaborate Ast.expr → texpr.  Each typed node carries the result of
   `type_of` in `.ty`, so codegen never has to re-run typing.  Validation
   (already done by the time we reach elab_expr from elab_body / external
   callers) is invariant — we still call type_of internally to produce the
   ty field; on an already-validated tree it succeeds without raising. *)
let rec elab_expr ?(allow_void = false) ctx env e : texpr =
  let ty = type_of ~allow_void ctx env e in
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
    | Ast.StructLit { tname; fields; base; _ } ->
        let s =
          match lookup_struct ctx tname with
          | Some s -> s
          | None -> assert false   (* validated upstream *)
        in
        TStructLit {
          sname_path = s.sname_path;
          fields = List.map (fun (n, fe) -> (n, elab_expr ctx env fe)) fields;
          base = Option.map (elab_expr ctx env) base;
        }
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
    | Ast.Call (path, args, _) ->
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
             TCall { mangled; args = targs })
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
  in
  { e = node; ty; pos }

(* Single-walk variant of the old `collect_lets`: it both validates the
   body (mirroring the per-stmt type checks that lived there) and produces
   the elaborated `tstmt list`, alongside the hoisted let-decl list
   that the function-top declarations need.  Replaces `collect_lets` —
   `check_program` calls this once per function. *)
let elab_body ctx param_env stmts : (string * typ) list * tstmt list =
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
        let tvalue = elab_expr ctx env value in
        let t_inferred = tvalue.ty in
        let t_actual =
          match ty_ann with
          | None ->
              (match t_inferred with
               | TNullPtr ->
                   Error.failf pos
                     "cannot infer pointer type for 'null'; add a type \
                      annotation like 'let %s: *T = null;'" name
               | _ -> t_inferred)
          | Some ann ->
              let t_ann = resolve_type_ann ctx ann in
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
          match lookup_struct ctx path with
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
        let tvalue = elab_expr ctx env e in
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
  (List.rev !decls, tstmts)

(* Result of one walk over the program tree: every function (with its
   absolute module path and mangled C name), every struct, and every
   module (with its pub flag).  Order matches source order at each
   nesting level — preserves the user-visible declaration order in the
   emitted C. *)
type flat = {
  funcs : (string list * Ast.func * string) list;
  structs : (string list * Ast.struct_decl) list;
  modules : (string list * bool) list;
  (* Each `impl` block keeps its enclosing module path; target struct
     resolution (relative-to-scope, ancestor walk-up) happens later
     once the struct index is built. *)
  impls : (string list * Ast.impl_block) list;
}

let flatten_items program =
  let funcs = ref [] in
  let structs = ref [] in
  let modules = ref [] in
  let impls = ref [] in
  let rec walk path items =
    List.iter
      (fun item -> match item with
        | Ast.Function (f : Ast.func) ->
            let m = if f.name = "main" then "main" else mangle path f.name in
            funcs := (path, f, m) :: !funcs
        | Ast.Struct s ->
            structs := (path, s) :: !structs
        | Ast.Module m ->
            let mod_path = path @ [m.Ast.mname] in
            modules := (mod_path, m.Ast.mis_pub) :: !modules;
            walk mod_path m.Ast.mitems
        | Ast.Impl ib ->
            impls := (path, ib) :: !impls
        | Ast.Use { pos; _ } ->
            Error.failf pos
              "internal: 'use' declaration reached codegen unresolved \
               (loader pass missing?)")
      items
  in
  walk [] program;
  { funcs = List.rev !funcs;
    structs = List.rev !structs;
    modules = List.rev !modules;
    impls = List.rev !impls }

(* Build the global function index: every function with its module path,
   exile-side name, and signature.  main() is excluded — it is not callable. *)
let build_global_index ~struct_index ~modules flat =
  List.filter_map
    (fun (p, (f : Ast.func), mangled) ->
      if f.name = "main" then None
      else
        let ctx = {
          global = []; structs = struct_index;
          modules; scope = p
        } in
        Some
          (p, f.name,
           { param_tys =
               List.map (fun pp -> resolve_type_ann ctx pp.Ast.pty) f.params;
             ret_ty = Option.map (resolve_type_ann ctx) f.ret_ty;
             mangled;
             fn_pub = f.is_pub }))
    flat

(* Build the struct registry from the flattened struct declarations.
   Two-pass: first collect every struct's absolute path with empty
   fields, then resolve each declaration's field types against that
   skeleton.  Two passes are necessary because field types can refer
   to other structs declared in any order, and `resolve_type_ann`
   needs to see them all to rewrite relative paths to absolute. *)
let build_struct_index ~modules struct_flat =
  let skeleton =
    List.map
      (fun (p, (s : Ast.struct_decl)) ->
        { sname_path = p @ [s.sname];
          sfields_ty = [];
          sis_pub = s.sis_pub })
      struct_flat
  in
  List.map2
    (fun (p, (s : Ast.struct_decl)) skel ->
      let ctx = {
        global = []; structs = skeleton;
        modules; scope = p
      } in
      { skel with
        sfields_ty =
          List.map (fun (n, t) -> (n, resolve_type_ann ctx t)) s.sfields })
    struct_flat skeleton

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
let expand_impls flat struct_index modules =
  let resolved =
    List.map
      (fun (parent_path, ib) ->
        let ctx = {
          global = []; structs = struct_index;
          modules; scope = parent_path;
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

let check_program program : tprogram =
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
  let struct_index = build_struct_index ~modules:flat.modules flat.structs in
  let (impl_funcs, virtual_modules) =
    expand_impls flat struct_index flat.modules
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
  let global = build_global_index ~struct_index ~modules all_funcs in
  let tp_funcs =
    List.map
      (fun (path, (f : Ast.func), mangled) ->
        let ctx = { global; structs = struct_index; modules; scope = path } in
        let param_tys =
          List.map (fun (p : Ast.param) -> resolve_type_ann ctx p.pty) f.params
        in
        let ret_ty = Option.map (resolve_type_ann ctx) f.ret_ty in
        let param_env =
          List.combine (List.map (fun (p : Ast.param) -> p.pname) f.params)
            param_tys
        in
        let (lets, tbody) = elab_body ctx param_env f.body in
        { tf_path = path; tf_func = f; tf_mangled = mangled;
          tf_param_tys = param_tys; tf_ret_ty = ret_ty;
          tf_body = tbody; tf_lets = lets })
      all_funcs
  in
  let tp_tuple_types = collect_tuple_types_of tp_funcs in
  let tp_uses_heap = uses_heap_of tp_funcs in
  { tp_funcs;
    tp_struct_decls = flat.structs;
    tp_struct_index = struct_index;
    tp_global = global;
    tp_modules = modules;
    tp_uses_heap;
    tp_tuple_types }
