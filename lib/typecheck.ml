(* Semantic / type-checking layer.  Owns the typed view of the program:
   the type domain, name mangling, function lookup with module visibility,
   builtin signatures, expression typing, and let-collection.  Codegen
   consumes this module's exports — it does not duplicate type rules. *)

type int_kind = { signed : bool; width : Ast.int_width }

type typ =
  | TInt of int_kind
  | TBool
  | TString
  | TTuple of typ list
  | TStruct of string list             (* absolute path: e.g. ["foo"; "Point"] *)
  | TPtr of typ                        (* `*T` *)
  | TNullPtr                           (* type of `null` literal — compatible
                                          with any TPtr; never reaches codegen
                                          as a declaration type *)

(* Default integer type — what `int` and bare integer literals reduce to. *)
let t_i32 = TInt { signed = true; width = Ast.W32 }

let int_width_bits = function Ast.W8 -> 8 | Ast.W16 -> 16 | Ast.W32 -> 32

(* Does literal value [n] fit into an integer type of given signedness/width?
   OCaml's int is 63-bit on a 64-bit host, so all our ranges fit safely. *)
let int_fits n typ =
  match typ with
  | TInt { signed = true; width = Ast.W8 } -> n >= -128 && n <= 127
  | TInt { signed = false; width = Ast.W8 } -> n >= 0 && n <= 255
  | TInt { signed = true; width = Ast.W16 } -> n >= -32768 && n <= 32767
  | TInt { signed = false; width = Ast.W16 } -> n >= 0 && n <= 65535
  | TInt { signed = true; width = Ast.W32 } ->
      n >= -2147483648 && n <= 2147483647
  | TInt { signed = false; width = Ast.W32 } -> n >= 0 && n <= 4294967295
  | _ -> false

type fn_sig = {
  param_tys : typ list;
  ret_ty : typ option;
  mangled : string;            (* C-level name (e.g. "ex_foo" or "foo__bar") *)
  fn_pub : bool;
}

(* Struct signatures share the same module-aware resolution as functions —
   the registered path is the struct's absolute location. *)
type struct_sig = {
  sname_path : string list;     (* full path including struct name, e.g. ["foo"; "Point"] *)
  sfields_ty : (string * typ) list;
  sis_pub : bool;
}

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

(* Try to find a function in [global] at exactly [(mod_path, name)].
   Returns the resolved (mod_path, sig) when found, so callers can do
   visibility checks against the actual location. *)
let try_resolve ctx mod_path name =
  List.find_map
    (fun (p, n, s) ->
      if p = mod_path && n = name then Some (mod_path, s) else None)
    ctx.global

(* Resolve a call path to a function.  Walks the current scope from the
   deepest ancestor down to the root, trying [prefix @ suggested_mod] at
   each level. *)
let lookup_fn ctx (path : string list) =
  let (suggested_mod, name) =
    match List.rev path with
    | [] -> failwith "empty call path"
    | n :: rest -> (List.rev rest, n)
  in
  let rec walk prefix =
    match try_resolve ctx (prefix @ suggested_mod) name with
    | Some r -> Some r
    | None ->
        (match prefix with
         | [] -> None
         | _ -> walk (List.rev (List.tl (List.rev prefix))))
  in
  walk ctx.scope

(* Same ancestor walk-up as functions, but for struct names. *)
let try_resolve_struct ctx mod_path name =
  List.find_opt
    (fun s -> s.sname_path = mod_path @ [name])
    ctx.structs

let lookup_struct ctx (path : string list) =
  let (suggested_mod, name) =
    match List.rev path with
    | [] -> failwith "empty struct path"
    | n :: rest -> (List.rev rest, n)
  in
  let rec walk prefix =
    match try_resolve_struct ctx (prefix @ suggested_mod) name with
    | Some r -> Some r
    | None ->
        (match prefix with
         | [] -> None
         | _ -> walk (List.rev (List.tl (List.rev prefix))))
  in
  walk ctx.scope

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
  | Ast.TyStr -> TString
  | Ast.TyBool -> TBool
  | Ast.TyTuple ts -> TTuple (List.map type_of_ann ts)
  | Ast.TyStruct path -> TStruct path
  | Ast.TyPtr t -> TPtr (type_of_ann t)

(* Recognise an integer literal expression — bare or negated — for type-fitting
   checks at let-binding sites. *)
let expr_int_lit = function
  | Ast.IntLit n -> Some n
  | Ast.Neg (Ast.IntLit n) -> Some (-n)
  | _ -> None

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

let int_typ_name signed width =
  let prefix = if signed then "i" else "u" in
  let bits = match width with Ast.W8 -> "8" | Ast.W16 -> "16" | Ast.W32 -> "32" in
  prefix ^ bits

let rec typ_name = function
  | TInt { signed; width } -> int_typ_name signed width
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts -> "(" ^ String.concat ", " (List.map typ_name ts) ^ ")"
  | TStruct path -> String.concat "::" path
  | TPtr t -> "*" ^ typ_name t
  | TNullPtr -> "*<null>"

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
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts ->
      Printf.sprintf "tup%d_%s" (List.length ts)
        (String.concat "_" (List.map mangle_typ ts))
  | TStruct path ->
      (match List.rev path with
       | [] -> failwith "empty struct path"
       | n :: rest -> mangle (List.rev rest) n)
  | TPtr t -> "ptr_" ^ mangle_typ t
  | TNullPtr -> failwith "TNullPtr should never be mangled"

let tuple_struct_name ts = "ex_" ^ mangle_typ (TTuple ts)

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
  | Ast.BinOp (op, l, r) ->
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
  | Ast.Neg e ->
      let _ = type_of ctx env e in
      t_i32
  | Ast.Cast (e, ann, pos) ->
      let src = type_of ctx env e in
      let tgt = type_of_ann ann in
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

(* Resolve operand types for a BinOp.  An integer literal on either side
   adopts the other operand's int type if it fits, so `x + 5` keeps x's
   width without forcing a cast. *)
and binop_operand_types ctx env l r =
  match l, r with
  | Ast.IntLit n, _ ->
      let rt = type_of ctx env r in
      let lt =
        match rt with
        | TInt _ when int_fits n rt -> rt
        | _ -> type_of ctx env l
      in
      (lt, rt)
  | _, Ast.IntLit n ->
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
  if (not s.sis_pub) && not (is_prefix
                               (List.rev (List.tl (List.rev s.sname_path)))
                               ctx.scope) then
    Error.failf pos "struct '%s' is private to module '%s'"
      display
      (let parent = List.rev (List.tl (List.rev s.sname_path)) in
       if parent = [] then "<root>" else String.concat "::" parent);
  let rec dups = function
    | [] -> ()
    | (n, _) :: rest ->
        if List.exists (fun (m, _) -> m = n) rest then
          Error.failf pos
            "duplicate field '%s' in struct literal '%s'" n display;
        dups rest
  in
  dups fields;
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
      let lit_match =
        match expr_int_lit fe, fty with
        | Some n, TInt _ -> int_fits n fty
        | _ -> false
      in
      if not (typ_eq act fty) && not lit_match then
        Error.failf pos
          "field '%s' of struct '%s': expected %s, got %s"
          fn display (typ_name fty) (typ_name act))
    fields;
  s

(* Collect let-bound (name, type) pairs for C89 function-top hoisting.
   Type resolution uses block-scoped env (then/else branches start from
   the same pre-if env — no leak). Accumulation is function-scoped:
   one name per function, no shadowing of parameters. *)
let collect_lets ctx param_env stmts =
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
    | [] -> env
    | Ast.Let { name; value; ty_ann; pos } :: rest ->
        let t_inferred = type_of ctx env value in
        (match t_inferred with
         | TTuple _ ->
             Error.failf pos
               "tuple value must be destructured: use 'let (...) = ...' \
                instead of 'let %s = ...'" name
         | _ -> ());
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
              let t_ann = type_of_ann ann in
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
        walk ((name, t_actual) :: env) rest
    | Ast.LetTuple { names; value; pos } :: rest ->
        let t = type_of ctx env value in
        let elem_tys =
          match t with
          | TTuple ts -> ts
          | _ ->
              Error.failf pos
                "destructuring 'let (...)' expects a tuple value, got %s"
                (typ_name t)
        in
        let n_names = List.length names in
        let n_elems = List.length elem_tys in
        if n_names <> n_elems then
          Error.failf pos
            "destructuring 'let (...)' has %d names but value is a %d-tuple"
            n_names n_elems;
        let pairs = List.combine names elem_tys in
        (* Reject in-tuple duplicates up front for a clearer message. *)
        let rec check_dups = function
          | [] -> ()
          | (n, _) :: rest_pairs ->
              if List.exists (fun (m, _) -> m = n) rest_pairs then
                Error.failf pos
                  "duplicate name '%s' in 'let (...)'" n;
              check_dups rest_pairs
        in
        check_dups pairs;
        List.iter (fun (n, ty) -> add_decl n ty pos) pairs;
        walk (List.rev_append pairs env) rest
    | Ast.Assign { name; value; pos } :: rest ->
        if not (List.mem_assoc name env) then
          Error.failf pos "assignment to undefined variable '%s'" name;
        let _ = type_of ctx env value in
        walk env rest
    | Ast.AssignField { target; field; value; pos } :: rest ->
        let tt = type_of ctx env target in
        let path =
          match tt with
          | TStruct p -> p
          | TPtr (TStruct p) -> p   (* auto-deref pointer-to-struct LHS *)
          | _ ->
              Error.failf pos
                "assignment to field '.%s' requires a struct value or \
                 pointer to struct, got %s"
                field (typ_name tt)
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
        let act = type_of ctx env value in
        let lit_match =
          match expr_int_lit value, fty with
          | Some n, TInt _ -> int_fits n fty
          | _ -> false
        in
        if not (typ_eq act fty) && not lit_match then
          Error.failf pos
            "field '%s' of struct '%s': expected %s, got %s"
            field (String.concat "::" path) (typ_name fty) (typ_name act);
        walk env rest
    | Ast.AssignDeref { target; value; pos } :: rest ->
        let tt = type_of ctx env target in
        let inner =
          match tt with
          | TPtr t -> t
          | _ ->
              Error.failf pos
                "assignment through '*' requires a pointer, got %s"
                (typ_name tt)
        in
        let act = type_of ctx env value in
        let lit_match =
          match expr_int_lit value, inner with
          | Some n, TInt _ -> int_fits n inner
          | _ -> false
        in
        if not (typ_eq act inner) && not lit_match then
          Error.failf pos
            "deref assignment: expected %s, got %s"
            (typ_name inner) (typ_name act);
        walk env rest
    | Ast.Return (e, _) :: rest ->
        let _ = type_of ctx env e in
        walk env rest
    | Ast.ExprStmt e :: rest ->
        (* ExprStmt discards the result, so void calls are allowed here. *)
        let _ = type_of ~allow_void:true ctx env e in
        walk env rest
    | Ast.If { cond; then_body; else_body } :: rest ->
        let _ = type_of ctx env cond in
        let _ = walk env then_body in
        let _ = walk env else_body in
        walk (param_env @ List.rev !decls) rest
    | Ast.While { cond; body } :: rest ->
        let _ = type_of ctx env cond in
        let _ = walk env body in
        walk (param_env @ List.rev !decls) rest
    | Ast.Defer { body; _ } :: rest ->
        (* Defer body type-checks like the surrounding scope's stmts but
           introduces no new env bindings to the caller. *)
        let _ = walk env body in
        walk env rest
  in
  let _ = walk param_env stmts in
  List.rev !decls

(* Walk the program tree and produce a flat list of every function with its
   module path and mangled C name.  Recurses into nested modules. *)
let flatten_funcs program =
  let rec walk path acc items =
    List.fold_left
      (fun acc item -> match item with
        | Ast.Function f ->
            let m = if f.name = "main" then "main" else mangle path f.name in
            (path, f, m) :: acc
        | Ast.Module m ->
            walk (path @ [m.Ast.mname]) acc m.Ast.mitems
        | Ast.Struct _ -> acc
        | Ast.Use { pos; _ } ->
            Error.failf pos
              "internal: 'use' declaration reached codegen unresolved \
               (loader pass missing?)")
      acc items
  in
  List.rev (walk [] [] program)

(* Walk the program tree and produce a flat list of every struct with its
   module path; the C name for each is `mangle path name` (so it lives in
   the same naming convention as functions). *)
let flatten_structs program =
  let rec walk path acc items =
    List.fold_left
      (fun acc item -> match item with
        | Ast.Struct s -> (path, s) :: acc
        | Ast.Module m -> walk (path @ [m.Ast.mname]) acc m.Ast.mitems
        | Ast.Function _ | Ast.Use _ -> acc)
      acc items
  in
  List.rev (walk [] [] program)

(* Walk the program tree and produce a flat list of every module with its
   absolute path and pub flag.  Used for visibility checks on qualified calls. *)
let flatten_modules program =
  let rec walk path acc items =
    List.fold_left
      (fun acc item -> match item with
        | Ast.Function _ | Ast.Struct _ -> acc
        | Ast.Module m ->
            let mod_path = path @ [m.Ast.mname] in
            walk mod_path ((mod_path, m.Ast.mis_pub) :: acc) m.Ast.mitems
        | Ast.Use _ -> acc)
      acc items
  in
  List.rev (walk [] [] program)

(* Build the global function index: every function with its module path,
   exile-side name, and signature.  main() is excluded — it is not callable. *)
let build_global_index flat =
  List.filter_map
    (fun (p, (f : Ast.func), mangled) ->
      if f.name = "main" then None
      else
        Some
          (p, f.name,
           { param_tys = List.map (fun p -> type_of_ann p.Ast.pty) f.params;
             ret_ty = Option.map type_of_ann f.ret_ty;
             mangled;
             fn_pub = f.is_pub }))
    flat

(* Build the struct registry from the flattened struct declarations. *)
let build_struct_index struct_flat =
  List.map
    (fun (p, (s : Ast.struct_decl)) ->
      { sname_path = p @ [s.sname];
        sfields_ty = List.map (fun (n, t) -> (n, type_of_ann t)) s.sfields;
        sis_pub = s.sis_pub })
    struct_flat

(* Walk every function body looking for tuple types in use.  We collect
   them deduplicated by mangled name; codegen later emits one C struct per
   unique tuple shape.  Sources of tuples: fn signatures (ret_ty, params)
   and any `TupleLit` expression in code (typed via `type_of`). *)
let collect_tuple_types flat global structs modules =
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
  let rec walk_expr ctx env e =
    (match e with
     | Ast.TupleLit (es, _) ->
         walk_typ (type_of ctx env e);
         List.iter (walk_expr ctx env) es
     | Ast.StructLit { fields; _ } ->
         List.iter (fun (_, fe) -> walk_expr ctx env fe) fields
     | Ast.FieldAccess (target, _, _) -> walk_expr ctx env target
     | Ast.Ref (sub, _) | Ast.Deref (sub, _) -> walk_expr ctx env sub
     | Ast.Cast (sub, _, _) -> walk_expr ctx env sub
     | Ast.Neg sub -> walk_expr ctx env sub
     | Ast.BinOp (_, l, r) -> walk_expr ctx env l; walk_expr ctx env r
     | Ast.Call (_, args, _) -> List.iter (walk_expr ctx env) args
     | _ -> ())
  in
  let rec walk_stmt ctx env = function
    | Ast.Let { value; _ } -> walk_expr ctx env value
    | Ast.LetTuple { value; _ } -> walk_expr ctx env value
    | Ast.Assign { value; _ } -> walk_expr ctx env value
    | Ast.AssignField { target; value; _ } ->
        walk_expr ctx env target; walk_expr ctx env value
    | Ast.AssignDeref { target; value; _ } ->
        walk_expr ctx env target; walk_expr ctx env value
    | Ast.Return (e, _) -> walk_expr ctx env e
    | Ast.ExprStmt e -> walk_expr ctx env e
    | Ast.If { cond; then_body; else_body } ->
        walk_expr ctx env cond;
        List.iter (walk_stmt ctx env) then_body;
        List.iter (walk_stmt ctx env) else_body
    | Ast.While { cond; body } ->
        walk_expr ctx env cond;
        List.iter (walk_stmt ctx env) body
    | Ast.Defer { body; _ } ->
        List.iter (walk_stmt ctx env) body
  in
  List.iter
    (fun (path, (f : Ast.func), _) ->
      Option.iter walk_typ_ann f.ret_ty;
      List.iter (fun (p : Ast.param) -> walk_typ_ann p.pty) f.params;
      let ctx = { global; structs; modules; scope = path } in
      let param_env =
        List.map (fun (p : Ast.param) ->
          (p.pname, type_of_ann p.pty)) f.params
      in
      let lets = collect_lets ctx param_env f.body in
      let env = param_env @ lets in
      List.iter (walk_stmt ctx env) f.body)
    flat;
  List.rev !seen

(* Detect any heap usage in the program — `new ...` expressions or `free(p)`
   calls — so codegen can conditionally include `<stdlib.h>` for malloc/free. *)
let uses_heap flat =
  let rec walk_expr = function
    | Ast.New _ -> true
    | Ast.Call (["free"], _, _) -> true
    | Ast.Ref (e, _) | Ast.Deref (e, _) | Ast.Cast (e, _, _) | Ast.Neg e ->
        walk_expr e
    | Ast.BinOp (_, l, r) -> walk_expr l || walk_expr r
    | Ast.Call (_, args, _) -> List.exists walk_expr args
    | Ast.TupleLit (es, _) -> List.exists walk_expr es
    | Ast.StructLit { fields; _ } ->
        List.exists (fun (_, e) -> walk_expr e) fields
    | Ast.FieldAccess (target, _, _) -> walk_expr target
    | _ -> false
  in
  let rec walk_stmt = function
    | Ast.Let { value; _ } | Ast.LetTuple { value; _ }
    | Ast.Assign { value; _ } | Ast.Return (value, _)
    | Ast.ExprStmt value -> walk_expr value
    | Ast.AssignField { target; value; _ }
    | Ast.AssignDeref { target; value; _ } ->
        walk_expr target || walk_expr value
    | Ast.If { cond; then_body; else_body } ->
        walk_expr cond
        || List.exists walk_stmt then_body
        || List.exists walk_stmt else_body
    | Ast.While { cond; body } ->
        walk_expr cond || List.exists walk_stmt body
    | Ast.Defer { body; _ } -> List.exists walk_stmt body
  in
  List.exists
    (fun (_, (f : Ast.func), _) -> List.exists walk_stmt f.body)
    flat

(* Result of one validation pass over the whole program.  Codegen consumes
   this without re-running validation: typecheck guarantees that every
   function's body has been type-checked, every let has a recorded type
   for hoisting, and the struct/module/global indexes are ready for
   resolution. *)
type checked_func = {
  cf_path : string list;
  cf_func : Ast.func;
  cf_mangled : string;
  cf_lets : (string * typ) list;
}

type checked_program = {
  cp_funcs : checked_func list;
  cp_struct_decls : (string list * Ast.struct_decl) list;
  cp_struct_index : struct_sig list;
  cp_global : (string list * string * fn_sig) list;
  cp_modules : (string list * bool) list;
  cp_uses_heap : bool;
  cp_tuple_types : (string * typ) list;
}

let check_program program =
  let flat = flatten_funcs program in
  (* main() must be at top level, not inside a module. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
      if f.name = "main" && path <> [] then
        Error.raise_ f.pos
          "'main' must be at top level, not inside a module")
    flat;
  (* Top-level function names land in C unmangled (modulo the `ex_`
     prefix), so they must not collide with C keywords.  Mod-internal fns
     get a `mod__` prefix and are safe. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
      if path = [] then check_c_ident f.pos "function" f.name)
    flat;
  (* Param names are emitted unprefixed in C parameter lists, so they
     also need the keyword check.  (Local lets are checked inside
     collect_lets.) *)
  List.iter
    (fun (_, (f : Ast.func), _) ->
      List.iter
        (fun (p : Ast.param) -> check_c_ident f.pos "parameter" p.pname)
        f.params)
    flat;
  let global = build_global_index flat in
  let struct_decls = flatten_structs program in
  let struct_index = build_struct_index struct_decls in
  let modules = flatten_modules program in
  (* Per-fn validation: collect_lets type-checks the body and returns the
     hoisted let-decl list.  Codegen will use cf_lets directly without
     repeating the walk. *)
  let cp_funcs =
    List.map
      (fun (path, (f : Ast.func), mangled) ->
        let ctx = { global; structs = struct_index; modules; scope = path } in
        let param_env =
          List.map (fun (p : Ast.param) -> (p.pname, type_of_ann p.pty))
            f.params
        in
        let lets = collect_lets ctx param_env f.body in
        { cf_path = path; cf_func = f; cf_mangled = mangled; cf_lets = lets })
      flat
  in
  let cp_tuple_types =
    collect_tuple_types flat global struct_index modules
  in
  let cp_uses_heap = uses_heap flat in
  { cp_funcs;
    cp_struct_decls = struct_decls;
    cp_struct_index = struct_index;
    cp_global = global;
    cp_modules = modules;
    cp_uses_heap;
    cp_tuple_types }
