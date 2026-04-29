type int_kind = { signed : bool; width : Ast.int_width }

type typ =
  | TInt of int_kind
  | TBool
  | TString

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
  mangled : string;            (* C-level name (e.g. "foo__bar" or "main") *)
  fn_pub : bool;
}

(* Resolution context: every function in the program (keyed by module path
   and name), plus a flat list of every module with its pub flag, plus the
   path of the function we are currently emitting code for.  Local calls
   (path = [name]) resolve within the current scope; qualified calls
   (path > 1) resolve absolutely from the root and are visibility-checked
   per segment. *)
type fn_ctx = {
  global : (string list * string * fn_sig) list;
  modules : (string list * bool) list;   (* full path -> is_pub *)
  scope : string list;
}

(* Is xs a prefix of ys? *)
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

(* Resolve a call path to a function.  We walk the current scope from the
   deepest ancestor down to the root, trying [prefix @ suggested_mod] at
   each level.  Local function names (path=[name]) thus shadow outer
   definitions; multi-segment paths likewise try the most specific match
   first and fall back to absolute (root) lookup. *)
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

(* Mangle a function name with its module path.  Top-level (path = []) keeps
   the bare name.  Inside a module, names join with "__".  C99 reserves
   double-underscores for the implementation, but we accept the risk for
   simplicity — collisions only happen if a user writes "__" in identifiers. *)
let mangle path name =
  match path with
  | [] -> name
  | _ -> String.concat "__" path ^ "__" ^ name

let add_separated buf sep f xs =
  List.iteri
    (fun i x -> if i > 0 then Buffer.add_string buf sep; f x)
    xs

let escape_c s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\000' -> Buffer.add_string buf "\\0"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let type_of_ann = function
  | Ast.TyInt { signed; width } -> TInt { signed; width }
  | Ast.TyStr -> TString
  | Ast.TyBool -> TBool

(* Recognise an integer literal expression — bare or negated — for type-fitting
   checks at let-binding sites. *)
let expr_int_lit = function
  | Ast.IntLit n -> Some n
  | Ast.Neg (Ast.IntLit n) -> Some (-n)
  | _ -> None

(* C89 reserved words.  Exile identifiers that survive into the generated C
   without mangling (locals, parameters, top-level function names) must not
   collide. *)
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

let typ_name = function
  | TInt { signed; width } -> int_typ_name signed width
  | TBool -> "bool"
  | TString -> "str"

let c_type_prefix = function
  | TInt { signed; width } ->
      let s = if signed then "" else "unsigned " in
      let core = match width with
        | Ast.W8 -> "char"
        | Ast.W16 -> "short"
        | Ast.W32 -> "int"
      in
      (* "signed char" is needed because C89 leaves plain `char` signedness
         implementation-defined; for i8 we must be explicit. *)
      let signed_core =
        if signed && width = Ast.W8 then "signed char " else core ^ " "
      in
      s ^ signed_core
  | TBool -> "int "
  | TString -> "const char *"

let c_decl t name = c_type_prefix t ^ name

let c_param p = c_decl (type_of_ann p.Ast.pty) p.Ast.pname

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
  | Ast.Call ([ "print" ], args, _) ->
      List.iter (fun a -> ignore (type_of ctx env a)) args;
      t_i32
  | Ast.Call (path, args, pos) ->
      let arg_tys = List.map (type_of ctx env) args in
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
               if exp <> act then
                 Error.failf pos
                   "argument %d of '%s': expected %s, got %s"
                   (i + 1) display (typ_name exp) (typ_name act))
             (List.combine param_tys arg_tys);
           (match ret_ty with
            | Some t -> t
            | None when allow_void -> t_i32   (* placeholder, caller discards *)
            | None ->
                Error.failf pos "'%s' returns void, cannot use as a value" display))

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

let prec = function
  | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq | Ast.EqEq | Ast.NotEq -> 0
  | Ast.Add | Ast.Sub -> 1
  | Ast.Mul | Ast.Div -> 2

let rec gen_expr buf ctx env = function
  | Ast.IntLit n -> Buffer.add_string buf (string_of_int n)
  | Ast.BoolLit b -> Buffer.add_string buf (if b then "1" else "0")
  | Ast.StringLit s ->
      Buffer.add_char buf '"';
      Buffer.add_string buf (escape_c s);
      Buffer.add_char buf '"'
  | Ast.Var (name, _) -> Buffer.add_string buf name
  | Ast.Neg e ->
      Buffer.add_char buf '-';
      (match e with
       | Ast.IntLit _ | Ast.Var _ -> gen_expr buf ctx env e
       | _ ->
           Buffer.add_char buf '(';
           gen_expr buf ctx env e;
           Buffer.add_char buf ')')
  | Ast.Cast (e, ann, _) ->
      let prefix = c_type_prefix (type_of_ann ann) in
      let trimmed =
        if String.length prefix > 0 && prefix.[String.length prefix - 1] = ' '
        then String.sub prefix 0 (String.length prefix - 1)
        else prefix
      in
      Buffer.add_string buf "((";
      Buffer.add_string buf trimmed;
      Buffer.add_string buf ")";
      gen_expr buf ctx env e;
      Buffer.add_char buf ')'
  | Ast.BinOp (op, l, r) ->
      let op_str =
        match op with
        | Ast.Add -> " + " | Ast.Sub -> " - "
        | Ast.Mul -> " * " | Ast.Div -> " / "
        | Ast.Lt -> " < " | Ast.Gt -> " > "
        | Ast.LtEq -> " <= " | Ast.GtEq -> " >= "
        | Ast.EqEq -> " == " | Ast.NotEq -> " != "
      in
      let p = prec op in
      (match l with
       | Ast.BinOp (lop, _, _) when prec lop < p ->
           Buffer.add_char buf '('; gen_expr buf ctx env l; Buffer.add_char buf ')'
       | _ -> gen_expr buf ctx env l);
      Buffer.add_string buf op_str;
      (match r with
       | Ast.BinOp (rop, _, _)
         when prec rop < p || (prec rop = p && (op = Ast.Sub || op = Ast.Div)) ->
           Buffer.add_char buf '('; gen_expr buf ctx env r; Buffer.add_char buf ')'
       | _ -> gen_expr buf ctx env r)
  | Ast.Call ([ "print" ], [ arg ], _) ->
      (* %d for signed (and bool), %u for unsigned.  Varargs default-promote
         smaller widths up to (un)signed int, so a single specifier per
         signedness works for all widths. *)
      let fmt =
        match type_of ctx env arg with
        | TBool -> "\"%d\\n\""
        | TInt { signed = true; _ } -> "\"%d\\n\""
        | TInt { signed = false; _ } -> "\"%u\\n\""
        | TString -> "\"%s\\n\""
      in
      Buffer.add_string buf "printf(";
      Buffer.add_string buf fmt;
      Buffer.add_string buf ", ";
      gen_expr buf ctx env arg;
      Buffer.add_char buf ')'
  | Ast.Call ([ "print" ], _, pos) ->
      Error.failf pos "print() takes exactly one argument"
  | Ast.Call (path, args, pos) ->
      let mangled =
        match lookup_fn ctx path with
        | Some (_, s) -> s.mangled
        | None ->
            Error.failf pos "unknown function '%s'" (String.concat "::" path)
      in
      Buffer.add_string buf mangled;
      Buffer.add_char buf '(';
      add_separated buf ", " (gen_expr buf ctx env) args;
      Buffer.add_char buf ')'

let rec gen_if buf ctx env indent cond then_body else_body =
  Buffer.add_string buf "if (";
  gen_expr buf ctx env cond;
  Buffer.add_string buf ") {\n";
  List.iter (gen_stmt buf ctx env (indent ^ "    ")) then_body;
  Buffer.add_string buf indent;
  Buffer.add_char buf '}';
  (match else_body with
   | [] -> Buffer.add_char buf '\n'
   | [ Ast.If { cond = ec; then_body = et; else_body = ee } ] ->
       Buffer.add_string buf " else ";
       gen_if buf ctx env indent ec et ee
   | _ ->
       Buffer.add_string buf " else {\n";
       List.iter (gen_stmt buf ctx env (indent ^ "    ")) else_body;
       Buffer.add_string buf indent;
       Buffer.add_string buf "}\n")

and gen_stmt buf ctx env indent = function
  | Ast.Let { name; value } | Ast.Assign { name; value } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf (name ^ " = ");
      gen_expr buf ctx env value;
      Buffer.add_string buf ";\n"
  | Ast.Return expr ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "return ";
      gen_expr buf ctx env expr;
      Buffer.add_string buf ";\n"
  | Ast.ExprStmt e ->
      Buffer.add_string buf indent;
      gen_expr buf ctx env e;
      Buffer.add_string buf ";\n"
  | Ast.If { cond; then_body; else_body } ->
      Buffer.add_string buf indent;
      gen_if buf ctx env indent cond then_body else_body
  | Ast.While { cond; body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "while (";
      gen_expr buf ctx env cond;
      Buffer.add_string buf ") {\n";
      List.iter (gen_stmt buf ctx env (indent ^ "    ")) body;
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"

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
        let t_actual =
          match ty_ann with
          | None -> t_inferred
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
                   if t_ann = t_inferred then t_ann
                   else
                     Error.failf pos
                       "variable '%s' declared as %s but initializer has type %s"
                       name (typ_name t_ann) (typ_name t_inferred))
        in
        add_decl name t_actual pos;
        walk ((name, t_actual) :: env) rest
    | Ast.Assign { name; value; pos } :: rest ->
        if not (List.mem_assoc name env) then
          Error.failf pos "assignment to undefined variable '%s'" name;
        let _ = type_of ctx env value in
        walk env rest
    | Ast.Return e :: rest ->
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
  in
  let _ = walk param_env stmts in
  List.rev !decls

(* Emit a function signature using a mangled C-level name (or "main" for the
   entry point — main() is special and not mangled).  Non-pub functions get
   a "static" linkage prefix so they are invisible across translation units
   (and act as documentation that they are module-internal). *)
let emit_fn_sig buf (f : Ast.func) mangled =
  if f.name = "main" then
    Buffer.add_string buf "int main(void)"
  else begin
    if not f.is_pub then Buffer.add_string buf "static ";
    let ret =
      match f.ret_ty with
      | None -> "void "
      | Some ty -> c_type_prefix (type_of_ann ty)
    in
    Buffer.add_string buf ret;
    Buffer.add_string buf mangled;
    Buffer.add_char buf '(';
    add_separated buf ", " (fun p -> Buffer.add_string buf (c_param p)) f.params;
    Buffer.add_char buf ')'
  end

let gen_function buf ctx (f : Ast.func) mangled =
  List.iter (fun p -> check_c_ident f.pos "parameter" p.Ast.pname) f.params;
  emit_fn_sig buf f mangled;
  Buffer.add_string buf " {\n";
  let param_env = List.map (fun p -> (p.Ast.pname, type_of_ann p.Ast.pty)) f.params in
  let lets = collect_lets ctx param_env f.body in
  let full_env = param_env @ lets in
  List.iter
    (fun (name, t) ->
      Buffer.add_string buf (Printf.sprintf "    %s;\n" (c_decl t name)))
    lets;
  List.iter (gen_stmt buf ctx full_env "    ") f.body;
  if f.name = "main" then Buffer.add_string buf "    return 0;\n";
  Buffer.add_string buf "}\n"

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
        | Ast.Use { pos; _ } ->
            Error.failf pos
              "internal: 'use' declaration reached codegen unresolved \
               (loader pass missing?)")
      acc items
  in
  List.rev (walk [] [] program)

(* Walk the program tree and produce a flat list of every module with its
   absolute path and pub flag.  Used for visibility checks on qualified calls. *)
let flatten_modules program =
  let rec walk path acc items =
    List.fold_left
      (fun acc item -> match item with
        | Ast.Function _ -> acc
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

let gen_program program =
  let flat = flatten_funcs program in
  (* main() must be at top level, not inside a module. *)
  List.iter
    (fun (path, f, _) ->
      if f.Ast.name = "main" && path <> [] then
        Error.raise_ f.Ast.pos
          "'main' must be at top level, not inside a module")
    flat;
  (* Top-level function names land in C unmangled, so they must not collide
     with C keywords. Names inside modules get a "mod__" prefix and are safe. *)
  List.iter
    (fun (path, f, _) ->
      if path = [] then check_c_ident f.Ast.pos "function" f.Ast.name)
    flat;
  let global = build_global_index flat in
  let modules = flatten_modules program in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n";
  let non_main = List.filter (fun (_, f, _) -> f.Ast.name <> "main") flat in
  if non_main <> [] then begin
    Buffer.add_char buf '\n';
    List.iter
      (fun (_, f, mangled) ->
        emit_fn_sig buf f mangled;
        Buffer.add_string buf ";\n")
      non_main
  end;
  Buffer.add_char buf '\n';
  let last = List.length flat - 1 in
  List.iteri
    (fun i (path, f, mangled) ->
      let ctx = { global; modules; scope = path } in
      gen_function buf ctx f mangled;
      if i < last then Buffer.add_char buf '\n')
    flat;
  Buffer.contents buf
