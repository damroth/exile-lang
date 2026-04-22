type typ = TInt | TBool | TString

type fn_sig = {
  param_tys : typ list;
  ret_ty : typ option;
}

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

let type_of_ann = function Ast.TyInt -> TInt | Ast.TyStr -> TString | Ast.TyBool -> TBool

let typ_name = function TInt -> "int" | TBool -> "bool" | TString -> "str"

let c_type_prefix = function TInt | TBool -> "int " | TString -> "const char *"

let c_decl t name = c_type_prefix t ^ name

let c_param p = c_decl (type_of_ann p.Ast.pty) p.Ast.pname

let rec type_of fn_table env = function
  | Ast.IntLit _ -> TInt
  | Ast.BoolLit _ -> TBool
  | Ast.StringLit _ -> TString
  | Ast.BinOp (op, l, r) ->
      let _ = type_of fn_table env l in
      let _ = type_of fn_table env r in
      (match op with
       | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> TInt
       | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq | Ast.EqEq | Ast.NotEq -> TBool)
  | Ast.Neg e ->
      let _ = type_of fn_table env e in
      TInt
  | Ast.Var (name, pos) ->
      (match List.assoc_opt name env with
       | Some t -> t
       | None -> Error.failf pos "undefined variable '%s'" name)
  | Ast.Call ("print", args, _) ->
      List.iter (fun a -> ignore (type_of fn_table env a)) args;
      TInt
  | Ast.Call (name, args, pos) ->
      let arg_tys = List.map (type_of fn_table env) args in
      (match List.assoc_opt name fn_table with
       | None -> Error.failf pos "unknown function '%s'" name
       | Some { param_tys; ret_ty } ->
           let expected = List.length param_tys in
           let got = List.length args in
           if expected <> got then
             Error.failf pos "function '%s' expects %d argument(s), got %d"
               name expected got;
           List.iteri
             (fun i (exp, act) ->
               if exp <> act then
                 Error.failf pos
                   "argument %d of '%s': expected %s, got %s"
                   (i + 1) name (typ_name exp) (typ_name act))
             (List.combine param_tys arg_tys);
           (match ret_ty with
            | Some t -> t
            | None -> Error.failf pos "'%s' returns void, cannot use as a value" name))

let prec = function
  | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq | Ast.EqEq | Ast.NotEq -> 0
  | Ast.Add | Ast.Sub -> 1
  | Ast.Mul | Ast.Div -> 2

let rec gen_expr buf fn_table env = function
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
       | Ast.IntLit _ | Ast.Var _ -> gen_expr buf fn_table env e
       | _ ->
           Buffer.add_char buf '(';
           gen_expr buf fn_table env e;
           Buffer.add_char buf ')')
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
           Buffer.add_char buf '('; gen_expr buf fn_table env l; Buffer.add_char buf ')'
       | _ -> gen_expr buf fn_table env l);
      Buffer.add_string buf op_str;
      (match r with
       | Ast.BinOp (rop, _, _)
         when prec rop < p || (prec rop = p && (op = Ast.Sub || op = Ast.Div)) ->
           Buffer.add_char buf '('; gen_expr buf fn_table env r; Buffer.add_char buf ')'
       | _ -> gen_expr buf fn_table env r)
  | Ast.Call ("print", [ arg ], _) ->
      let fmt =
        match type_of fn_table env arg with
        | TInt | TBool -> "\"%d\\n\""
        | TString -> "\"%s\\n\""
      in
      Buffer.add_string buf "printf(";
      Buffer.add_string buf fmt;
      Buffer.add_string buf ", ";
      gen_expr buf fn_table env arg;
      Buffer.add_char buf ')'
  | Ast.Call ("print", _, pos) ->
      Error.failf pos "print() takes exactly one argument"
  | Ast.Call (name, args, _) ->
      Buffer.add_string buf name;
      Buffer.add_char buf '(';
      add_separated buf ", " (gen_expr buf fn_table env) args;
      Buffer.add_char buf ')'

let rec gen_if buf fn_table env indent cond then_body else_body =
  Buffer.add_string buf "if (";
  gen_expr buf fn_table env cond;
  Buffer.add_string buf ") {\n";
  List.iter (gen_stmt buf fn_table env (indent ^ "    ")) then_body;
  Buffer.add_string buf indent;
  Buffer.add_char buf '}';
  (match else_body with
   | [] -> Buffer.add_char buf '\n'
   | [ Ast.If { cond = ec; then_body = et; else_body = ee } ] ->
       Buffer.add_string buf " else ";
       gen_if buf fn_table env indent ec et ee
   | _ ->
       Buffer.add_string buf " else {\n";
       List.iter (gen_stmt buf fn_table env (indent ^ "    ")) else_body;
       Buffer.add_string buf indent;
       Buffer.add_string buf "}\n")

and gen_stmt buf fn_table env indent = function
  | Ast.Let { name; value } | Ast.Assign { name; value } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf (name ^ " = ");
      gen_expr buf fn_table env value;
      Buffer.add_string buf ";\n"
  | Ast.Return expr ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "return ";
      gen_expr buf fn_table env expr;
      Buffer.add_string buf ";\n"
  | Ast.ExprStmt e ->
      Buffer.add_string buf indent;
      gen_expr buf fn_table env e;
      Buffer.add_string buf ";\n"
  | Ast.If { cond; then_body; else_body } ->
      Buffer.add_string buf indent;
      gen_if buf fn_table env indent cond then_body else_body
  | Ast.While { cond; body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "while (";
      gen_expr buf fn_table env cond;
      Buffer.add_string buf ") {\n";
      List.iter (gen_stmt buf fn_table env (indent ^ "    ")) body;
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"

(* Collect let-bound (name, type) pairs for C89 function-top hoisting.
   Type resolution uses block-scoped env (then/else branches start from
   the same pre-if env — no leak). Accumulation is function-scoped:
   one name per function, no shadowing of parameters. *)
let collect_lets fn_table param_env stmts =
  let decls = ref [] in
  let add_decl name t pos =
    if List.mem_assoc name param_env then
      Error.failf pos "variable '%s' shadows a parameter" name;
    if List.mem_assoc name !decls then
      Error.failf pos "variable '%s' already declared in this function" name;
    decls := (name, t) :: !decls
  in
  let rec walk env = function
    | [] -> env
    | Ast.Let { name; value; ty_ann; pos } :: rest ->
        let t = type_of fn_table env value in
        (match ty_ann with
         | Some ann when type_of_ann ann <> t ->
             Error.failf pos "variable '%s' declared as %s but initializer has type %s"
               name (typ_name (type_of_ann ann)) (typ_name t)
         | _ -> ());
        add_decl name t pos;
        walk ((name, t) :: env) rest
    | Ast.Assign { name; value; pos } :: rest ->
        if not (List.mem_assoc name env) then
          Error.failf pos "assignment to undefined variable '%s'" name;
        let _ = type_of fn_table env value in
        walk env rest
    | Ast.Return e :: rest ->
        let _ = type_of fn_table env e in
        walk env rest
    | Ast.ExprStmt e :: rest ->
        let _ = type_of fn_table env e in
        walk env rest
    | Ast.If { cond; then_body; else_body } :: rest ->
        let _ = type_of fn_table env cond in
        let _ = walk env then_body in
        let _ = walk env else_body in
        walk (param_env @ List.rev !decls) rest
    | Ast.While { cond; body } :: rest ->
        let _ = type_of fn_table env cond in
        let _ = walk env body in
        walk (param_env @ List.rev !decls) rest
  in
  let _ = walk param_env stmts in
  List.rev !decls

let emit_fn_sig buf (Ast.Function f) =
  if f.name = "main" then
    Buffer.add_string buf "int main(void)"
  else begin
    let ret =
      match f.ret_ty with
      | None -> "void "
      | Some ty -> c_type_prefix (type_of_ann ty)
    in
    Buffer.add_string buf ret;
    Buffer.add_string buf f.name;
    Buffer.add_char buf '(';
    add_separated buf ", " (fun p -> Buffer.add_string buf (c_param p)) f.params;
    Buffer.add_char buf ')'
  end

let gen_function buf fn_table (Ast.Function f) =
  emit_fn_sig buf (Ast.Function f);
  Buffer.add_string buf " {\n";
  let param_env = List.map (fun p -> (p.Ast.pname, type_of_ann p.Ast.pty)) f.params in
  let lets = collect_lets fn_table param_env f.body in
  let full_env = param_env @ lets in
  List.iter
    (fun (name, t) ->
      Buffer.add_string buf (Printf.sprintf "    %s;\n" (c_decl t name)))
    lets;
  List.iter (gen_stmt buf fn_table full_env "    ") f.body;
  if f.name = "main" then Buffer.add_string buf "    return 0;\n";
  Buffer.add_string buf "}\n"

let build_fn_table program =
  List.filter_map
    (function
      | Ast.Function f when f.name <> "main" ->
          let s =
            { param_tys = List.map (fun p -> type_of_ann p.Ast.pty) f.params;
              ret_ty = Option.map type_of_ann f.ret_ty }
          in
          Some (f.name, s)
      | _ -> None)
    program

let gen_program program =
  let fn_table = build_fn_table program in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n";
  let non_main =
    List.filter (function Ast.Function f -> f.name <> "main") program
  in
  if non_main <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (fun fn -> emit_fn_sig buf fn; Buffer.add_string buf ";\n") non_main
  end;
  Buffer.add_char buf '\n';
  let last = List.length program - 1 in
  List.iteri
    (fun i fn ->
      gen_function buf fn_table fn;
      if i < last then Buffer.add_char buf '\n')
    program;
  Buffer.contents buf
