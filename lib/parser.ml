type state = {
  mutable tokens : (Token.t * Pos.t) list;
  mutable last_pos : Pos.t;
}

let make_state tokens = { tokens; last_pos = Pos.zero }

let peek s = match s.tokens with [] -> Token.Eof | (t, _) :: _ -> t

let peek2 s =
  match s.tokens with
  | _ :: (t, _) :: _ -> t
  | _ -> Token.Eof

let peek_pos s =
  match s.tokens with (_, p) :: _ -> p | [] -> s.last_pos

let advance s =
  match s.tokens with
  | [] -> (Token.Eof, s.last_pos)
  | (t, p) :: rest ->
      s.tokens <- rest;
      s.last_pos <- p;
      (t, p)

let expect s tok =
  let (t, p) = advance s in
  if t <> tok then
    Error.failf p "expected %s, got %s" (Token.pp tok) (Token.pp t)

let parse_type s =
  match advance s with
  | (Token.Ident "int", _) -> Ast.TyInt
  | (Token.Ident "str", _) -> Ast.TyStr
  | (Token.Ident "bool", _) -> Ast.TyBool
  | (t, p) -> Error.failf p "expected type ('int', 'str' or 'bool'), got %s" (Token.pp t)

let parse_param s =
  let name =
    match advance s with
    | (Token.Ident n, _) -> n
    | (t, p) -> Error.failf p "expected parameter name, got %s" (Token.pp t)
  in
  expect s Token.Colon;
  let ty = parse_type s in
  Ast.{ pname = name; pty = ty }

(* Parse comma-separated items until `close`. Opener must already be consumed. *)
let parse_comma_list ~close ~item s =
  if peek s = close then (ignore (advance s); [])
  else
    let first = item s in
    let rec rest acc =
      match peek s with
      | t when t = close -> ignore (advance s); List.rev acc
      | Token.Comma -> ignore (advance s); rest (item s :: acc)
      | _ ->
          Error.failf (peek_pos s) "expected ',' or %s" (Token.pp close)
    in
    rest [ first ]

let parse_params s =
  expect s Token.LParen;
  parse_comma_list ~close:Token.RParen ~item:parse_param s

let parse_ret_ty s =
  match peek s with
  | Token.Arrow -> ignore (advance s); Some (parse_type s)
  | _ -> None

let rec parse_primary s =
  let (t, p) = advance s in
  match t with
  | Token.Int n -> Ast.IntLit n
  | Token.True -> Ast.BoolLit true
  | Token.False -> Ast.BoolLit false
  | Token.String str -> Ast.StringLit str
  | Token.Minus -> Ast.Neg (parse_primary s)
  | Token.Ident name ->
      if peek s = Token.LParen then begin
        ignore (advance s);
        Ast.Call (name, parse_args s, p)
      end else
        Ast.Var (name, p)
  | Token.LParen ->
      let e = parse_expr s in
      expect s Token.RParen;
      e
  | _ -> Error.raise_ p "expected expression"

and parse_mul s =
  let rec loop left =
    match peek s with
    | Token.Star ->
        ignore (advance s); loop (Ast.BinOp (Ast.Mul, left, parse_primary s))
    | Token.Slash ->
        ignore (advance s); loop (Ast.BinOp (Ast.Div, left, parse_primary s))
    | _ -> left
  in
  loop (parse_primary s)

and parse_add s =
  let rec loop left =
    match peek s with
    | Token.Plus ->
        ignore (advance s); loop (Ast.BinOp (Ast.Add, left, parse_mul s))
    | Token.Minus ->
        ignore (advance s); loop (Ast.BinOp (Ast.Sub, left, parse_mul s))
    | _ -> left
  in
  loop (parse_mul s)

(* comparison binds looser than arithmetic; only one comparison per expression *)
and parse_expr s =
  let left = parse_add s in
  let cmp_op = match peek s with
    | Token.Lt -> Some Ast.Lt   | Token.Gt -> Some Ast.Gt
    | Token.LtEq -> Some Ast.LtEq | Token.GtEq -> Some Ast.GtEq
    | Token.EqEq -> Some Ast.EqEq | Token.NotEq -> Some Ast.NotEq
    | _ -> None
  in
  match cmp_op with
  | None -> left
  | Some op -> ignore (advance s); Ast.BinOp (op, left, parse_add s)

and parse_args s = parse_comma_list ~close:Token.RParen ~item:parse_expr s

let rec parse_block s =
  expect s Token.LBrace;
  let body = parse_stmts s [] in
  expect s Token.RBrace;
  body

and parse_stmt s =
  match peek s with
  | Token.Let ->
      ignore (advance s);
      let (name, pos) =
        match advance s with
        | (Token.Ident n, p) -> (n, p)
        | (t, p) -> Error.failf p "expected variable name after 'let', got %s" (Token.pp t)
      in
      let ty_ann =
        if peek s = Token.Colon then (ignore (advance s); Some (parse_type s))
        else None
      in
      expect s Token.Eq;
      let value = parse_expr s in
      expect s Token.Semicolon;
      Ast.Let { name; value; ty_ann; pos }
  | Token.Return ->
      ignore (advance s);
      let e = parse_expr s in
      expect s Token.Semicolon;
      Ast.Return e
  | Token.If ->
      ignore (advance s);
      let cond = parse_expr s in
      let then_body = parse_block s in
      let else_body =
        match peek s with
        | Token.Else ->
            ignore (advance s);
            (match peek s with
             | Token.If -> [ parse_stmt s ]
             | _ -> parse_block s)
        | _ -> []
      in
      Ast.If { cond; then_body; else_body }
  | Token.While ->
      ignore (advance s);
      let cond = parse_expr s in
      let body = parse_block s in
      Ast.While { cond; body }
  | Token.Ident name when peek2 s = Token.Eq ->
      let pos = peek_pos s in
      ignore (advance s);
      ignore (advance s);
      let value = parse_expr s in
      expect s Token.Semicolon;
      Ast.Assign { name; value; pos }
  | _ ->
      let e = parse_expr s in
      expect s Token.Semicolon;
      Ast.ExprStmt e

and parse_stmts s acc =
  match peek s with
  | Token.RBrace -> List.rev acc
  | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
  | _ -> parse_stmts s (parse_stmt s :: acc)

let parse_function s seen_fns =
  expect s Token.Fn;
  let (name, name_pos) =
    match advance s with
    | (Token.Ident n, p) -> (n, p)
    | (_, p) -> Error.raise_ p "expected function name after 'fn'"
  in
  if List.mem name seen_fns then
    Error.failf name_pos "function '%s' already defined" name;
  let params = parse_params s in
  let rec check_dup_params = function
    | [] -> ()
    | p :: rest ->
        if List.exists (fun q -> q.Ast.pname = p.Ast.pname) rest then
          Error.failf name_pos "duplicate parameter '%s' in function '%s'"
            p.Ast.pname name;
        check_dup_params rest
  in
  check_dup_params params;
  let ret_ty = parse_ret_ty s in
  if name = "main" && params <> [] then
    Error.raise_ name_pos "'main' must take no parameters";
  if name = "main" && ret_ty <> None then
    Error.raise_ name_pos "'main' must not declare a return type";
  expect s Token.LBrace;
  let body = parse_stmts s [] in
  expect s Token.RBrace;
  (name, Ast.{ name; params; ret_ty; body })

let parse_program tokens =
  let s = make_state tokens in
  let rec loop seen acc =
    match peek s with
    | Token.Eof -> List.rev acc
    | _ ->
        let (name, fn) = parse_function s seen in
        loop (name :: seen) (fn :: acc)
  in
  loop [] []
