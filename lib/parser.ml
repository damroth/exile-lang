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
  let ti signed width = Ast.TyInt { signed; width } in
  match advance s with
  | (Token.Ident "int", _) -> ti true Ast.W32     (* alias for i32 *)
  | (Token.Ident "i8",  _) -> ti true Ast.W8
  | (Token.Ident "i16", _) -> ti true Ast.W16
  | (Token.Ident "i32", _) -> ti true Ast.W32
  | (Token.Ident "u8",  _) -> ti false Ast.W8
  | (Token.Ident "u16", _) -> ti false Ast.W16
  | (Token.Ident "u32", _) -> ti false Ast.W32
  | (Token.Ident "str", _) -> Ast.TyStr
  | (Token.Ident "bool", _) -> Ast.TyBool
  | (t, p) ->
      Error.failf p
        "expected type (int, i8/i16/i32, u8/u16/u32, str, bool), got %s"
        (Token.pp t)

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
      (* Path-qualified identifiers: foo::bar::baz(...).  Build the full
         path, then decide if it ends in a call or a bare value. *)
      let rec collect_path acc =
        if peek s = Token.DoubleColon then begin
          ignore (advance s);
          match advance s with
          | (Token.Ident n, _) -> collect_path (n :: acc)
          | (t, p2) ->
              Error.failf p2 "expected identifier after '::', got %s" (Token.pp t)
        end else
          List.rev acc
      in
      let path = collect_path [name] in
      if peek s = Token.LParen then begin
        ignore (advance s);
        Ast.Call (path, parse_args s, p)
      end else begin
        match path with
        | [single] -> Ast.Var (single, p)
        | _ ->
            Error.failf p
              "qualified path '%s' is only valid as a function call"
              (String.concat "::" path)
      end
  | Token.LParen ->
      let e = parse_expr s in
      expect s Token.RParen;
      e
  | _ -> Error.raise_ p "expected expression"

and parse_cast s =
  let rec loop left =
    if peek s = Token.As then begin
      let p = peek_pos s in
      ignore (advance s);
      let ty = parse_type s in
      loop (Ast.Cast (left, ty, p))
    end else
      left
  in
  loop (parse_primary s)

and parse_mul s =
  let rec loop left =
    match peek s with
    | Token.Star ->
        ignore (advance s); loop (Ast.BinOp (Ast.Mul, left, parse_cast s))
    | Token.Slash ->
        ignore (advance s); loop (Ast.BinOp (Ast.Div, left, parse_cast s))
    | _ -> left
  in
  loop (parse_cast s)

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

let parse_function s seen_fns ~is_pub =
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
  if name = "main" && is_pub then
    Error.raise_ name_pos "'pub' has no meaning on 'main'";
  expect s Token.LBrace;
  let body = parse_stmts s [] in
  expect s Token.RBrace;
  (name, Ast.{ name; params; ret_ty; body; is_pub; pos = name_pos })

(* Parse a `use` declaration and return one or more `(name option, Use item)`
   pairs.  Wildcard imports introduce no name into the surrounding scope so
   they pair with `None`.
   Forms supported:
     use foo;                  -> [(Some "foo", Use {[foo]})]
     use foo::bar;             -> [(Some "bar", Use {[foo;bar]})]
     use foo::{a, b};          -> [(Some "a", Use {[foo;a]}); (Some "b", Use {[foo;b]})]
     use foo::*;               -> [(None, Use {[foo]; is_wildcard=true})] *)
let parse_use_items s =
  let p = peek_pos s in
  expect s Token.Use;
  (* Parse `ident (:: ident)*` until we hit `;`, `:: {`, or `:: *`.  The
     accumulator holds the path in reverse. *)
  let rec collect_segments acc =
    match advance s with
    | (Token.Ident n, _) ->
        let acc = n :: acc in
        (match peek s with
         | Token.DoubleColon ->
             ignore (advance s);
             (match peek s with
              | Token.LBrace -> ignore (advance s); (List.rev acc, `Group)
              | Token.Star -> ignore (advance s); (List.rev acc, `Wildcard)
              | _ -> collect_segments acc)
         | _ -> (List.rev acc, `Single))
    | (t, pp) ->
        Error.failf pp "expected identifier in 'use', got %s" (Token.pp t)
  in
  let (prefix, kind) = collect_segments [] in
  match kind with
  | `Single ->
      expect s Token.Semicolon;
      let name = List.hd (List.rev prefix) in
      [ (Some name, Ast.Use { path = prefix; is_wildcard = false; pos = p }) ]
  | `Wildcard ->
      expect s Token.Semicolon;
      [ (None, Ast.Use { path = prefix; is_wildcard = true; pos = p }) ]
  | `Group ->
      let rec collect_names acc =
        match advance s with
        | (Token.Ident n, _) ->
            let path = prefix @ [n] in
            let acc =
              (Some n, Ast.Use { path; is_wildcard = false; pos = p }) :: acc
            in
            (match peek s with
             | Token.RBrace -> ignore (advance s); List.rev acc
             | Token.Comma ->
                 ignore (advance s);
                 if peek s = Token.RBrace then begin
                   (* trailing comma: `use foo::{a, b,};` *)
                   ignore (advance s);
                   List.rev acc
                 end else
                   collect_names acc
             | _ ->
                 Error.failf (peek_pos s)
                   "expected ',' or '}' in 'use' group")
        | (t, pp) ->
            Error.failf pp "expected identifier in 'use' group, got %s"
              (Token.pp t)
      in
      let items = collect_names [] in
      expect s Token.Semicolon;
      (* Reject duplicates within the group itself. *)
      let rec check_internal_dups = function
        | [] -> ()
        | (Some n, _) :: rest ->
            if List.exists (fun (m, _) -> m = Some n) rest then
              Error.failf p "duplicate name '%s' in 'use' group" n;
            check_internal_dups rest
        | (None, _) :: rest -> check_internal_dups rest
      in
      check_internal_dups items;
      items

(* parse_item handles `fn`, `mod`, and `use` at any nesting level, with an
   optional `pub` prefix where it makes sense.  Returns a list because a
   single `use foo::{a, b}` declaration introduces multiple bindings. *)
let rec parse_item s seen =
  let is_pub = peek s = Token.Pub in
  if is_pub then ignore (advance s);
  match peek s with
  | Token.Fn ->
      let (name, fn) = parse_function s seen ~is_pub in
      [ (Some name, Ast.Function fn) ]
  | Token.Mod ->
      let (name, m) = parse_module s seen ~is_pub in
      [ (Some name, Ast.Module m) ]
  | Token.Use ->
      if is_pub then
        Error.failf (peek_pos s) "'pub use' is not yet supported";
      let items = parse_use_items s in
      (* Each named item gets dedup-checked against the surrounding scope.
         Wildcards introduce no name and are skipped here; the loader handles
         file-level deduplication. *)
      List.iter
        (fun (name_opt, item) ->
          match name_opt with
          | None -> ()
          | Some n ->
              let p =
                match item with
                | Ast.Use { pos; _ } -> pos
                | _ -> Pos.zero
              in
              if List.mem n seen then
                Error.failf p "name '%s' already used in this scope" n)
        items;
      items
  | _ ->
      Error.failf (peek_pos s) "expected 'fn', 'mod' or 'use', got %s"
        (Token.pp (peek s))

and parse_module s seen ~is_pub =
  expect s Token.Mod;
  let (name, name_pos) =
    match advance s with
    | (Token.Ident n, p) -> (n, p)
    | (_, p) -> Error.raise_ p "expected module name after 'mod'"
  in
  if List.mem name seen then
    Error.failf name_pos "module '%s' already defined in this scope" name;
  expect s Token.LBrace;
  let rec loop inner_seen acc =
    match peek s with
    | Token.RBrace -> ignore (advance s); List.rev acc
    | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
    | _ ->
        let new_pairs = parse_item s inner_seen in
        let new_names = List.filter_map fst new_pairs in
        let new_items = List.map snd new_pairs in
        loop (List.rev_append new_names inner_seen)
             (List.rev_append new_items acc)
  in
  let items = loop [] [] in
  (name,
   Ast.{ mname = name; mitems = items; mpos = name_pos; mis_pub = is_pub })

let parse_program tokens =
  let s = make_state tokens in
  let rec loop seen acc =
    match peek s with
    | Token.Eof -> List.rev acc
    | _ ->
        let new_pairs = parse_item s seen in
        let new_names = List.filter_map fst new_pairs in
        let new_items = List.map snd new_pairs in
        loop (List.rev_append new_names seen)
             (List.rev_append new_items acc)
  in
  loop [] []
