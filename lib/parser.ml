type state = {
  mutable tokens : (Token.t * Pos.t) list;
  mutable last_pos : Pos.t;
  (* Suppresses `Ident { ... }` parsing as a struct literal in positions
     where `{` opens a block (the condition of an if/while). *)
  mutable allow_struct_lit : bool;
}

let make_state tokens =
  { tokens; last_pos = Pos.zero; allow_struct_lit = true }

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

let expect_ident s ~what =
  match advance s with
  | (Token.Ident n, p) -> (n, p)
  | (t, p) -> Error.failf p "expected %s, got %s" what (Token.pp t)

(* Continue parsing `:: ident :: ident ...` segments after [head] is already
   parsed.  Returns the full path (head plus any tail segments). *)
let rec parse_path_tail s acc =
  if peek s = Token.DoubleColon then begin
    ignore (advance s);
    let (n, _) = expect_ident s ~what:"identifier after '::'" in
    parse_path_tail s (n :: acc)
  end else List.rev acc

(* Parse comma-separated items until `close`. Opener must already be consumed.
   Trailing comma allowed before `close`. *)
let parse_comma_list ~close ~item s =
  if peek s = close then (ignore (advance s); [])
  else
    let first = item s in
    let rec rest acc =
      match peek s with
      | t when t = close -> ignore (advance s); List.rev acc
      | Token.Comma ->
          ignore (advance s);
          if peek s = close then (ignore (advance s); List.rev acc)
          else rest (item s :: acc)
      | _ ->
          Error.failf (peek_pos s) "expected ',' or %s" (Token.pp close)
    in
    rest [ first ]

let rec parse_type s =
  let ti signed width = Ast.TyInt { signed; width } in
  match advance s with
  | (Token.Star, _) -> Ast.TyPtr (parse_type s)
  | (Token.Ident "int", _) -> ti true Ast.W32     (* alias for i32 *)
  | (Token.Ident "i8",  _) -> ti true Ast.W8
  | (Token.Ident "i16", _) -> ti true Ast.W16
  | (Token.Ident "i32", _) -> ti true Ast.W32
  | (Token.Ident "u8",  _) -> ti false Ast.W8
  | (Token.Ident "u16", _) -> ti false Ast.W16
  | (Token.Ident "u32", _) -> ti false Ast.W32
  | (Token.Ident "str", _) -> Ast.TyStr
  | (Token.Ident "bool", _) -> Ast.TyBool
  | (Token.Ident n, _) ->
      (* Any other identifier is a struct path, possibly qualified
         (`mod::Inner::Point`). *)
      Ast.TyStruct (parse_path_tail s [n])
  | (Token.LParen, p) ->
      (* Tuple type `(T1, T2, ...)`.  Single-element parens unwrap to the
         underlying type (parens act as grouping); 0 elements is rejected. *)
      let tys = parse_comma_list ~close:Token.RParen ~item:parse_type s in
      (match tys with
       | [] -> Error.failf p "empty tuple type '()' is not supported"
       | [ t ] -> t
       | _ -> Ast.TyTuple tys)
  | (t, p) ->
      Error.failf p
        "expected type (int, i8/i16/i32, u8/u16/u32, str, bool, struct \
         name, (T,...)), got %s"
        (Token.pp t)

let parse_param s =
  let (name, _) = expect_ident s ~what:"parameter name" in
  expect s Token.Colon;
  let ty = parse_type s in
  Ast.{ pname = name; pty = ty }

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
  | Token.Int n -> Ast.IntLit (n, p)
  | Token.True -> Ast.BoolLit (true, p)
  | Token.False -> Ast.BoolLit (false, p)
  | Token.Null -> Ast.NullLit p
  | Token.String str -> Ast.StringLit (str, p)
  | Token.Minus -> Ast.Neg (parse_primary s, p)
  | Token.Amp -> Ast.Ref (parse_postfix s (parse_primary s), p)
  | Token.Star -> Ast.Deref (parse_postfix s (parse_primary s), p)
  | Token.New ->
      (* `new Path { f1: e1, ... }` — heap-allocate struct + init.
         The struct path is required; the brace body is mandatory and
         allowed even in cond positions (no ambiguity since `new` is a
         dedicated keyword). *)
      let (first, _) = expect_ident s ~what:"struct name after 'new'" in
      let path = parse_path_tail s [first] in
      expect s Token.LBrace;
      let (fields, base) = parse_struct_lit_body s in
      Ast.New { tname = path; fields; base; pos = p }
  | Token.Ident name ->
      (* Path-qualified identifiers: foo::bar::baz(...).  Build the full
         path, then decide if it ends in a call, a struct literal, or a
         bare value. *)
      let path = parse_path_tail s [name] in
      (match peek s with
       | Token.LParen ->
           ignore (advance s);
           Ast.Call (path, parse_args s, p)
       | Token.LBrace when s.allow_struct_lit ->
           ignore (advance s);
           let (fields, base) = parse_struct_lit_body s in
           Ast.StructLit { tname = path; fields; base; pos = p }
       | _ ->
           (match path with
            | [single] -> Ast.Var (single, p)
            | _ ->
                Error.failf p
                  "qualified path '%s' must be followed by '(' or '{'"
                  (String.concat "::" path)))
  | Token.LParen ->
      (* Grouping `(e)` for a single expression, tuple literal `(e1, e2, ...)`
         for two or more. *)
      let first = parse_expr s in
      (match peek s with
       | Token.Comma ->
           ignore (advance s);
           if peek s = Token.RParen then begin
             (* trailing comma after a single expr — still grouping, not a 1-tuple *)
             ignore (advance s);
             first
           end else begin
             let rest = parse_comma_list ~close:Token.RParen ~item:parse_expr s in
             Ast.TupleLit (first :: rest, p)
           end
       | _ ->
           expect s Token.RParen;
           first)
  | _ -> Error.raise_ p "expected expression"

and parse_struct_lit_field s =
  let (n, _) = expect_ident s ~what:"field name in struct literal" in
  expect s Token.Colon;
  (n, parse_expr s)

(* Parse the body of a struct literal — `f1: e1, f2: e2, ..base }` — after
   the opening `{` has been consumed.  `..base` (functional update) is
   optional and must come last; only one base is allowed. *)
and parse_struct_lit_body s =
  let rec loop fields_acc =
    match peek s with
    | Token.RBrace -> ignore (advance s); (List.rev fields_acc, None)
    | Token.DotDot ->
        ignore (advance s);
        let base = parse_expr s in
        (* Allow optional trailing comma after `..base`. *)
        (match peek s with
         | Token.Comma ->
             ignore (advance s);
             expect s Token.RBrace
         | _ -> expect s Token.RBrace);
        (List.rev fields_acc, Some base)
    | _ ->
        let f = parse_struct_lit_field s in
        (match peek s with
         | Token.RBrace -> ignore (advance s); (List.rev (f :: fields_acc), None)
         | Token.Comma ->
             ignore (advance s);
             loop (f :: fields_acc)
         | _ ->
             Error.failf (peek_pos s)
               "expected ',' or '}' in struct literal")
  in
  loop []

(* Chain `.field` accesses or `.name(args)` method calls onto an already-parsed
   primary.  Disambiguation is by lookahead on the token after the name. *)
and parse_postfix s base =
  let rec loop e =
    if peek s = Token.Dot then begin
      let p = peek_pos s in
      ignore (advance s);
      let (n, _) = expect_ident s ~what:"field or method name after '.'" in
      if peek s = Token.LParen then begin
        ignore (advance s);
        let args = parse_args s in
        loop (Ast.MethodCall { receiver = e; name = n; args; pos = p })
      end else
        loop (Ast.FieldAccess (e, n, p))
    end else e
  in
  loop base

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
  loop (parse_postfix s (parse_primary s))

and parse_mul s =
  let rec loop left =
    match peek s with
    | Token.Star ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Mul, left, parse_cast s, p))
    | Token.Slash ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Div, left, parse_cast s, p))
    | _ -> left
  in
  loop (parse_cast s)

and parse_add s =
  let rec loop left =
    match peek s with
    | Token.Plus ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Add, left, parse_mul s, p))
    | Token.Minus ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Sub, left, parse_mul s, p))
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
  | Some op ->
      let p = peek_pos s in
      ignore (advance s); Ast.BinOp (op, left, parse_add s, p)

and parse_args s = parse_comma_list ~close:Token.RParen ~item:parse_expr s

let rec parse_block s =
  expect s Token.LBrace;
  let body = parse_stmts s [] in
  expect s Token.RBrace;
  body

and parse_stmt s =
  match peek s with
  | Token.Let ->
      let pos = peek_pos s in
      ignore (advance s);
      (match peek s with
       | Token.LParen ->
           (* Destructuring binding `let (a, b, ...) = expr;` — names only,
              no per-name type annotation. *)
           ignore (advance s);
           let names =
             parse_comma_list ~close:Token.RParen
               ~item:(fun s ->
                 let (n, _) = expect_ident s ~what:"name in 'let (...)'" in
                 n)
               s
           in
           if List.length names < 2 then
             Error.failf pos
               "destructuring 'let (...)' needs at least two names";
           expect s Token.Eq;
           let value = parse_expr s in
           expect s Token.Semicolon;
           Ast.LetTuple { names; value; pos }
       | _ ->
           let (name, name_pos) =
             expect_ident s ~what:"variable name after 'let'"
           in
           let ty_ann =
             if peek s = Token.Colon then (ignore (advance s); Some (parse_type s))
             else None
           in
           expect s Token.Eq;
           let value = parse_expr s in
           expect s Token.Semicolon;
           Ast.Let { name; value; ty_ann; pos = name_pos })
  | Token.Return ->
      let pos = peek_pos s in
      ignore (advance s);
      let e = parse_expr s in
      expect s Token.Semicolon;
      Ast.Return (e, pos)
  | Token.Defer ->
      let pos = peek_pos s in
      ignore (advance s);
      let body =
        match peek s with
        | Token.LBrace ->
            ignore (advance s);
            let stmts = parse_stmts s [] in
            expect s Token.RBrace;
            stmts
        | _ -> [ parse_stmt s ]
      in
      Ast.Defer { body; pos }
  | Token.If ->
      ignore (advance s);
      let prev = s.allow_struct_lit in
      s.allow_struct_lit <- false;
      let cond = parse_expr s in
      s.allow_struct_lit <- prev;
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
      let prev = s.allow_struct_lit in
      s.allow_struct_lit <- false;
      let cond = parse_expr s in
      s.allow_struct_lit <- prev;
      let body = parse_block s in
      Ast.While { cond; body }
  | _ ->
      (* General statement: parse an expression, then dispatch on what
         follows.  An `=` makes it an assignment whose target is either a
         bare variable (`x = ...;`) or a field path (`p.x = ...;`); a `;`
         keeps it as an expression statement. *)
      let e = parse_expr s in
      (match peek s with
       | Token.Eq ->
           let pos = peek_pos s in
           ignore (advance s);
           let value = parse_expr s in
           expect s Token.Semicolon;
           (match e with
            | Ast.Var (name, vp) ->
                Ast.Assign { name; value; pos = vp }
            | Ast.FieldAccess (target, field, fp) ->
                Ast.AssignField { target; field; value; pos = fp }
            | Ast.Deref (target, dp) ->
                Ast.AssignDeref { target; value; pos = dp }
            | _ ->
                Error.failf pos "invalid assignment target")
       | _ ->
           expect s Token.Semicolon;
           Ast.ExprStmt e)

and parse_stmts s acc =
  match peek s with
  | Token.RBrace -> List.rev acc
  | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
  | _ -> parse_stmts s (parse_stmt s :: acc)

let parse_function s seen_fns ~is_pub =
  expect s Token.Fn;
  let (name, name_pos) = expect_ident s ~what:"function name after 'fn'" in
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
  | Token.Struct ->
      let sd = parse_struct_decl s ~is_pub in
      if List.mem sd.Ast.sname seen then
        Error.failf sd.Ast.spos
          "name '%s' already used in this scope" sd.Ast.sname;
      [ (Some sd.Ast.sname, Ast.Struct sd) ]
  | Token.Impl ->
      if is_pub then
        Error.failf (peek_pos s)
          "'pub' has no meaning on 'impl' (set visibility per method)";
      let ib = parse_impl_block s in
      (* impl blocks introduce no name into the surrounding scope — their
         methods are looked up via the target struct, not by a free name. *)
      [ (None, Ast.Impl ib) ]
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
      Error.failf (peek_pos s) "expected 'fn', 'mod', 'use' or 'struct', got %s"
        (Token.pp (peek s))

and parse_struct_decl s ~is_pub =
  expect s Token.Struct;
  let (name, name_pos) =
    match advance s with
    | (Token.Ident n, p) -> (n, p)
    | (_, p) -> Error.raise_ p "expected struct name after 'struct'"
  in
  expect s Token.LBrace;
  let parse_field s =
    let (fname, _) = expect_ident s ~what:"field name in struct" in
    expect s Token.Colon;
    let ty = parse_type s in
    (fname, ty)
  in
  let fields =
    parse_comma_list ~close:Token.RBrace ~item:parse_field s
  in
  (* Reject in-struct duplicate field names. *)
  let rec check_dups = function
    | [] -> ()
    | (n, _) :: rest ->
        if List.exists (fun (m, _) -> m = n) rest then
          Error.failf name_pos
            "duplicate field '%s' in struct '%s'" n name;
        check_dups rest
  in
  check_dups fields;
  Ast.{ sname = name; sfields = fields; spos = name_pos; sis_pub = is_pub }

(* `impl <Path> { fn ... fn ... }` — methods get registered against the
   target struct, not into the surrounding scope.  Each method is parsed
   like a free-standing function (with optional `pub`), with a per-impl
   duplicate-name check.  Cross-block dup checks against earlier `impl`
   blocks for the same target happen later in typecheck. *)
and parse_impl_block s =
  expect s Token.Impl;
  let pos = s.last_pos in
  let (head, _) = expect_ident s ~what:"struct name after 'impl'" in
  let target = head :: parse_path_tail s [] in
  let target =
    match target with
    | first :: rest -> first :: rest  (* already-rev'd by parse_path_tail *)
    | [] -> Error.failf pos "empty 'impl' target"
  in
  expect s Token.LBrace;
  let rec loop seen acc =
    match peek s with
    | Token.RBrace -> ignore (advance s); List.rev acc
    | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
    | _ ->
        let is_pub = peek s = Token.Pub in
        if is_pub then ignore (advance s);
        (match peek s with
         | Token.Fn ->
             let (name, fn) = parse_function s seen ~is_pub in
             loop (name :: seen) (fn :: acc)
         | t ->
             Error.failf (peek_pos s)
               "expected 'fn' inside 'impl' block, got %s" (Token.pp t))
  in
  let methods = loop [] [] in
  Ast.{ itarget = target; iitems = methods; ipos = pos }

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
