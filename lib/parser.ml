type state = {
  mutable tokens : (Token.t * Pos.t) list;
  mutable last_pos : Pos.t;
  (* Suppresses `Ident { ... }` parsing as a struct literal in positions
     where `{` opens a block (the condition of an if/while). *)
  mutable allow_struct_lit : bool;
  (* Suppresses a bare top-level `|` (bitor) so it reads as the match-arm
     SEPARATOR.  Cleared inside a match arm body; restored to `true` inside
     any parenthesised / argument context, where `|` is unambiguously
     bitor (`=> (A | B)`). *)
  mutable allow_bitor : bool;
}

let make_state tokens =
  { tokens; last_pos = Pos.zero; allow_struct_lit = true; allow_bitor = true }

let peek s = match s.tokens with [] -> Token.Eof | (t, _) :: _ -> t

let peek2 s =
  match s.tokens with
  | _ :: (t, _) :: _ -> t
  | _ -> Token.Eof

let peek3 s =
  match s.tokens with
  | _ :: _ :: (t, _) :: _ -> t
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

(* Closing a generic argument list with `>` when the lexer merged two
   closers into a single `>>` (Shr) — e.g. `Option<Option<int>>`.  Consume
   one `>` for the current level and leave a `>` (Gt) pending for the
   enclosing one.  C++11's token-split trick, localised to parse_type. *)
let split_shr s =
  match s.tokens with
  | (Token.Shr, p) :: rest ->
      s.tokens <- (Token.Gt, { p with col = p.col + 1 }) :: rest
  | _ -> ()

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

(* Parse a qualified path `ident (:: ident)*` from the current position.
   Combines `expect_ident` + `parse_path_tail`; use this when an ident
   is syntactically required to start the path (decl head, type ref,
   pattern head).  Use `parse_path_tail` directly when the head ident
   was already consumed by the surrounding `match advance s with`. *)
let parse_path s ~what =
  let (head, _) = expect_ident s ~what in
  parse_path_tail s [head]

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

(* Forward reference to [parse_expr], set once the expression parser is
   defined below.  Lets `parse_type` read the size of `[T; N]` (a constant
   expression) without merging the type and expression parser groups. *)
let parse_expr_fwd : (state -> Ast.expr) ref =
  ref (fun _ -> failwith "parse_expr_fwd not yet set")

let rec parse_type s =
  let ti signed width = Ast.TyInt { signed; width } in
  match advance s with
  | (Token.Star, _) ->
      (* `*T` is the default mutable pointee; `*const T` marks the pointee
         as read-only (maps to C `const T *`).  The `const` keyword reuses
         Token.Const (top-level item-decl keyword); inside a type context
         it's unambiguously the const-ptr marker. *)
      if peek s = Token.Const then begin
        ignore (advance s);
        Ast.TyConstPtr (parse_type s)
      end else
        Ast.TyPtr (parse_type s)
  | (Token.LBracket, _) ->
      (* `[T; N]` — fixed-size array type. *)
      let elem = parse_type s in
      expect s Token.Semicolon;
      let size = !parse_expr_fwd s in
      expect s Token.RBracket;
      Ast.TyArray { elem; size }
  | (Token.Question, _) ->
      (* `?T` is sugar for `Option<T>` — the prelude's optional-value
         enum.  Resolves at typecheck like any other generic application. *)
      Ast.TyStruct { path = ["Option"]; args = [ parse_type s ] }
  | (Token.Fn, _) ->
      (* `fn(T1, T2) -> R` — function pointer type.  No variadic on
         this form for now; come back when there's a real use case. *)
      expect s Token.LParen;
      let params =
        if peek s = Token.RParen then begin
          ignore (advance s); []
        end else
          parse_comma_list ~close:Token.RParen ~item:parse_type s
      in
      let ret = parse_ret_ty s in
      Ast.TyFnPtr { params; ret }
  | (Token.Ident "int", _) -> ti true Ast.W32     (* alias for i32 *)
  | (Token.Ident "i8",  _) -> ti true Ast.W8
  | (Token.Ident "i16", _) -> ti true Ast.W16
  | (Token.Ident "i32", _) -> ti true Ast.W32
  | (Token.Ident "u8",  _) -> ti false Ast.W8
  | (Token.Ident "u16", _) -> ti false Ast.W16
  | (Token.Ident "u32", _) -> ti false Ast.W32
  | (Token.Ident "c_int", _) -> Ast.TyCInt { signed = true }
  | (Token.Ident "c_uint", _) -> Ast.TyCInt { signed = false }
  | (Token.Ident "c_short", _) -> Ast.TyCShort { signed = true }
  | (Token.Ident "c_ushort", _) -> Ast.TyCShort { signed = false }
  | (Token.Ident "c_long", _) -> Ast.TyCLong { signed = true }
  | (Token.Ident "c_ulong", _) -> Ast.TyCLong { signed = false }
  | (Token.Ident "c_char", _) -> Ast.TyCChar
  | (Token.Ident "c_schar", _) -> Ast.TyCSChar
  | (Token.Ident "c_uchar", _) -> Ast.TyCUChar
  | (Token.Ident "c_void", _) -> Ast.TyCVoid
  | (Token.Ident "str", _) -> Ast.TyStr
  | (Token.Ident "bool", _) -> Ast.TyBool
  | (Token.Ident n, _) ->
      (* Any other identifier is a struct path, possibly qualified
         (`mod::Inner::Point`).  An optional generic argument list may
         follow: `Option<int>`, `Pair<A, B>`.  Single-segment unknown
         paths can also be type-parameter references inside a generic
         decl body (`T` in `Some(T)` for `enum Option<T>`); typecheck
         later disambiguates struct path vs type-param binding. *)
      let path = parse_path_tail s [n] in
      let args =
        if peek s = Token.Lt then begin
          ignore (advance s);
          if peek s = Token.Gt then
            Error.failf (peek_pos s) "empty generic argument list <>";
          (* Close on `>`; a `>>` (Shr) closes this level and leaves a `>`
             pending for the enclosing generic. *)
          let rec loop acc =
            let t = parse_type s in
            let acc = t :: acc in
            match peek s with
            | Token.Comma -> ignore (advance s); loop acc
            | Token.Gt -> ignore (advance s); List.rev acc
            | Token.Shr -> split_shr s; List.rev acc
            | other ->
                Error.failf (peek_pos s)
                  "expected ',' or '>' in generic arguments, got %s"
                  (Token.pp other)
          in
          loop []
        end else []
      in
      Ast.TyStruct { path; args }
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

and parse_ret_ty s =
  match peek s with
  | Token.Arrow -> ignore (advance s); Some (parse_type s)
  | _ -> None

(* `@reg(d0)` attribute on a parameter pins it to an m68k register in
   the emitted C declaration.  Only legal on extern fns (validated
   later by typecheck); accepted in the regular [parse_param] grammar
   here for syntactic uniformity, then rejected if misused. *)
let parse_param_reg_attr s =
  match peek s with
  | Token.At ->
      let at_pos = peek_pos s in
      ignore (advance s);
      let (attr_name, _) = expect_ident s ~what:"attribute name after '@'" in
      if attr_name <> "reg" then
        Error.failf at_pos
          "only '@reg(<m68k-register>)' is supported on parameters, \
           got '@%s'" attr_name;
      expect s Token.LParen;
      let (reg_name, reg_pos) = expect_ident s ~what:"m68k register name" in
      expect s Token.RParen;
      Some (reg_name, reg_pos)
  | _ -> None

let parse_param s =
  let reg = parse_param_reg_attr s in
  (* `mut x: T` / `mut self` — the binding is mutable.  Immutable by
     default, mirroring `let` vs `let mut`. *)
  let is_mut =
    if peek s = Token.Mut then (ignore (advance s); true) else false
  in
  (* Bare method receivers: `self` (by value), `*self` (mut pointee),
     `*const self` (read-only pointee).  All typed as TySelf-shaped
     here; parse_impl_block substitutes the impl's target type once
     it's known.  An explicit `self: T` still goes through the
     regular `name: type` path below. *)
  match peek s with
  | Token.Ident "self" when peek2 s <> Token.Colon ->
      ignore (advance s);
      Ast.{ pname = "self"; pty = Ast.TySelf; preg = None; is_mut }
  | Token.Star when peek2 s = Token.Ident "self" ->
      ignore (advance s); ignore (advance s);
      Ast.{ pname = "self"; pty = Ast.TyPtr Ast.TySelf; preg = None; is_mut }
  | Token.Star when peek2 s = Token.Const
                 && peek3 s = Token.Ident "self" ->
      ignore (advance s); ignore (advance s); ignore (advance s);
      Ast.{ pname = "self"; pty = Ast.TyConstPtr Ast.TySelf;
            preg = None; is_mut }
  | _ ->
      let (name, _) = expect_ident s ~what:"parameter name" in
      expect s Token.Colon;
      let ty = parse_type s in
      Ast.{ pname = name; pty = ty; preg = Option.map fst reg; is_mut }

let parse_params s =
  expect s Token.LParen;
  parse_comma_list ~close:Token.RParen ~item:parse_param s

let rec parse_primary s =
  let (t, p) = advance s in
  match t with
  | Token.Int n -> Ast.IntLit (n, p)
  | Token.True -> Ast.BoolLit (true, p)
  | Token.False -> Ast.BoolLit (false, p)
  | Token.Null -> Ast.NullLit p
  | Token.String str -> Ast.StringLit (str, p)
  | Token.LBracket ->
      (* Array literal: `[e1, e2, ...]` (explicit) or `[v; N]` (repeat).
         Empty `[]` is rejected — no element type or size to infer. *)
      if peek s = Token.RBracket then
        Error.failf p "empty array literal `[]` is not allowed";
      let first = parse_expr s in
      (match peek s with
       | Token.Semicolon ->
           ignore (advance s);
           let count = parse_expr s in
           expect s Token.RBracket;
           Ast.ArrayRepeat { value = first; count; pos = p }
       | Token.Comma ->
           ignore (advance s);
           let rest = parse_comma_list ~close:Token.RBracket ~item:parse_expr s in
           Ast.ArrayLit (first :: rest, p)
       | Token.RBracket ->
           ignore (advance s);
           Ast.ArrayLit ([ first ], p)
       | t ->
           Error.failf (peek_pos s)
             "expected ',', ';' or ']' in array literal, got %s" (Token.pp t))
  | Token.Minus -> Ast.Neg (parse_primary s, p)
  | Token.Tilde -> Ast.BitNot (parse_primary s, p)
  (* `!e` — logical not.  Postfix-tight so `!a.eq(b)` = `!(a.eq(b))`. *)
  | Token.Bang -> Ast.Not (parse_postfix s (parse_primary s), p)
  | Token.Try ->
      (* `try expr` — unwrap-or-early-return.  Postfix-tight, like
         Rust's `?`: the operand is a primary plus its postfix chain,
         so `try foo().bar` = `try (foo().bar)`, but `try a + b` =
         `(try a) + b`.  Wrap in parens for the latter shape. *)
      Ast.Try (parse_postfix s (parse_primary s), p)
  | Token.SizeOf ->
      (* `size_of(T)` — yields C `sizeof(T)` as c_uint.  The argument
         is a type annotation, not an expression: parser dispatches on
         the SizeOf keyword and consumes a parenthesised type. *)
      expect s Token.LParen;
      let ty = parse_type s in
      expect s Token.RParen;
      Ast.SizeOf (ty, p)
  | Token.If ->
      (* `if` in expression position (`let x = if c { a } else { b }`).
         The `if` keyword is already consumed by [advance]. *)
      parse_if_after_kw s p
  | Token.Amp -> Ast.Ref (parse_postfix s (parse_primary s), p)
  | Token.Star -> Ast.Deref (parse_postfix s (parse_primary s), p)
  | Token.New ->
      (* `new Path { f1: e1, ... }` — heap-allocate struct + init.
         The struct path is required; the brace body is mandatory and
         allowed even in cond positions (no ambiguity since `new` is a
         dedicated keyword). *)
      let path = parse_path s ~what:"struct name after 'new'" in
      expect s Token.LBrace;
      let (fields, base) = parse_struct_lit_body s in
      Ast.New { tname = path; fields; base; pos = p }
  | Token.Match ->
      (* `match scrutinee { | pat => expr | pat => expr }` — leading `|`
         on every arm (OCaml/F# style; differs from Rust's trailing `,`).
         The scrutinee is parsed in `allow_struct_lit = false` mode so
         the opening `{` always begins the match body, never a struct
         literal. *)
      let prev = s.allow_struct_lit in
      s.allow_struct_lit <- false;
      let scrutinee = parse_expr s in
      s.allow_struct_lit <- prev;
      expect s Token.LBrace;
      let arms = parse_match_arms s [] in
      Ast.Match { scrutinee; arms; pos = p }
  | Token.Ident name ->
      (* Path-qualified identifiers: foo::bar::baz(...).  Build the full
         path, then decide if it ends in a call, a struct literal, an
         enum unit-variant constructor, or a bare value. *)
      let path = parse_path_tail s [name] in
      (match peek s with
       | Token.LParen ->
           ignore (advance s);
           Ast.Call { callee = path; args = parse_args s; pos = p }
       | Token.LBrace when s.allow_struct_lit ->
           ignore (advance s);
           let (fields, base) = parse_struct_lit_body s in
           Ast.StructLit { tname = path; fields; base; pos = p }
       | _ ->
           (match path with
            | [single] -> Ast.Var (single, p)
            | _ ->
                (* Bare qualified path with no parens / braces — the
                   only legal use is constructing a unit enum variant
                   (`Foo::A`).  Elab dispatches the path; if it doesn't
                   resolve to an enum variant we get a clearer error
                   there than guessing at the parse level. *)
                let (init, last) =
                  match List.rev path with
                  | [] -> assert false
                  | n :: rest -> (List.rev rest, n)
                in
                Ast.EnumLit { tname = init; variant = last;
                              args = Ast.EATuple []; pos = p }))
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
    match peek s with
    | Token.Dot ->
        let p = peek_pos s in
        ignore (advance s);
        let (n, _) = expect_ident s ~what:"field or method name after '.'" in
        if peek s = Token.LParen then begin
          ignore (advance s);
          let args = parse_args s in
          loop (Ast.MethodCall { receiver = e; name = n; args; pos = p })
        end else
          loop (Ast.FieldAccess (e, n, p))
    | Token.LBracket ->
        (* `e[i]` — array indexing.  The index resets allow_bitor (fresh
           bracketed sub-expression). *)
        let p = peek_pos s in
        ignore (advance s);
        let index = parse_expr s in
        expect s Token.RBracket;
        loop (Ast.Index { base = e; index; pos = p })
    | Token.PipeGt ->
        (* `e |> f(a, b)` ≡ `f(e, a, b)` — desugar at the parser, no AST
           node.  Same tightness as `.method()` (Model B / Elixir-first-arg).
           RHS grammar: path with optional `(args)` — explicitly no
           method-call (`obj.m()`) on the right; use `e.m()` directly. *)
        let p = peek_pos s in
        ignore (advance s);
        let (head, _) =
          expect_ident s ~what:"function name after '|>'"
        in
        let path = parse_path_tail s [head] in
        let args =
          if peek s = Token.LParen then begin
            ignore (advance s); parse_args s
          end else []
        in
        loop (Ast.Call { callee = path; args = e :: args; pos = p })
    | _ -> e
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
    | Token.Percent ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Mod, left, parse_cast s, p))
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
    | Token.PlusPlus ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Concat, left, parse_mul s, p))
    | _ -> left
  in
  loop (parse_mul s)

(* Bitwise / shift ladder, tighter than comparison and looser than `+`/`-`
   (Rust order: `a & b == c` is `(a & b) == c`).  Tight→loose within:
   shift, then `&`, then `^`, then `|`. *)
and parse_shift s =
  let rec loop left =
    match peek s with
    | Token.Shl ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Shl, left, parse_add s, p))
    | Token.Shr ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Shr, left, parse_add s, p))
    | _ -> left
  in
  loop (parse_add s)

and parse_bitand s =
  let rec loop left =
    match peek s with
    | Token.Amp ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.BitAnd, left, parse_shift s, p))
    | _ -> left
  in
  loop (parse_shift s)

and parse_bitxor s =
  let rec loop left =
    match peek s with
    | Token.Caret ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.BitXor, left, parse_bitand s, p))
    | _ -> left
  in
  loop (parse_bitand s)

(* `|` (bitor) is suppressed at the top level of a match arm body, where a
   bare `|` is the arm separator; parens/args restore it (see allow_bitor). *)
and parse_bitor s =
  let rec loop left =
    match peek s with
    | Token.Pipe when s.allow_bitor ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.BitOr, left, parse_bitxor s, p))
    | _ -> left
  in
  loop (parse_bitxor s)

(* comparison binds looser than the bitwise ladder; only one comparison
   per expression *)
and parse_cmp s =
  let left = parse_bitor s in
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
      ignore (advance s); Ast.BinOp (op, left, parse_bitor s, p)

(* `orelse` has the lowest precedence — `a == b orelse default` parses as
   `(a == b) orelse default` (the comparison happens first, then the
   whole thing is the scrutinee).  Right-associative: `a orelse b orelse
   c` = `a orelse (b orelse c)`.

   Entering a full expression re-enables bitor (`|`): inside any
   parenthesised / argument / condition context `|` is unambiguously the
   bitwise operator.  Only the top level of a match arm body suppresses it
   (see parse_arm_body), so a bare `|` there reads as the arm separator. *)
(* Short-circuiting logical operators `&&` / `||`, looser than comparisons
   and tighter than `orelse` (Rust-order).  `||` is the loosest of the two
   so `a && b || c` parses as `(a && b) || c`. *)
and parse_and s =
  let rec loop left =
    match peek s with
    | Token.AmpAmp ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.And, left, parse_cmp s, p))
    | _ -> left
  in
  loop (parse_cmp s)

and parse_or s =
  let rec loop left =
    match peek s with
    | Token.PipePipe ->
        let p = peek_pos s in
        ignore (advance s); loop (Ast.BinOp (Ast.Or, left, parse_and s, p))
    | _ -> left
  in
  loop (parse_and s)

(* Range as a value: `a..b` / `a..=b`.  Non-associative (no `a..b..c`),
   Rust-order — looser than `||` / `&&` / arithmetic, tighter than
   `orelse`.  So `0..n+1` parses as `0..(n+1)` and `0..len(a)` as
   `0..(len(a))`. *)
and parse_range s =
  let left = parse_or s in
  match peek s with
  | Token.DotDot ->
      let p = peek_pos s in
      ignore (advance s);
      let right = parse_or s in
      Ast.Range { lo = left; hi = right; inclusive = false; pos = p }
  | Token.DotDotEq ->
      let p = peek_pos s in
      ignore (advance s);
      let right = parse_or s in
      Ast.Range { lo = left; hi = right; inclusive = true; pos = p }
  | _ -> left

and parse_expr s =
  let prev = s.allow_bitor in
  s.allow_bitor <- true;
  let left = parse_range s in
  let r =
    if peek s = Token.Orelse then begin
      let p = peek_pos s in
      ignore (advance s);
      Ast.Orelse (left, parse_expr s, p)
    end else left
  in
  s.allow_bitor <- prev;
  r

(* A match arm body: a full expression but with a bare top-level `|`
   suppressed (it is the arm separator).  Mirrors parse_expr's orelse
   handling without re-enabling bitor; nested parens / args restore it via
   parse_expr.  When the body opens with `{`, parse a statement block —
   the block becomes an `Ast.Block` whose elaboration walks the stmts
   in the surrounding fn-body's decl scope.  Restores bitor inside the
   block (statements may contain bitwise exprs). *)
and parse_arm_body s =
  if peek s = Token.LBrace then begin
    let p = peek_pos s in
    let prev = s.allow_bitor in
    s.allow_bitor <- true;
    let stmts = parse_block s in
    s.allow_bitor <- prev;
    Ast.Block (stmts, p)
  end else
    let prev = s.allow_bitor in
    s.allow_bitor <- false;
    let rec go () =
      let left = parse_range s in
      if peek s = Token.Orelse then begin
        let p = peek_pos s in
        ignore (advance s);
        Ast.Orelse (left, go (), p)
      end else left
    in
    let r = go () in
    s.allow_bitor <- prev;
    r

and parse_args s = parse_comma_list ~close:Token.RParen ~item:parse_expr s

(* Pattern grammar:
     `_`                              → wildcard
     `<ident>`                        → bind (single segment, no `::`)
     `<Path>::<Variant>`              → unit variant
     `<Path>::<Variant>(p, ...)`      → tuple variant
     `<Path>::<Variant> { f: p, ... }`→ struct variant (shorthand `f` ok)

   Distinguishing bind from unit-variant relies on the path having more
   than one segment for variants — bare `name` is always a binding. *)
and parse_pattern s =
  let p = peek_pos s in
  let head = parse_pattern_atom s in
  (* Or-pattern continuation: `pat | pat | pat`.  Greedy — consumes all
     `|`-prefixed alternatives.  Disambiguates from the arm separator
     (which only fires *after* `=>` and body): in pattern position, `|`
     can only be or-pattern.  Single-pattern paths bypass POr (no
     allocation for the common case). *)
  if peek s <> Token.Pipe then head
  else
    let rec gather acc =
      ignore (advance s);            (* consume '|' *)
      let next = parse_pattern_atom s in
      let acc = next :: acc in
      if peek s = Token.Pipe then gather acc else List.rev acc
    in
    Ast.POr (gather [head], p)

and parse_pattern_atom s =
  let p = peek_pos s in
  match peek s with
  | Token.Ident "_" ->
      ignore (advance s); Ast.PWildcard p
  | Token.Ident name ->
      ignore (advance s);
      let path = parse_path_tail s [name] in
      (match path with
       | [single] -> Ast.PVar (single, p)
       | _ ->
           let (init, last) =
             match List.rev path with
             | [] -> assert false
             | n :: rest -> (List.rev rest, n)
           in
           let binds =
             match peek s with
             | Token.LParen ->
                 ignore (advance s);
                 Ast.PBTuple
                   (parse_comma_list ~close:Token.RParen
                      ~item:parse_pattern s)
             | Token.LBrace ->
                 ignore (advance s);
                 (* Struct-variant pattern.  Each entry is `field: pat`,
                    or shorthand `field` (desugars to `field: PVar field`).
                    Order matches the field order; missing/extra checked
                    later in typecheck. *)
                 let parse_struct_pat_field s =
                   let (fname, fpos) =
                     expect_ident s ~what:"field name in variant pattern"
                   in
                   if peek s = Token.Colon then begin
                     ignore (advance s);
                     (fname, parse_pattern s)
                   end else
                     (fname, Ast.PVar (fname, fpos))
                 in
                 Ast.PBStruct
                   (parse_comma_list ~close:Token.RBrace
                      ~item:parse_struct_pat_field s)
             | _ -> Ast.PBTuple []
           in
           Ast.PVariant { tname = init; variant = last; binds; pos = p })
  | t ->
      Error.failf p "expected pattern, got %s" (Token.pp t)

(* Parse `pat => expr | pat => expr ... }` after the opening `{` has been
   consumed.  Arms are SEPARATED by `|` (no leading `|`); the same `|` is
   the bitor operator elsewhere, disambiguated by position (a bare `|` in
   an arm body is suppressed — see parse_arm_body, so it reads as this
   separator).  `}` is consumed on exit. *)
and parse_match_arms s acc =
  (match peek s with
   | Token.RBrace ->
       Error.raise_ (peek_pos s) "'match' must have at least one arm"
   | Token.Eof ->
       Error.raise_ s.last_pos "unexpected end of file inside 'match'"
   | _ -> ());
  let arm_pos = peek_pos s in
  let pat = parse_pattern s in
  (* Optional `if <cond>` guard between pattern and `=>`.  Bitor stays
     enabled inside the guard expression — the guard runs to `=>`, and
     the arm-separator `|` only appears *after* the body. *)
  let guard =
    if peek s = Token.If then begin
      ignore (advance s);
      let prev = s.allow_struct_lit in
      s.allow_struct_lit <- false;
      let g = parse_expr s in
      s.allow_struct_lit <- prev;
      Some g
    end else None
  in
  expect s Token.FatArrow;
  let body = parse_arm_body s in
  let acc = Ast.{ pat; guard; body; arm_pos } :: acc in
  match peek s with
  | Token.Pipe -> ignore (advance s); parse_match_arms s acc
  | Token.RBrace -> ignore (advance s); List.rev acc
  | Token.Eof ->
      Error.raise_ s.last_pos "unexpected end of file inside 'match'"
  | t ->
      Error.failf (peek_pos s)
        "expected '|' (next arm) or '}' after match arm, got %s" (Token.pp t)

and parse_block s =
  expect s Token.LBrace;
  let body = parse_stmts s [] in
  expect s Token.RBrace;
  body

(* `if cond { ... } [else { ... } | else if ...]`.  One syntactic form
   for both the statement and the expression role; elab decides which.
   The condition is parsed with struct literals disabled so the opening
   `{` always begins the then-block.  `else if` nests as the else
   block's trailing expression. *)
and parse_if s =
  let pos = peek_pos s in
  expect s Token.If;
  parse_if_after_kw s pos
and parse_if_after_kw s pos =
  let prev = s.allow_struct_lit in
  s.allow_struct_lit <- false;
  let cond = parse_expr s in
  s.allow_struct_lit <- prev;
  let then_blk = parse_block s in
  let else_blk =
    match peek s with
    | Token.Else ->
        ignore (advance s);
        (match peek s with
         | Token.If -> Some [ Ast.Tail (parse_if s) ]
         | _ -> Some (parse_block s))
    | _ -> None
  in
  Ast.If { cond; then_blk; else_blk; pos }

and parse_stmt s =
  match peek s with
  | Token.Let ->
      let pos = peek_pos s in
      ignore (advance s);
      (* `let mut x` / `let mut (a, b)` — mutable binding.  Immutable by
         default. *)
      let is_mut =
        if peek s = Token.Mut then (ignore (advance s); true) else false
      in
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
           Ast.LetTuple { names; value; is_mut; pos }
       | _ ->
           (* FP-2 let-else detection: a refutable-variant pattern
              always starts with a qualified path (`Option::Some(v)`,
              `Foo::Bar(x)`).  Peek two tokens — `Ident :: ...` →
              parse as let-else; everything else takes the existing
              single-ident `let name [: type] = expr;` path
              (covers `let _ = ...` since `_` is also an Ident). *)
           let is_let_else =
             match s.tokens with
             | (Token.Ident _, _) :: (Token.DoubleColon, _) :: _ -> true
             | _ -> false
           in
           if is_let_else then begin
             if is_mut then
               Error.failf pos
                 "'let mut' is not supported with a refutable pattern \
                  (pattern binds are immutable by default)";
             let pat = parse_pattern s in
             expect s Token.Eq;
             let value = parse_expr s in
             (match advance s with
              | (Token.Else, _) -> ()
              | (t, p) ->
                  Error.failf p
                    "expected 'else { ... }' after `let <pattern> = \
                     expr`, got %s" (Token.pp t));
             let else_body = parse_block s in
             expect s Token.Semicolon;
             Ast.LetElse { pat; value; else_body; pos }
           end else
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
             Ast.Let { name; value; ty_ann; is_mut; pos = name_pos })
  | Token.Return ->
      let pos = peek_pos s in
      ignore (advance s);
      (* `return;` (no value) early-exits a void fn / main; `return e;`
         carries a value. *)
      let e =
        if peek s = Token.Semicolon then None
        else Some (parse_expr s)
      in
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
      (* Block-shaped, self-terminating: a trailing `if` (last in its
         block, `}` next) is the block's value (`Tail`); otherwise it is a
         void statement. *)
      let e = parse_if s in
      if peek s = Token.RBrace then Ast.Tail e else Ast.ExprStmt e
  | Token.While ->
      ignore (advance s);
      let prev = s.allow_struct_lit in
      s.allow_struct_lit <- false;
      let cond = parse_expr s in
      s.allow_struct_lit <- prev;
      let body = parse_block s in
      Ast.While { cond; body }
  | Token.Loop ->
      (* `loop { body }` — infinite loop, desugars to `while true`.
         No new AST node: the `true` literal is a plain bool cond. *)
      let pos = peek_pos s in
      ignore (advance s);
      let body = parse_block s in
      Ast.While { cond = Ast.BoolLit (true, pos); body }
  | Token.Break ->
      let pos = peek_pos s in
      ignore (advance s);
      expect s Token.Semicolon;
      Ast.Break pos
  | Token.Continue ->
      let pos = peek_pos s in
      ignore (advance s);
      expect s Token.Semicolon;
      Ast.Continue pos
  | Token.For ->
      (* `for v in <range> { body }`.  `<range>` is parsed as a full
         expression with struct-lits disabled so the body's opening `{`
         isn't mistaken for a literal.  A literal `a..b` / `a..=b` is
         folded by `parse_range` into an `Ast.Range`; any other Range-typed
         value works at typecheck time via field extraction. *)
      let pos = peek_pos s in
      ignore (advance s);
      let (var, _) = expect_ident s ~what:"loop variable after 'for'" in
      expect s Token.In;
      let prev = s.allow_struct_lit in
      s.allow_struct_lit <- false;
      let range = parse_expr s in
      s.allow_struct_lit <- prev;
      let body = parse_block s in
      Ast.For { var; range; body; pos }
  | Token.Match ->
      (* `match` is parsed as an expression but its statement form
         needs no trailing `;` — block-shaped, like `if`/`while`.
         A trailing `match` (last in its block) is the block's value. *)
      let e = parse_primary s in
      if peek s = Token.RBrace then Ast.Tail e else Ast.ExprStmt e
  | _ ->
      (* General statement: parse an expression, then dispatch on what
         follows.  An `=` makes it an assignment whose target is either a
         bare variable (`x = ...;`) or a field path (`p.x = ...;`); a `;`
         keeps it as an expression statement; a closing `}` (no `;`) makes
         it the block's trailing value. *)
      let e = parse_expr s in
      (match peek s with
       | Token.Eq ->
           let pos = peek_pos s in
           ignore (advance s);
           let value = parse_expr s in
           expect s Token.Semicolon;
           (match e with
            | Ast.Var (name, vp) ->
                Ast.Assign { path = [name]; value; pos = vp }
            | Ast.EnumLit { tname; variant; args = Ast.EATuple []; pos = ep } ->
                (* `raw::DOSBase = ...` parses LHS as EnumLit (qualified
                   path with no parens).  Lower to a path-assign — typecheck
                   resolves the path to an `extern var`. *)
                Ast.Assign { path = tname @ [variant]; value; pos = ep }
            | Ast.FieldAccess (target, field, fp) ->
                Ast.AssignField { target; field; value; pos = fp }
            | Ast.Index { base; index; pos = ip } ->
                Ast.AssignIndex { base; index; value; pos = ip }
            | Ast.Deref (target, dp) ->
                Ast.AssignDeref { target; value; pos = dp }
            | _ ->
                Error.failf pos "invalid assignment target")
       | Token.RBrace -> Ast.Tail e
       | _ ->
           expect s Token.Semicolon;
           Ast.ExprStmt e)

and parse_stmts s acc =
  match peek s with
  | Token.RBrace -> List.rev acc
  | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
  | Token.Semicolon ->
      (* A `;` in statement-start position is almost always a stray
         semicolon after a self-terminating block (`if`/`while`/
         `match`/inner `{ ... }`).  Without this dedicated case the
         parser would fall through to `parse_stmt` -> `parse_expr` and
         die on the generic "expected expression", pointing at the `;`
         with no hint about why it's wrong. *)
      Error.raise_ (peek_pos s)
        "stray ';' — `if`/`while`/`match` and inner blocks are \
         self-terminating, no trailing semicolon needed"
  | _ -> parse_stmts s (parse_stmt s :: acc)

(* Close the forward reference so `parse_type` can read `[T; N]` sizes. *)
let () = parse_expr_fwd := parse_expr

(* Quadratic dedup over small parser lists (params, fields, variants,
   use items).  Returns the first repeated key string, or None if all
   keys distinct.  O(n²) is fine here — n is the size of one decl's
   member list (typically ≤ 20); Hashtbl would dwarf the work. *)
let find_dup_key ~key xs =
  let rec loop seen = function
    | [] -> None
    | x :: rest ->
        let k = key x in
        if List.mem k seen then Some k
        else loop (k :: seen) rest
  in
  loop [] xs

(* Reject duplicate parameter names within a single fn / extern fn
   parameter list.  [kind] tags the decl shape in the error message
   ("function" / "extern fn"), [name_pos] points at the fn name so
   the user can navigate from the report to the offending decl. *)
let check_dup_params params ~kind ~name ~name_pos =
  match find_dup_key ~key:(fun (p : Ast.param) -> p.pname) params with
  | None -> ()
  | Some n ->
      Error.failf name_pos "duplicate parameter '%s' in %s '%s'" n kind name

(* `<T>` / `<T, U>` after a struct/enum/fn name — generic type
   parameter list.  Returns names in source order; rejects empty `<>`
   and duplicate names within the list. *)
let parse_tparams s =
  if peek s <> Token.Lt then []
  else begin
    ignore (advance s);
    let names =
      parse_comma_list ~close:Token.Gt
        ~item:(fun s ->
          let (n, _) = expect_ident s ~what:"type parameter name" in n)
        s
    in
    if names = [] then
      Error.failf (peek_pos s) "empty type parameter list <>";
    (match find_dup_key ~key:Fun.id names with
     | None -> ()
     | Some n ->
         Error.failf (peek_pos s) "duplicate type parameter '%s'" n);
    names
  end

(* Like [parse_tparams] but also reads trait bounds: `<T: Area, U: A + B>`.
   Returns (names, bounds) where bounds is a flat list of (tparam,
   trait_path) — one entry per bound (so `T: A + B` yields two).  Used by
   functions; structs/enums/impls use the plain [parse_tparams]. *)
let parse_tparams_bounded s =
  if peek s <> Token.Lt then ([], [])
  else begin
    ignore (advance s);
    let bounds = ref [] in
    let names =
      parse_comma_list ~close:Token.Gt
        ~item:(fun s ->
          let (n, _) = expect_ident s ~what:"type parameter name" in
          if peek s = Token.Colon then begin
            ignore (advance s);
            (* one or more `+`-separated trait paths *)
            let rec read_bounds () =
              let path = parse_path s ~what:"trait name in bound" in
              bounds := (n, path) :: !bounds;
              if peek s = Token.Plus then (ignore (advance s); read_bounds ())
            in
            read_bounds ()
          end;
          n)
        s
    in
    if names = [] then
      Error.failf (peek_pos s) "empty type parameter list <>";
    (match find_dup_key ~key:Fun.id names with
     | None -> ()
     | Some n ->
         Error.failf (peek_pos s) "duplicate type parameter '%s'" n);
    (names, List.rev !bounds)
  end

let rec parse_function s seen_fns ~is_pub =
  expect s Token.Fn;
  let (name, name_pos) = expect_ident s ~what:"function name after 'fn'" in
  if List.mem name seen_fns then
    Error.failf name_pos "function '%s' already defined" name;
  let (tparams, tbounds) = parse_tparams_bounded s in
  let params = parse_params s in
  check_dup_params params ~kind:"function" ~name ~name_pos;
  (* `@reg(...)` makes sense only on extern fn params (it pins the
     param to an m68k register in the emitted C declaration).  Regular
     fns don't get a calling convention from us — reject up front. *)
  List.iter (fun (p : Ast.param) ->
    match p.preg with
    | Some _ ->
        Error.failf name_pos
          "'@reg(...)' on parameter '%s' is only allowed on `extern fn`"
          p.pname
    | None -> ())
    params;
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
  (name, Ast.{ name; c_name = name; tparams; tbounds; params; ret_ty; body;
               is_pub; is_extern = false; is_variadic = false;
               tier_hint = None; amiga_lib = None; must_use = false;
               pos = name_pos })

(* `extern fn name(args) -> T;` — forward decl for a C-side symbol.
   Same param/return grammar as a regular fn, but body is replaced by
   `;`.  Generic params, `pub`, and a body block are all rejected at
   parse time so the parsed `Ast.Function` is always well-formed.
   The `extern` keyword is consumed by the caller (parse_item). *)
and parse_extern_fn_after_keyword s seen_fns =
  expect s Token.Fn;
  let (name, name_pos) =
    expect_ident s ~what:"function name after 'extern fn'"
  in
  if List.mem name seen_fns then
    Error.failf name_pos "function '%s' already defined" name;
  (* Optional `as <C-symbol>` rename.  When present, [name] is the
     exile-side identifier used at call sites and [c_name] is the
     symbol the linker resolves against.  Matches use cases like
     `extern fn alloc_mem as AllocMem(...)` where exile prefers
     snake_case but the C library exposes CamelCase. *)
  let c_name =
    if peek s = Token.As then begin
      ignore (advance s);
      let (cn, _) =
        expect_ident s ~what:"C symbol name after 'as'"
      in
      cn
    end else name
  in
  if peek s = Token.Lt then
    Error.failf name_pos
      "'extern fn %s' cannot have generic parameters — C signatures \
       must be concrete" name;
  let (params, is_variadic) = parse_extern_params s ~name in
  check_dup_params params ~kind:"extern fn" ~name ~name_pos;
  (* Validate `@reg(...)` register names.  m68k has d0-d7 + a0-a6 (a7
     is the stack pointer and isn't user-addressable).  Reject invalid
     names early — Bebbo gcc would otherwise emit obscure C errors. *)
  let valid_regs =
    [ "d0"; "d1"; "d2"; "d3"; "d4"; "d5"; "d6"; "d7";
      "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6" ]
  in
  List.iter (fun (p : Ast.param) ->
    match p.preg with
    | Some r when not (List.mem r valid_regs) ->
        Error.failf name_pos
          "invalid m68k register '%s' in '@reg(%s)' on parameter '%s' \
           — expected one of d0..d7 or a0..a6" r r p.pname
    | _ -> ())
    params;
  let ret_ty = parse_ret_ty s in
  (match peek s with
   | Token.Semicolon -> ignore (advance s)
   | Token.LBrace ->
       Error.failf (peek_pos s)
         "'extern fn %s' must end with ';', not a body — extern \
          declares an existing C symbol" name
   | t ->
       Error.failf (peek_pos s)
         "expected ';' after 'extern fn %s' signature, got %s"
         name (Token.pp t));
  (name, Ast.{ name; c_name; tparams = []; tbounds = []; params; ret_ty;
               body = [];
               (* extern items are implicitly pub: they live in `mod raw`
                  by FFI hygiene rule, and the whole point is for the
                  surrounding stdlib / wrappers to call them. *)
               is_pub = true; is_extern = true; is_variadic;
               tier_hint = None; amiga_lib = None; must_use = false;
               pos = name_pos })

(* Like parse_params but accepts a trailing `, ...` to mark the fn as
   C-style variadic.  `(...)` alone (no fixed params before ellipsis)
   is rejected — C requires at least one fixed param so the callee
   has somewhere to call `va_start` from. *)
and parse_extern_params s ~name =
  expect s Token.LParen;
  match peek s with
  | Token.RParen ->
      ignore (advance s);
      ([], false)
  | Token.Ellipsis ->
      Error.failf (peek_pos s)
        "'extern fn %s' variadic '...' must come after at least one \
         fixed parameter (e.g. `(fmt: str, ...)`)" name
  | _ ->
      let rec loop acc =
        let p = parse_param s in
        match peek s with
        | Token.Comma ->
            ignore (advance s);
            (match peek s with
             | Token.Ellipsis ->
                 ignore (advance s);
                 expect s Token.RParen;
                 (List.rev (p :: acc), true)
             | _ -> loop (p :: acc))
        | Token.RParen ->
            ignore (advance s);
            (List.rev (p :: acc), false)
        | t ->
            Error.failf (peek_pos s)
              "expected ',' or ')' in 'extern fn %s' parameter list, got %s"
              name (Token.pp t)
      in
      loop []

(* `extern struct Name;` — opaque C struct (legal use: `*Name`).
   `extern struct Name { f: T, ... }` — exposed: fields readable /
   writable from exile.  Layout / size live on the C side (typically
   via `@c_include("...")`); exile trusts the declaration to match.
   Generic params, `pub`, and methods are rejected.  Caller consumed
   the `extern` keyword. *)
and parse_extern_struct_after_keyword s seen =
  expect s Token.Struct;
  let (name, name_pos) =
    expect_ident s ~what:"struct name after 'extern struct'"
  in
  if List.mem name seen then
    Error.failf name_pos "name '%s' already used in this scope" name;
  if peek s = Token.Lt then
    Error.failf name_pos
      "'extern struct %s' cannot have generic parameters — extern \
       types live on the C side" name;
  let esfields = match peek s with
    | Token.Semicolon -> ignore (advance s); None
    | Token.LBrace ->
        ignore (advance s);
        let parse_field s =
          let (fname, _) = expect_ident s ~what:"field name in extern struct" in
          expect s Token.Colon;
          let ty = parse_type s in
          (fname, ty)
        in
        let fields =
          parse_comma_list ~close:Token.RBrace ~item:parse_field s
        in
        (match find_dup_key ~key:fst fields with
         | None -> ()
         | Some n ->
             Error.failf name_pos
               "duplicate field '%s' in extern struct '%s'" n name);
        if fields = [] then
          Error.failf name_pos
            "'extern struct %s {}' is empty — use `extern struct %s;` \
             for opaque types"
            name name;
        Some fields
    | t ->
        Error.failf (peek_pos s)
          "expected ';' (opaque) or '{ ... }' (with fields) after \
           'extern struct %s', got %s"
          name (Token.pp t)
  in
  Ast.{ esname = name; esfields; espos = name_pos }

(* Shared head for `extern <kw> NAME ...` decls (type / const / var):
   consume the keyword, expect an ident, reject duplicates against
   the surrounding scope, reject a generic-param list.  The tail
   (just `;` for type, `: T;` for const/var) is parsed by the caller.
   [label] names the decl kind for error messages tying to the source
   syntax ("type" / "const" / "var"); [ident_label] names the
   identifier ("type name" / "const name" / "variable name") for the
   "expected X, got Y" prompt. *)
and parse_extern_decl_head s seen ~keyword ~label ~ident_label =
  expect s keyword;
  let (name, name_pos) =
    expect_ident s ~what:(ident_label ^ " after 'extern " ^ label ^ "'")
  in
  if List.mem name seen then
    Error.failf name_pos "name '%s' already used in this scope" name;
  if peek s = Token.Lt then
    Error.failf name_pos
      "'extern %s %s' cannot have generic parameters" label name;
  (name, name_pos)

(* Shared tail for typed extern decls (const / var): `: T ;`. *)
and parse_typed_extern_tail s ~label ~name =
  (match peek s with
   | Token.Colon -> ignore (advance s)
   | t ->
       Error.failf (peek_pos s)
         "'extern %s %s' must declare its type with `: T`, got %s"
         label name (Token.pp t));
  let ty = parse_type s in
  (match peek s with
   | Token.Semicolon -> ignore (advance s)
   | t ->
       Error.failf (peek_pos s)
         "expected ';' after 'extern %s %s: ...', got %s"
         label name (Token.pp t));
  ty

(* `extern type T;` — raw C type alias.  Different from extern struct:
   no `struct` prefix on the C side, used directly as a typedef'd name
   (LONG, APTR, ULONG, etc).  Caller consumed the `extern` keyword. *)
and parse_extern_type_after_keyword s seen =
  let (name, name_pos) =
    parse_extern_decl_head s seen ~keyword:Token.Type ~label:"type"
      ~ident_label:"type name"
  in
  (match peek s with
   | Token.Semicolon -> ignore (advance s)
   | t ->
       Error.failf (peek_pos s)
         "expected ';' after 'extern type %s', got %s" name (Token.pp t));
  Ast.{ xtname = name; xtpos = name_pos }

(* `extern const NAME: T;` — top-level value declared on the C side.
   Resolved by the linker; exile uses [name] in expression positions
   wherever a value of type T is expected.  Common case: importing
   `#define` constants via a companion C-stub or via a header. *)
and parse_extern_const_after_keyword s seen =
  let (name, name_pos) =
    parse_extern_decl_head s seen ~keyword:Token.Const ~label:"const"
      ~ident_label:"const name"
  in
  let ty = parse_typed_extern_tail s ~label:"const" ~name in
  Ast.{ ecname = name; ecty = ty; ecpos = name_pos }

(* `extern var NAME: T;` — top-level mutable global declared on the C
   side.  Like extern const but assignable; codegen omits the `const`
   qualifier.  Used for AmigaOS library bases (DOSBase, SysBase, ...)
   that get set by `OpenLibrary()` at runtime. *)
and parse_extern_var_after_keyword s seen =
  let (name, name_pos) =
    parse_extern_decl_head s seen ~keyword:Token.Var ~label:"var"
      ~ident_label:"variable name"
  in
  let ty = parse_typed_extern_tail s ~label:"var" ~name in
  Ast.{ evname = name; evty = ty; evpos = name_pos }

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
      [ (Some name, Ast.Use { path = prefix; is_wildcard = false;
                               is_pub = false; pos = p }) ]
  | `Wildcard ->
      expect s Token.Semicolon;
      [ (None, Ast.Use { path = prefix; is_wildcard = true;
                         is_pub = false; pos = p }) ]
  | `Group ->
      let rec collect_names acc =
        match advance s with
        | (Token.Ident n, _) ->
            let path = prefix @ [n] in
            let acc =
              (Some n, Ast.Use { path; is_wildcard = false;
                                 is_pub = false; pos = p }) :: acc
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
      (* Reject duplicates within the group itself.  Wildcard items
         (None) carry no name and are excluded from the dedup. *)
      let named_items =
        List.filter_map (fun (n, _) -> n) items
      in
      (match find_dup_key ~key:Fun.id named_items with
       | None -> ()
       | Some n ->
           Error.failf p "duplicate name '%s' in 'use' group" n);
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
  | Token.Extern ->
      if is_pub then
        Error.failf (peek_pos s)
          "'pub' is redundant on 'extern' — extern items are always \
           callable / referenceable";
      (* `extern` is a shared prefix: fn for forward fn-decl, struct
         for opaque type decl.  Decide on the next token. *)
      ignore (advance s);  (* consume `extern` *)
      (match peek s with
       | Token.Fn ->
           let (name, fn) = parse_extern_fn_after_keyword s seen in
           [ (Some name, Ast.Function fn) ]
       | Token.Struct ->
           let es = parse_extern_struct_after_keyword s seen in
           [ (Some es.Ast.esname, Ast.ExternStruct es) ]
       | Token.Type ->
           let et = parse_extern_type_after_keyword s seen in
           [ (Some et.Ast.xtname, Ast.ExternType et) ]
       | Token.Const ->
           let ec = parse_extern_const_after_keyword s seen in
           [ (Some ec.Ast.ecname, Ast.ExternConst ec) ]
       | Token.Var ->
           let ev = parse_extern_var_after_keyword s seen in
           [ (Some ev.Ast.evname, Ast.ExternVar ev) ]
       | t ->
           Error.failf (peek_pos s)
             "expected 'fn', 'struct', 'type', 'const' or 'var' after \
              'extern', got %s"
             (Token.pp t))
  | Token.Mod ->
      let (name, m) = parse_module s seen ~is_pub in
      [ (Some name, Ast.Module m) ]
  | Token.Struct ->
      let sd = parse_struct_decl s ~is_pub in
      if List.mem sd.Ast.sname seen then
        Error.failf sd.Ast.spos
          "name '%s' already used in this scope" sd.Ast.sname;
      [ (Some sd.Ast.sname, Ast.Struct sd) ]
  | Token.Enum ->
      let ed = parse_enum_decl s ~is_pub in
      if List.mem ed.Ast.ename seen then
        Error.failf ed.Ast.epos
          "name '%s' already used in this scope" ed.Ast.ename;
      [ (Some ed.Ast.ename, Ast.Enum ed) ]
  | Token.Const ->
      let cd = parse_const_decl s ~is_pub in
      if List.mem cd.Ast.kname seen then
        Error.failf cd.Ast.kpos
          "name '%s' already used in this scope" cd.Ast.kname;
      [ (Some cd.Ast.kname, Ast.Const cd) ]
  | Token.Type ->
      let ta = parse_type_alias_decl s ~is_pub in
      if List.mem ta.Ast.taname seen then
        Error.failf ta.Ast.tapos
          "name '%s' already used in this scope" ta.Ast.taname;
      [ (Some ta.Ast.taname, Ast.TypeAlias ta) ]
  | Token.Impl ->
      if is_pub then
        Error.failf (peek_pos s)
          "'pub' has no meaning on 'impl' (set visibility per method)";
      let ib = parse_impl_block s in
      (* impl blocks introduce no name into the surrounding scope — their
         methods are looked up via the target struct, not by a free name. *)
      [ (None, Ast.Impl ib) ]
  | Token.Trait ->
      let td = parse_trait s ~is_pub in
      [ (Some td.Ast.trname, Ast.Trait td) ]
  | Token.Use ->
      let items = parse_use_items s in
      let items =
        if is_pub then
          List.map (fun (name_opt, item) ->
            match item with
            | Ast.Use u ->
                (* `pub use foo::*;` is a synonym for `use foo::*;` — the
                   loader inlines foo's public items into this scope, and
                   they keep their own `pub` flag, so they are re-exported
                   transitively either way.  `pub use foo::bar;` (single
                   name) still rides the typecheck alias table. *)
                (name_opt, Ast.Use { u with is_pub = true })
            | other -> (name_opt, other))
            items
        else items
      in
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
  | Token.At ->
      if is_pub then
        Error.failf (peek_pos s) "'pub' has no meaning on '@' attributes";
      let at_pos = peek_pos s in
      ignore (advance s);
      let (attr_name, _) = expect_ident s ~what:"attribute name after '@'" in
      (* Decorate every item produced by parse_item with [apply].  The four
         "decorating" attributes (@tier, @debug, @must_use, @amiga_lib) all
         share this shape — the only thing each contributes is the per-item
         rewrite, with errors pointing at the @ position. *)
      let apply_to_next ~apply =
        parse_item s seen
        |> List.map (fun (name_opt, item) -> (name_opt, apply item))
      in
      (match attr_name with
       | "c_include" ->
           expect s Token.LParen;
           let path =
             match advance s with
             | (Token.String p, _) -> p
             | (t, p) ->
                 Error.failf p
                   "expected string literal in '@c_include(\"...\")', got %s"
                   (Token.pp t)
           in
           expect s Token.RParen;
           [ (None, Ast.CInclude { path; pos = at_pos }) ]
       | "tier" ->
           expect s Token.LParen;
           let (tier_name, tier_pos) =
             match advance s with
             | (Token.Ident n, p) -> (n, p)
             | (t, p) ->
                 Error.failf p
                   "expected tier name (core|standard|full) after \
                    '@tier(', got %s"
                   (Token.pp t)
           in
           if not (List.mem tier_name ["core"; "standard"; "full"]) then
             Error.failf tier_pos
               "unknown tier '%s' (expected: core, standard, full)" tier_name;
           expect s Token.RParen;
           apply_to_next ~apply:(function
             | Ast.Function f ->
                 Ast.Function { f with tier_hint = Some tier_name }
             | Ast.Struct s ->
                 Ast.Struct { s with stier_hint = Some tier_name }
             | Ast.Enum e ->
                 Ast.Enum { e with etier_hint = Some tier_name }
             | _ ->
                 Error.failf at_pos
                   "'@tier' can only decorate fn / struct / enum decls")
       | "debug" ->
           (* `@debug` works on generic struct/enum too: monomorphization
              gives each instance concrete fields, and the printer is
              synthesized per instance.  The per-instance @debug-able field
              check (in typecheck) runs on concrete instances, not the
              skeleton (whose fields are free TVars). *)
           apply_to_next ~apply:(function
             | Ast.Struct s -> Ast.Struct { s with sis_debug = true }
             | Ast.Enum e -> Ast.Enum { e with eis_debug = true }
             | _ ->
                 Error.failf at_pos
                   "'@debug' can only decorate struct / enum decls")
       | "must_use" ->
           apply_to_next ~apply:(function
             | Ast.Function f -> Ast.Function { f with must_use = true }
             | Ast.Enum e -> Ast.Enum { e with emust_use = true }
             | _ ->
                 Error.failf at_pos
                   "'@must_use' can only decorate fn / enum decls")
       | "move" ->
           (* `@move` marks a struct affine / use-at-most-once.  Picked up
              by the DR-002 move-pass; a transitional spelling that retires
              once `own *u8` (capability model) makes affine-ness inferable
              from a field's type.  Enums are not supported in v1 — the
              marker only describes by-value-aliasing structs (String,
              StringBuilder, future Vec). *)
           apply_to_next ~apply:(function
             | Ast.Struct s -> Ast.Struct { s with sis_move = true }
             | _ ->
                 Error.failf at_pos
                   "'@move' can only decorate struct decls")
       | "amiga_lib" ->
           expect s Token.LParen;
           let (base_name, _) =
             expect_ident s ~what:"library base name after '@amiga_lib('"
           in
           expect s Token.RParen;
           apply_to_next ~apply:(function
             | Ast.Function f when f.is_extern ->
                 Ast.Function { f with amiga_lib = Some base_name }
             | _ ->
                 Error.failf at_pos
                   "'@amiga_lib' can only decorate `extern fn` declarations")
       | "doc" ->
           (* `@doc("...")` — explicit attribute form of `///` line
              doc-comments.  Both are syntactically accepted today but
              are not yet propagated into the AST / generated C; the
              syntax exists so source files can carry documentation
              that future tooling (formatters, doc generators) can
              consume without breaking the build.  Validates the arg
              and drops the attribute. *)
           expect s Token.LParen;
           let _doc =
             match advance s with
             | (Token.String d, _) -> d
             | (t, p) ->
                 Error.failf p
                   "expected string literal in '@doc(\"...\")', got %s"
                   (Token.pp t)
           in
           expect s Token.RParen;
           parse_item s seen
       | "derive" ->
           (* `@derive(Eq, Clone, ...)` — auto-implement the named traits
              for the decorated struct / enum.  A pre-typecheck pass
              synthesizes real `impl Trait for Foo` blocks. *)
           expect s Token.LParen;
           let names =
             parse_comma_list ~close:Token.RParen
               ~item:(fun s ->
                 let (n, _) = expect_ident s ~what:"trait name in '@derive'" in n)
               s
           in
           if names = [] then
             Error.failf at_pos "'@derive(...)' needs at least one trait";
           apply_to_next ~apply:(function
             | Ast.Struct s -> Ast.Struct { s with sderives = s.sderives @ names }
             | Ast.Enum e -> Ast.Enum { e with ederives = e.ederives @ names }
             | _ ->
                 Error.failf at_pos
                   "'@derive' can only decorate struct / enum decls")
       | other ->
           Error.failf at_pos
             "unknown attribute '@%s' (only '@c_include', '@tier', \
              '@must_use', '@move', '@debug', '@doc', '@derive' and \
              '@amiga_lib' are supported)"
             other)
  | _ ->
      Error.failf (peek_pos s)
        "expected 'fn', 'extern fn', 'mod', 'use', 'struct', 'enum', \
         'type', 'impl' or '@c_include', got %s"
        (Token.pp (peek s))

(* `const NAME: T = <expr>;` — a compile-time constant.  The `: T`
   annotation is required (explicitness); the initialiser is any
   expression, folded to a literal by typecheck. *)
and parse_const_decl s ~is_pub =
  expect s Token.Const;
  let (name, name_pos) = expect_ident s ~what:"constant name after 'const'" in
  expect s Token.Colon;
  let ty = parse_type s in
  expect s Token.Eq;
  let value = parse_expr s in
  expect s Token.Semicolon;
  Ast.{ kname = name; kty = ty; kvalue = value;
        kis_pub = is_pub; kpos = name_pos }

(* `type Name<T...> = Type;` — pure alias.  Parsed as a top-level
   item (FP-1, design 2026-05-28).  Generic tparams optional; the
   target is any type annotation, including another alias (cycle is
   caught at resolve time, not parse time). *)
and parse_type_alias_decl s ~is_pub =
  expect s Token.Type;
  let (name, name_pos) =
    match advance s with
    | (Token.Ident n, p) -> (n, p)
    | (_, p) -> Error.raise_ p "expected type alias name after 'type'"
  in
  let tparams = parse_tparams s in
  (match advance s with
   | (Token.Eq, _) -> ()
   | (t, p) ->
       Error.failf p
         "expected '=' after 'type %s', got %s" name (Token.pp t));
  let target = parse_type s in
  (match peek s with
   | Token.Semicolon -> ignore (advance s)
   | t ->
       Error.failf (peek_pos s)
         "expected ';' after 'type %s = ...', got %s" name (Token.pp t));
  Ast.{ taname = name; tatparams = tparams; tatarget = target;
        tais_pub = is_pub; tapos = name_pos }

and parse_struct_decl s ~is_pub =
  expect s Token.Struct;
  let (name, name_pos) =
    match advance s with
    | (Token.Ident n, p) -> (n, p)
    | (_, p) -> Error.raise_ p "expected struct name after 'struct'"
  in
  let stparams = parse_tparams s in
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
  (match find_dup_key ~key:fst fields with
   | None -> ()
   | Some n ->
       Error.failf name_pos "duplicate field '%s' in struct '%s'" n name);
  Ast.{ sname = name; stparams; sfields = fields;
        spos = name_pos; sis_pub = is_pub;
        stier_hint = None; sis_debug = false; sderives = [];
        sis_move = false }

(* `enum Foo { A | B(int) | C { f: T, ... } }` — variants SEPARATED by `|`
   (no leading `|`); the same `|` is the bitor operator elsewhere, here
   unambiguous in type context.  Each variant is `Name` (unit),
   `Name(T1, T2, ...)` (tuple), or `Name { f: T, ... }` (struct).  Generic
   params on the enum head: `enum Option<T> { None | Some(T) }`. *)
and parse_enum_decl s ~is_pub =
  expect s Token.Enum;
  let (name, name_pos) =
    match advance s with
    | (Token.Ident n, p) -> (n, p)
    | (_, p) -> Error.raise_ p "expected enum name after 'enum'"
  in
  let etparams = parse_tparams s in
  expect s Token.LBrace;
  let parse_variant s =
    let (vname, vpos) = expect_ident s ~what:"variant name" in
    let vkind =
      match peek s with
      | Token.LParen ->
          ignore (advance s);
          let tys = parse_comma_list ~close:Token.RParen
                      ~item:parse_type s in
          Ast.VTuple tys
      | Token.LBrace ->
          ignore (advance s);
          let parse_field s =
            let (fname, _) = expect_ident s ~what:"field name in variant" in
            expect s Token.Colon;
            (fname, parse_type s)
          in
          let fields = parse_comma_list ~close:Token.RBrace
                         ~item:parse_field s in
          Ast.VStruct fields
      | _ -> Ast.VUnit
    in
    Ast.{ vname; vkind; vpos }
  in
  let rec loop acc =
    let acc = parse_variant s :: acc in
    match peek s with
    | Token.Pipe -> ignore (advance s); loop acc
    | Token.RBrace -> ignore (advance s); List.rev acc
    | Token.Eof ->
        Error.raise_ s.last_pos "unexpected end of file inside 'enum'"
    | t ->
        Error.failf (peek_pos s)
          "expected '|' (next variant) or '}' after enum variant, got %s"
          (Token.pp t)
  in
  let variants =
    match peek s with
    | Token.RBrace ->
        Error.failf name_pos "enum '%s' must declare at least one variant" name
    | _ -> loop []
  in
  if variants = [] then
    Error.failf name_pos "enum '%s' must declare at least one variant" name;
  let rec check_dups = function
    | [] -> ()
    | (v : Ast.enum_variant) :: rest ->
        if List.exists (fun (w : Ast.enum_variant) -> w.vname = v.vname) rest
        then
          Error.failf v.vpos
            "duplicate variant '%s' in enum '%s'" v.vname name;
        check_dups rest
  in
  check_dups variants;
  Ast.{ ename = name; etparams; evariants = variants;
        epos = name_pos; eis_pub = is_pub;
        etier_hint = None; emust_use = false; eis_debug = false;
        ederives = [] }

(* `impl <Path> { fn ... fn ... }` — methods get registered against the
   target struct, not into the surrounding scope.  Each method is parsed
   like a free-standing function (with optional `pub`), with a per-impl
   duplicate-name check.  Cross-block dup checks against earlier `impl`
   blocks for the same target happen later in typecheck. *)
and parse_impl_block s =
  expect s Token.Impl;
  let pos = s.last_pos in
  (* `impl<A, B> Pair<A, B> { ... }` — the `<A, B>` declares the impl's
     type parameters; the target carries them as arguments.  `impl Foo`
     (mono) parses with no tparams and no target args.
     `impl Trait for Foo { ... }` — trait impl: the first type is the
     trait, `for` introduces the target. *)
  let itparams = parse_tparams s in
  let first_ty = parse_type s in
  (* Disambiguate `impl Foo { ... }` (inherent) from `impl Trait for Foo
     { ... }` (trait impl) by the presence of `for`. *)
  let (itrait, target_ty) =
    if peek s = Token.For then begin
      ignore (advance s);
      let trait_path =
        match first_ty with
        | Ast.TyStruct { path; args = [] } -> path
        | Ast.TyStruct { path; _ } ->
            Error.failf pos
              "trait '%s' in 'impl ... for' must not carry type arguments \
               (generic traits are not supported)" (String.concat "::" path)
        | _ -> Error.failf pos "'impl' trait must be a named trait"
      in
      (Some trait_path, parse_type s)
    end else
      (None, first_ty)
  in
  let (target_path, target_args) =
    match target_ty with
    | Ast.TyStruct { path; args } -> (path, args)
    | _ ->
        Error.failf pos "'impl' target must be a (possibly generic) struct"
  in
  (if itparams <> [] then begin
     let expected =
       List.map (fun n -> Ast.TyStruct { path = [ n ]; args = [] }) itparams
     in
     if target_args <> expected then
       Error.failf pos
         "'impl<%s>' target must be '%s<%s>' — the type arguments must \
          match the declared parameters in order"
         (String.concat ", " itparams)
         (String.concat "::" target_path)
         (String.concat ", " itparams)
   end
   else if target_args <> [] then
     Error.failf pos
       "generic 'impl' needs its parameters declared after 'impl', e.g. \
        'impl<T> %s<T>'" (String.concat "::" target_path));
  expect s Token.LBrace;
  let rec loop seen acc assoc =
    match peek s with
    | Token.RBrace -> ignore (advance s); (List.rev acc, List.rev assoc)
    | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
    | Token.Type ->
        (* Associated-type binding: `type Item = Concrete;`. *)
        ignore (advance s);
        let (an, _) = expect_ident s ~what:"associated type name after 'type'" in
        expect s Token.Eq;
        let ty = parse_type s in
        expect s Token.Semicolon;
        loop seen acc ((an, ty) :: assoc)
    | _ ->
        let is_pub = peek s = Token.Pub in
        if is_pub then ignore (advance s);
        (match peek s with
         | Token.Fn ->
             let (name, fn) = parse_function s seen ~is_pub in
             loop (name :: seen) (fn :: acc) assoc
         | t ->
             Error.failf (peek_pos s)
               "expected 'fn' or 'type' inside 'impl' block, got %s"
               (Token.pp t))
  in
  let (methods, iassoc) = loop [] [] [] in
  (* Substitute the bare-`self` placeholder (TySelf, possibly under a
     pointer) with the impl target type, so downstream sees an explicit
     `self: Pair<A, B>` / `self: *Pair<A, B>`. *)
  let rec replace_self = function
    | Ast.TySelf -> target_ty
    | Ast.TyPtr t -> Ast.TyPtr (replace_self t)
    | Ast.TyConstPtr t -> Ast.TyConstPtr (replace_self t)
    | other -> other
  in
  let methods =
    List.map (fun (m : Ast.func) ->
      (* Trait-impl methods follow the trait's visibility (they're part of
         its public API), so an explicit per-method `pub` is unnecessary
         and they're never private.  Inherent-impl methods keep their own
         `pub`. *)
      let is_pub = m.Ast.is_pub || itrait <> None in
      { m with Ast.is_pub;
        Ast.params =
          List.map (fun (p : Ast.param) ->
            { p with Ast.pty = replace_self p.pty }) m.params })
      methods
  in
  Ast.{ itparams; itrait; iassoc; itarget = target_path; iitems = methods;
        ipos = pos }

(* `trait Name { fn m(self, ...) -> R; ... }` — method signatures.  A
   method ending with `;` is required; one with a `{ ... }` body provides
   a default an `impl` may omit.  `self` stays `TySelf` (= `Self`);
   conformance against a concrete `impl Trait for Foo` substitutes the
   target type. *)
and parse_trait s ~is_pub =
  expect s Token.Trait;
  let (name, name_pos) = expect_ident s ~what:"trait name after 'trait'" in
  (* Optional supertraits: `trait B: A + C { ... }`. *)
  let supers =
    if peek s = Token.Colon then begin
      ignore (advance s);
      let rec read acc =
        let path = parse_path s ~what:"supertrait name" in
        let acc = path :: acc in
        if peek s = Token.Plus then (ignore (advance s); read acc)
        else List.rev acc
      in
      read []
    end else []
  in
  expect s Token.LBrace;
  let rec loop seen acc defaults assoc =
    match peek s with
    | Token.RBrace ->
        ignore (advance s);
        (List.rev acc, List.rev defaults, List.rev assoc)
    | Token.Eof -> Error.raise_ s.last_pos "unexpected end of file, expected '}'"
    | Token.Type ->
        (* Associated type declaration: `type Item;` (no default in MVP). *)
        ignore (advance s);
        let (an, _) = expect_ident s ~what:"associated type name after 'type'" in
        expect s Token.Semicolon;
        loop seen acc defaults (an :: assoc)
    | Token.Fn ->
        let (m, is_default) = parse_trait_method s seen in
        let defaults = if is_default then m.Ast.name :: defaults else defaults in
        loop (m.Ast.name :: seen) (m :: acc) defaults assoc
    | t ->
        Error.failf (peek_pos s)
          "expected 'fn' signature or 'type' inside 'trait' block, got %s"
          (Token.pp t)
  in
  let (methods, defaults, assoc) = loop [] [] [] [] in
  Ast.{ trname = name; trassoc = assoc; trsupers = supers; trmethods = methods;
        trdefaults = defaults; trpos = name_pos; tris_pub = is_pub }

(* Returns (method, is_default).  Required form ends with `;`; default
   form has a `{ ... }` body. *)
and parse_trait_method s seen =
  expect s Token.Fn;
  let (name, name_pos) = expect_ident s ~what:"method name after 'fn'" in
  if List.mem name seen then
    Error.failf name_pos "method '%s' already declared in this trait" name;
  if peek s = Token.Lt then
    Error.failf name_pos
      "generic trait methods are not supported (method '%s')" name;
  let params = parse_params s in
  let ret_ty = parse_ret_ty s in
  let (body, is_default) =
    match peek s with
    | Token.Semicolon -> ignore (advance s); ([], false)
    | Token.LBrace -> (parse_block s, true)
    | t ->
        Error.failf (peek_pos s)
          "expected ';' or '{ ... }' after trait method '%s', got %s"
          name (Token.pp t)
  in
  (Ast.{ name; c_name = name; tparams = []; tbounds = []; params; ret_ty;
         body; is_pub = true; is_extern = false; is_variadic = false;
         tier_hint = None; amiga_lib = None; must_use = false;
         pos = name_pos },
   is_default)

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
