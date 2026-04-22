let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_alpha c || (c >= '0' && c <= '9')
let is_digit c = c >= '0' && c <= '9'

let keyword_or_ident = function
  | "fn" -> Token.Fn
  | "let" -> Token.Let
  | "return" -> Token.Return
  | "if" -> Token.If
  | "else" -> Token.Else
  | "while" -> Token.While
  | "true" -> Token.True
  | "false" -> Token.False
  | s -> Token.Ident s

let tokenize src =
  let len = String.length src in
  let buf = Buffer.create 16 in
  let ln = ref 1 and co = ref 1 in
  let here () = Pos.{ line = !ln; col = !co } in
  let adv c = if c = '\n' then (incr ln; co := 1) else incr co in
  let rec loop i acc =
    if i >= len then List.rev ((Token.Eof, here ()) :: acc)
    else
      let c = src.[i] in
      let p = here () in
      adv c;
      match c with
      | ' ' | '\t' | '\r' | '\n' -> loop (i + 1) acc
      | '(' -> loop (i + 1) ((Token.LParen, p) :: acc)
      | ')' -> loop (i + 1) ((Token.RParen, p) :: acc)
      | '{' -> loop (i + 1) ((Token.LBrace, p) :: acc)
      | '}' -> loop (i + 1) ((Token.RBrace, p) :: acc)
      | ';' -> loop (i + 1) ((Token.Semicolon, p) :: acc)
      | ',' -> loop (i + 1) ((Token.Comma, p) :: acc)
      | ':' -> loop (i + 1) ((Token.Colon, p) :: acc)
      | '+' -> loop (i + 1) ((Token.Plus, p) :: acc)
      | '*' -> loop (i + 1) ((Token.Star, p) :: acc)
      | '/' when i + 1 < len && src.[i + 1] = '/' ->
          let rec skip j =
            if j >= len || src.[j] = '\n' then j else (adv src.[j]; skip (j + 1))
          in
          loop (skip (i + 1)) acc
      | '/' when i + 1 < len && src.[i + 1] = '*' ->
          adv '*';
          let rec skip j =
            if j + 1 >= len then Error.raise_ p "unterminated block comment"
            else if src.[j] = '*' && src.[j + 1] = '/' then (adv '*'; adv '/'; j + 2)
            else (adv src.[j]; skip (j + 1))
          in
          loop (skip (i + 2)) acc
      | '/' -> loop (i + 1) ((Token.Slash, p) :: acc)
      | '-' when i + 1 < len && src.[i + 1] = '>' ->
          adv '>'; loop (i + 2) ((Token.Arrow, p) :: acc)
      | '-' -> loop (i + 1) ((Token.Minus, p) :: acc)
      | '<' when i + 1 < len && src.[i + 1] = '=' ->
          adv '='; loop (i + 2) ((Token.LtEq, p) :: acc)
      | '<' -> loop (i + 1) ((Token.Lt, p) :: acc)
      | '>' when i + 1 < len && src.[i + 1] = '=' ->
          adv '='; loop (i + 2) ((Token.GtEq, p) :: acc)
      | '>' -> loop (i + 1) ((Token.Gt, p) :: acc)
      | '=' when i + 1 < len && src.[i + 1] = '=' ->
          adv '='; loop (i + 2) ((Token.EqEq, p) :: acc)
      | '=' -> loop (i + 1) ((Token.Eq, p) :: acc)
      | '!' when i + 1 < len && src.[i + 1] = '=' ->
          adv '='; loop (i + 2) ((Token.NotEq, p) :: acc)
      | '!' -> Error.failf p "unexpected '!'; did you mean '!='"
      | '"' ->
          Buffer.clear buf;
          let rec str j =
            if j >= len then Error.raise_ p "unterminated string literal"
            else
              let sc = src.[j] in
              match sc with
              | '"' -> adv sc; j + 1
              | '\\' when j + 1 < len ->
                  let esc_pos = here () in
                  adv sc;
                  let ec = src.[j + 1] in
                  let esc =
                    match ec with
                    | 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r'
                    | 'b' -> '\b' | '0' -> '\000'
                    | '\\' -> '\\' | '"' -> '"'
                    | c -> Error.failf esc_pos "unknown escape \\%c" c
                  in
                  adv ec;
                  Buffer.add_char buf esc;
                  str (j + 2)
              | c -> adv c; Buffer.add_char buf c; str (j + 1)
          in
          let next = str (i + 1) in
          loop next ((Token.String (Buffer.contents buf), p) :: acc)
      | c when is_digit c ->
          let rec scan j =
            if j < len && is_digit src.[j] then (adv src.[j]; scan (j + 1)) else j
          in
          let stop = scan (i + 1) in
          let n = int_of_string (String.sub src i (stop - i)) in
          loop stop ((Token.Int n, p) :: acc)
      | c when is_alpha c ->
          let rec scan j =
            if j < len && is_alnum src.[j] then (adv src.[j]; scan (j + 1)) else j
          in
          let stop = scan (i + 1) in
          loop stop ((keyword_or_ident (String.sub src i (stop - i)), p) :: acc)
      | c -> Error.failf p "unexpected character %C" c
  in
  loop 0 []
