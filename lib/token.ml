type t =
  | Fn
  | Let
  | Return
  | If
  | Else
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon
  | Comma
  | Colon
  | Arrow
  | Eq
  | EqEq
  | NotEq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Plus
  | Minus
  | Star
  | Slash
  | Ident of string
  | Int of int
  | String of string
  | Eof

let pp = function
  | Fn -> "'fn'" | Let -> "'let'" | Return -> "'return'"
  | If -> "'if'" | Else -> "'else'"
  | LParen -> "'('" | RParen -> "')'"
  | LBrace -> "'{'" | RBrace -> "'}'"
  | Semicolon -> "';'" | Comma -> "','" | Colon -> "':'"
  | Arrow -> "'->'" | Eq -> "'='" | EqEq -> "'=='" | NotEq -> "'!='"
  | Lt -> "'<'" | Gt -> "'>'" | LtEq -> "'<='" | GtEq -> "'>='"
  | Plus -> "'+'" | Minus -> "'-'" | Star -> "'*'" | Slash -> "'/'"
  | Ident s -> Printf.sprintf "identifier '%s'" s
  | Int n -> Printf.sprintf "integer %d" n
  | String s -> Printf.sprintf "string \"%s\"" s
  | Eof -> "end of file"
