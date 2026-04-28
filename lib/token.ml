type t =
  | Fn
  | Let
  | Return
  | If
  | Else
  | While
  | Mod
  | Pub
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon
  | Comma
  | Colon
  | DoubleColon
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
  | True
  | False
  | Ident of string
  | Int of int
  | String of string
  | Eof

let pp = function
  | Fn -> "'fn'" | Let -> "'let'" | Return -> "'return'"
  | If -> "'if'" | Else -> "'else'" | While -> "'while'"
  | Mod -> "'mod'" | Pub -> "'pub'"
  | LParen -> "'('" | RParen -> "')'"
  | LBrace -> "'{'" | RBrace -> "'}'"
  | Semicolon -> "';'" | Comma -> "','" | Colon -> "':'"
  | DoubleColon -> "'::'"
  | Arrow -> "'->'" | Eq -> "'='" | EqEq -> "'=='" | NotEq -> "'!='"
  | Lt -> "'<'" | Gt -> "'>'" | LtEq -> "'<='" | GtEq -> "'>='"
  | Plus -> "'+'" | Minus -> "'-'" | Star -> "'*'" | Slash -> "'/'"
  | True -> "'true'" | False -> "'false'"
  | Ident s -> Printf.sprintf "identifier '%s'" s
  | Int n -> Printf.sprintf "integer %d" n
  | String s -> Printf.sprintf "string \"%s\"" s
  | Eof -> "end of file"
