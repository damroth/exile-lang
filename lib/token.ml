type t =
  | Fn
  | Let
  | Return
  | If
  | Else
  | While
  | Mod
  | Pub
  | Use
  | As
  | Defer
  | Struct
  | Impl
  | Enum
  | Match
  | FatArrow
  | Pipe
  | Dot
  | DotDot
  | Amp
  | New
  | Null
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
  | Question
  | Orelse
  | Try
  | Ident of string
  | Int of int
  | String of string
  | Eof

let pp = function
  | Fn -> "'fn'" | Let -> "'let'" | Return -> "'return'"
  | If -> "'if'" | Else -> "'else'" | While -> "'while'"
  | Mod -> "'mod'" | Pub -> "'pub'" | Use -> "'use'" | As -> "'as'"
  | Defer -> "'defer'"
  | Struct -> "'struct'" | Impl -> "'impl'"
  | Enum -> "'enum'" | Match -> "'match'" | FatArrow -> "'=>'"
  | Pipe -> "'|'"
  | Dot -> "'.'" | DotDot -> "'..'"
  | Amp -> "'&'" | New -> "'new'" | Null -> "'null'"
  | LParen -> "'('" | RParen -> "')'"
  | LBrace -> "'{'" | RBrace -> "'}'"
  | Semicolon -> "';'" | Comma -> "','" | Colon -> "':'"
  | DoubleColon -> "'::'"
  | Arrow -> "'->'" | Eq -> "'='" | EqEq -> "'=='" | NotEq -> "'!='"
  | Lt -> "'<'" | Gt -> "'>'" | LtEq -> "'<='" | GtEq -> "'>='"
  | Plus -> "'+'" | Minus -> "'-'" | Star -> "'*'" | Slash -> "'/'"
  | True -> "'true'" | False -> "'false'"
  | Question -> "'?'" | Orelse -> "'orelse'" | Try -> "'try'"
  | Ident s -> Printf.sprintf "identifier '%s'" s
  | Int n -> Printf.sprintf "integer %d" n
  | String s -> Printf.sprintf "string \"%s\"" s
  | Eof -> "end of file"
