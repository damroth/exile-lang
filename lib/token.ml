type t =
  | Fn
  | Let
  | Mut
  | Return
  | If
  | Else
  | While
  | Loop
  | Break
  | Continue
  | For
  | In
  | Mod
  | Pub
  | Use
  | As
  | Defer
  | Struct
  | Impl
  | Trait
  | Enum
  | Match
  | FatArrow
  | Pipe
  | PipePipe
  | PipeGt
  | AmpAmp
  | Dot
  | DotDot
  | DotDotEq
  | Amp
  | New
  | Null
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Semicolon
  | Comma
  | Colon
  | DoubleColon
  | Arrow
  | Eq
  | EqEq
  | NotEq
  | Bang
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Plus
  | PlusPlus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Tilde
  | Shl
  | Shr
  | True
  | False
  | Question
  | Orelse
  | Try
  | Extern
  | Ellipsis
  | At
  | Type
  | Const
  | Var
  | SizeOf
  | Ident of string
  | Int of int
  | String of string
  | Eof

let pp = function
  | Fn -> "'fn'" | Let -> "'let'" | Mut -> "'mut'" | Return -> "'return'"
  | If -> "'if'" | Else -> "'else'" | While -> "'while'"
  | Loop -> "'loop'" | Break -> "'break'" | Continue -> "'continue'"
  | For -> "'for'" | In -> "'in'"
  | Mod -> "'mod'" | Pub -> "'pub'" | Use -> "'use'" | As -> "'as'"
  | Defer -> "'defer'"
  | Struct -> "'struct'" | Impl -> "'impl'" | Trait -> "'trait'"
  | Enum -> "'enum'" | Match -> "'match'" | FatArrow -> "'=>'"
  | Pipe -> "'|'" | PipePipe -> "'||'" | PipeGt -> "'|>'" | AmpAmp -> "'&&'"
  | Dot -> "'.'" | DotDot -> "'..'" | DotDotEq -> "'..='"
  | Amp -> "'&'" | New -> "'new'" | Null -> "'null'"
  | LParen -> "'('" | RParen -> "')'"
  | LBrace -> "'{'" | RBrace -> "'}'"
  | LBracket -> "'['" | RBracket -> "']'"
  | Semicolon -> "';'" | Comma -> "','" | Colon -> "':'"
  | DoubleColon -> "'::'"
  | Arrow -> "'->'" | Eq -> "'='" | EqEq -> "'=='" | NotEq -> "'!='"
  | Bang -> "'!'"
  | Lt -> "'<'" | Gt -> "'>'" | LtEq -> "'<='" | GtEq -> "'>='"
  | Plus -> "'+'" | PlusPlus -> "'++'"
  | Minus -> "'-'" | Star -> "'*'" | Slash -> "'/'"
  | Percent -> "'%'" | Caret -> "'^'" | Tilde -> "'~'"
  | Shl -> "'<<'" | Shr -> "'>>'"
  | True -> "'true'" | False -> "'false'"
  | Question -> "'?'" | Orelse -> "'orelse'" | Try -> "'try'"
  | Extern -> "'extern'" | Ellipsis -> "'...'" | At -> "'@'"
  | Type -> "'type'" | Const -> "'const'" | Var -> "'var'"
  | SizeOf -> "'size_of'"
  | Ident s -> Printf.sprintf "identifier '%s'" s
  | Int n -> Printf.sprintf "integer %d" n
  | String s -> Printf.sprintf "string \"%s\"" s
  | Eof -> "end of file"
