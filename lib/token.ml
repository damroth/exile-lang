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
  | View
  | With
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
  | Own           (* DR-030 Faza-1a: `own *T` owner-sigil pointer type *)
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
  | Float of float * bool   (* value, is_f32 (true = `f32` suffix, false = `f64`
                                or bare double — bare `3.14` defaults to f64) *)
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
  | View -> "'view'"
  | With -> "'with'"
  | Enum -> "'enum'" | Match -> "'match'" | FatArrow -> "'=>'"
  | Pipe -> "'|'" | PipePipe -> "'||'" | PipeGt -> "'|>'" | AmpAmp -> "'&&'"
  | Dot -> "'.'" | DotDot -> "'..'" | DotDotEq -> "'..='"
  | Amp -> "'&'" | New -> "'new'" | Own -> "'own'" | Null -> "'null'"
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
  | Float (f, is32) ->
      Printf.sprintf "float %g%s" f (if is32 then "f32" else "")
  | String s -> Printf.sprintf "string \"%s\"" s
  | Eof -> "end of file"
