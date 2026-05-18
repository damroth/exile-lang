(* Lexer interface.  Single entry point: tokenize a source string into a
   list of (token, position) pairs terminated by [Token.Eof].  Errors
   (unterminated string, integer literal that doesn't fit a 63-bit
   OCaml int, stray characters) raise [Error.Compile_error]. *)

val tokenize : file:string -> string -> (Token.t * Pos.t) list
