(* Parser interface.  Consumes a token stream from [Lexer.tokenize] and
   produces a parsed [Ast.program] (a list of top-level items).
   Errors raise [Error.Compile_error] with a position pointing at the
   first unexpected token. *)

val parse_program : (Token.t * Pos.t) list -> Ast.program
