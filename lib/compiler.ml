let compile src =
  src
  |> Lexer.tokenize
  |> Parser.parse_program
  |> Codegen.gen_program
