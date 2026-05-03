(* String-only entry point used by tests; `use` is rejected. *)
let compile src =
  src
  |> Lexer.tokenize ~file:"<input>"
  |> Parser.parse_program
  |> Typecheck.check_program
  |> Codegen.gen_program

(* File-based entry point that resolves `use` via the loader. *)
let compile_file path =
  Loader.load path
  |> Typecheck.check_program
  |> Codegen.gen_program
