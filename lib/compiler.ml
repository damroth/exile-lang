(* Compile a single source string with no filesystem access.  Used by the
   test harness; rejects `use` declarations because they need a file path
   to resolve. *)
let compile src =
  src
  |> Lexer.tokenize
  |> Parser.parse_program
  |> Codegen.gen_program

(* Compile starting from an entry .exl file.  Resolves `use` declarations
   relative to the entry file (and subsequent imports relative to wherever
   they appear). *)
let compile_file path =
  Loader.load path |> Codegen.gen_program
