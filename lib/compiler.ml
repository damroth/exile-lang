(* Default profile when callers (mainly the OCaml test harness) don't
   pass one explicitly.  `Full` is the loose end of the spectrum, so
   tests don't get tier warnings unless they opt in.  bin/main.ml
   passes the user-selected profile. *)
let default_profile = Profile.Full

(* String-only entry point used by tests; `use` is rejected. *)
let compile ?(annotate = false) ?(profile = default_profile) src =
  let tp =
    src
    |> Lexer.tokenize ~file:"<input>"
    |> Parser.parse_program
    |> Typecheck.check_program
  in
  Lint.lint ~profile tp;
  Codegen.gen_program ~annotate tp

(* File-based entry point that resolves `use` via the loader. *)
let compile_file ?(annotate = false) ?(profile = default_profile) path =
  let tp =
    Loader.load path
    |> Typecheck.check_program
  in
  Lint.lint ~profile tp;
  Codegen.gen_program ~annotate tp
