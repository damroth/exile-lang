(* Loader interface.  Parses [entry_path] and recursively inlines every
   [use] declaration into a single flat [Ast.program].  Each resolved
   file is loaded at most once; cycles raise [Error.Compile_error]
   with a "circular import" diagnostic. *)

val load : string -> Ast.program
