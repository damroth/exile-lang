(* Compile-time error API.  All typecheck/parser/lexer/loader failure
   paths funnel through here so the CLI has a single exception to catch
   and format.  Use [failf] for printf-style messages; [raise_] for
   pre-formatted strings.  Neither returns. *)

exception Compile_error of { pos : Pos.t; msg : string }

val raise_ : Pos.t -> string -> 'a
val failf : Pos.t -> ('a, unit, string, 'b) format4 -> 'a
