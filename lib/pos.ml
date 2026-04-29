type t = { line : int; col : int; file : string }

let to_string p = Printf.sprintf "%s:%d:%d" p.file p.line p.col

(* Synthetic position used when no real source location is available
   (e.g. internal compiler errors generated during AST post-processing). *)
let zero = { line = 1; col = 1; file = "<unknown>" }
