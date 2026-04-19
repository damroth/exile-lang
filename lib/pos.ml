type t = { line : int; col : int }

let to_string p = Printf.sprintf "line %d, col %d" p.line p.col

let zero = { line = 1; col = 1 }
