exception Compile_error of { pos : Pos.t; msg : string }

let raise_ pos msg = raise (Compile_error { pos; msg })
let failf pos fmt = Printf.ksprintf (raise_ pos) fmt
