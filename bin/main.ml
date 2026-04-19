let usage () =
  prerr_endline "usage: exile-lang <file.exl>";
  exit 1

let show_error src (pos : Exile_lang.Pos.t) msg =
  Printf.eprintf "%s:%d:%d: error: %s\n" "(input)" pos.line pos.col msg;
  let lines = String.split_on_char '\n' src in
  match List.nth_opt lines (pos.line - 1) with
  | Some line ->
      Printf.eprintf "%s\n%s^\n" line (String.make (max 0 (pos.col - 1)) ' ')
  | None -> ()

let () =
  match Array.to_list Sys.argv with
  | _ :: input :: _ ->
      let src = In_channel.with_open_text input In_channel.input_all in
      (try
         let c_code = Exile_lang.Compiler.compile src in
         let output = Filename.remove_extension input ^ ".c" in
         Out_channel.with_open_text output (fun oc ->
             Out_channel.output_string oc c_code);
         Printf.printf "wrote %s\n" output
       with
       | Exile_lang.Error.Compile_error { pos; msg } ->
           show_error src pos msg;
           exit 1
       | Failure msg ->
           Printf.eprintf "error: %s\n" msg;
           exit 1)
  | _ -> usage ()
