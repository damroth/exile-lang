type target = Target_c | Target_host | Target_amiga

let usage () =
  prerr_endline "usage: exilc [--target c|host|amiga] [-o <output>] <file.exl>";
  exit 1

let show_error file src (pos : Exile_lang.Pos.t) msg =
  Printf.eprintf "%s:%d:%d: error: %s\n" file pos.line pos.col msg;
  let lines = String.split_on_char '\n' src in
  match List.nth_opt lines (pos.line - 1) with
  | Some line ->
      Printf.eprintf "%s\n%s^\n" line (String.make (max 0 (pos.col - 1)) ' ')
  | None -> ()

let parse_target = function
  | "c" -> Target_c
  | "host" -> Target_host
  | "amiga" -> Target_amiga
  | t ->
      Printf.eprintf "unknown target '%s' (expected: c, host, amiga)\n" t;
      exit 1

let parse_args argv =
  let target = ref Target_c in
  let output = ref None in
  let input = ref None in
  let rec loop = function
    | [] -> ()
    | "--target" :: t :: rest -> target := parse_target t; loop rest
    | "-o" :: o :: rest -> output := Some o; loop rest
    | "--help" :: _ | "-h" :: _ -> usage ()
    | f :: rest when String.length f > 0 && f.[0] <> '-' ->
        if !input <> None then begin
          Printf.eprintf "multiple input files given\n";
          exit 1
        end;
        input := Some f;
        loop rest
    | f :: _ ->
        Printf.eprintf "unknown flag: %s\n" f;
        exit 1
  in
  loop argv;
  match !input with
  | None -> usage ()
  | Some i -> (!target, !output, i)

let toolchain_path () =
  try Sys.getenv "EXILE_TOOLCHAIN"
  with Not_found -> Filename.concat (Sys.getcwd ()) "_build/toolchain"

let amiga_gcc () =
  let gcc = Filename.concat (toolchain_path ()) "bin/m68k-amigaos-gcc" in
  if not (Sys.file_exists gcc) then begin
    Printf.eprintf "amiga cross-compiler not found at: %s\n" gcc;
    Printf.eprintf "run 'make toolchain' to build it, or set EXILE_TOOLCHAIN to its prefix\n";
    exit 1
  end;
  gcc

let run_cmd cmd =
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "command failed: %s\n" cmd;
    exit 1
  end

let compile_host c_path output =
  run_cmd (Printf.sprintf "cc -ansi -pedantic -Wall -o %s %s"
    (Filename.quote output) (Filename.quote c_path));
  Printf.printf "built host binary: %s\n" output

let compile_amiga c_path output =
  let gcc = amiga_gcc () in
  run_cmd (Printf.sprintf "%s -noixemul -o %s %s"
    (Filename.quote gcc) (Filename.quote output) (Filename.quote c_path));
  Printf.printf "built amiga binary: %s\n" output

let default_output_for input =
  Filename.remove_extension input

let () =
  let (target, output, input) =
    parse_args (List.tl (Array.to_list Sys.argv))
  in
  let src = In_channel.with_open_text input In_channel.input_all in
  try
    let c_code = Exile_lang.Compiler.compile src in
    let c_path = Filename.remove_extension input ^ ".c" in
    Out_channel.with_open_text c_path (fun oc ->
        Out_channel.output_string oc c_code);
    Printf.printf "wrote %s\n" c_path;
    match target with
    | Target_c -> ()
    | Target_host ->
        let out = Option.value output ~default:(default_output_for input) in
        compile_host c_path out
    | Target_amiga ->
        let out = Option.value output ~default:(default_output_for input) in
        compile_amiga c_path out
  with
  | Exile_lang.Error.Compile_error { pos; msg } ->
      show_error input src pos msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "error: %s\n" msg;
      exit 1