type target = Target_c | Target_host | Target_amiga

let usage () =
  prerr_endline
    "usage: exilc [--target c|host|amiga] [--profile core|standard|full] \
     [-o <output>] [--c-out <path>] [--link <c-stub>]... [--annotate] \
     [--bloat-report] <file.exl>";
  exit 1

let show_error (pos : Exile_lang.Pos.t) msg =
  Printf.eprintf "%s:%d:%d: error: %s\n" pos.file pos.line pos.col msg;
  let src =
    try In_channel.with_open_text pos.file In_channel.input_all
    with _ -> ""
  in
  if src <> "" then
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

let parse_profile s =
  match Exile_lang.Profile.of_string s with
  | Some p -> p
  | None ->
      Printf.eprintf
        "unknown profile '%s' (expected: core, standard, full)\n" s;
      exit 1

(* Default profile per target — overridable via --profile.  host (modern
   dev) and target=c (backend-only) both default to full because there's
   no platform budget pressure; amiga defaults to standard since the
   typical AmigaOS app has a few-MB envelope. *)
let default_profile_for_target = function
  | Target_c | Target_host -> Exile_lang.Profile.Full
  | Target_amiga -> Exile_lang.Profile.Standard

type args = {
  target : target;
  profile : Exile_lang.Profile.t;
  output : string option;
  c_out : string option;
  link_files : string list;
  annotate : bool;
  bloat_report : bool;
  input : string;
}

let parse_args argv =
  let target = ref Target_c in
  let profile = ref None in
  let output = ref None in
  let c_out = ref None in
  let link_files = ref [] in
  let input = ref None in
  let annotate = ref false in
  let bloat_report = ref false in
  let rec loop = function
    | [] -> ()
    | "--target" :: t :: rest -> target := parse_target t; loop rest
    | "--profile" :: p :: rest -> profile := Some (parse_profile p); loop rest
    | "-o" :: o :: rest -> output := Some o; loop rest
    | "--c-out" :: p :: rest -> c_out := Some p; loop rest
    | "--link" :: p :: rest -> link_files := p :: !link_files; loop rest
    | "--annotate" :: rest -> annotate := true; loop rest
    | "--bloat-report" :: rest -> bloat_report := true; loop rest
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
  | Some i ->
      let profile =
        match !profile with
        | Some p -> p
        | None -> default_profile_for_target !target
      in
      { target = !target; profile;
        output = !output; c_out = !c_out;
        link_files = List.rev !link_files;
        annotate = !annotate;
        bloat_report = !bloat_report;
        input = i }

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

let quote_paths paths =
  String.concat " " (List.map Filename.quote paths)

(* Headers referenced by `@c_include("local.h")` are conventionally
   placed next to the .exl source.  Auto-add the source directory as
   an `-I` path so quoted-include resolution finds them.  External
   stubs supplied via `--link` are usually in the same dir. *)
let include_flag input =
  let dir = Filename.dirname input in
  if dir = "" || dir = "." then ""
  else Printf.sprintf "-I %s" (Filename.quote dir)

let compile_host c_path link_files input output =
  run_cmd (Printf.sprintf "cc -ansi -pedantic -Wall %s -o %s %s %s"
    (include_flag input)
    (Filename.quote output) (Filename.quote c_path) (quote_paths link_files));
  Printf.printf "built host binary: %s\n" output

let compile_amiga c_path link_files input output =
  let gcc = amiga_gcc () in
  run_cmd (Printf.sprintf "%s -noixemul %s -o %s %s %s"
    (Filename.quote gcc) (include_flag input)
    (Filename.quote output) (Filename.quote c_path)
    (quote_paths link_files));
  Printf.printf "built amiga binary: %s\n" output

let default_output_for input =
  Filename.remove_extension input

let ensure_dir path =
  let dir = Filename.dirname path in
  if dir <> "" && dir <> "." && not (Sys.file_exists dir) then
    let cmd = Printf.sprintf "mkdir -p %s" (Filename.quote dir) in
    if Sys.command cmd <> 0 then begin
      Printf.eprintf "failed to create directory: %s\n" dir;
      exit 1
    end

let print_bloat_report () =
  let entries = Exile_lang.Codegen.last_bloat () in
  let sorted =
    List.sort (fun (_, a) (_, b) -> compare b a) entries
  in
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 entries in
  let count = List.length entries in
  Printf.eprintf "\nbloat report (%d fns, %d B total):\n" count total;
  let top_n = 20 in
  let shown = ref 0 in
  List.iter (fun (name, bytes) ->
    if !shown < top_n then begin
      Printf.eprintf "  %6d B  %s\n" bytes name;
      incr shown
    end)
    sorted;
  if count > top_n then
    Printf.eprintf "  ... %d more\n" (count - top_n)

let () =
  let a = parse_args (List.tl (Array.to_list Sys.argv)) in
  try
    let c_code = Exile_lang.Compiler.compile_file ~annotate:a.annotate a.input in
    let c_path =
      match a.c_out with
      | Some p -> p
      | None -> Filename.remove_extension a.input ^ ".c"
    in
    ensure_dir c_path;
    Out_channel.with_open_text c_path (fun oc ->
        Out_channel.output_string oc c_code);
    Printf.printf "wrote %s [profile=%s]\n"
      c_path (Exile_lang.Profile.to_string a.profile);
    if a.bloat_report then print_bloat_report ();
    match a.target with
    | Target_c -> ()
    | Target_host ->
        let out = Option.value a.output ~default:(default_output_for a.input) in
        ensure_dir out;
        compile_host c_path a.link_files a.input out
    | Target_amiga ->
        let out = Option.value a.output ~default:(default_output_for a.input) in
        ensure_dir out;
        compile_amiga c_path a.link_files a.input out
  with
  | Exile_lang.Error.Compile_error { pos; msg } ->
      show_error pos msg;
      exit 1
  | Failure msg ->
      Printf.eprintf "error: %s\n" msg;
      exit 1