type target = Target_c | Target_host | Target_amiga

let usage () =
  prerr_endline
    "usage: exilc [--target c|host|amiga] [--profile core|standard|full] \
     [-o <output>] [--c-out <path>] [--link <c-stub>]... [--annotate] \
     [--freestanding] [--bloat-report] [--perf-report[=json]] \
     [--show-cc-warnings] <file.exl>";
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

(* Differential-harness dumps (DR self-host bring-up Faza −1).  Only
   one emit-* is honoured at a time; the pipeline short-circuits as
   soon as the requested form is produced and writes it to `-o` (or
   stdout when no `-o` is set). *)
type emit_kind = EmitTokens | EmitAst | EmitTypedIr

type perf_report_fmt = PerfHuman | PerfJson

type args = {
  target : target;
  profile : Exile_lang.Profile.t;
  output : string option;
  c_out : string option;
  link_files : string list;
  annotate : bool;
  freestanding : bool;
  bloat_report : bool;
  perf_report : perf_report_fmt option;
  show_cc_warnings : bool;
  emit : emit_kind option;
  emit_user_only : bool;
  input : string;
}

(* Default target is `host` — the common case is "build me a binary I
   can run".  `--target c` opts out for pure transpile (e.g. inspecting
   the generated source, or feeding it into a separate toolchain). *)
let parse_args argv =
  let target = ref Target_host in
  let profile = ref None in
  let output = ref None in
  let c_out = ref None in
  let link_files = ref [] in
  let input = ref None in
  let annotate = ref false in
  let freestanding = ref false in
  let bloat_report = ref false in
  let perf_report = ref None in
  let show_cc_warnings = ref false in
  let emit = ref None in
  let emit_user_only = ref false in
  let set_emit k =
    if !emit <> None then begin
      Printf.eprintf
        "only one of --emit-tokens / --emit-ast / --emit-typed-ir at a time\n";
      exit 1
    end;
    emit := Some k
  in
  let rec loop = function
    | [] -> ()
    | "--target" :: t :: rest -> target := parse_target t; loop rest
    | "--profile" :: p :: rest -> profile := Some (parse_profile p); loop rest
    | "-o" :: o :: rest -> output := Some o; loop rest
    | "--c-out" :: p :: rest -> c_out := Some p; loop rest
    | "--link" :: p :: rest -> link_files := p :: !link_files; loop rest
    | "--annotate" :: rest -> annotate := true; loop rest
    | "--freestanding" :: rest -> freestanding := true; loop rest
    | "--bloat-report" :: rest -> bloat_report := true; loop rest
    | "--perf-report" :: rest -> perf_report := Some PerfHuman; loop rest
    | "--perf-report=json" :: rest -> perf_report := Some PerfJson; loop rest
    | "--perf-report=human" :: rest -> perf_report := Some PerfHuman; loop rest
    | "--show-cc-warnings" :: rest -> show_cc_warnings := true; loop rest
    | "--emit-tokens" :: rest -> set_emit EmitTokens; loop rest
    | "--emit-ast" :: rest -> set_emit EmitAst; loop rest
    | "--emit-typed-ir" :: rest -> set_emit EmitTypedIr; loop rest
    | "--user-only" :: rest -> emit_user_only := true; loop rest
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
        freestanding = !freestanding;
        bloat_report = !bloat_report;
        perf_report = !perf_report;
        show_cc_warnings = !show_cc_warnings;
        emit = !emit;
        emit_user_only = !emit_user_only;
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

(* Mask the cc warnings that exilc's own Lint already covers, so a
   single finding doesn't get reported twice in two wordings.
   `--show-cc-warnings` lifts the mask for compiler dev. *)
let cc_warn_suppress show_cc_warnings =
  if show_cc_warnings then ""
  else "-Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-function"

let compile_host ~show_cc_warnings ~profile c_path link_files input output =
  run_cmd (Printf.sprintf "cc -ansi -pedantic -Wall %s %s -o %s %s %s"
    (cc_warn_suppress show_cc_warnings)
    (include_flag input)
    (Filename.quote output) (Filename.quote c_path) (quote_paths link_files));
  Printf.printf "built host binary: %s [profile=%s, target=host]\n"
    output (Exile_lang.Profile.to_string profile)

let compile_amiga ~show_cc_warnings ~profile c_path link_files input output =
  let gcc = amiga_gcc () in
  (* `-lm` pulls in Bebbo's soft-float thunks (`__adddf3`, `__muldf3`,
     `__addsf3`, ...) — bare 68000 has no FPU, every `f32`/`f64` op
     lowers to a math-library call.  Bebbo packages these in libm
     (not libgcc — libgcc here only carries soft-int helpers).
     `-noixemul` keeps the libnix POSIX shim out; libm is independent
     and links cleanly under that profile. *)
  run_cmd (Printf.sprintf "%s -noixemul %s %s -o %s %s %s -lm"
    (Filename.quote gcc) (cc_warn_suppress show_cc_warnings)
    (include_flag input)
    (Filename.quote output) (Filename.quote c_path)
    (quote_paths link_files));
  Printf.printf "built amiga binary: %s [profile=%s, target=amiga]\n"
    output (Exile_lang.Profile.to_string profile)

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

let print_perf_report fmt tp =
  let bloat = Exile_lang.Codegen.last_bloat () in
  let report = Exile_lang.Perf_report.collect tp bloat in
  let text = match fmt with
    | PerfHuman -> Exile_lang.Perf_report.to_human report
    | PerfJson -> Exile_lang.Perf_report.to_json report
  in
  output_string stderr text;
  output_char stderr '\n'

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

let write_dump output content =
  match output with
  | None -> print_string content
  | Some path ->
      ensure_dir path;
      Out_channel.with_open_text path (fun oc ->
        Out_channel.output_string oc content)

(* Drive the requested differential dump and short-circuit before
   codegen.  The dumps are golden-input for the future exile port —
   each pipeline stage runs and we emit at exactly the spot the port
   targets: tokens after lex, AST after parse + loader, typed IR
   after typecheck + lift (the move / escape / lint passes do not
   change the IR, so post-typecheck is the right anchor). *)
let run_emit (a : args) (kind : emit_kind) =
  let file = a.input in
  match kind with
  | EmitTokens ->
      let src = In_channel.with_open_text file In_channel.input_all in
      let toks = Exile_lang.Lexer.tokenize ~file src in
      write_dump a.output
        (Exile_lang.Dump.dump_tokens ~file toks)
  | EmitAst ->
      let program = Exile_lang.Loader.load file in
      write_dump a.output
        (Exile_lang.Dump.dump_ast ~file program)
  | EmitTypedIr ->
      let tp =
        Exile_lang.Loader.load file
        |> Exile_lang.Typecheck.check_program
      in
      write_dump a.output
        (Exile_lang.Dump.dump_typed_ir
           ~file ~user_only:a.emit_user_only tp)

let () =
  Printexc.record_backtrace true;
  let a = parse_args (List.tl (Array.to_list Sys.argv)) in
  try
    (* Emit-* runs the relevant prefix of the pipeline and exits.
       It is mutually exclusive with codegen / cc — the harness is
       a read-only diagnostic. *)
    (match a.emit with
     | Some kind -> run_emit a kind; exit 0
     | None -> ());
    let (tp_opt, c_code) =
      if a.perf_report <> None then
        let (tp, c) =
          Exile_lang.Compiler.compile_file_capture
            ~annotate:a.annotate ~freestanding:a.freestanding
            ~profile:a.profile a.input
        in (Some tp, c)
      else
        (None,
         Exile_lang.Compiler.compile_file
           ~annotate:a.annotate ~freestanding:a.freestanding
           ~profile:a.profile a.input)
    in
    let c_path =
      match a.c_out with
      | Some p -> p
      | None -> Filename.remove_extension a.input ^ ".c"
    in
    ensure_dir c_path;
    Out_channel.with_open_text c_path (fun oc ->
        Out_channel.output_string oc c_code);
    let target_name = match a.target with
      | Target_c -> "c"
      | Target_host -> "host"
      | Target_amiga -> "amiga"
    in
    if a.bloat_report then print_bloat_report ();
    (match a.perf_report, tp_opt with
     | Some fmt, Some tp -> print_perf_report fmt tp
     | _ -> ());
    (* Success line goes out only after cc succeeds (it `exit 1`s on
       failure).  target=c stops at the transpile and reports `wrote
       ...`; host/amiga delegate the success message to
       compile_host/_amiga, whose `built ... binary` line covers both
       the .c emission and the cc build. *)
    match a.target with
    | Target_c ->
        Printf.printf "wrote %s [profile=%s, target=%s]\n"
          c_path (Exile_lang.Profile.to_string a.profile) target_name
    | Target_host ->
        let out = Option.value a.output ~default:(default_output_for a.input) in
        ensure_dir out;
        compile_host ~show_cc_warnings:a.show_cc_warnings ~profile:a.profile
          c_path a.link_files a.input out
    | Target_amiga ->
        let out = Option.value a.output ~default:(default_output_for a.input) in
        ensure_dir out;
        compile_amiga ~show_cc_warnings:a.show_cc_warnings ~profile:a.profile
          c_path a.link_files a.input out
  with
  | Exile_lang.Error.Compile_error { pos; msg } ->
      show_error pos msg;
      exit 1
  | e ->
      (* Anything that isn't a Compile_error is a broken internal
         invariant (our `failwith "internal: ..."`) or an unexpected
         exception (Assert_failure, Not_found, Match_failure, ...) — a
         compiler bug, not a diagnostic about the user's program.  Frame
         it as such so it isn't mistaken for a normal error, and never
         let a raw OCaml backtrace leak as the primary output.  Set
         EXILE_BACKTRACE=1 to see the trace while hacking on exilc. *)
      let backtrace = Printexc.get_backtrace () in
      let detail = match e with
        | Failure msg -> msg
        | e -> Printexc.to_string e
      in
      Printf.eprintf
        "internal compiler error: %s\n\
         this is a bug in exilc, not your program — please report it \
         (input: %s)\n"
        detail a.input;
      if Sys.getenv_opt "EXILE_BACKTRACE" <> None && backtrace <> "" then
        Printf.eprintf "%s" backtrace;
      exit 1