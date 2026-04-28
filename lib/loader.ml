(* Multi-file loader.  Walks the entry file's AST, replaces each `use foo;`
   item with a top-level `mod foo { ... }` whose contents come from
   `<dir>/foo.exl` next to the importing file.  Each file is loaded at most
   once; cycles are rejected.

   MVP scope: single-segment `use foo;` only, sibling files in the same
   directory.  Subdirectory hierarchies (`foo/mod.exl`, `foo/bar.exl`) and
   multi-segment paths are follow-up work. *)

let read_file path =
  In_channel.with_open_text path In_channel.input_all

let parse_file path =
  if not (Sys.file_exists path) then
    Error.raise_ Pos.zero (Printf.sprintf "cannot read file: %s" path);
  let src = read_file path in
  Lexer.tokenize src |> Parser.parse_program

(* Resolve `use NAME;` declared in `from_file` to a path on disk.
   Sibling-file model: same directory as the importing file. *)
let resolve_use ~from_file name =
  let dir = Filename.dirname from_file in
  Filename.concat dir (name ^ ".exl")

(* Recursively expand `Use` items in a list of items.  `loaded` is the set
   of file paths already inlined; `stack` is the current load chain (for
   cycle detection). *)
let rec expand_items ~from_file ~loaded ~stack items =
  List.concat_map (expand_item ~from_file ~loaded ~stack) items

and expand_item ~from_file ~loaded ~stack item =
  match item with
  | Ast.Function _ -> [ item ]
  | Ast.Module m ->
      let mitems' =
        expand_items ~from_file ~loaded ~stack m.Ast.mitems
      in
      [ Ast.Module { m with mitems = mitems' } ]
  | Ast.Use { path; pos } ->
      let name =
        match path with
        | [n] -> n
        | _ ->
            Error.failf pos
              "multi-segment 'use' is not yet supported (only `use foo;`)"
      in
      let dep_path = resolve_use ~from_file name in
      if List.mem dep_path stack then
        Error.failf pos
          "circular import: '%s' is already being loaded" name;
      if List.mem dep_path !loaded then
        (* Already loaded by another importer — `use` here is a no-op, the
           module has already been inlined elsewhere.  Each file appears
           in the final program at most once. *)
        []
      else begin
        loaded := dep_path :: !loaded;
        if not (Sys.file_exists dep_path) then
          Error.failf pos "cannot find module '%s' (looked for %s)"
            name dep_path;
        let items = parse_file dep_path in
        let stack' = dep_path :: stack in
        let inner =
          expand_items ~from_file:dep_path ~loaded ~stack:stack' items
        in
        [ Ast.Module { mname = name; mitems = inner; mpos = pos } ]
      end

let load entry_path =
  let loaded = ref [ entry_path ] in
  let items = parse_file entry_path in
  expand_items ~from_file:entry_path ~loaded ~stack:[ entry_path ] items
