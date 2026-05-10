(* Multi-file loader.  Walks the entry file's AST, replaces each `use` item
   with a top-level `mod NAME { ... }` whose contents come from a sibling
   `.exl` file (or a directory containing `mod.exl`).  Each resolved file is
   loaded at most once; cycles are rejected.

   Resolution rules:
   - `use foo;` looks for `<dir>/foo.exl`, falling back to `<dir>/foo/mod.exl`.
   - `use foo::bar;` looks for `<dir>/foo/bar.exl`, falling back to
     `<dir>/foo/bar/mod.exl`.
   - The module name introduced into the using scope is the last segment of
     the path (matches the Rust-like `use` semantics: `use string::ascii;`
     gives access to `ascii::...`, not `string::ascii::...`). *)

let read_file path =
  In_channel.with_open_text path In_channel.input_all

let parse_file path =
  if not (Sys.file_exists path) then
    Error.raise_ Pos.zero (Printf.sprintf "cannot read file: %s" path);
  let src = read_file path in
  Lexer.tokenize ~file:path src |> Parser.parse_program

(* Resolve a `use` path declared in `from_file` to a file on disk.
   For path `[a; b; c]` we try `<dir>/a/b/c.exl` first, then
   `<dir>/a/b/c/mod.exl`.  When the path has only one segment, this collapses
   to `<dir>/foo.exl` then `<dir>/foo/mod.exl`. *)
let resolve_use ~from_file path =
  let dir = Filename.dirname from_file in
  let joined = List.fold_left Filename.concat dir path in
  let direct = joined ^ ".exl" in
  let mod_file = Filename.concat joined "mod.exl" in
  if Sys.file_exists direct then direct
  else if Sys.file_exists mod_file then mod_file
  else direct  (* fall back to the .exl form for the error message *)

(* Recursively expand `Use` items in a list of items.  `loaded` is the set
   of file paths already inlined; `stack` is the current load chain (for
   cycle detection). *)
let rec expand_items ~from_file ~loaded ~stack items =
  List.concat_map (expand_item ~from_file ~loaded ~stack) items

and expand_item ~from_file ~loaded ~stack item =
  match item with
  | Ast.Function _ | Ast.Struct _ | Ast.ExternStruct _ | Ast.ExternType _
  | Ast.ExternConst _ | Ast.Enum _ | Ast.Impl _ | Ast.CInclude _ -> [ item ]
  | Ast.Module m ->
      let mitems' =
        expand_items ~from_file ~loaded ~stack m.Ast.mitems
      in
      [ Ast.Module { m with mitems = mitems' } ]
  | Ast.Use ({ path; is_wildcard; is_pub; pos } as u) ->
      (* `pub use foo::bar;` is a re-export — handled entirely by typecheck
         via its alias table, never crosses file boundaries.  Pass through
         unchanged so flatten_items sees it. *)
      if is_pub then [ Ast.Use u ]
      else
      let display =
        String.concat "::" path ^ (if is_wildcard then "::*" else "")
      in
      let dep_path = resolve_use ~from_file path in
      if List.mem dep_path stack then
        Error.failf pos
          "circular import: '%s' is already being loaded" display;
      if List.mem dep_path !loaded then []
      else begin
        loaded := dep_path :: !loaded;
        if not (Sys.file_exists dep_path) then
          Error.failf pos "cannot find module '%s' (looked for %s)"
            display dep_path;
        let items = parse_file dep_path in
        let stack' = dep_path :: stack in
        let inner =
          expand_items ~from_file:dep_path ~loaded ~stack:stack' items
        in
        if is_wildcard then
          (* Wildcard import: drop the module wrapper and inline all public
             items directly into the importing scope.  Private items stay
             behind in the source file. *)
          List.filter
            (function
              | Ast.Function f -> f.Ast.is_pub
              | Ast.Module m -> m.Ast.mis_pub
              | Ast.Struct s -> s.Ast.sis_pub
              | Ast.ExternStruct _ -> true  (* extern struct is always visible *)
              | Ast.ExternType _ -> true   (* extern type is always visible *)
              | Ast.ExternConst _ -> true  (* extern const is always visible *)
              | Ast.Enum e -> e.Ast.eis_pub
              | Ast.Impl _ -> true   (* impls follow their target struct's visibility *)
              | Ast.CInclude _ -> true  (* @c_include is always visible *)
              | Ast.Use _ -> false)
            inner
        else begin
          (* Non-wildcard `use`: introduce the file as a module whose name is
             the last segment of the path (Rust-like). *)
          let name =
            match List.rev path with
            | n :: _ -> n
            | [] -> Error.failf pos "internal: empty 'use' path"
          in
          [ Ast.Module { mname = name; mitems = inner; mpos = pos;
                         mis_pub = true } ]
        end
      end

let load entry_path =
  let loaded = ref [ entry_path ] in
  let items = parse_file entry_path in
  expand_items ~from_file:entry_path ~loaded ~stack:[ entry_path ] items
