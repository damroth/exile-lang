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

(* Wildcard-import visibility policy.  When `use foo::*;` inlines the
   contents of `foo` into the using scope, only items reachable from
   outside the source file ride along.  `pub` flags gate user-level
   decls; extern items (struct / type / const / var) and @c_include
   are always visible — their whole point is to expose a C-side
   symbol to anything that imports the module.  `impl` blocks track
   the visibility of their target struct (handled at typecheck), so
   passing them through wildcard is safe.  `use` items never
   propagate — they're scope-local. *)
let item_visible_in_wildcard = function
  | Ast.Function f -> f.Ast.is_pub
  | Ast.Module m -> m.Ast.mis_pub
  | Ast.Struct s -> s.Ast.sis_pub
  | Ast.Enum e -> e.Ast.eis_pub
  | Ast.Const c -> c.Ast.kis_pub
  | Ast.ExternStruct _
  | Ast.ExternType _
  | Ast.ExternConst _
  | Ast.ExternVar _
  | Ast.Impl _
  | Ast.CInclude _ -> true
  | Ast.Use _ -> false

let parse_file path =
  if not (Sys.file_exists path) then
    Error.failf Pos.zero "cannot read file: %s" path;
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
  | Ast.ExternConst _ | Ast.ExternVar _ | Ast.Const _ | Ast.Enum _
  | Ast.Impl _ | Ast.CInclude _ -> [ item ]
  | Ast.Module m ->
      let mitems' =
        expand_items ~from_file ~loaded ~stack m.Ast.mitems
      in
      [ Ast.Module { m with mitems = mitems' } ]
  | Ast.Use ({ path; is_wildcard; is_pub; pos } as u) ->
      (* `pub use foo::bar;` (single name) is a re-export handled by
         typecheck's alias table — pass through unchanged.  But a
         wildcard `pub use foo::*;` is a synonym for `use foo::*;`: inline
         foo's public items here (they keep their own `pub`, so the
         re-export happens for free).  So only NON-wildcard pub use is
         passed through; wildcard pub use falls into the load+inline path
         below. *)
      if is_pub && not is_wildcard then [ Ast.Use u ]
      else
      let display =
        String.concat "::" path ^ (if is_wildcard then "::*" else "")
      in
      let dep_path = resolve_use ~from_file path in
      if List.mem dep_path stack then
        Error.failf pos
          "circular import: '%s' is already being loaded" display;
      if Hashtbl.mem loaded dep_path then []
      else begin
        Hashtbl.add loaded dep_path ();
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
          List.filter item_visible_in_wildcard inner
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
  let loaded = Hashtbl.create 32 in
  Hashtbl.add loaded entry_path ();
  let items = parse_file entry_path in
  expand_items ~from_file:entry_path ~loaded ~stack:[ entry_path ] items
