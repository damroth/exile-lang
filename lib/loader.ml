(* Multi-file loader.  Walks the entry file's AST and resolves every `use`
   item against sibling `.exl` files.

   Each `use`d file becomes a top-level `mod NAME { ... }` hoisted to the
   program root EXACTLY ONCE — so a module shared by several importers (a
   DAG: `pos` used by `ast`, `token`, `ir`, …) is loaded a single time and
   its private items stay encapsulated inside it.  A wildcard `use foo::*;`
   additionally lifts foo's exported names into the using scope as re-export
   aliases (`pub use foo::X;`), which typecheck resolves without copying.

   Three `use` shapes:
   - `use foo;`        -> hoist `mod foo` (qualified access `foo::X`).
   - `use foo::*;`     -> hoist `mod foo` + alias its exported names in.
   - `pub use foo::*;` -> re-export: inline foo's public items here so they
                          flatten up into THIS module's exports (the prelude
                          idiom — see examples/reexport.exl).
   - `pub use foo::bar;` -> single-name re-export, passed through to
                            typecheck's alias table (no file load).

   Resolution rules (file paths):
   - `use foo;`      looks for `<dir>/foo.exl`, then `<dir>/foo/mod.exl`.
   - `use foo::bar;` looks for `<dir>/foo/bar.exl`, then `.../bar/mod.exl`.
   - The module name introduced is the last path segment (Rust-like). *)

let read_file path =
  In_channel.with_open_text path In_channel.input_all

(* Wildcard-import visibility policy.  When `pub use foo::*;` inlines the
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
  | Ast.Trait t -> t.Ast.tris_pub
  | Ast.View v -> v.Ast.vis_pub
  | Ast.TypeAlias ta -> ta.Ast.tais_pub
  | Ast.ExternStruct _
  | Ast.ExternType _
  | Ast.ExternConst _
  | Ast.ExternVar _
  | Ast.Impl _
  | Ast.CInclude _ -> true
  | Ast.Use _ -> false

(* The single externally-visible name of a top-level item, if it has one
   and is exported.  Drives the alias list a `use foo::*;` expands to. *)
let item_export_name = function
  | Ast.Function f when f.Ast.is_pub -> Some f.Ast.name
  | Ast.Struct s when s.Ast.sis_pub -> Some s.Ast.sname
  | Ast.Enum e when e.Ast.eis_pub -> Some e.Ast.ename
  | Ast.Trait t when t.Ast.tris_pub -> Some t.Ast.trname
  | Ast.Const c when c.Ast.kis_pub -> Some c.Ast.kname
  | Ast.TypeAlias ta when ta.Ast.tais_pub -> Some ta.Ast.taname
  | Ast.Module m when m.Ast.mis_pub -> Some m.Ast.mname
  | Ast.ExternStruct es -> Some es.Ast.esname
  | Ast.ExternType et -> Some et.Ast.xtname
  | Ast.ExternConst ec -> Some ec.Ast.ecname
  | Ast.ExternVar ev -> Some ev.Ast.evname
  | _ -> None

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

let last_seg path = match List.rev path with n :: _ -> n | [] -> ""

let load entry_path =
  (* Parsed-AST cache — every file is read + parsed at most once. *)
  let parsed : (string, Ast.item list) Hashtbl.t = Hashtbl.create 32 in
  let parse file =
    match Hashtbl.find_opt parsed file with
    | Some items -> items
    | None -> let items = parse_file file in Hashtbl.add parsed file items; items
  in
  (* Files already hoisted to the program root as a `mod NAME { ... }`. *)
  let hoisted : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let hoisted_mods = ref [] in  (* reverse load order *)
  (* Cached export-name lists (for wildcard alias synthesis). *)
  let exports = Hashtbl.create 32 in

  (* Names a file exports to importers: own public items + transitive
     `pub use Y::*` re-exports + `pub use Y::name` single re-exports.
     Computed from the *source* items (so a private `use Z::*` is not
     re-exported), with a cycle guard. *)
  let rec exports_of file =
    match Hashtbl.find_opt exports file with
    | Some ns -> ns
    | None ->
        Hashtbl.add exports file [];  (* cycle guard: empty while computing *)
        let ns =
          List.concat_map (fun item -> match item with
            | Ast.Use { path; is_wildcard = true; is_pub = true; _ } ->
                exports_of (resolve_use ~from_file:file path)
            | Ast.Use { path; is_wildcard = false; is_pub = true; _ } ->
                [ last_seg path ]
            | Ast.Use _ -> []
            | other ->
                (match item_export_name other with Some n -> [n] | None -> []))
            (parse file)
        in
        Hashtbl.replace exports file ns; ns
  in

  (* Expand the `use` items in one file's item list.  `stack` is the load
     chain (cycle detection); `from_file` anchors relative resolution. *)
  let rec expand_items ~from_file ~stack items =
    List.concat_map (expand_item ~from_file ~stack) items
  and expand_item ~from_file ~stack item =
    match item with
    | Ast.Function _ | Ast.Struct _ | Ast.ExternStruct _ | Ast.ExternType _
    | Ast.ExternConst _ | Ast.ExternVar _ | Ast.Const _ | Ast.Enum _
    | Ast.Impl _ | Ast.Trait _ | Ast.View _ | Ast.CInclude _ | Ast.TypeAlias _ ->
        [ item ]
    | Ast.Module m ->
        [ Ast.Module { m with mitems = expand_items ~from_file ~stack m.Ast.mitems } ]
    | Ast.Use { path; is_wildcard = false; is_pub = true; _ } as u ->
        (* `pub use foo::bar;` — single-name re-export handled by
           typecheck's alias table; pass through unchanged. *)
        ignore path; [ u ]
    | Ast.Use { path; is_wildcard = true; is_pub = true; pos } ->
        (* `pub use foo::*;` — re-export: inline foo's public items here so
           they flatten up into this module's own exports (a real item at
           this path, not an alias, which keeps two-hop re-export resolving
           in one lookup). *)
        let dep = resolve_use ~from_file path in
        if List.mem dep stack then
          Error.failf pos "circular import: '%s' is already being loaded"
            (String.concat "::" path ^ "::*");
        let inner = expand_items ~from_file:dep ~stack:(dep :: stack) (parse dep) in
        List.filter item_visible_in_wildcard inner
    | Ast.Use { path; is_wildcard; is_pub = _; pos } ->
        (* `use foo;` / `use foo::*;` — hoist `mod foo` once, then (for the
           wildcard form) alias foo's exported names into this scope. *)
        let dep = resolve_use ~from_file path in
        if List.mem dep stack then
          Error.failf pos "circular import: '%s' is already being loaded"
            (String.concat "::" path);
        hoist ~stack dep (last_seg path) pos;
        if is_wildcard then
          List.map (fun n ->
            Ast.Use { path = [ last_seg path; n ];
                      is_wildcard = false; is_pub = true; pos })
            (exports_of dep)
        else []
  and hoist ~stack dep modname pos =
    if not (Hashtbl.mem hoisted dep) then begin
      Hashtbl.add hoisted dep ();
      if not (Sys.file_exists dep) then
        Error.failf pos "cannot find module '%s' (looked for %s)" modname dep;
      let inner = expand_items ~from_file:dep ~stack:(dep :: stack) (parse dep) in
      hoisted_mods :=
        Ast.Module { mname = modname; mitems = inner; mpos = pos; mis_pub = true }
        :: !hoisted_mods
    end
  in
  let entry = expand_items ~from_file:entry_path ~stack:[ entry_path ]
                (parse entry_path) in
  (* Hoisted modules first (load order), then the entry file's own items. *)
  List.rev !hoisted_mods @ entry
