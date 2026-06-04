(* DR-013 perf-introspection v1.  Compile-time cost model report,
   read off typed-IR + Codegen.last_bloat.  Surfaces the cost-sites the
   2026-06-03 perf-audit pinned (i32 mul/div/mod soft-call, indirect
   call, no-capacity collection construction, struct-by-value copy),
   tags each one with loop-nesting and a folds-at-O2 marker so the
   user knows when GCC will hide the soft-call at -O2.  Reads from
   pre-codegen IR for the folds-at-O2 honesty caveat. *)

open Ir

type cost_kind =
  | Mul32
  | DivuDiv
  | DivuMod
  | IndirectCall
  | NoCapacity
  | StructByValueCopy

type cost_site = {
  cs_kind : cost_kind;
  cs_pos : Pos.t;
  cs_folds_at_o2 : bool;
  cs_in_loop : bool;
}

type fn_metrics = {
  fm_mangled : string;
  fm_skeleton_path : string list;
  fm_skeleton_name : string;
  fm_pos : Pos.t;
  fm_c_text_bytes : int;
  fm_sites : cost_site list;
  fm_hot : bool;
  fm_is_prelude : bool;
}

type skeleton_group = {
  sg_path : string list;
  sg_name : string;
  sg_instances : fn_metrics list;
  sg_total_bytes : int;
}

type report = {
  r_fns : fn_metrics list;
  r_groups : skeleton_group list;
  r_total_bytes : int;
}

let cost_kind_tag = function
  | Mul32 -> "mul32"
  | DivuDiv -> "div32"
  | DivuMod -> "mod32"
  | IndirectCall -> "indirect"
  | NoCapacity -> "no-cap"
  | StructByValueCopy -> "copy"

let is_i32 = function
  | TInt { signed = true; width = Ast.W32 } -> true
  | _ -> false

let is_aggregate_for_copy = function
  | TStruct _ | TTuple _ | TArray _ -> true
  | _ -> false

let rec literal_int_lit (te : texpr) =
  match te.e with
  | TIntLit n -> Some n
  | TCast (sub, _) -> literal_int_lit sub
  | _ -> None

let is_with_capacity_call mangled =
  let suffix = "__with_capacity" in
  let mlen = String.length mangled in
  let slen = String.length suffix in
  if mlen < slen + 4 then false
  else
    let rec find i =
      if i + slen > mlen then false
      else if String.sub mangled i slen = suffix then true
      else find (i + 1)
    in
    find 0

let collect_sites (tf : tfunc) : cost_site list =
  let sites = ref [] in
  let add k ~pos ~folds ~in_loop =
    sites := { cs_kind = k; cs_pos = pos; cs_folds_at_o2 = folds;
               cs_in_loop = in_loop } :: !sites
  in
  let rec scan_expr ~in_loop (te : texpr) =
    (match te.e with
     | TBinOp (op, l, r) when is_i32 te.ty ->
         let folds =
           Option.is_some (literal_int_lit l)
           && Option.is_some (literal_int_lit r)
         in
         (match op with
          | Ast.Mul -> add Mul32 ~pos:te.pos ~folds ~in_loop
          | Ast.Div -> add DivuDiv ~pos:te.pos ~folds ~in_loop
          | Ast.Mod -> add DivuMod ~pos:te.pos ~folds ~in_loop
          | _ -> ())
     | TIndirectCall _ ->
         add IndirectCall ~pos:te.pos ~folds:false ~in_loop
     | TCall { mangled; args } when is_with_capacity_call mangled ->
         (match args with
          | _ :: hint :: _ ->
              (match literal_int_lit hint with
               | Some n when n < 8 ->
                   add NoCapacity ~pos:te.pos ~folds:false ~in_loop
               | _ -> ())
          | _ -> ())
     | TCall { args; _ } ->
         List.iter (fun (a : texpr) ->
           if is_aggregate_for_copy a.ty then
             add StructByValueCopy ~pos:a.pos ~folds:false ~in_loop) args
     | _ -> ());
    List.iter (scan_expr ~in_loop) (texpr_children te)
  in
  let rec scan_stmt ~in_loop s =
    List.iter (scan_expr ~in_loop) (tstmt_own_exprs s);
    (match s with
     | TReturn { value = Some e; pos } when is_aggregate_for_copy e.ty ->
         add StructByValueCopy ~pos ~folds:false ~in_loop
     | _ -> ());
    (match s with
     | TWhile { body; post; _ } ->
         List.iter (scan_stmt ~in_loop:true) (body @ post)
     | TFor { body; _ } | TForEach { body; _ } ->
         List.iter (scan_stmt ~in_loop:true) body
     | TIf { then_body; else_body; _ } ->
         List.iter (scan_stmt ~in_loop) then_body;
         List.iter (scan_stmt ~in_loop) else_body
     | TDefer { body; _ } ->
         List.iter (scan_stmt ~in_loop) body
     | _ -> ())
  in
  List.iter (scan_stmt ~in_loop:false) tf.tf_body;
  List.rev !sites

(* hot heuristic per design: fn-hot ⟺ DIVU(modulo) anywhere
   ∨ indirect-call inside a loop ∨ unbounded-copy in a loop. *)
let hot_of (sites : cost_site list) =
  List.exists (fun s ->
    match s.cs_kind with
    | DivuMod | DivuDiv -> true
    | IndirectCall | StructByValueCopy -> s.cs_in_loop
    | Mul32 | NoCapacity -> false) sites

let bloat_lookup (bloat : (string * int) list) =
  let h = Hashtbl.create (List.length bloat) in
  List.iter (fun (n, b) -> Hashtbl.replace h n b) bloat;
  h

let collect (tp : tprogram) (bloat : (string * int) list) : report =
  let bloat_h = bloat_lookup bloat in
  (* Only the fns codegen actually emitted are bloat-tracked.  Filter
     skeletons (uninstantiated generics) so groups like `Vec::grow [N
     inst]` count emitted instances only. *)
  let fns =
    List.filter_map (fun tf ->
      if tf.tf_func.Ast.is_extern then None
      else if not (Hashtbl.mem bloat_h tf.tf_mangled) then None
      else begin
        let sites = collect_sites tf in
        let bytes = Hashtbl.find bloat_h tf.tf_mangled in
        Some {
          fm_mangled = tf.tf_mangled;
          fm_skeleton_path = tf.tf_path;
          fm_skeleton_name = tf.tf_func.Ast.name;
          fm_pos = tf.tf_func.Ast.pos;
          fm_c_text_bytes = bytes;
          fm_sites = sites;
          fm_hot = hot_of sites;
          fm_is_prelude = tf.tf_func.Ast.pos.Pos.file = "<prelude>";
        }
      end) tp.tp_funcs
  in
  let total = List.fold_left (fun a f -> a + f.fm_c_text_bytes) 0 fns in
  (* Skeleton grouping: same (path, source name) means same skeleton.
     Mono-instances share their skeleton's source name; non-generic fns
     form a single-element group. *)
  let groups_tbl : (string list * string, fn_metrics list) Hashtbl.t =
    Hashtbl.create 64
  in
  List.iter (fun f ->
    let key = (f.fm_skeleton_path, f.fm_skeleton_name) in
    let prev = try Hashtbl.find groups_tbl key with Not_found -> [] in
    Hashtbl.replace groups_tbl key (f :: prev)) fns;
  let groups =
    Hashtbl.fold (fun (path, name) insts acc ->
      let total = List.fold_left (fun a f -> a + f.fm_c_text_bytes) 0 insts in
      { sg_path = path; sg_name = name;
        sg_instances = List.rev insts;
        sg_total_bytes = total } :: acc)
      groups_tbl []
  in
  let groups =
    List.sort (fun a b -> compare b.sg_total_bytes a.sg_total_bytes) groups
  in
  { r_fns = fns; r_groups = groups; r_total_bytes = total }

(* ── HUMAN TABLE ──────────────────────────────────────────────────── *)

let qualified path name =
  match path with
  | [] -> name
  | _ -> String.concat "::" path ^ "::" ^ name

let format_site_inline (s : cost_site) =
  let tag = cost_kind_tag s.cs_kind in
  let in_loop = if s.cs_in_loop then " in-loop" else "" in
  let folds = if s.cs_folds_at_o2 then " folds-at-O2" else "" in
  Printf.sprintf "%s:%d:%d %s%s%s"
    s.cs_pos.Pos.file s.cs_pos.Pos.line s.cs_pos.Pos.col
    tag in_loop folds

let to_human (r : report) : string =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    "exile perf-report v1 (DR-013) — cost-model on typed IR pre-O2-fold\n";
  Buffer.add_string buf
    "  kinds: mul32 div32 mod32 = i32 soft-call (libgcc),\n\
    \         indirect = JSR through fn-ptr, no-cap = with_capacity(_,<8),\n\
    \         copy = aggregate by-value pass/return\n";
  Buffer.add_string buf
    "  flags: hot = DIVU(any) or indirect/copy-in-loop;\n\
    \         folds-at-O2 = both operands literal → GCC O2 const-folds\n";
  Buffer.add_string buf
    (Printf.sprintf "\nTotals: %d fns, %d B C-text\n\n"
       (List.length r.r_fns) r.r_total_bytes);
  (* Skeleton-grouped: name + total + N inst.  Single-inst skeletons
     get the inst's mangled name in the trailing detail. *)
  Buffer.add_string buf "Per skeleton (by total bytes desc):\n";
  List.iter (fun g ->
    let inst_count = List.length g.sg_instances in
    let qname = qualified g.sg_path g.sg_name in
    if inst_count = 1 then
      Buffer.add_string buf
        (Printf.sprintf "  %7d B  %s\n" g.sg_total_bytes qname)
    else
      Buffer.add_string buf
        (Printf.sprintf "  %7d B  %s  [%d inst]\n"
           g.sg_total_bytes qname inst_count)) r.r_groups;
  Buffer.add_string buf "\nHot fns + cost-sites:\n";
  let hot_fns = List.filter (fun f -> f.fm_hot || f.fm_sites <> []) r.r_fns in
  let sorted_hot =
    List.sort (fun a b -> compare b.fm_c_text_bytes a.fm_c_text_bytes) hot_fns
  in
  if sorted_hot = [] then
    Buffer.add_string buf "  (none)\n"
  else
    List.iter (fun f ->
      let hot_tag = if f.fm_hot then " HOT" else "" in
      Buffer.add_string buf
        (Printf.sprintf "  %7d B  %s%s\n"
           f.fm_c_text_bytes f.fm_mangled hot_tag);
      List.iter (fun s ->
        Buffer.add_string buf
          (Printf.sprintf "             ↳ %s\n" (format_site_inline s)))
        f.fm_sites) sorted_hot;
  Buffer.contents buf

(* ── JSON ─────────────────────────────────────────────────────────── *)

let json_escape s =
  let buf = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c when Char.code c < 32 ->
        Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let json_str s = "\"" ^ json_escape s ^ "\""

let json_site (s : cost_site) =
  Printf.sprintf
    "{\"kind\":%s,\"file\":%s,\"line\":%d,\"col\":%d,\
     \"in_loop\":%b,\"folds_at_o2\":%b}"
    (json_str (cost_kind_tag s.cs_kind))
    (json_str s.cs_pos.Pos.file)
    s.cs_pos.Pos.line s.cs_pos.Pos.col
    s.cs_in_loop s.cs_folds_at_o2

let json_fn (f : fn_metrics) =
  let sites = String.concat "," (List.map json_site f.fm_sites) in
  Printf.sprintf
    "{\"mangled\":%s,\"skeleton\":%s,\"pos\":%s,\
     \"c_text_bytes\":%d,\"hot\":%b,\"is_prelude\":%b,\"sites\":[%s]}"
    (json_str f.fm_mangled)
    (json_str (qualified f.fm_skeleton_path f.fm_skeleton_name))
    (json_str (Printf.sprintf "%s:%d:%d"
                 f.fm_pos.Pos.file f.fm_pos.Pos.line f.fm_pos.Pos.col))
    f.fm_c_text_bytes f.fm_hot f.fm_is_prelude sites

let json_group (g : skeleton_group) =
  Printf.sprintf
    "{\"name\":%s,\"total_bytes\":%d,\"instances\":[%s]}"
    (json_str (qualified g.sg_path g.sg_name))
    g.sg_total_bytes
    (String.concat "," (List.map (fun f -> json_str f.fm_mangled)
                          g.sg_instances))

let to_json (r : report) : string =
  let fns_sorted =
    List.sort (fun a b -> compare b.fm_c_text_bytes a.fm_c_text_bytes) r.r_fns
  in
  Printf.sprintf
    "{\"version\":1,\"total_bytes\":%d,\"fn_count\":%d,\
     \"groups\":[%s],\"fns\":[%s]}\n"
    r.r_total_bytes (List.length r.r_fns)
    (String.concat "," (List.map json_group r.r_groups))
    (String.concat "," (List.map json_fn fns_sorted))
