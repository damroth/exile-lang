(* Tier-aware diagnostics.  Runs after typecheck, before codegen, with
   the active compilation profile in hand.  Walks the typed program
   looking for items whose effective tier exceeds the profile, and
   emits warnings on stderr.  Warnings never block compilation —
   exit code stays 0; the user decides whether to act on them.

   Today the linter focuses on the highest-impact tier source:
   monomorphic instances of generic fns (each one adds a fresh
   function body to the emitted C, the dominant bloat vector under
   tight memory targets like Amiga 256K).  Generic struct/enum
   instances are cheap (one struct decl, no executable code) and
   skipped.  Future passes may add deep-nesting and threshold-based
   summaries. *)

open Ir

(* Effective tier of a fn: explicit `@tier(...)` hint wins; otherwise
   default to Full for generics and Core for mono fns. *)
let fn_effective_tier (f : Ast.func) : Tier.t =
  match f.tier_hint with
  | Some name ->
      (* Parser already validated the name; failing here would be a
         compiler bug, not user error. *)
      (match Tier.of_string name with
       | Some t -> t
       | None -> failwith ("internal: invalid tier_hint '" ^ name ^ "'"))
  | None ->
      if f.tparams <> [] then Tier.Full
      else Tier.Core

(* True when every typ in [tf]'s resolved signature is concrete — i.e.
   it's a monomorphic instance (or an originally-mono fn), not a
   generic skeleton waiting on instantiation. *)
let is_concrete_instance (tf : tfunc) : bool =
  List.for_all is_concrete tf.tf_param_tys
  && (match tf.tf_ret_ty with
      | Some t -> is_concrete t
      | None -> true)

type warning = { pos : Pos.t; msg : string }

(* Names starting with '_' are an explicit "I know this is unused" tag —
   mirrors Rust/OCaml convention.  Bare '_' also exempt (binding-as-throwaway
   in destructure patterns, though parser doesn't currently emit it as a
   let name). *)
let is_silenced_name n =
  String.length n > 0 && n.[0] = '_'

(* Walk a texpr collecting every name that appears in a reading position
   (TVar).  Writes (LHS of TAssign) are not collected here — they don't
   count as "use" for the unused-let check. *)
let rec reads_in_expr acc (e : texpr) =
  match e.e with
  | TVar n -> n :: acc
  | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _
  | TFnRef _ | TSizeOf _ -> acc
  | TNeg x | TCast (x, _) | TRef x | TDeref x -> reads_in_expr acc x
  | TBinOp (_, a, b) -> reads_in_expr (reads_in_expr acc a) b
  | TCall { mangled; args } ->
      (* Local fn-ptr vars are called via TCall with `mangled` = the
         local's name (see Ir.texpr_node note on TIndirectCall).  Count
         it as a read so `let f = add; f(40, 2)` doesn't warn.  Global
         fn names (`ex_add`, `mod__foo`) won't collide with let names
         so the extra entries are harmless noise. *)
      List.fold_left reads_in_expr (mangled :: acc) args
  | TBuiltinCall { args; _ } ->
      List.fold_left reads_in_expr acc args
  | TIndirectCall { fn_expr; args } ->
      List.fold_left reads_in_expr (reads_in_expr acc fn_expr) args
  | TTupleLit xs -> List.fold_left reads_in_expr acc xs
  | TStructLit { fields; base; _ } | TNew { fields; base; _ } ->
      let acc = List.fold_left (fun a (_, x) -> reads_in_expr a x) acc fields in
      (match base with Some b -> reads_in_expr acc b | None -> acc)
  | TFieldAccess { target; _ } -> reads_in_expr acc target
  | TEnumLit { args; _ } ->
      List.fold_left (fun a (_, x) -> reads_in_expr a x) acc args
  | TMatch { scrutinee; arms; _ } ->
      let acc = reads_in_expr acc scrutinee in
      List.fold_left (fun a arm -> reads_in_expr a arm.tbody) acc arms

let rec reads_in_stmts acc stmts = List.fold_left reads_in_stmt acc stmts
and reads_in_stmt acc s =
  match s with
  | TLet { value; _ } | TLetTuple { value; _ }
  | TAssign { value; _ } -> reads_in_expr acc value
  | TAssignField { target; value; _ } | TAssignDeref { target; value; _ } ->
      reads_in_expr (reads_in_expr acc target) value
  | TReturn { value; _ } -> reads_in_expr acc value
  | TExprStmt e -> reads_in_expr acc e
  | TIf { cond; then_body; else_body } ->
      let acc = reads_in_expr acc cond in
      reads_in_stmts (reads_in_stmts acc then_body) else_body
  | TWhile { cond; body } -> reads_in_stmts (reads_in_expr acc cond) body
  | TDefer { body; _ } -> reads_in_stmts acc body

(* Collect (name, pos) for every TLet / TLetTuple binding in the tree.
   Names starting with '_' are skipped at collection time so they never
   show up in the unused list. *)
let rec lets_in_stmts acc stmts = List.fold_left let_in_stmt acc stmts
and let_in_stmt acc s =
  match s with
  | TLet { name; pos; _ } ->
      if is_silenced_name name then acc else (name, pos) :: acc
  | TLetTuple { names; pos; _ } ->
      List.fold_left (fun a n ->
        if is_silenced_name n then a else (n, pos) :: a) acc names
  | TIf { then_body; else_body; _ } ->
      lets_in_stmts (lets_in_stmts acc then_body) else_body
  | TWhile { body; _ } -> lets_in_stmts acc body
  | TDefer { body; _ } -> lets_in_stmts acc body
  | TAssign _ | TAssignField _ | TAssignDeref _
  | TReturn _ | TExprStmt _ -> acc

(* Per-function unused-let check.  Builds the set of names read anywhere
   in the body, then flags any let-binding whose name doesn't appear.
   Shadowing is rare in practice and harmless here: if any of the
   shadow chain is read, none of them warn (false negative we accept). *)
let unused_lets_for (tf : tfunc) : warning list =
  let lets = List.rev (lets_in_stmts [] tf.tf_body) in
  if lets = [] then []
  else begin
    let reads = reads_in_stmts [] tf.tf_body in
    let read_set = Hashtbl.create (List.length reads) in
    List.iter (fun n -> Hashtbl.replace read_set n ()) reads;
    List.filter_map (fun (name, pos) ->
      if Hashtbl.mem read_set name then None
      else
        let msg = Printf.sprintf
          "unused variable '%s' (prefix name with '_' to silence)" name
        in
        Some { pos; msg })
      lets
  end

(* Pure analysis: returns the warnings the linter would emit, without
   touching stderr.  Tests compare the list directly; CLI prints via
   [emit_warnings]. *)
let collect ~(profile : Profile.t) (tp : tprogram) : warning list =
  (* Dedupe by call-site (origin_pos when available, fallback to decl
     pos) so each unique site warns once.  Origin pos points at the
     actual `.alloc()` / `id(42)` rather than the prelude/decl, which
     is far more useful for finding what to refactor. *)
  let seen = Hashtbl.create 16 in
  let acc = ref [] in
  List.iter (fun tf ->
    let f = tf.tf_func in
    if f.tparams <> [] && is_concrete_instance tf then begin
      let item_tier = fn_effective_tier f in
      let warn_pos = match tf.tf_origin_pos with
        | Some p -> p
        | None -> f.pos
      in
      if Tier.exceeds ~profile ~item_tier
         && not (Hashtbl.mem seen warn_pos) then begin
        Hashtbl.add seen warn_pos ();
        let msg =
          Printf.sprintf
            "generic fn '%s' is tier=%s but compiled under \
             --profile=%s; each instantiation adds a body copy \
             (use --profile=%s to silence, or refactor to a mono \
             alternative)"
            f.name
            (Tier.to_string item_tier)
            (Profile.to_string profile)
            (Tier.to_string item_tier)
        in
        acc := { pos = warn_pos; msg } :: !acc
      end
    end)
    tp.tp_funcs;
  let tier_warnings = List.rev !acc in
  (* Unused-let warnings: walk each source-level fn once.  tp_funcs
     contains both generic skeletons and their concrete instances; we
     walk the skeleton (it has the original let-names and positions)
     and skip the instance.  Prelude code is library territory — never
     warn the user about it. *)
  let is_prelude tf = tf.tf_func.pos.file = "<prelude>" in
  let is_generic_instance tf =
    tf.tf_func.tparams <> [] && is_concrete_instance tf
  in
  let unused =
    List.concat_map (fun tf ->
      if is_prelude tf || is_generic_instance tf then []
      else unused_lets_for tf)
      tp.tp_funcs
  in
  tier_warnings @ unused

let emit_warnings (ws : warning list) : unit =
  List.iter (fun w ->
    Printf.eprintf "%s:%d:%d: warning: %s\n"
      w.pos.file w.pos.line w.pos.col w.msg)
    ws

let lint ~(profile : Profile.t) (tp : tprogram) : unit =
  emit_warnings (collect ~profile tp)
