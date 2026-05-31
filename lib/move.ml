(* DR-002 move-pass: blocking affine / use-at-most-once check between
   typecheck and lint.  Raises `Error.failf` on consumed-then-reused
   binding (the silent double-free trap that motivates @move).

   Scope this commit: tracks `@move`-marked struct values through the
   five consume sites (let RHS, fn arg, return value, assign RHS,
   method receiver) sequentially.  TIf merges per-branch; TWhile is
   conservative (no consume permitted inside the body — re-iter
   would re-consume).  TMatch arm-bodies, TDefer end-of-scope, and
   the divergence oracle (break / continue / `try`-as-TMatch) land
   in the follow-up commit. *)

open Ir

(* Per-binding state.  Tracked names live in an association list keyed
   by binding name; only `@move` bindings are ever inserted, so the
   zero-blast-radius guarantee is structural. *)
type state = Live | Consumed of Pos.t

(* True iff [t] resolves to a struct sig marked `@move`.  Pointer-
   shaped slots (`*T`, `*const T`) are never affine — borrows can
   never consume. *)
let is_affine_typ ~structs t =
  match t with
  | TStruct path ->
      (match List.find_opt
               (fun (s : struct_sig) -> s.sname_path = path) structs with
       | Some s -> s.ss_is_move
       | None -> false)
  | _ -> false

let set_consumed live n pos =
  List.map (fun (k, v) ->
    if k = n && v = Live then (k, Consumed pos) else (k, v))
    live

(* Any TVar of a tracked binding in Consumed state is a use-after-
   consume — report at the use site, point at the move site. *)
let rec check_reads live (te : texpr) =
  (match te.e with
   | TVar n ->
       (match List.assoc_opt n live with
        | Some (Consumed mv) ->
            Error.failf te.pos
              "use of '%s' after it was consumed at %s:%d:%d \
               (move-marked types are use-at-most-once — borrow with \
               '&%s' / take '*const %s' or clone to keep the source live)"
              n mv.file mv.line mv.col n (typ_name te.ty)
        | _ -> ())
   | _ -> ());
  List.iter (check_reads live) (texpr_children te)

(* If [te] is a bare affine `TVar` sitting in a by-value slot, mark
   the binding Consumed.  The slot's "by-value-ness" is encoded in
   [te.ty]: a value slot has the struct type directly, a borrow slot
   has `TPtr` / `TConstPtr` and isn't affine.  Non-TVar arguments
   (literals, struct lits, calls) flow without consuming a binding. *)
let consume_var ~structs live (te : texpr) =
  match te.e with
  | TVar n when is_affine_typ ~structs te.ty ->
      set_consumed live n te.pos
  | _ -> live

(* Merge two per-branch states (from TIf / future TMatch arms).  A
   binding ends up Consumed iff Consumed on EVERY branch; otherwise
   one path still has it live and reading it post-merge is legal.
   Pos comes from the then-branch when both consume; arbitrary but
   deterministic. *)
let merge_states a b =
  let names =
    List.sort_uniq compare (List.map fst a @ List.map fst b) in
  List.map (fun n ->
    let sa = try List.assoc n a with Not_found -> Live in
    let sb = try List.assoc n b with Not_found -> Live in
    let merged = match sa, sb with
      | Consumed pa, Consumed _ -> Consumed pa
      | _ -> Live
    in (n, merged))
    names

(* Walk an expression: validate every TVar read against the current
   state, then descend into sub-expressions.  Sub-call args are
   walked recursively, then each arg's binding is consumed if it's a
   bare affine TVar.  Other shapes (TRef, TStructLit, ...) read
   without consuming. *)
let rec walk_expr ~structs live (te : texpr) =
  check_reads live te;
  match te.e with
  | TCall { args; _ } | TBuiltinCall { args; _ } ->
      List.fold_left (fun live arg ->
        let live = walk_expr ~structs live arg in
        consume_var ~structs live arg)
        live args
  | TBlock { stmts; trailing } ->
      let live = walk_stmts ~structs ~ret_ty:None live stmts in
      (match trailing with
       | Some e -> walk_expr ~structs live e
       | None -> live)
  | _ ->
      List.fold_left (walk_expr ~structs)
        live (texpr_children te)

and walk_stmts ~structs ~ret_ty live stmts =
  List.fold_left (walk_stmt ~structs ~ret_ty) live stmts

and walk_stmt ~structs ~ret_ty live = function
  | TLet { name; value; _ } ->
      let live = walk_expr ~structs live value in
      let live = consume_var ~structs live value in
      if is_affine_typ ~structs value.ty
      then (name, Live) :: live
      else live
  | TLetTuple { value; _ } ->
      (* Tuple destructuring of affine fields is rare today (affine
         types aren't tuples).  Walk reads; consume nothing extra. *)
      walk_expr ~structs live value
  | TAssign { value; _ }
  | TAssignField { value; _ }
  | TAssignIndex { value; _ }
  | TAssignDeref { value; _ } ->
      let live = walk_expr ~structs live value in
      consume_var ~structs live value
  | TReturn { value = Some v; _ } ->
      let live = walk_expr ~structs live v in
      (match ret_ty with
       | Some rt when is_affine_typ ~structs rt -> consume_var ~structs live v
       | _ -> live)
  | TReturn { value = None; _ } -> live
  | TExprStmt e -> walk_expr ~structs live e
  | TIf { cond; then_body; else_body } ->
      let live = walk_expr ~structs live cond in
      let s_then = walk_stmts ~structs ~ret_ty live then_body in
      let s_else = walk_stmts ~structs ~ret_ty live else_body in
      merge_states s_then s_else
  | TWhile { cond; body; post } ->
      let live = walk_expr ~structs live cond in
      (* Conservative: walk one iteration, error if any tracked
         binding becomes Consumed — re-iteration would re-consume. *)
      let after =
        walk_stmts ~structs ~ret_ty
          (walk_stmts ~structs ~ret_ty live body) post in
      List.iter (fun (n, st) ->
        let pre = try List.assoc n live with Not_found -> Live in
        match pre, st with
        | Live, Consumed pos ->
            Error.failf pos
              "'%s' is consumed inside a loop body — the next iteration \
               would re-consume the (already-moved) binding" n
        | _ -> ())
        after;
      live
  | TFor _ | TForEach _ | TDefer _ | TBreak _ | TContinue _ -> live

(* Per-fn entry: seed live map with affine parameters, walk body. *)
let check_fn ~structs (tf : tfunc) =
  let params = tf.tf_func.Ast.params in
  let init_live =
    List.filter_map (fun (p, ty) ->
      if is_affine_typ ~structs ty then Some (p.Ast.pname, Live) else None)
      (List.combine params tf.tf_param_tys)
  in
  let _ : (string * state) list =
    walk_stmts ~structs ~ret_ty:tf.tf_ret_ty init_live tf.tf_body
  in
  ()

(* Whole-program entry.  Skipped if no struct in the program is
   marked `@move` — in that case [is_affine_typ] returns false
   everywhere and the walk is a no-op. *)
let check (tp : tprogram) =
  let any_marked =
    List.exists (fun (s : struct_sig) -> s.ss_is_move)
      tp.tp_struct_index
  in
  if any_marked then
    List.iter (check_fn ~structs:tp.tp_struct_index) tp.tp_funcs
