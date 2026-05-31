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

(* Merge per-branch states.  A binding ends up Consumed iff Consumed
   on EVERY non-diverging fall-through; a diverging branch (return /
   break / continue / `try`-arm) can't reach the post-branch program
   point so its state is dropped.  Pos comes from the first consuming
   branch — arbitrary but deterministic. *)
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

(* Predicate: does this expression unconditionally diverge?  Used to
   drop branch states from the merge — a `try`-arm that early-returns
   never reaches the post-match program point, so a consume inside
   it can't make the post-match binding Consumed (the live path
   doesn't run). *)
let rec expr_diverges (te : texpr) =
  match te.e with
  | TMatch { arms; _ } ->
      arms <> [] && List.for_all
        (fun (a : tmatch_arm) -> a.tdiverges || expr_diverges a.tbody) arms
  | TIfExpr { then_val; else_val; _ } ->
      expr_diverges then_val && expr_diverges else_val
  | TBlock { stmts; trailing } ->
      stmts_diverge stmts
      || (match trailing with
          | Some t -> expr_diverges t
          | None -> false)
  | _ -> false

and stmts_diverge stmts =
  List.exists (fun s ->
    match s with
    | TReturn _ | TBreak _ | TContinue _ -> true
    | TIf { then_body; else_body; _ } ->
        stmts_diverge then_body && stmts_diverge else_body
    | TExprStmt e -> expr_diverges e
    | _ -> false)
    stmts

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
  | TMatch { scrutinee; arms; _ } ->
      let live = walk_expr ~structs live scrutinee in
      (* Walk every arm for read-checking; only non-diverging arms
         contribute to the post-merge state.  Empty match (no arms)
         passes state through unchanged. *)
      let contributions = List.filter_map (fun (a : tmatch_arm) ->
        Option.iter (fun g -> ignore (walk_expr ~structs live g)) a.tguard;
        let after = walk_expr ~structs live a.tbody in
        if a.tdiverges || expr_diverges a.tbody then None
        else Some after)
        arms
      in
      (match contributions with
       | [] -> live
       | s :: rest -> List.fold_left merge_states s rest)
  | TIfExpr { cond; then_val; else_val } ->
      let live = walk_expr ~structs live cond in
      let s_then = walk_expr ~structs live then_val in
      let s_else = walk_expr ~structs live else_val in
      let then_dvg = expr_diverges then_val in
      let else_dvg = expr_diverges else_val in
      (match then_dvg, else_dvg with
       | true, true -> live
       | true, false -> s_else
       | false, true -> s_then
       | false, false -> merge_states s_then s_else)
  | _ ->
      List.fold_left (walk_expr ~structs)
        live (texpr_children te)

and walk_stmts ~structs ~ret_ty live stmts =
  (* Two-phase walk so `defer`-bodies see end-of-scope state in LIFO
     order: gather defers as we encounter them; after the regular
     stmts settle the live map, fire each defer body against the
     accumulated state (newest-first, so a later defer's consume
     can affect what an earlier defer sees). *)
  let live, defers =
    List.fold_left (fun (live, defers) s ->
      match s with
      | TDefer { body; _ } -> (live, body :: defers)
      | _ -> (walk_stmt ~structs ~ret_ty live s, defers))
      (live, []) stmts
  in
  List.fold_left (walk_stmts ~structs ~ret_ty) live defers

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
      let then_dvg = stmts_diverge then_body in
      let else_dvg = stmts_diverge else_body in
      (match then_dvg, else_dvg with
       | true, true -> live
       | true, false -> s_else
       | false, true -> s_then
       | false, false -> merge_states s_then s_else)
  | TWhile { cond; body; post } ->
      let live = walk_expr ~structs live cond in
      (* If every fall-through path through the body diverges (break /
         return), no re-iteration happens and the consume is safe.
         Otherwise any Live → Consumed transition would re-consume on
         the next iter — reject. *)
      let after_body = walk_stmts ~structs ~ret_ty live body in
      let after = walk_stmts ~structs ~ret_ty after_body post in
      if not (stmts_diverge body) then
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
