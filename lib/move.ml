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

(* True iff [t] should be tracked by the move-pass.  Borrowed pointers
   (`*T`, `*const T`) are never affine — a borrow can't consume.  The
   `own *T` sigil (DR-030 Faza-1a) marks unique ownership of the
   pointee and is unconditionally affine.  Structs are affine when
   the user marked them `@move` OR when at least one of their
   fields carries ownership (transitive predicate — String/Vec drop
   the `@move` attribute once their `ptr` field becomes `own *u8`). *)
let rec is_affine_typ ~structs t =
  match t with
  | TOwnPtr _ -> true
  | TStruct path ->
      (match List.find_opt
               (fun (s : struct_sig) -> s.sname_path = path) structs with
       | Some s ->
           s.ss_is_move
           || List.exists
                (fun (_, ft) -> is_affine_typ ~structs ft) s.sfields_ty
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
   (literals, struct lits, calls) flow without consuming a binding.

   GATE-2 S4 — a bare field access of an affine type in a by-value
   slot is a move-out-of-field: it would create a second owner of the
   field's storage while the parent struct keeps (and later drops)
   the first.  v1 rejects it outright — move the whole parent or
   borrow instead.  An explicit `as` cast remains the sanctioned
   escape (the prelude's String::build transfers `sb.buf` that way:
   the cast wrapper marks deliberate intent). *)
let consume_var ~structs live (te : texpr) =
  match te.e with
  | TVar n when is_affine_typ ~structs te.ty ->
      set_consumed live n te.pos
  | TFieldAccess _
    when is_affine_typ ~structs te.ty
      && te.pos.Pos.file <> "<prelude>" ->
      (* Prelude bodies (HashMap slot shuffles, String::build's buf
         transfer) move out of fields deliberately as part of their
         hand-written memory management — the reject is a USER-code
         gate. *)
      Error.failf te.pos
        "cannot move %s out of a field — the parent struct still owns \
         (and will drop) this storage; move the whole struct, or borrow \
         the field instead"
        (typ_name te.ty)
  | _ -> live

(* Merge per-branch states — may-consume union (DR-002 S0).  A binding
   ends up Consumed iff Consumed on AT LEAST ONE non-diverging
   fall-through; a diverging branch (return / break / continue /
   `try`-arm) can't reach the post-branch program point so its state
   is dropped BEFORE entering this function (callers filter divergence
   out).  Sound-conservative: `if c { sink(s) }` followed by `s.use()`
   is rejected even though the else-branch left s Live — the move-pass
   can't prove `c` at compile time and reuse on either path is a
   double-consume.  False-positive forces the user to write a
   deterministic consume (or refactor); deliberate trade.  Pos comes
   from the first consuming branch — arbitrary but deterministic. *)
let merge_states a b =
  let names =
    List.sort_uniq compare (List.map fst a @ List.map fst b) in
  List.map (fun n ->
    let sa = try List.assoc n a with Not_found -> Live in
    let sb = try List.assoc n b with Not_found -> Live in
    let merged = match sa, sb with
      | Consumed pa, _ | _, Consumed pa -> Consumed pa
      | Live, Live -> Live
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

(* DR-002 S2 — collect affine bindings introduced by a match-arm
   pattern.  Walks [pat] against the scrutinee's type, resolving
   `TPVariant` binds against the enum sig's variant_sig.vsfields so
   each sub-pattern matches its declared payload-field type.  The
   move-pass seeds each returned `(name, ty)` into the arm's live
   map; without this, `match h { Has(inner)=>{sink(inner);
   inner.free()} }` left `inner` untracked and double-fired
   silently.  `TPOr` contributes nothing (MVP rule: alternatives
   bind zero variables). *)
let rec affine_binds_of_pat ~structs ~enums (scrutinee_ty : typ)
    (p : tpattern) =
  match p with
  | TPWildcard -> []
  | TPVar n ->
      if is_affine_typ ~structs scrutinee_ty then [(n, scrutinee_ty)]
      else []
  | TPVariant { variant; binds; _ } ->
      let enum_path = match scrutinee_ty with
        | TEnum p | TEnumApp { path = p; _ }
        | TPtr (TEnum p) | TConstPtr (TEnum p)
        | TPtr (TEnumApp { path = p; _ })
        | TConstPtr (TEnumApp { path = p; _ }) -> Some p
        | _ -> None
      in
      (match enum_path with
       | None -> []
       | Some path ->
           (match List.find_opt
                    (fun (e : enum_sig) -> e.ename_path = path) enums with
            | None -> []
            | Some esig ->
                (match List.find_opt
                         (fun (v : variant_sig) -> v.vsname = variant)
                         esig.evariants with
                 | None -> []
                 | Some vsig ->
                     List.concat_map (fun (bname, sub_pat) ->
                       match List.assoc_opt bname vsig.vsfields with
                       | None -> []
                       | Some field_ty ->
                           affine_binds_of_pat ~structs ~enums field_ty
                             sub_pat)
                       binds)))
  | TPOr _ -> []

(* Walk each arg left-to-right: validate reads against the in-flight
   state, then mark the binding Consumed if the arg is a bare affine
   TVar.  Shared by every consume-site shape — TCall, aggregate
   literals (TStructLit / TNew / TTupleLit / TEnumLit / TArrayLit),
   etc.  Same fold as the original TCall arm; factored so DR-002 S1
   aggregate-literal consumes stay one-line per shape. *)
let rec consume_args ~structs ~enums live args =
  List.fold_left (fun live arg ->
    let live = walk_expr ~structs ~enums live arg in
    consume_var ~structs live arg)
    live args

(* Walk an expression: validate every TVar read against the current
   state, then descend into sub-expressions.  Sub-call args are
   walked recursively, then each arg's binding is consumed if it's a
   bare affine TVar.  Other shapes (TRef, TFieldAccess, ...) read
   without consuming. *)
and walk_expr ~structs ~enums live (te : texpr) =
  check_reads live te;
  match te.e with
  | TCall { mangled; args } ->
      (* An explicit `x.drop()` releases x's storage even though the
         synthesised drop method borrows its receiver (`*self`): the
         by-ref first arg of a `__drop`-mangled call consumes the
         binding.  This lives HERE (the single consume model) so both
         use-after-drop is rejected and the drop-pass elides the
         auto-drop (GATE-2 / OWN-D2). *)
      let is_drop_mangled m =
        let ends_with s suf =
          let ls = String.length s and lf = String.length suf in
          ls >= lf && String.sub s (ls - lf) lf = suf
        in
        let rec contains i =
          let sub = "__drop_" in
          let ls = String.length m and lsub = String.length sub in
          if i + lsub > ls then false
          else if String.sub m i lsub = sub then true
          else contains (i + 1)
        in
        ends_with m "__drop" || contains 0
      in
      let live = consume_args ~structs ~enums live args in
      (match args with
       | { e = TRef ({ e = TVar n; _ } as inner); _ } :: _
         when is_drop_mangled mangled
           && is_affine_typ ~structs inner.ty ->
           set_consumed live n inner.pos
       | _ -> live)
  | TBuiltinCall { name; args } ->
      (* Built-ins that just read through the pointer (mem_zero ≡
         memset) don't consume their first arg.  Only walk the
         children for any nested affine uses. *)
      let is_non_consuming = match name with
        | "mem_zero" -> true
        | _ -> false
      in
      if is_non_consuming then
        List.fold_left
          (fun live a -> walk_expr ~structs ~enums live a) live args
      else
        consume_args ~structs ~enums live args
  (* DR-002 S1 — aggregate literals shallow-copy each field into the
     fresh value; a bare-TVar affine field aliases the source, so the
     binding must end Consumed.  Without this, `Wrap { f: s }` /
     `Option::Some(s)` / `(s, 1)` / `[s]` / `new Wrap { f: s }` would
     leave `s` Live and a subsequent `take(s)` / `s.free()` would
     silently double-fire at runtime.  `..base` consumes the base
     binding the same way the field args consume — partial-overwrite
     still ships every untouched field through. *)
  | TStructLit { fields; base; _ } | TNew { fields; base; _ } ->
      let live =
        consume_args ~structs ~enums live (List.map snd fields) in
      (match base with
       | Some b ->
           let live = walk_expr ~structs ~enums live b in
           consume_var ~structs live b
       | None -> live)
  | TTupleLit es | TArrayLit es ->
      consume_args ~structs ~enums live es
  | TEnumLit { args; _ } | TNewEnum { args; _ } ->
      consume_args ~structs ~enums live (List.map snd args)
  | TArrayRepeat { value; _ } ->
      let live = walk_expr ~structs ~enums live value in
      (* `[v; N]` codegen emits a fill loop that shallow-copies the
         evaluated value into every slot — for an @move type that
         creates N aliases of the same heap-owning binding (banned
         outright; the user must build N values explicitly).  Refuse
         BEFORE the shallow-copy ships. *)
      if is_affine_typ ~structs value.ty then
        Error.failf value.pos
          "cannot use a @move value in `[expr; N]` — the array-repeat \
           lowering shallow-copies the same value into every slot, \
           aliasing the heap-owning source N times (build each element \
           explicitly or use a non-@move element type)"
      else live
  | TBlock { stmts; trailing } ->
      let live =
        walk_stmts ~structs ~enums ~ret_ty:None live stmts in
      (match trailing with
       | Some e -> walk_expr ~structs ~enums live e
       | None -> live)
  | TMatch { scrutinee; arms; _ } ->
      let live = walk_expr ~structs ~enums live scrutinee in
      (* DR-002 S2 — seed each arm's live map with affine
         pattern-binds drawn from the scrutinee type so the arm body
         tracks `match h { Has(inner)=>... }`'s `inner`.  Filter the
         binds back out of the post-arm contribution before merge —
         arm-local names go out of scope at the arm's closing
         brace, they must not survive into the post-match state.
         DR-002 S3 — partial-move scrutinee.  If an arm consumed
         any affine pattern-bind, the scrutinee carries a stale
         payload pointer; re-using the scrutinee binding (`let _b
         = h`, `take(h)`, `return h`) would alias freed memory.
         Mark the scrutinee TVar Consumed in this arm's
         contribution so the merge-states union (may-consume,
         per S0) raises the post-match state to Consumed even if a
         sibling arm left it Live. *)
      let contributions = List.filter_map (fun (a : tmatch_arm) ->
        let arm_binds =
          affine_binds_of_pat ~structs ~enums scrutinee.ty a.tpat in
        let seeded =
          List.fold_left (fun lv (n, _) -> (n, Live) :: lv)
            live arm_binds in
        Option.iter
          (fun g -> ignore (walk_expr ~structs ~enums seeded g))
          a.tguard;
        let after_full = walk_expr ~structs ~enums seeded a.tbody in
        if a.tdiverges || expr_diverges a.tbody then None
        else
          let bind_names = List.map fst arm_binds in
          let any_bind_consumed =
            List.exists (fun n ->
              match List.assoc_opt n after_full with
              | Some (Consumed _) -> true
              | _ -> false)
              bind_names
          in
          let after =
            List.filter (fun (n, _) -> not (List.mem n bind_names))
              after_full
          in
          let after =
            if any_bind_consumed then
              match scrutinee.e with
              | TVar sn ->
                  (sn, Consumed scrutinee.pos)
                  :: List.remove_assoc sn after
              | _ -> after
            else after
          in Some after)
        arms
      in
      (match contributions with
       | [] -> live
       | s :: rest -> List.fold_left merge_states s rest)
  | TIfExpr { cond; then_val; else_val } ->
      let live = walk_expr ~structs ~enums live cond in
      let s_then = walk_expr ~structs ~enums live then_val in
      let s_else = walk_expr ~structs ~enums live else_val in
      let then_dvg = expr_diverges then_val in
      let else_dvg = expr_diverges else_val in
      (match then_dvg, else_dvg with
       | true, true -> live
       | true, false -> s_else
       | false, true -> s_then
       | false, false -> merge_states s_then s_else)
  | _ ->
      List.fold_left (walk_expr ~structs ~enums)
        live (texpr_children te)

and walk_stmts ~structs ~enums ~ret_ty live stmts =
  (* Two-phase walk so `defer`-bodies see end-of-scope state in LIFO
     order: gather defers as we encounter them; after the regular
     stmts settle the live map, fire each defer body against the
     accumulated state (newest-first, so a later defer's consume
     can affect what an earlier defer sees). *)
  let live, defers =
    List.fold_left (fun (live, defers) s ->
      match s with
      | TDefer { body; _ } -> (live, body :: defers)
      | _ -> (walk_stmt ~structs ~enums ~ret_ty live s, defers))
      (live, []) stmts
  in
  List.fold_left (walk_stmts ~structs ~enums ~ret_ty) live defers

and walk_stmt ~structs ~enums ~ret_ty live = function
  | TLet { name; value; _ } ->
      let live = walk_expr ~structs ~enums live value in
      let live = consume_var ~structs live value in
      if is_affine_typ ~structs value.ty
      then (name, Live) :: live
      else live
  | TLetTuple { value; _ } ->
      (* Tuple destructuring of affine fields is rare today (affine
         types aren't tuples).  Walk reads; consume nothing extra. *)
      walk_expr ~structs ~enums live value
  | TAssign { path; value; _ } ->
      (* GATE-2 L3 — rebind resurrects.  Assigning a fresh value to an
         affine binding makes it Live again: the old value has either
         been moved away (possibly into this very RHS — `s = next(a,
         s)` consumes the old `s` as an argument first) or is dropped
         by the drop-pass (L2) before the store.  This is what lets
         loop-shaped ownership flow (`root = insert(a, root, v)`)
         type-check: the body consumes and rebinds, so the binding is
         Live again at the loop back-edge. *)
      let live = walk_expr ~structs ~enums live value in
      let live = consume_var ~structs live value in
      (match path with
       | [ n ] when List.mem_assoc n live ->
           List.map (fun (k, v) -> if k = n then (k, Live) else (k, v)) live
       | _ -> live)
  | TAssignField { value; _ }
  | TAssignIndex { value; _ }
  | TAssignDeref { value; _ } ->
      let live = walk_expr ~structs ~enums live value in
      consume_var ~structs live value
  | TReturn { value = Some v; _ } ->
      let live = walk_expr ~structs ~enums live v in
      (match ret_ty with
       | Some rt when is_affine_typ ~structs rt -> consume_var ~structs live v
       | _ -> live)
  | TReturn { value = None; _ } -> live
  | TExprStmt e -> walk_expr ~structs ~enums live e
  | TIf { cond; then_body; else_body } ->
      let live = walk_expr ~structs ~enums live cond in
      let s_then = walk_stmts ~structs ~enums ~ret_ty live then_body in
      let s_else = walk_stmts ~structs ~enums ~ret_ty live else_body in
      let then_dvg = stmts_diverge then_body in
      let else_dvg = stmts_diverge else_body in
      (match then_dvg, else_dvg with
       | true, true -> live
       | true, false -> s_else
       | false, true -> s_then
       | false, false -> merge_states s_then s_else)
  | TWhile { cond; body; post } ->
      let live = walk_expr ~structs ~enums live cond in
      (* If every fall-through path through the body diverges (break /
         return), no re-iteration happens and the consume is safe.
         Otherwise any Live → Consumed transition would re-consume on
         the next iter — reject. *)
      let after_body = walk_stmts ~structs ~enums ~ret_ty live body in
      let after = walk_stmts ~structs ~enums ~ret_ty after_body post in
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
let check_fn ~structs ~enums (tf : tfunc) =
  let params = tf.tf_func.Ast.params in
  let init_live =
    List.filter_map (fun (p, ty) ->
      if is_affine_typ ~structs ty then Some (p.Ast.pname, Live) else None)
      (List.combine params tf.tf_param_tys)
  in
  let _ : (string * state) list =
    walk_stmts ~structs ~enums ~ret_ty:tf.tf_ret_ty init_live tf.tf_body
  in
  ()

(* Whole-program entry.  Skipped if no struct in the program is
   marked `@move` — in that case [is_affine_typ] returns false
   everywhere and the walk is a no-op. *)
let check (tp : tprogram) =
  (* DR-045: post-migration most prelude affineness derives from
     `own *T` field ownership rather than the legacy `@move` flag.
     The early-exit predicate now catches either shape — a struct
     marked `@move` OR a struct carrying any affine field. *)
  let any_affine =
    List.exists
      (fun (s : struct_sig) ->
        s.ss_is_move
        || List.exists
             (fun (_, ft) -> is_affine_typ ~structs:tp.tp_struct_index ft)
             s.sfields_ty)
      tp.tp_struct_index
  in
  if any_affine then
    List.iter
      (check_fn ~structs:tp.tp_struct_index ~enums:tp.tp_enum_index)
      tp.tp_funcs
