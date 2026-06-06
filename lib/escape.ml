(* DR-010 escape pass.
   Sound conservative barrier on values whose pointer / Slice payload
   roots in a local binding: catches the four canonical escape-sites
   (TReturn, store through non-local *ptr, container-insert,
   TNew/TStructLit as a return value).

   Phase A (Tier-1 floor, 2026-06-03): subsumed S5a lint; three-value
   provenance (CallerOrStatic / Local / Unknown); intra-procedural walk.

   Phase B (Tier-2 param-SET-summary, 2026-06-03): added `Param of
   IntSet.t` to the prov domain + whole-program summary fixpoint.
   Closed S5b laundering through methods like `vec.as_slice()`.

   Phase C (Tier-3 borrow-invalidation, 2026-06-03): per-binding
   `owners` field tracks which local an inferred borrow root-points
   to; invalidating calls (Vec::push, Vec::grow, HashMap::insert,
   StringBuilder::push_*, String::free, ...) on a tracked owner kill
   every borrow rooted in that owner.  Subsequent reads of a killed
   binding are hard errors.  Closes S5c (`let s = v.as_slice(); v.
   push(x); s[0]`) and S5d (`v.free(); s[0]`).

   v1 deliberately tight:
   * Without a relational min(param-regions) → capability model.
   * Recursive forward-refs before convergence are seen as bottom
     (Param ∅); the fixpoint discovers them.

   Hatch: `@escapes` on a fn suppresses the pass for that fn — for
   arena/region-allocated borrow returns the analyser can't yet
   model.  Skipped fns contribute no summary; callers see an
   Unknown-style worst case (over-approximation → every param in
   summary), sound. *)

open Ir

module IntSet = Set.Make (Int)

(* Provenance domain — lattice over the question "where does this
   value's pointer-content come from?".

   Ordering (bottom → top, increasing escape-toxicity):
     CallerOrStatic ⊏ Param S ⊏ Unknown ⊏ Local

   Meet = least-upper-bound: at composition / branch-merge sites we
   keep the MOST-LOCAL provenance among the inputs, because a single
   tainted source is enough to taint the result. *)
type prov =
  | CallerOrStatic
  | Param of IntSet.t
  | Local
  | Unknown

let meet a b = match a, b with
  | Local, _ | _, Local -> Local
  | Unknown, _ | _, Unknown -> Unknown
  | Param sa, Param sb -> Param (IntSet.union sa sb)
  | Param s, CallerOrStatic | CallerOrStatic, Param s -> Param s
  | CallerOrStatic, CallerOrStatic -> CallerOrStatic

(* Per-binding state.  Carries the value's provenance plus two
   Phase-C tracking channels:

   * [bowners] — local names this binding's pointer-content borrows
     from.  A `let s = v.as_slice()` records `["v"]`; subsequent
     mutation of `v` invalidates `s`.

   * [binvalid] — kill reason if a tracked invalidator ran since
     the binding was created.  None = still alive; Some = subsequent
     reads are a hard error. *)
type binding = {
  bprov : prov;
  bowners : string list;
  binvalid : (string * Pos.t) option;
}

let mk_binding ?(owners = []) prov =
  { bprov = prov; bowners = owners; binvalid = None }

type state = (string * binding) list

(* Whole-program summary: mangled fn name → IntSet of param indices
   that may appear (transitively) in any return value of that fn. *)
let summary : (string, IntSet.t) Hashtbl.t = Hashtbl.create 64

(* Per-function param-name → param-index table, refreshed at the
   start of each fn analysis (compute and enforcement both reset it). *)
let params_idx : (string, int) Hashtbl.t = Hashtbl.create 8

(* Shared lvalue walker — mirrors [Lint.lvalue_root].  TDeref sub
   means the rvalue lives through a pointer, so the root is the
   pointer's referent, not a stack slot; treat as not-local. *)
let rec lvalue_root (te : texpr) =
  match te.e with
  | TVar n -> Some n
  | TFieldAccess { target; _ } -> lvalue_root target
  | TIndex { base; _ } -> lvalue_root base
  | _ -> None

let receiver_root (te : texpr) =
  match te.e with
  | TVar n -> Some n
  | TRef sub | TDeref sub -> lvalue_root sub
  | _ -> lvalue_root te

(* Phase C — methods that return a borrow rooted in the receiver
   (`*self` / `*const self`).  Filter is deliberately narrow: only
   methods whose body literally wraps `self.ptr` / `self.buf` in a
   view struct (or `self`-rooted iterator).  `clone` / `with_str` /
   `with_capacity` return INDEPENDENT storage and must NOT trigger
   owner propagation. *)
let returns_borrow_from_recv_prefixes = [
  "Vec__as_slice";
  "Vec__iter";
  "String__as_str";
  "String__as_slice";
  "StringBuilder__as_slice";
  "HashMap__iter";
]

let mangled_has_prefix prefix mangled =
  let pl = String.length prefix in
  let ml = String.length mangled in
  ml >= pl && String.sub mangled 0 pl = prefix

let returns_borrow_from_receiver mangled =
  List.exists (fun p -> mangled_has_prefix p mangled)
    returns_borrow_from_recv_prefixes

(* TVar lookup for prov: locals win over params, params next, then
   CallerOrStatic.  Doesn't check `binvalid` — that lives in the
   dedicated [check_uses] walker so we can attach a precise message
   at the use site. *)
let prov_of_var (live : state) n =
  match List.assoc_opt n live with
  | Some b -> b.bprov
  | None ->
      (match Hashtbl.find_opt params_idx n with
       | Some i -> Param (IntSet.singleton i)
       | None -> CallerOrStatic)

(* Project a call's return-prov from the callee's summary and the
   caller's argument provs.  Each i ∈ summary[callee] contributes
   the prov of args[i] under meet; ∅ summary → CallerOrStatic. *)
let prov_of_call ~prov_of_arg mangled args =
  match Hashtbl.find_opt summary mangled with
  | None -> CallerOrStatic
  | Some idx_set ->
      IntSet.fold (fun i acc ->
        match List.nth_opt args i with
        | Some arg -> meet acc (prov_of_arg arg)
        | None -> acc)
        idx_set CallerOrStatic

(* Compute provenance of [te] in the current state.  Composite
   literals meet over per-field provs; calls go through the summary
   table. *)
let rec prov_of live (te : texpr) =
  match te.e with
  | TVar n -> prov_of_var live n
  | TRef sub ->
      (match lvalue_root sub with
       | Some n when List.mem_assoc n live -> Local
       | Some n ->
           (match Hashtbl.find_opt params_idx n with
            | Some i -> Param (IntSet.singleton i)
            | None -> CallerOrStatic)
       | None -> CallerOrStatic)
  | TStructLit { fields; base; _ } | TNew { fields; base; _ } ->
      let from_fields =
        List.fold_left
          (fun acc (_, v) -> meet acc (prov_of live v))
          CallerOrStatic fields
      in
      (match base with
       | Some b -> meet from_fields (prov_of live b)
       | None -> from_fields)
  | TEnumLit { args; _ } | TNewEnum { args; _ } ->
      List.fold_left
        (fun acc (_, v) -> meet acc (prov_of live v))
        CallerOrStatic args
  | TTupleLit es | TArrayLit es ->
      List.fold_left
        (fun acc v -> meet acc (prov_of live v))
        CallerOrStatic es
  | TArrayRepeat { value; _ } -> prov_of live value
  | TFieldAccess { target; _ } -> prov_of live target
  | TIndex { base; _ } -> prov_of live base
  | TDeref sub -> prov_of live sub
  | TNeg sub | TBitNot sub | TNot sub | TCast (sub, _) -> prov_of live sub
  | TBinOp (_, l, r) -> meet (prov_of live l) (prov_of live r)
  | TIfExpr { then_val; else_val; _ } ->
      meet (prov_of live then_val) (prov_of live else_val)
  | TMatch { arms; _ } ->
      List.fold_left
        (fun acc (a : tmatch_arm) -> meet acc (prov_of live a.tbody))
        CallerOrStatic arms
  | TBlock { trailing = Some t; _ } -> prov_of live t
  | TIntLit _ | TFloatLit _ | TBoolLit _ | TNullLit | TStringLit _
  | TFnRef _ | TSizeOf _ -> CallerOrStatic
  | TCall { mangled; args } ->
      prov_of_call ~prov_of_arg:(prov_of live) mangled args
  | TBuiltinCall _ | TIndirectCall _ -> CallerOrStatic
  | TBlock { trailing = None; _ } -> CallerOrStatic

(* Phase C — owner-chain propagation.  For a binding that ultimately
   carries a borrow, [owners_of] returns the union of local names
   the borrow root-points to.  Mirrors prov_of's structural recursion;
   diverges on calls (uses the same summary projection) so a chain
   like `let s = pass_through(&v)` correctly records `["v"]` as the
   owner of `s`. *)
let rec owners_of (live : state) (te : texpr) : string list =
  match te.e with
  | TVar n ->
      (match List.assoc_opt n live with
       | Some b -> b.bowners
       | None -> [])
  | TRef sub ->
      (match lvalue_root sub with
       | Some n when List.mem_assoc n live -> [n]
       | _ -> [])
  | TStructLit { fields; base; _ } | TNew { fields; base; _ } ->
      let from_fields =
        List.concat_map (fun (_, v) -> owners_of live v) fields
      in
      let from_base =
        match base with Some b -> owners_of live b | None -> []
      in
      List.sort_uniq compare (from_fields @ from_base)
  | TEnumLit { args; _ } | TNewEnum { args; _ } ->
      List.sort_uniq compare
        (List.concat_map (fun (_, v) -> owners_of live v) args)
  | TTupleLit es | TArrayLit es ->
      List.sort_uniq compare (List.concat_map (owners_of live) es)
  | TArrayRepeat { value; _ } -> owners_of live value
  | TFieldAccess { target; _ } -> owners_of live target
  | TIndex { base; _ } -> owners_of live base
  | TDeref sub | TCast (sub, _) | TNeg sub | TBitNot sub | TNot sub ->
      owners_of live sub
  | TBinOp (_, l, r) ->
      List.sort_uniq compare (owners_of live l @ owners_of live r)
  | TIfExpr { then_val; else_val; _ } ->
      List.sort_uniq compare
        (owners_of live then_val @ owners_of live else_val)
  | TMatch { arms; _ } ->
      List.sort_uniq compare
        (List.concat_map (fun (a : tmatch_arm) ->
          owners_of live a.tbody) arms)
  | TBlock { trailing = Some t; _ } -> owners_of live t
  | TCall { mangled; args } when returns_borrow_from_receiver mangled ->
      (match args with
       | recv :: _ ->
           (match receiver_root recv with
            | Some n when List.mem_assoc n live -> [n]
            | _ -> [])
       | [] -> [])
  | TCall _ | TBuiltinCall _ | TIndirectCall _ -> []
  | _ -> []

(* Hardcoded swap-points for container-insert and invalidating calls.
   ward / Owner-sigil zamienia te listy na strukturalny test, gdy
   capability model dochodzi. *)
let container_insert_prefixes = [
  "Vec__push";
  "HashMap__insert";
  "StringBuilder__push_str";
]

(* Phase C — methods that return a borrow rooted in the receiver
   (`*self` / `*const self`).  A `let s = v.as_slice()` makes `s` a
   tracked borrow of `v`; a subsequent `v.push(...)` / `v.grow()`
   invalidates `s`.

   Filter is deliberately narrow: only methods whose body literally
   wraps `self.ptr` / `self.buf` in a view struct (or `self`-rooted
   iterator).  `clone` / `with_str` / `with_capacity` return
   INDEPENDENT storage (new alloc) and must NOT trigger owner
   propagation — without this filter Phase C false-positives
   `let h2 = h1.clone(); h1.name.free();` as "use h2 after free".

   ward / Owner-sigil będzie source-of-truth gdy capability model
   dochodzi. *)
(* [returns_borrow_from_recv_prefixes] / [returns_borrow_from_receiver]
   defined up top so [owners_of] can use them. *)

(* Phase C — invalidating-mutation list.  A method whose mangled
   name starts with one of these prefixes is treated as "may have
   reallocated / mutated the owner's storage"; every binding rooted
   in the call's receiver is killed.  push/grow/insert grow buffers
   (Vec.ptr/HashMap.buf change → outstanding Slice<T> dangles);
   free releases storage (every outstanding borrow dangles).  The
   list is hardcoded — ward / Owner-sigil will turn it into a
   structural property once the capability model lands. *)
let invalidating_call_prefixes = [
  "Vec__push";
  "Vec__grow";
  "Vec__pop";
  "Vec__clear";
  "HashMap__insert";
  "HashMap__remove";
  "HashMap__grow";
  "StringBuilder__push_str";
  "StringBuilder__push_byte";
  "StringBuilder__push_int";
  "StringBuilder__grow";
  "StringBuilder__free";
  "Vec__free";
  "HashMap__free";
  "String__free";
  "String__clear";
]

let is_container_insert mangled =
  List.exists (fun p -> mangled_has_prefix p mangled)
    container_insert_prefixes

let is_invalidating_call mangled =
  List.exists (fun p -> mangled_has_prefix p mangled)
    invalidating_call_prefixes

(* Short reason for the user-facing message — mostly which method
   killed the borrow.  Falls back to the bare mangled name. *)
let invalidation_reason mangled =
  let strip pat =
    let p = pat ^ "_" in
    let pl = String.length p in
    let ml = String.length mangled in
    if ml >= pl && String.sub mangled 0 pl = p then Some pat
    else if mangled = pat then Some pat
    else None
  in
  match List.find_map strip invalidating_call_prefixes with
  | Some s ->
      (* Render `Vec__push` as `Vec::push`. *)
      let buf = Buffer.create (String.length s) in
      String.iter (fun c ->
        if c = '_' then Buffer.add_char buf ':' else Buffer.add_char buf c)
        s;
      Buffer.contents buf
  | None -> mangled

(* Merge two states branch-wise.  prov: meet.  owners: intersection
   (only owners agreed on by both branches survive — sound for
   propagation; if one branch lost an owner, the post-merge view
   shouldn't pretend it's still tracked).  invalid: union (if either
   branch killed the binding, the post-merge view treats it as killed
   — strictest, minimises silent UB; the user-facing message
   inherits whichever branch killed first deterministically). *)
let merge_owners a b =
  List.filter (fun n -> List.mem n b) a

let merge_states (a : state) (b : state) : state =
  let names = List.sort_uniq compare (List.map fst a @ List.map fst b) in
  List.map (fun n ->
    let bind_a = try Some (List.assoc n a) with Not_found -> None in
    let bind_b = try Some (List.assoc n b) with Not_found -> None in
    match bind_a, bind_b with
    | Some ba, Some bb ->
        let prov = meet ba.bprov bb.bprov in
        let owners = merge_owners ba.bowners bb.bowners in
        let invalid =
          match ba.binvalid, bb.binvalid with
          | Some r, _ | _, Some r -> Some r
          | None, None -> None
        in
        (n, { bprov = prov; bowners = owners; binvalid = invalid })
    | Some ba, None -> (n, ba)
    | None, Some bb -> (n, bb)
    | None, None -> assert false)
    names

(* Phase C — invalidate every binding whose owner-chain contains
   [owner], stamping the kill site with [reason] + [pos]. *)
let invalidate_borrows_of (live : state) (owner : string) ~reason ~pos =
  List.map (fun (n, b) ->
    if b.binvalid = None && List.mem owner b.bowners
    then (n, { b with binvalid = Some (reason, pos) })
    else (n, b))
    live

(* Escape-site enforcement.  Compute-phase passes ~report:false to
   make it silent. *)
let fail_escape pos kind =
  Error.failf pos
    "%s embeds the address of a local binding — the local goes out \
     of scope at the end of its enclosing block, leaving the caller \
     with a dangling borrow.  Wrap the storage in a caller-owned \
     region, return a copy / `String::with_str(...)` instead of a \
     borrow, or — for arena/region-allocated returns — mark the fn \
     `@escapes` (forward-compat hatch)"
    kind

let ret_typ_is_pure_value = function
  | TInt _ | TBool | TString
  | TCInt _ | TCShort _ | TCLong _
  | TCChar | TCSChar | TCUChar -> true
  | _ -> false

let check_return ~report live (te : texpr) (pos : Pos.t) =
  if report && not (ret_typ_is_pure_value te.ty) then
    match prov_of live te with
    | Local -> fail_escape pos "returning a value that"
    | CallerOrStatic | Param _ | Unknown -> ()

let check_store_through_pointer ~report live ~target value pos =
  if report then begin
    let dest_is_local =
      match lvalue_root target with
      | Some n -> List.mem_assoc n live
      | None -> false
    in
    if not dest_is_local then
      match prov_of live value with
      | Local -> fail_escape pos "storing through a non-local pointer a value that"
      | CallerOrStatic | Param _ | Unknown -> ()
  end

let receiver_is_local (live : state) (recv : texpr) =
  match receiver_root recv with
  | Some n -> List.mem_assoc n live
  | None -> false

let check_container_insert ~report live (args : texpr list) pos =
  if report then
    match args with
    | recv :: _ when receiver_is_local live recv -> ()
    | _ :: storing_args ->
        List.iter (fun arg ->
          match prov_of live arg with
          | Local ->
              fail_escape pos
                "inserting into a non-local container an argument that"
          | CallerOrStatic | Param _ | Unknown -> ())
          storing_args
    | [] -> ()

(* Phase C — use-after-invalidation check.  Walks any expression
   reachable from an enforcement site; a TVar whose binding has a
   stamped [binvalid] is a hard error pointing at the kill site. *)
let fail_use_after_invalidation pos name reason kill_pos =
  Error.failf pos
    "use of borrow '%s' after it was invalidated by '%s' at %s:%d:%d \
     — growing / freeing the owner reallocates the buffer the borrow \
     pointed into, so subsequent reads dangle (rebuild the borrow \
     after the mutation, or use a copy that doesn't share the \
     buffer)"
    name reason kill_pos.Pos.file kill_pos.Pos.line kill_pos.Pos.col

let rec check_uses ~report (live : state) (te : texpr) =
  if report then begin
    (match te.e with
     | TVar n ->
         (match List.assoc_opt n live with
          | Some { binvalid = Some (reason, kill_pos); _ } ->
              fail_use_after_invalidation te.pos n reason kill_pos
          | _ -> ())
     | _ -> ());
    List.iter (check_uses ~report live) (texpr_children te)
  end

(* Forward dataflow walk. *)
let rec walk_stmts ~report ~ret_acc (live : state) stmts : state =
  List.fold_left (walk_stmt ~report ~ret_acc) live stmts

and walk_stmt ~report ~ret_acc (live : state) = function
  | TLet { name; value; _ } ->
      check_uses ~report live value;
      let p = prov_of live value in
      let owners = owners_of live value in
      walk_expr_for_sites ~report ~ret_acc live value;
      let live = apply_call_effects live value in
      (name, mk_binding ~owners p) :: List.remove_assoc name live
  | TLetTuple { names; value; _ } ->
      check_uses ~report live value;
      let p = prov_of live value in
      let owners = owners_of live value in
      walk_expr_for_sites ~report ~ret_acc live value;
      let live = apply_call_effects live value in
      List.fold_left
        (fun lv n -> (n, mk_binding ~owners p) :: List.remove_assoc n lv)
        live names
  | TAssign { path; value; _ } ->
      check_uses ~report live value;
      walk_expr_for_sites ~report ~ret_acc live value;
      let live = apply_call_effects live value in
      (match path with
       | [n] ->
           let p = prov_of live value in
           let owners = owners_of live value in
           (n, mk_binding ~owners p) :: List.remove_assoc n live
       | _ -> live)
  | TAssignField { target; value; pos; _ } ->
      check_uses ~report live target;
      check_uses ~report live value;
      walk_expr_for_sites ~report ~ret_acc live target;
      walk_expr_for_sites ~report ~ret_acc live value;
      check_store_through_pointer ~report live ~target value pos;
      apply_call_effects (apply_call_effects live target) value
  | TAssignIndex { base; index; value; pos } ->
      check_uses ~report live base;
      check_uses ~report live index;
      check_uses ~report live value;
      walk_expr_for_sites ~report ~ret_acc live base;
      walk_expr_for_sites ~report ~ret_acc live index;
      walk_expr_for_sites ~report ~ret_acc live value;
      check_store_through_pointer ~report live ~target:base value pos;
      apply_call_effects
        (apply_call_effects (apply_call_effects live base) index)
        value
  | TAssignDeref { target; value; pos } ->
      check_uses ~report live target;
      check_uses ~report live value;
      walk_expr_for_sites ~report ~ret_acc live target;
      walk_expr_for_sites ~report ~ret_acc live value;
      check_store_through_pointer ~report live ~target value pos;
      apply_call_effects (apply_call_effects live target) value
  | TReturn { value = Some v; pos } ->
      check_uses ~report live v;
      walk_expr_for_sites ~report ~ret_acc live v;
      if not (ret_typ_is_pure_value v.ty) then begin
        let p = prov_of live v in
        ret_acc := meet !ret_acc p;
        check_return ~report live v pos
      end;
      apply_call_effects live v
  | TReturn { value = None; _ } -> live
  | TExprStmt e ->
      check_uses ~report live e;
      walk_expr_for_sites ~report ~ret_acc live e;
      apply_call_effects live e
  | TIf { cond; then_body; else_body } ->
      check_uses ~report live cond;
      walk_expr_for_sites ~report ~ret_acc live cond;
      let live = apply_call_effects live cond in
      let s_then = walk_stmts ~report ~ret_acc live then_body in
      let s_else = walk_stmts ~report ~ret_acc live else_body in
      merge_states s_then s_else
  | TWhile { cond; body; post } ->
      check_uses ~report live cond;
      walk_expr_for_sites ~report ~ret_acc live cond;
      let live = apply_call_effects live cond in
      let after_body = walk_stmts ~report ~ret_acc live body in
      walk_stmts ~report ~ret_acc after_body post
  | TFor { lo; hi; body; counter; _ } ->
      check_uses ~report live lo;
      check_uses ~report live hi;
      walk_expr_for_sites ~report ~ret_acc live lo;
      walk_expr_for_sites ~report ~ret_acc live hi;
      let live_with_counter =
        (counter, mk_binding CallerOrStatic) :: live in
      let _ = walk_stmts ~report ~ret_acc live_with_counter body in
      live
  | TForEach { it_var; it_init; body; _ } ->
      check_uses ~report live it_init;
      walk_expr_for_sites ~report ~ret_acc live it_init;
      let p = prov_of live it_init in
      let owners = owners_of live it_init in
      let live_in_loop =
        (it_var, mk_binding ~owners p) :: List.remove_assoc it_var live
      in
      let _ = walk_stmts ~report ~ret_acc live_in_loop body in
      live
  | TDefer { body; _ } ->
      let _ = walk_stmts ~report ~ret_acc live body in
      live
  | TBreak _ | TContinue _ -> live

(* Pre-order scan over the expression tree for nested escape-sites:
   container-insert calls live anywhere; TBlock and TMatch arm-bodies
   carry stmt-level escape-sites that need the dataflow walker. *)
and walk_expr_for_sites ~report ~ret_acc (live : state) (te : texpr) =
  (match te.e with
   | TCall { mangled; args } when is_container_insert mangled ->
       check_container_insert ~report live args te.pos
   | TBuiltinCall { name; args } when is_container_insert name ->
       check_container_insert ~report live args te.pos
   | _ -> ());
  List.iter (walk_expr_for_sites ~report ~ret_acc live) (texpr_children te);
  match te.e with
  | TBlock { stmts; _ } ->
      let _ = walk_stmts ~report ~ret_acc live stmts in
      ()
  | TMatch { arms; _ } ->
      List.iter (fun (a : tmatch_arm) ->
        Option.iter (walk_expr_for_sites ~report ~ret_acc live) a.tguard;
        walk_expr_for_sites ~report ~ret_acc live a.tbody)
        arms
  | _ -> ()

(* Phase C — discover invalidating calls anywhere in [te] and kill
   borrows of the matched receiver.  Applied AFTER the use/escape
   checks ran on [te], so a use of an already-invalidated binding
   nested inside the same expression still reports correctly first.
   The order [child receiver first, then parent's own call] means
   `vec.push(some_method(other_vec))` invalidates `other_vec` first,
   then `vec` — matches evaluation order and lets a follow-up read
   in the same statement see both kills. *)
and apply_call_effects (live : state) (te : texpr) : state =
  let live =
    List.fold_left apply_call_effects live (texpr_children te) in
  match te.e with
  | TCall { mangled; args } when is_invalidating_call mangled ->
      (match args with
       | recv :: _ ->
           (match receiver_root recv with
            | Some owner when List.mem_assoc owner live ->
                invalidate_borrows_of live owner
                  ~reason:(invalidation_reason mangled) ~pos:te.pos
            | _ -> live)
       | [] -> live)
  | _ -> live

(* Project the body's accumulated return-prov to a summary IntSet. *)
let project_to_summary ~num_params = function
  | CallerOrStatic | Local -> IntSet.empty
  | Param s -> s
  | Unknown ->
      let rec range i = if i >= num_params then [] else i :: range (i + 1) in
      IntSet.of_list (range 0)

let analyze_fn ~report (tf : tfunc) : prov =
  Hashtbl.clear params_idx;
  List.iteri (fun i (p : Ast.param) ->
    Hashtbl.replace params_idx p.pname i)
    tf.tf_func.params;
  let init_live : state = [] in
  let ret_acc = ref CallerOrStatic in
  let _ = walk_stmts ~report ~ret_acc init_live tf.tf_body in
  !ret_acc

let compute_summaries (tp : tprogram) =
  Hashtbl.clear summary;
  let is_concrete_instance (tf : tfunc) =
    List.for_all is_concrete tf.tf_param_tys
    && (match tf.tf_ret_ty with
        | Some t -> is_concrete t
        | None -> true)
  in
  let analyzable = List.filter (fun tf ->
    not tf.tf_func.is_extern
    && is_concrete_instance tf
    && not tf.tf_func.escapes_hatch)
    tp.tp_funcs
  in
  let max_iter =
    let n = List.length analyzable in
    let p = List.fold_left (fun m tf ->
      max m (List.length tf.tf_func.params))
      0 analyzable
    in
    (n * (p + 1)) + 16
  in
  let rec loop i =
    if i > max_iter then
      failwith "internal: DR-010 escape summary fixpoint failed to converge"
    else begin
      let changed = ref false in
      List.iter (fun tf ->
        let np = List.length tf.tf_func.params in
        let ret = analyze_fn ~report:false tf in
        let new_set = project_to_summary ~num_params:np ret in
        let old_set =
          try Hashtbl.find summary tf.tf_mangled
          with Not_found -> IntSet.empty
        in
        if not (IntSet.equal new_set old_set) then begin
          Hashtbl.replace summary tf.tf_mangled new_set;
          changed := true
        end)
        analyzable;
      if !changed then loop (i + 1)
    end
  in
  loop 0

let check (tp : tprogram) =
  compute_summaries tp;
  List.iter (fun tf ->
    if tf.tf_func.pos.file = "<prelude>" then ()
    else if tf.tf_func.is_extern then ()
    else if tf.tf_func.escapes_hatch then ()
    else
      let _ : prov = analyze_fn ~report:true tf in
      ())
    tp.tp_funcs
