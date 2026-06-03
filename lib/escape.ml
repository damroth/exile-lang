(* DR-010 escape pass.
   Sound conservative barrier on values whose pointer / Slice payload
   roots in a local binding: catches the four canonical escape-sites
   (TReturn, store through non-local *ptr, container-insert,
   TNew/TStructLit as a return value).

   Faza A (Tier-1 floor, 2026-06-03): wchłonęła S5a-lint; provenance
   trójwartościowy (CallerOrStatic / Local / Unknown); intra-procedural
   walk.

   Faza B (Tier-2 param-SET-summary, 2026-06-03): dodaje czwartą
   wartość [Param of IntSet.t] do prov-domeny + summary-tabelę nad
   zbiorem mangled-name → set indeksów params które pojawiają się
   w wartości zwracanej.  Computowana whole-program-fixpointem
   (least fixed point nad skończonym powerset → terminuje); enforce
   reuses ten sam dataflow, idiomatyczne traversale `exilc.exl`
   (recursive AST-fold) zwracające param-derived wskaźniki zbiegają
   do soundnego `{param}` bez reject (Decyzja #3 DR-010).

   Closes S5b laundering: `vec.as_slice()` summary = {0}, więc
   `return local_vec.as_slice()` reject się strukturalnie.

   v1 deliberately tight:
   * Bez DerivedFrom / borrow-invalidation channel (Tier-3, DR-010 graft C).
   * Bez relacyjnego min(param-regions) → capability-model.
   * Recursive forward-refs przed konwergencją widziane jako bottom
     (Param ∅); fixpoint je odkryje.

   Hatch: `@escapes` na fn suppresses pass dla tej fn — arena/region-
   allocated borrow returns analyser jeszcze nie modeluje.  Skipped fn
   nie wnosi summary; callerzy widzą `Unknown`-style worst-case (over-
   approximation → wszystkie params w summary), sound. *)

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

let prov_eq a b = match a, b with
  | CallerOrStatic, CallerOrStatic
  | Local, Local
  | Unknown, Unknown -> true
  | Param sa, Param sb -> IntSet.equal sa sb
  | _ -> false

(* Whole-program summary: mangled fn name → IntSet of param indices
   that may appear (transitively) in any return value of that fn.
   ∅ summary = "returns nothing borrowed from params" (e.g. fresh
   allocation, integer fold).  Full set = "may return any param".

   Filled by [compute_summaries] then read by [prov_of_call] during
   enforcement.  External fns (no body) never get a summary entry;
   prov_of_call defaults to CallerOrStatic for them — sound because
   FFI surface receives ptr-or-int args and any returned borrow
   originates outside our trust boundary (`mod raw` is the hygiene
   gate per DR-005). *)
let summary : (string, IntSet.t) Hashtbl.t = Hashtbl.create 64

(* Per-function param-name → param-index table, refreshed at the
   start of each fn analysis (compute and enforcement both reset it).
   Lets [prov_of_var] turn a bare `TVar "self"` into `Param {0}`
   without threading the param list everywhere. *)
let params_idx : (string, int) Hashtbl.t = Hashtbl.create 8

(* True iff this expression's lvalue root names a tracked local
   binding.  Shared lvalue walker — mirrors [Lint.lvalue_root].  TDeref
   sub means the rvalue lives through a pointer, so the root is the
   pointer's referent, not a stack slot; treat it as not-local
   (sound-conservative: pointer-rooted borrows aren't auto-local). *)
let rec lvalue_root (te : texpr) =
  match te.e with
  | TVar n -> Some n
  | TFieldAccess { target; _ } -> lvalue_root target
  | TIndex { base; _ } -> lvalue_root base
  | _ -> None

(* TVar lookup: locals win over params (a `let mut self = ...` shadows
   the param), params next, otherwise CallerOrStatic (global, extern,
   module fn). *)
let prov_of_var live n =
  match List.assoc_opt n live with
  | Some p -> p
  | None ->
      (match Hashtbl.find_opt params_idx n with
       | Some i -> Param (IntSet.singleton i)
       | None -> CallerOrStatic)

(* Project a call's return-prov from the callee's summary and the
   caller's argument provs.  Each i ∈ summary[callee] contributes the
   prov of args[i] under meet; ∅ summary → CallerOrStatic. *)
let prov_of_call ~prov_of_arg mangled args =
  match Hashtbl.find_opt summary mangled with
  | None -> CallerOrStatic   (* unknown fn (extern / pre-fixpoint) *)
  | Some idx_set ->
      IntSet.fold (fun i acc ->
        match List.nth_opt args i with
        | Some arg -> meet acc (prov_of_arg arg)
        | None -> acc)
        idx_set CallerOrStatic

(* Compute provenance of [te] in the current state.  Composite literals
   meet over per-field provs; calls go through the summary table. *)
let rec prov_of live (te : texpr) =
  match te.e with
  | TVar n -> prov_of_var live n
  | TRef sub ->
      (* &lvalue — Local iff the lvalue's root names a tracked local;
         param-rooted refs surface as Param {idx} so the borrow's
         lifetime tracks the param the caller owns. *)
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
  | TEnumLit { args; _ } ->
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
  | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _
  | TFnRef _ | TSizeOf _ -> CallerOrStatic
  | TCall { mangled; args } ->
      prov_of_call ~prov_of_arg:(prov_of live) mangled args
  | TBuiltinCall _ | TIndirectCall _ -> CallerOrStatic
  | TBlock { trailing = None; _ } -> CallerOrStatic

(* Container-insert detection — same hardcoded swap-point as Faza A.
   ward / Owner-sigil zamienia tę listę na strukturalny test, gdy
   capability model dochodzi. *)
let container_insert_prefixes = [
  "Vec__push";
  "HashMap__insert";
  "StringBuilder__push_str";
]

let mangled_has_prefix prefix mangled =
  let pl = String.length prefix in
  let ml = String.length mangled in
  ml >= pl && String.sub mangled 0 pl = prefix

let is_container_insert mangled =
  List.exists (fun p -> mangled_has_prefix p mangled)
    container_insert_prefixes

(* Most-local-meet over per-branch states.  Names absent from either
   side default to CallerOrStatic on that side. *)
let merge_states a b =
  let names = List.sort_uniq compare (List.map fst a @ List.map fst b) in
  List.map (fun n ->
    let pa = try List.assoc n a with Not_found -> CallerOrStatic in
    let pb = try List.assoc n b with Not_found -> CallerOrStatic in
    (n, meet pa pb))
    names

(* Escape-site enforcement (compute-phase passes ~report:false to make
   it silent).  Computes prov of the candidate value and raises on
   Local; Param / CallerOrStatic / Unknown flow through. *)
let fail_escape pos kind =
  Error.failf pos
    "%s embeds the address of a local binding — the local goes out \
     of scope at the end of its enclosing block, leaving the caller \
     with a dangling borrow.  Wrap the storage in a caller-owned \
     region, return a copy / `String::with_str(...)` instead of a \
     borrow, or — for arena/region-allocated returns — mark the fn \
     `@escapes` (forward-compat hatch)"
    kind

(* Pure-value types — primitives that don't embed a pointer at all,
   so `return x` where `x : T` for one of these copies the value out
   and can't carry a borrow with it.  Without this filter
   `return *p` (typed as int) drags `p`'s Local prov through the
   deref and trips check_return on what is actually a value copy
   (false positive caught by example/escape_pass.exl).  Pointer-
   carrying composites (TStruct / TTuple with a *T field, TPtr,
   Slice/Vec/String — anything with a `*T` somewhere inside) stay
   checked. *)
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

(* DR-010 graft B (origin-depth shortcut) — intra-frame is sound.
   Receiver rooted in a local binding ⇒ container lives in current
   frame ⇒ borrowing same-frame locals is safe (both die together).
   Param-rooted / global receivers are "outside" — Local args reject. *)
let receiver_root (te : texpr) =
  match te.e with
  | TVar n -> Some n
  | TRef sub | TDeref sub -> lvalue_root sub
  | _ -> lvalue_root te

let receiver_is_local live (recv : texpr) =
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

(* Forward dataflow walk — gathers return-provs into [ret_acc] for
   summary computation, regardless of [report].  When [report] is
   true, escape-sites raise hard errors; when false (compute phase),
   they collect prov silently into ret_acc. *)
let rec walk_stmts ~report ~ret_acc live stmts =
  List.fold_left (walk_stmt ~report ~ret_acc) live stmts

and walk_stmt ~report ~ret_acc live = function
  | TLet { name; value; _ } ->
      let p = prov_of live value in
      walk_expr_for_sites ~report ~ret_acc live value;
      (name, p) :: List.remove_assoc name live
  | TLetTuple { names; value; _ } ->
      let p = prov_of live value in
      walk_expr_for_sites ~report ~ret_acc live value;
      List.fold_left (fun lv n -> (n, p) :: List.remove_assoc n lv)
        live names
  | TAssign { path; value; _ } ->
      walk_expr_for_sites ~report ~ret_acc live value;
      (match path with
       | [n] ->
           let p = prov_of live value in
           (n, p) :: List.remove_assoc n live
       | _ -> live)
  | TAssignField { target; value; pos; _ } ->
      walk_expr_for_sites ~report ~ret_acc live target;
      walk_expr_for_sites ~report ~ret_acc live value;
      check_store_through_pointer ~report live ~target value pos;
      live
  | TAssignIndex { base; index; value; pos } ->
      walk_expr_for_sites ~report ~ret_acc live base;
      walk_expr_for_sites ~report ~ret_acc live index;
      walk_expr_for_sites ~report ~ret_acc live value;
      check_store_through_pointer ~report live ~target:base value pos;
      live
  | TAssignDeref { target; value; pos } ->
      walk_expr_for_sites ~report ~ret_acc live target;
      walk_expr_for_sites ~report ~ret_acc live value;
      check_store_through_pointer ~report live ~target value pos;
      live
  | TReturn { value = Some v; pos } ->
      walk_expr_for_sites ~report ~ret_acc live v;
      if not (ret_typ_is_pure_value v.ty) then begin
        let p = prov_of live v in
        ret_acc := meet !ret_acc p;
        check_return ~report live v pos
      end;
      live
  | TReturn { value = None; _ } -> live
  | TExprStmt e ->
      walk_expr_for_sites ~report ~ret_acc live e;
      live
  | TIf { cond; then_body; else_body } ->
      walk_expr_for_sites ~report ~ret_acc live cond;
      let s_then = walk_stmts ~report ~ret_acc live then_body in
      let s_else = walk_stmts ~report ~ret_acc live else_body in
      merge_states s_then s_else
  | TWhile { cond; body; post } ->
      walk_expr_for_sites ~report ~ret_acc live cond;
      let after_body = walk_stmts ~report ~ret_acc live body in
      walk_stmts ~report ~ret_acc after_body post
  | TFor { lo; hi; body; counter; _ } ->
      walk_expr_for_sites ~report ~ret_acc live lo;
      walk_expr_for_sites ~report ~ret_acc live hi;
      let live_with_counter = (counter, CallerOrStatic) :: live in
      let _ = walk_stmts ~report ~ret_acc live_with_counter body in
      live
  | TForEach { it_var; it_init; body; _ } ->
      walk_expr_for_sites ~report ~ret_acc live it_init;
      let live_in_loop =
        (it_var, prov_of live it_init) :: List.remove_assoc it_var live
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
and walk_expr_for_sites ~report ~ret_acc live (te : texpr) =
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

(* Project the body's accumulated return-prov to a summary IntSet.
   Local → ∅ (the fn will fail enforcement; pre-fail summary is
   irrelevant).  Unknown → all params (over-approximation; the fn's
   summary widens to "may return anything").  Param S → S.
   CallerOrStatic → ∅. *)
let project_to_summary ~num_params = function
  | CallerOrStatic | Local -> IntSet.empty
  | Param s -> s
  | Unknown ->
      let rec range i = if i >= num_params then [] else i :: range (i + 1) in
      IntSet.of_list (range 0)

(* Seed live + params_idx for a fn, walk body, return accumulated
   ret-prov.  Caller decides report and reads ret_acc. *)
let analyze_fn ~report (tf : tfunc) : prov =
  Hashtbl.clear params_idx;
  List.iteri (fun i (p : Ast.param) ->
    Hashtbl.replace params_idx p.pname i)
    tf.tf_func.params;
  let init_live = [] in
  let ret_acc = ref CallerOrStatic in
  let _ = walk_stmts ~report ~ret_acc init_live tf.tf_body in
  !ret_acc

(* Whole-program summary fixpoint.  Initial: every fn → ∅.  Iterate
   per-fn analyze (silent), update summary[mangled], stop when no
   set grows.  Domain: 2^params per fn, finite & monotonic ⇒
   terminates.  Cap iterations at a generous bound — pathological
   programs should hit the upper bound (n_fns * max_params + slack)
   before triggering, and exceeding it would be a compiler bug. *)
let compute_summaries (tp : tprogram) =
  Hashtbl.clear summary;
  (* Compute summaries for prelude too — `Vec::as_slice` / `String::
     as_str` / etc carry the load-bearing param→param-borrow signatures
     for S5b detection.  Enforcement still skips prelude bodies (user
     didn't write them, errors there would be confusing); only the
     summary they expose matters here. *)
  (* Generic skeletons keep `tf_func.tparams = [T]` even on
     monomorphic instances (Mono reuses the source AST and only
     specialises types via `tf_param_tys`).  Use type-level
     concreteness — every param's resolved typ has no `TVar` —
     to tell instances and originally-mono fns apart from
     unsubstituted skeletons. *)
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

(* Whole-program entry.  Skips prelude and extern fns; @escapes fns
   contribute no summary entry (callers see Unknown-style worst case)
   and are not enforced. *)
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
