(* DR-030 Faza-1a Step D — auto-drop insertion pass.

   Runs AFTER Move.check.  Move.check has already verified that
   every affine binding is consumed at most once on every path —
   so the only question left is: for those that are NOT consumed
   on every path, where does the implicit `local.drop()` go?

   Strategy: walk fn body left-to-right with a state mapping
   `name → status` (Live | Consumed).  On every scope-exit
   (TReturn, end-of-function), inject `local.drop()` for every
   Live owner in LIFO declaration order.  Consumed owners are
   skipped (the consume already released the storage).  This pass
   is a pure IR rewrite — `TFn.tf_body` is replaced; nothing else
   in the program shape changes.

   v1 honest limits (OWN-D3 etc.):
     - No conditional-move-out support: if a binding is Live on
       some paths and Consumed on others, the merge picks the
       most-conservative (Live everywhere) — the synth drop
       fires after the conditional too.  Move.check already
       prevented use-after-consume, so the extra drop is at
       worst a wasted call (the field has been zeroed by the
       consuming method).  Future refinement: reject the
       branch-divergent case per OWN-D3 instead of allowing.
     - Cycles in owning-struct graphs are not detected; the
       synth drop walks the field tree depth-first and would
       recurse forever.  v1 trusts the user to keep the graph
       acyclic.  Step F's example exercises one level. *)

open Ir

(* Mirror Move's predicate so the two passes agree on which
   bindings carry ownership. *)
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

(* True iff [path] resolves to a struct that synth'd a `drop`
   method.  Step C synthesises the impl whenever the struct
   carries an `own *T` field.  The mangled drop name is
   `<Struct>__drop_<targ>...`. *)
let has_drop ~structs path =
  match List.find_opt
          (fun (s : struct_sig) -> s.sname_path = path) structs with
  | Some s ->
      List.exists
        (fun (_, ft) -> match ft with TOwnPtr _ -> true | _ -> false)
        s.sfields_ty
  | None -> false

(* Per-binding state. *)
type status = Live | Consumed

(* Emit the inline equivalent of `<Struct>__drop(&local)`: for each
   `own *T` field on the local's struct, fire
   `local.alloc.free_fn(local.alloc.state, local.<field> as *c_void,
   size_of(T) as u32)`.  Inlining sidesteps the Mono dispatch gap
   that would otherwise need each mono-instance struct (Vec_i32,
   HashMap_str_i32, ...) to materialise its own drop body before
   codegen — the synth'd skel impl from Step C never gets
   instantiated unless user code explicitly calls `local.drop()`.

   Going inline also keeps the drop semantics symmetric with how
   the language's other ownership operations lower (allocator-as-
   value seam, no per-type runtime stub).  The synthesised
   `Struct__drop` method survives untouched for user-written
   explicit `local.drop()` — Mono dispatches it the normal way. *)
let cvoid_ptr_ty = TPtr TCVoid
let allocator_ty = TStruct ["Allocator"]

let free_fn_ptr_ty =
  TFnPtr {
    params = [ cvoid_ptr_ty; cvoid_ptr_ty;
               TInt { signed = false; width = Ast.W32 } ];
    ret = None;
  }

let drop_call ~structs ~struct_path local_name pos : tstmt list =
  let sig_opt =
    List.find_opt (fun (s : struct_sig) -> s.sname_path = struct_path) structs
  in
  match sig_opt with
  | None -> []
  | Some s ->
      let local_v = {
        e = TVar local_name;
        ty = TStruct struct_path;
        pos;
      } in
      let alloc_field = {
        e = TFieldAccess { target = local_v; field = "alloc" };
        ty = allocator_ty;
        pos;
      } in
      let alloc_state = {
        e = TFieldAccess { target = alloc_field; field = "state" };
        ty = cvoid_ptr_ty;
        pos;
      } in
      let alloc_free_fn = {
        e = TFieldAccess { target = alloc_field; field = "free_fn" };
        ty = free_fn_ptr_ty;
        pos;
      } in
      let u32_t = TInt { signed = false; width = Ast.W32 } in
      let make_free_for (fname, fty) =
        match fty with
        | TOwnPtr inner ->
            let field_access = {
              e = TFieldAccess { target = local_v; field = fname };
              ty = fty;
              pos;
            } in
            let cast_to_cvoid = {
              e = TCast (field_access, Ast.TyPtr Ast.TyCVoid);
              ty = cvoid_ptr_ty;
              pos;
            } in
            let size_arg = {
              e = TCast (
                { e = TSizeOf inner;
                  ty = TCInt { signed = false };
                  pos },
                Ast.TyInt { signed = false; width = Ast.W32 });
              ty = u32_t;
              pos;
            } in
            Some (TExprStmt {
              e = TIndirectCall {
                fn_expr = alloc_free_fn;
                args = [ alloc_state; cast_to_cvoid; size_arg ];
              };
              ty = TInt { signed = true; width = Ast.W32 };
              pos;
            })
        | _ -> None
      in
      List.filter_map make_free_for s.sfields_ty

(* Resolve a typ → struct_path if it's a TStruct, else None. *)
let struct_path_of = function
  | TStruct p -> Some p
  | _ -> None

(* Per-fn temp-let collector — populated when a TReturn needs
   value-then-drop sequencing.  rewrite_fn resets the ref before
   each function and copies it into tf_lets afterwards so the
   codegen hoist block emits the right declarations. *)
let tmp_decls : (string * typ) list ref = ref []

(* Walk statements with a live owner stack; emit drops before
   scope-exits.  Returns (rewritten_stmts, final_state) where
   final_state records the live owners surviving to the end of
   the block (so the caller can decide whether to drop them
   later). *)
let rec walk_stmts ~structs (live : (string * status * typ * Pos.t) list)
    stmts : tstmt list * (string * status * typ * Pos.t) list =
  let rec go acc live = function
    | [] -> (List.rev acc, live)
    | stmt :: rest ->
        let (stmt', live') = walk_stmt ~structs live stmt in
        go (List.rev_append stmt' acc) live' rest
  in
  go [] live stmts

and walk_stmt ~structs live stmt =
  (* Helper: given an expression, mark the binding it consumes (if
     any) as Consumed.  Shared by TLet RHS (a call as the let value)
     and TExprStmt (a call at statement position).
     `__drop(&local)` and any by-value first-arg call are the two
     shapes Move.check models as consumes. *)
  let is_drop_mangled m =
    let ends_with s suf =
      let ls = String.length s and lf = String.length suf in
      ls >= lf && String.sub s (ls - lf) lf = suf
    in
    let contains_substr s sub =
      let ls = String.length s and lsub = String.length sub in
      let rec go i =
        if i + lsub > ls then false
        else if String.sub s i lsub = sub then true
        else go (i + 1)
      in
      if lsub = 0 || ls < lsub then false else go 0
    in
    ends_with m "__drop" || contains_substr m "__drop_"
  in
  let consume_in_expr e =
    match e with
    | TCall { mangled; args = first :: _ } ->
        (match first.e with
         | TRef { e = TVar n; _ } when is_drop_mangled mangled -> Some n
         | TVar n -> Some n
         | _ -> None)
    | _ -> None
  in
  let mark_consumed n live =
    List.map (fun ((nm, st, ty, pos) as entry) ->
      if nm = n && st = Live then (nm, Consumed, ty, pos)
      else entry)
      live
  in
  match stmt with
  | TLet { name; value; pos } ->
      let live =
        match consume_in_expr value.e with
        | Some n -> mark_consumed n live
        | None -> live
      in
      if is_affine_typ ~structs value.ty
         && (match struct_path_of value.ty with
             | Some p -> has_drop ~structs p
             | None -> false)
      then ([stmt], (name, Live, value.ty, pos) :: live)
      else ([stmt], live)
  | TReturn { value; pos } ->
      (* `return local;` for an affine `local` is a consume — the
         caller takes ownership.  Mark it Consumed before emitting
         the drop list so its drop doesn't fire, otherwise the
         caller and the callee both free the same buffer. *)
      let consumed_name =
        match value with
        | Some { e = TVar n; _ } -> Some n
        | _ -> None
      in
      let live' =
        match consumed_name with
        | Some n ->
            List.map (fun ((nm, st, ty, pos) as entry) ->
              if nm = n && st = Live then (nm, Consumed, ty, pos)
              else entry)
              live
        | None -> live
      in
      let drops = drops_for_live ~structs live' in
      (* Sequencing: drops must run AFTER the return value is
         evaluated but BEFORE the actual return — otherwise
         `return *buf.p;` reads through `buf.p` after its backing
         storage was already freed.  Stash the value in a fresh
         temp, then drop, then return the temp.  Skip the dance
         when the return value is itself a Var (no eval order
         hazard) or when there are no drops to inject. *)
      let needs_temp =
        drops <> [] &&
        (match value with
         | Some { e = TVar _; _ } | None -> false
         | _ -> true)
      in
      if needs_temp then
        match value with
        | Some v ->
            let tmp = Printf.sprintf "__drop_ret_%d_%d"
                pos.Pos.line pos.Pos.col in
            let tmp_let = TLet { name = tmp; value = v; pos } in
            let tmp_var = { e = TVar tmp; ty = v.ty; pos = v.pos } in
            (* Record the temp so gen_function's hoisted-decls block
               emits a declaration for it. *)
            tmp_decls := (tmp, v.ty) :: !tmp_decls;
            ([tmp_let] @ drops
             @ [TReturn { value = Some tmp_var; pos }], live')
        | None -> (drops @ [TReturn { value; pos }], live')
      else
        (drops @ [TReturn { value; pos }], live')
  | TExprStmt e ->
      let live =
        match consume_in_expr e.e with
        | Some n -> mark_consumed n live
        | None -> live
      in
      ([stmt], live)
  | _ ->
      ([stmt], live)

and drops_for_live ~structs live =
  List.concat_map (fun (name, st, ty, pos) ->
    match st with
    | Live ->
        (match struct_path_of ty with
         | Some p when has_drop ~structs p ->
             drop_call ~structs ~struct_path:p name pos
         | _ -> [])
    | Consumed -> [])
    live

(* Per-fn entry: walk body, then top-up drops for owners that
   reached the end without consume or explicit return. *)
let rewrite_fn ~structs (tf : tfunc) : tfunc =
  tmp_decls := [];
  let (body', final_live) = walk_stmts ~structs [] tf.tf_body in
  (* If the fn returns nothing explicitly (void), the body ends
     without a return statement — emit drops at the tail. *)
  let body_with_tail =
    if List.exists (function TReturn _ -> true | _ -> false) tf.tf_body
    then body'
    else body' @ drops_for_live ~structs final_live
  in
  { tf with
    tf_body = body_with_tail;
    tf_lets = tf.tf_lets @ List.rev !tmp_decls }

(* Whole-program entry. *)
let insert (tp : tprogram) : tprogram =
  { tp with
    tp_funcs =
      List.map (rewrite_fn ~structs:tp.tp_struct_index) tp.tp_funcs }
