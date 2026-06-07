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

(* Build a synthetic `<local>.drop()` call statement.  The IR
   shape matches what an elaborated `local.drop()` produces in
   user code, so the rest of the compiler treats it uniformly. *)
let drop_call ~struct_path local_name pos : tstmt =
  let mangled =
    let last = match List.rev struct_path with
      | n :: _ -> n
      | [] -> "anon"
    in
    last ^ "__drop"
  in
  let self_arg = {
    e = TRef {
      e = TVar local_name;
      ty = TStruct struct_path;
      pos;
    };
    ty = TPtr (TStruct struct_path);
    pos;
  } in
  TExprStmt {
    e = TCall { mangled; args = [self_arg] };
    ty = TStruct ["c_void"];
    pos;
  }

(* Resolve a typ → struct_path if it's a TStruct, else None. *)
let struct_path_of = function
  | TStruct p -> Some p
  | _ -> None

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
  match stmt with
  | TLet { name; value; pos } when
      is_affine_typ ~structs value.ty
      && (match struct_path_of value.ty with
          | Some p -> has_drop ~structs p
          | None -> false) ->
      ([stmt], (name, Live, value.ty, pos) :: live)
  | TReturn { value; pos } ->
      let drops = drops_for_live ~structs live in
      (drops @ [TReturn { value; pos }], live)
  | TExprStmt e ->
      (* If the expression is an explicit `<Type>__drop(&local)` call
         (user wrote `local.drop()`), mark that owner Consumed so
         the scope-exit pass doesn't add a second drop call.  Any
         other call against an owner-typed `&local` first arg is
         conservatively treated as consume too — most owner-eating
         methods take `*self` and the worst case here is a missed
         auto-drop, never a double-free. *)
      let consumed_name =
        match e.e with
        | TCall { args = first :: _; _ } ->
            (match first.e with
             | TRef { e = TVar n; _ } -> Some n
             | _ -> None)
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
      ([stmt], live')
  | _ ->
      ([stmt], live)

and drops_for_live ~structs live =
  List.filter_map (fun (name, st, ty, pos) ->
    match st with
    | Live ->
        (match struct_path_of ty with
         | Some p when has_drop ~structs p ->
             Some (drop_call ~struct_path:p name pos)
         | _ -> None)
    | Consumed -> None)
    live

(* Per-fn entry: walk body, then top-up drops for owners that
   reached the end without consume or explicit return. *)
let rewrite_fn ~structs (tf : tfunc) : tfunc =
  let (body', final_live) = walk_stmts ~structs [] tf.tf_body in
  (* If the fn returns nothing explicitly (void), the body ends
     without a return statement — emit drops at the tail. *)
  let body_with_tail =
    if List.exists (function TReturn _ -> true | _ -> false) tf.tf_body
    then body'
    else body' @ drops_for_live ~structs final_live
  in
  { tf with tf_body = body_with_tail }

(* Whole-program entry. *)
let insert (tp : tprogram) : tprogram =
  { tp with
    tp_funcs =
      List.map (rewrite_fn ~structs:tp.tp_struct_index) tp.tp_funcs }
