(* GATE-2 (2026-06-09) — unified auto-drop / own-lifecycle pass.

   Runs AFTER Move.check.  OWN-D2's contract is that this pass and
   the move-pass agree on every consume: a binding the move-pass saw
   moved away must never be dropped here (double-free), and a binding
   it saw Live must always be released exactly once (no silent leak).
   The original implementation kept its own, much narrower consume
   detector and diverged (UAF / double-free / leak — review
   2026-06-09); this version DELEGATES all expression-level consume
   detection to `Move.walk_expr` itself, so there is one consume
   model in the compiler, not two.

   Tracked bindings (the only entries in the state):
     - struct values whose field tree transitively contains an
       `own *T` field (`has_drop_deep`) — released by walking the
       field tree depth-first and firing each level's
       `alloc.free_fn` (the own-field ⇒ sibling-`alloc` invariant
       holds at every level, enforced at struct declaration);
     - bare `own *T` bindings — released through their PROVENANCE
       allocator, remembered from the `new(a)` / `a.alloc()` site.
       Unknown provenance (own received from a call) cannot be
       auto-released: leaving it Live at scope exit is a hard error
       (never consumed — free it, move it, or return it).

   Lifecycle rules (L1–L3, ratified 2026-06-09):
     - L1: Live at scope exit → auto-release (known provenance) or
       reject (unknown).
     - L2: assignment over a Live tracked binding drops the old
       value first.
     - L3: assignment to a Consumed tracked binding resurrects it
       (move.ml side accepts the rebind; here it simply becomes
       Live again, with fresh provenance).

   OWN-D3: a tracked binding Consumed on one non-diverging branch
   and Live on a sibling branch is rejected — drops are static,
   there are no runtime drop-flags.

   v1 honest limits:
     - `break`/`continue` skip the loop-block's local drops (the
       loop-local affine binding leaks on that path);
     - lets inside match-arm TBlocks are not walked (carry-over);
     - a defer body that READS (without consuming) a binding this
       pass auto-drops runs after the drop in C — reorder manually;
     - own-pointee inner fields are not recursed (OWN-D5 shallow);
     - prelude fns are skipped entirely: they predate auto-drop and
       manage memory by hand. *)

open Ir

(* ---------- consume delegation (the OWN-D2 seam) ---------- *)

(* Names from [names] consumed by evaluating [te], per the move-pass's
   own walker.  Replaying Move.walk_expr on an all-Live seed cannot
   raise: any consume-then-read inside one expression would have
   failed Move.check on this same program already. *)
let states_by ~structs ~enums names (te : texpr)
  : string list * string list =
  if names = [] then ([], [])
  else
    let live = List.map (fun n -> (n, Move.Live)) names in
    Move.replaying := true;
    let after =
      Fun.protect ~finally:(fun () -> Move.replaying := false)
        (fun () -> Move.walk_expr ~structs ~enums live te)
    in
    let consumed =
      List.filter_map
        (function (n, Move.Consumed _) -> Some n | _ -> None) after
      |> List.sort_uniq compare
    in
    let partial =
      List.filter_map
        (function (n, Move.PartialMoved _) -> Some n | _ -> None) after
      |> List.sort_uniq compare
    in
    (consumed, partial)

let consumed_by ~structs ~enums names (te : texpr) : string list =
  fst (states_by ~structs ~enums names te)

(* Defer bodies are stmt lists; flatten to their top-level exprs in
   order (defer bodies cannot contain `defer`/`return`). *)
let stmt_exprs = function
  | TLet { value; _ } | TLetTuple { value; _ }
  | TAssign { value; _ } | TAssignDeref { value; _ }
  | TExprStmt value -> [ value ]
  | TAssignField { target; value; _ } -> [ target; value ]
  | TAssignIndex { base; index; value; _ } -> [ base; index; value ]
  | _ -> []

let consumed_by_stmts ~structs ~enums names stmts : string list =
  List.concat_map
    (fun s -> List.concat_map (consumed_by ~structs ~enums names)
        (stmt_exprs s))
    stmts
  |> List.sort_uniq compare

(* ---------- droppability ---------- *)

let find_struct ~structs path =
  List.find_opt (fun (s : struct_sig) -> s.sname_path = path) structs

(* Transitive: the struct's field tree (through struct-VALUE fields)
   reaches an `own *T`.  This is the droppable subset of
   Move.is_affine_typ — `@move`-only structs are affine but carry
   nothing to release, and tracking them here would change their
   (deliberately may-consume) branch semantics. *)
let rec has_drop_deep ~structs t =
  match t with
  | TStruct path ->
      (match find_struct ~structs path with
       | Some s ->
           List.exists
             (fun (_, ft) -> match ft with
                | TOwnPtr _ -> true
                | TStruct _ -> has_drop_deep ~structs ft
                | _ -> false)
             s.sfields_ty
       | None -> false)
  | _ -> false

(* ---------- state ---------- *)

type status =
  | Live
  | Consumed
  | Partial
      (* payload moved out through a match on an owned tree (move.ml
         PartialMoved) — the storage still needs an explicit
         `free(alloc, x)`; auto-drop would re-walk the stale payload *)

type kind =
  | StructDrop of string list                  (* droppable struct path *)
  | ArrayDrop of { elem_path : string list; size : int }
      (* fixed array of self-sufficient droppable structs — each
         element dropped in place (unrolled; stack storage itself
         needs no free) *)
  | BareOwn of { pointee : typ; prov : string option }

type entry = {
  ename : string;
  status : status;
  kind : kind;
  epos : Pos.t;
}

let names_of st = List.map (fun e -> e.ename) st

let mark_consumed names st =
  List.map (fun e ->
    match e.status with
    | (Live | Partial) when List.mem e.ename names ->
        { e with status = Consumed }
    | _ -> e)
    st

let mark_partial names st =
  List.map (fun e ->
    if e.status = Live && List.mem e.ename names
    then { e with status = Partial }
    else e)
    st

let apply_expr ~structs ~enums st (te : texpr) =
  let (consumed, partial) =
    states_by ~structs ~enums (names_of st) te in
  mark_partial partial (mark_consumed consumed st)

(* OWN-D2 consume parity for a transfer statement's RHS.  [apply_expr]
   replays only Move.walk_EXPR, which does not consume a top-level bare
   affine TVar the way Move.walk_stmt's `consume_var` does at every
   assignment/let/return site.  Without this, `let y = x` / `y = x` /
   `s.f = x` leave `x` Live and the pass frees it at scope exit — a
   double-free of storage the transfer moved into the destination.  Mark
   it Consumed so Drop and Move agree.  (Dropping a destination's OLD
   owned value before the overwrite is handled per-site where tracked;
   an untracked destination field still leaks the old value — a
   documented same-family limit.) *)
let consume_rhs_var ~structs st (value : texpr) =
  match value.e with
  | TVar rn when Move.is_affine_typ ~structs value.ty -> mark_consumed [ rn ] st
  | _ -> st

(* ---------- drop emission ---------- *)

let cvoid_ptr_ty = TPtr TCVoid
let allocator_ty = TStruct ["Allocator"]
let u32_ty = TInt { signed = false; width = Ast.W32 }

let free_fn_ptr_ty =
  TFnPtr {
    params = [ cvoid_ptr_ty; cvoid_ptr_ty; u32_ty ];
    ret = None;
  }

let size_of_as_u32 inner pos = {
  e = TCast (
    { e = TSizeOf inner; ty = TCInt { signed = false }; pos },
    Ast.TyInt { signed = false; width = Ast.W32 });
  ty = u32_ty; pos;
}

(* `(alloc.free_fn)(alloc.state, ptr as *c_void, bytes)` *)
let free_via ~alloc_expr ~ptr_expr ~bytes pos =
  let alloc_state = {
    e = TFieldAccess { target = alloc_expr; field = "state" };
    ty = cvoid_ptr_ty; pos;
  } in
  let alloc_free_fn = {
    e = TFieldAccess { target = alloc_expr; field = "free_fn" };
    ty = free_fn_ptr_ty; pos;
  } in
  let cast_to_cvoid = {
    e = TCast (ptr_expr, Ast.TyPtr Ast.TyCVoid);
    ty = cvoid_ptr_ty; pos;
  } in
  TExprStmt {
    e = TIndirectCall {
      fn_expr = alloc_free_fn;
      args = [ alloc_state; cast_to_cvoid; bytes ];
    };
    ty = TInt { signed = true; width = Ast.W32 };
    pos;
  }

(* ---------- B8: deep-drop glue (recursive by construction) ---------- *)

(* Freeze-audit B8 root: the old emission was SHALLOW — one free per
   direct own-field, never recursing into pointees or container
   elements.  Owning enum trees leaked every child, Vec<String> leaked
   every element buffer, HashMap<String, V> never released a key.

   The structural fix: for every type whose ownership graph needs
   recursion or iteration, synthesize ONE real drop function (post-mono
   IR, registered into tp_funcs at the end of the pass, emitted and
   DCE'd like any other fn):

     __drop_ptr_<T>(a: Allocator, p: own *T)
         struct pointee: drop its owned innards in place, then free p;
         enum pointee:   match on *p, recurse into owned payloads
                         (children of the same tree call this very
                         function), then free p.
     __drop_vec_<inst>(v: *Vec_inst)
         drop each of the count elements in place, then free backing.
     __drop_hm_<inst>(m: *HashMap_inst)
         drop key/value of every Occupied slot, then free the slots.

   Allocator sourcing: a struct's own-field frees through its sibling
   `alloc` (declaration invariant); a tree's children free through the
   SAME allocator as the root (single-allocator-per-tree assumption —
   documented; arenas make it trivially true); Vec/HashMap elements of
   bare-own type free through the container's allocator (same
   assumption), while self-sufficient elements (String et al.) carry
   their own. *)

let glue_tbl : (string, string) Hashtbl.t = Hashtbl.create 16
let glue_queue : (string * string) Queue.t = Queue.create ()
    (* (kind-key, fn-name); kind-key = "ptr:<mangled>" / "vec:<path>"
       / "hm:<path>" — the builder decodes it back via glue_meta. *)
let glue_meta : (string, typ) Hashtbl.t = Hashtbl.create 16
let glue_fns : tfunc list ref = ref []

let glue_reset () =
  Hashtbl.reset glue_tbl;
  Queue.clear glue_queue;
  Hashtbl.reset glue_meta;
  glue_fns := []

let prelude_pos = { Pos.file = "<prelude>"; line = 1; col = 1 }

(* Does dropping a VALUE of this type require real work?  (Transitive;
   enums recurse through their payloads, with a visited set for
   recursive trees.) *)
let rec droppable_deep ~structs ~enums ?(visited = []) t =
  match t with
  | TStruct _ -> has_drop_deep ~structs t
  | TEnum p ->
      if List.mem p visited then false
      else
        (match List.find_opt
                 (fun (e : enum_sig) -> e.ename_path = p) enums with
         | None -> false
         | Some es ->
             List.exists
               (fun (v : variant_sig) ->
                 List.exists
                   (fun (_, ft) ->
                     match ft with
                     | TOwnPtr _ -> true
                     | _ ->
                         droppable_deep ~structs ~enums
                           ~visited:(p :: visited) ft)
                   v.vsfields)
               es.evariants)
  | _ -> false

let request_glue ~key ~prefix (meta : typ) : string =
  match Hashtbl.find_opt glue_tbl key with
  | Some name -> name
  | None ->
      let name =
        Printf.sprintf "__drop_%s_%d" prefix (Hashtbl.length glue_tbl) in
      Hashtbl.replace glue_tbl key name;
      Hashtbl.replace glue_meta key meta;
      Queue.push (key, name) glue_queue;
      name

let request_ptr_glue pointee =
  request_glue ~key:("ptr:" ^ mangle_typ pointee) ~prefix:"ptr" pointee

let request_vec_glue inst_path =
  request_glue ~key:("vec:" ^ String.concat "::" inst_path) ~prefix:"vec"
    (TStruct inst_path)

let request_hm_glue inst_path =
  request_glue ~key:("hm:" ^ String.concat "::" inst_path) ~prefix:"hm"
    (TStruct inst_path)

(* Release everything [base] (a droppable struct value) owns: free its
   direct `own *T` fields through the SAME level's `alloc` field
   (recursing through the pointee first when it owns things itself),
   then recurse into droppable struct-value fields.  Vec / HashMap
   instances with droppable contents divert to their synthesized glue
   so elements are dropped too (B8). *)
let rec drop_stmts_for_struct ?(skip=None) ~structs ~enums (base : texpr) path
    pos : tstmt list =
  match find_struct ~structs path with
  | None -> []
  | Some s ->
      let elem_of_own_field fname =
        match List.assoc_opt fname s.sfields_ty with
        | Some (TOwnPtr inner) -> Some inner
        | _ -> None
      in
      let base_ref =
        match base.ty with
        | TPtr _ | TOwnPtr _ | TConstPtr _ -> base
        | _ -> { e = TRef base; ty = TPtr base.ty; pos }
      in
      let vec_like =
        Mono.is_instance_of ["Vec"] path
        || Mono.is_instance_of ["StringBuilder"] path
      in
      let elem_droppable inner =
        droppable_deep ~structs ~enums inner
        || (match inner with TOwnPtr _ -> true | _ -> false)
      in
      (match path, elem_of_own_field "ptr", elem_of_own_field "slots" with
       | _, Some elem, _ when vec_like && elem_droppable elem ->
           let g = request_vec_glue path in
           [ TExprStmt {
               e = TCall { mangled = g; args = [ base_ref ] };
               ty = TInt { signed = true; width = Ast.W32 }; pos } ]
       | _, _, Some (TStruct _ as slot_t)
         when Mono.is_instance_of ["HashMap"] path
           && (match find_struct ~structs
                       (match slot_t with TStruct sp -> sp | _ -> [])
               with
               | Some slot_s ->
                   List.exists (fun (fn, ft) ->
                     (fn = "key" || fn = "value")
                     && elem_droppable ft)
                     slot_s.sfields_ty
               | None -> false) ->
           let g = request_hm_glue path in
           [ TExprStmt {
               e = TCall { mangled = g; args = [ base_ref ] };
               ty = TInt { signed = true; width = Ast.W32 }; pos } ]
       | _ ->
           let alloc_field = {
             e = TFieldAccess { target = base; field = "alloc" };
             ty = allocator_ty; pos;
           } in
           let has_field name =
             List.exists (fun (n, _) -> n = name) s.sfields_ty in
           let field_u32 name = {
             e = TFieldAccess { target = base; field = name };
             ty = u32_ty; pos;
           } in
           (* Byte-count heuristic (DR-046): size-tracking allocators
              get the real footprint back.  Shape from sibling fields:
                cap -> cap * size_of(elem)   (growable buffers)
                len -> len + 1               (NUL-terminated: String)
                else -> size_of(elem)        (single-element) *)
           let bytes_expr inner =
             if has_field "cap" then
               { e = TBinOp (Ast.Mul, field_u32 "cap",
                             size_of_as_u32 inner pos);
                 ty = u32_ty; pos }
             else if has_field "len" then
               { e = TBinOp (Ast.Add, field_u32 "len",
                             { e = TIntLit 1; ty = u32_ty; pos });
                 ty = u32_ty; pos }
             else size_of_as_u32 inner pos
           in
           List.concat_map (fun (fname, fty) ->
             if Some fname = skip then [] else
             match fty with
             | TOwnPtr inner ->
                 let field_access = {
                   e = TFieldAccess { target = base; field = fname };
                   ty = fty; pos;
                 } in
                 if droppable_deep ~structs ~enums inner then
                   let g = request_ptr_glue inner in
                   [ TExprStmt {
                       e = TCall { mangled = g;
                                   args = [ alloc_field; field_access ] };
                       ty = TInt { signed = true; width = Ast.W32 };
                       pos } ]
                 else
                   [ free_via ~alloc_expr:alloc_field
                       ~ptr_expr:field_access
                       ~bytes:(bytes_expr inner) pos ]
             | TStruct p2 when has_drop_deep ~structs fty ->
                 let sub_base = {
                   e = TFieldAccess { target = base; field = fname };
                   ty = fty; pos;
                 } in
                 drop_stmts_for_struct ~structs ~enums sub_base p2 pos
             | _ -> [])
             s.sfields_ty)

let drop_stmts_for_entry ~structs ~enums (e : entry) : tstmt list =
  match e.kind with
  | StructDrop path ->
      let base = { e = TVar e.ename; ty = TStruct path; pos = e.epos } in
      drop_stmts_for_struct ~structs ~enums base path e.epos
  | BareOwn { pointee; prov = Some a } ->
      let alloc_expr = { e = TVar a; ty = allocator_ty; pos = e.epos } in
      let ptr_expr =
        { e = TVar e.ename; ty = TOwnPtr pointee; pos = e.epos } in
      if droppable_deep ~structs ~enums pointee then
        let g = request_ptr_glue pointee in
        [ TExprStmt {
            e = TCall { mangled = g; args = [ alloc_expr; ptr_expr ] };
            ty = TInt { signed = true; width = Ast.W32 }; pos = e.epos } ]
      else
        [ free_via ~alloc_expr ~ptr_expr
            ~bytes:(size_of_as_u32 pointee e.epos) e.epos ]
  | ArrayDrop { elem_path; size } ->
      let arr_ty =
        TArray { elem = TStruct elem_path; size } in
      let arr_var = { e = TVar e.ename; ty = arr_ty; pos = e.epos } in
      List.concat_map (fun i ->
        let idx = { e = TIntLit i; ty = u32_ty; pos = e.epos } in
        let elem_lv = {
          e = TIndex { base = arr_var; index = idx };
          ty = TStruct elem_path; pos = e.epos } in
        drop_stmts_for_struct ~structs ~enums elem_lv elem_path e.epos)
        (List.init size (fun i -> i))
  | BareOwn { prov = None; _ } ->
      (* L1 reject: nothing to release it with, and silence would be
         a leak.  The binding reached a scope exit Live. *)
      Error.failf e.epos
        "own value '%s' is never consumed — free it, move it, or \
         return it (its allocator is not known here, so it cannot \
         be auto-dropped)"
        e.ename

(* Drops for every Live entry, newest-first (LIFO declaration order —
   st keeps newest at the head). *)
let drops_for_live ~structs ~enums st : tstmt list =
  List.concat_map (fun e ->
    match e.status with
    | Live -> drop_stmts_for_entry ~structs ~enums e
    | Partial ->
        Error.failf e.epos
          "'%s' had its payload moved out and still owns its storage \
           at scope exit — release it explicitly with `free(alloc, %s)`"
          e.ename e.ename
    | Consumed -> [])
    st

(* ---------- glue body builders ---------- *)

let i32_ty = TInt { signed = true; width = Ast.W32 }

let glue_call name args pos =
  TExprStmt { e = TCall { mangled = name; args }; ty = i32_ty; pos }

(* Drop stmts for one VALUE lvalue [lv] (struct value, bare own ptr, or
   enum value is not expected here).  [container_alloc] supplies the
   allocator for bare-own elements (same-allocator assumption). *)
let elem_drop_stmts ~structs ~enums ~container_alloc (lv : texpr) pos
  : tstmt list =
  match lv.ty with
  | TStruct p when has_drop_deep ~structs lv.ty ->
      drop_stmts_for_struct ~structs ~enums lv p pos
  | TOwnPtr inner ->
      if droppable_deep ~structs ~enums inner then
        let g = request_ptr_glue inner in
        [ glue_call g [ container_alloc; lv ] pos ]
      else
        [ free_via ~alloc_expr:container_alloc ~ptr_expr:lv
            ~bytes:(size_of_as_u32 inner pos) pos ]
  | _ -> []

let mk_glue_tfunc ~name ~params ~lets ~body : tfunc =
  let rec ann_of = function
    | TStruct p -> Ast.TyStruct { path = p; args = [] }
    | TPtr t -> Ast.TyPtr (ann_of t)
    | TOwnPtr t -> Ast.TyOwnPtr (ann_of t)
    | TConstPtr t -> Ast.TyConstPtr (ann_of t)
    | _ -> Ast.TyPtr Ast.TyCVoid
  in
  let ast_params =
    List.map (fun (pname, ty) ->
      { Ast.pname; pty = ann_of ty; preg = None; is_mut = false })
      params
  in
  let f = Ast.{
    name; c_name = name; tparams = []; tbounds = [];
    params = ast_params; ret_ty = None; body = [];
    is_pub = false; is_extern = false; is_variadic = false;
    amiga_lib = None; tier_hint = None; must_use = false;
    escapes_hatch = false; pos = prelude_pos;
  } in
  { tf_path = [ name ];
    tf_func = f;
    tf_mangled = name;
    tf_param_tys = List.map snd params;
    tf_ret_ty = None;
    tf_body = body;
    tf_lets = lets;
    tf_origin_pos = None }

let build_ptr_glue ~structs ~enums ~name pointee : tfunc =
  let pos = prelude_pos in
  let a_var = { e = TVar "__a"; ty = allocator_ty; pos } in
  let p_var = { e = TVar "__p"; ty = TOwnPtr pointee; pos } in
  let free_self =
    free_via ~alloc_expr:a_var ~ptr_expr:p_var
      ~bytes:(size_of_as_u32 pointee pos) pos
  in
  let body, lets =
    match pointee with
    | TStruct sp ->
        let null_lit = { e = TNullLit; ty = TNullPtr; pos } in
        let deref = { e = TDeref p_var; ty = pointee; pos } in
        (* Fields of this struct that own the next node of the SAME type. *)
        let self_fields =
          match find_struct ~structs sp with
          | Some s ->
              List.filter_map (fun (fn, ft) ->
                match ft with
                | TOwnPtr (TStruct p2) when p2 = sp -> Some fn
                | _ -> None)
                s.sfields_ty
          | None -> [] in
        (match self_fields with
         | [ self_fn ] ->
             (* Linear list: exactly one next-of-same-type field.  Walk
                the spine iteratively (kernel-foundation decision #2,
                ported from the enum path) -- O(1) stack however long the
                list grows.  The self field is null-terminated, so the
                loop's `__p != null` guard also covers the terminator
                that the recursive form dereferenced (BUG-A SEGV). *)
             let next_var =
               { e = TVar "__next"; ty = TOwnPtr pointee; pos } in
             let self_field = {
               e = TFieldAccess { target = deref; field = self_fn };
               ty = TOwnPtr pointee; pos } in
             let cond =
               { e = TBinOp (Ast.NotEq, p_var, null_lit); ty = TBool; pos } in
             ([ TWhile {
                  cond;
                  body =
                    TAssign { path = [ "__next" ]; value = self_field; pos }
                    :: drop_stmts_for_struct ~skip:(Some self_fn)
                         ~structs ~enums deref sp pos
                    @ [ free_self;
                        TAssign { path = [ "__p" ]; value = next_var; pos } ];
                  post = [] } ],
              [ ("__next", TOwnPtr pointee) ])
         | _ ->
             (* No self field, or a tree (>=2 self children): keep the
                recursive form, but guard the null terminator a
                null-valued `own *Self` (or an optional owner) leaves
                behind.  The recursive call re-enters this same glue, so
                one guard at the top makes every level null-safe (BUG-A). *)
             let guard =
               TIf { cond = { e = TBinOp (Ast.EqEq, p_var, null_lit);
                              ty = TBool; pos };
                     then_body = [ TReturn { value = None; pos } ];
                     else_body = [] } in
             (guard
              :: drop_stmts_for_struct ~structs ~enums deref sp pos
              @ [ free_self ],
              []))
    | TEnum ep ->
        let es =
          match List.find_opt
                  (fun (e : enum_sig) -> e.ename_path = ep) enums with
          | Some es -> es
          | None -> Error.failf pos "internal: enum '%s' missing in drop \
                                     glue" (String.concat "::" ep)
        in
        let is_self_field ft =
          match ft with TOwnPtr (TEnum p2) -> p2 = ep | _ -> false in
        (* [self_action]: what an arm does with a bound field that owns
           the next node of the SAME enum (None = recurse like any
           other payload). *)
        let mk_arms ~self_action =
          List.mapi (fun tag (v : variant_sig) ->
            let binds =
              List.filter_map (fun (fname, ft) ->
                let want = match ft with
                  | TOwnPtr _ -> true
                  | TStruct _ -> has_drop_deep ~structs ft
                  | _ -> false
                in
                if want then Some (fname, TPVar ("__d_" ^ fname))
                else None)
                v.vsfields
            in
            let stmts =
              List.concat_map (fun (fname, _) ->
                let ft = List.assoc fname v.vsfields in
                let bind_var =
                  { e = TVar ("__d_" ^ fname); ty = ft; pos } in
                match self_action with
                | Some act when is_self_field ft -> [ act bind_var ]
                | _ ->
                    elem_drop_stmts ~structs ~enums ~container_alloc:a_var
                      bind_var pos)
                binds
            in
            { tpat = TPVariant { variant = v.vsname; tag; binds };
              tguard = None;
              tbody = { e = TBlock { stmts; trailing = None };
                        ty = i32_ty; pos };
              tdiverges = false;
              tarm_pos = pos })
            es.evariants
        in
        let deref = { e = TDeref p_var; ty = pointee; pos } in
        let match_stmt arms =
          TExprStmt {
            e = TMatch { scrutinee = deref; ename_path = ep; arms };
            ty = i32_ty; pos } in
        let linear =
          List.for_all
            (fun (v : variant_sig) ->
              List.length
                (List.filter (fun (_, ft) -> is_self_field ft) v.vsfields)
              <= 1)
            es.evariants
          && List.exists
               (fun (v : variant_sig) ->
                 List.exists (fun (_, ft) -> is_self_field ft) v.vsfields)
               es.evariants
        in
        if linear then begin
          (* Kernel-foundation decision #2 (2026-06-12): the recursive
             form burns one non-tail C frame per node — a long owned
             list overruns a small (kernel) stack at scope exit.  When
             every variant owns at most one next-node of the same enum
             (a linear list), tear the spine down iteratively: O(1)
             stack regardless of length.  Genuine trees (a variant
             with two self-owned children) keep recursion — depth on
             the C stack instead; documented honest limit. *)
          let next_var = { e = TVar "__next"; ty = TOwnPtr pointee; pos } in
          let null_lit = { e = TNullLit; ty = TNullPtr; pos } in
          let set_next value = TAssign { path = [ "__next" ]; value; pos } in
          let arms = mk_arms ~self_action:(Some set_next) in
          let cond =
            { e = TBinOp (Ast.NotEq, p_var, null_lit); ty = TBool; pos } in
          ([ TWhile {
               cond;
               body =
                 [ set_next null_lit;
                   match_stmt arms;
                   free_self;
                   TAssign { path = [ "__p" ]; value = next_var; pos } ];
               post = [] } ],
           [ ("__next", TOwnPtr pointee) ])
        end
        else begin
          (* Tree (a variant with >=2 self-owned children): keep the
             recursive form, but guard a null child.  An owning pointer
             into the same enum can hold a raw `null` (not just a Nil
             variant), and the recursive call re-enters this glue, so a
             top guard makes every level null-safe (BUG-A, enum side). *)
          let null_lit = { e = TNullLit; ty = TNullPtr; pos } in
          let guard =
            TIf { cond = { e = TBinOp (Ast.EqEq, p_var, null_lit);
                           ty = TBool; pos };
                  then_body = [ TReturn { value = None; pos } ];
                  else_body = [] } in
          ([ guard; match_stmt (mk_arms ~self_action:None); free_self ], [])
        end
    | _ -> ([ free_self ], [])
  in
  mk_glue_tfunc ~name
    ~params:[ ("__a", allocator_ty); ("__p", TOwnPtr pointee) ]
    ~lets ~body

(* Shared loop skeleton: `__i = 0; while (__i < <bound>) { <body>;
   __i = __i + 1; }`. *)
let count_loop ~bound ~mk_body pos : tstmt list =
  let i_var = { e = TVar "__i"; ty = u32_ty; pos } in
  let zero = { e = TIntLit 0; ty = u32_ty; pos } in
  let one = { e = TIntLit 1; ty = u32_ty; pos } in
  let cond = { e = TBinOp (Ast.Lt, i_var, bound); ty = TBool; pos } in
  let step =
    TAssign { path = [ "__i" ];
              value = { e = TBinOp (Ast.Add, i_var, one);
                        ty = u32_ty; pos };
              pos }
  in
  [ TLet { name = "__i"; value = zero; pos };
    TWhile { cond; body = mk_body i_var @ [ step ]; post = [] } ]

let build_vec_glue ~structs ~enums ~name inst_path : tfunc =
  let pos = prelude_pos in
  let s = match find_struct ~structs inst_path with
    | Some s -> s
    | None -> Error.failf pos "internal: vec instance missing in drop glue"
  in
  let elem = match List.assoc_opt "ptr" s.sfields_ty with
    | Some (TOwnPtr e) -> e
    | _ -> Error.failf pos "internal: vec ptr field shape"
  in
  let count_field =
    if List.mem_assoc "count" s.sfields_ty then "count" else "len" in
  let v_var = { e = TVar "__v"; ty = TPtr (TStruct inst_path); pos } in
  let vfield f ty = { e = TFieldAccess { target = v_var; field = f };
                      ty; pos } in
  let valloc = vfield "alloc" allocator_ty in
  let body =
    count_loop pos
      ~bound:(vfield count_field u32_ty)
      ~mk_body:(fun i_var ->
        let elem_lv = {
          e = TIndex { base = vfield "ptr" (TOwnPtr elem); index = i_var };
          ty = elem; pos } in
        elem_drop_stmts ~structs ~enums ~container_alloc:valloc elem_lv pos)
    @ [ free_via ~alloc_expr:valloc
          ~ptr_expr:(vfield "ptr" (TOwnPtr elem))
          ~bytes:{ e = TBinOp (Ast.Mul, vfield "cap" u32_ty,
                               size_of_as_u32 elem pos);
                   ty = u32_ty; pos }
          pos ]
  in
  mk_glue_tfunc ~name
    ~params:[ ("__v", TPtr (TStruct inst_path)) ]
    ~lets:[ ("__i", u32_ty) ] ~body

let build_hm_glue ~structs ~enums ~name inst_path : tfunc =
  let pos = prelude_pos in
  let s = match find_struct ~structs inst_path with
    | Some s -> s
    | None -> Error.failf pos "internal: hashmap instance missing"
  in
  let slot_t = match List.assoc_opt "slots" s.sfields_ty with
    | Some (TOwnPtr (TStruct sp as t)) -> (sp, t)
    | _ -> Error.failf pos "internal: hashmap slots field shape"
  in
  let (slot_path, slot_ty) = slot_t in
  let slot_s = match find_struct ~structs slot_path with
    | Some s -> s
    | None -> Error.failf pos "internal: slot struct missing"
  in
  let m_var = { e = TVar "__m"; ty = TPtr (TStruct inst_path); pos } in
  let mfield f ty = { e = TFieldAccess { target = m_var; field = f };
                      ty; pos } in
  let malloc_ = mfield "alloc" allocator_ty in
  let body =
    count_loop pos
      ~bound:(mfield "cap" u32_ty)
      ~mk_body:(fun i_var ->
        let slot_lv = {
          e = TIndex { base = mfield "slots" (TOwnPtr slot_ty);
                       index = i_var };
          ty = slot_ty; pos } in
        let state_f = {
          e = TFieldAccess { target = slot_lv; field = "state" };
          ty = TInt { signed = false; width = Ast.W8 }; pos } in
        let occupied = {
          e = TBinOp (Ast.EqEq, state_f,
                      { e = TIntLit 1;
                        ty = TInt { signed = false; width = Ast.W8 };
                        pos });
          ty = TBool; pos } in
        let kv_drops =
          List.concat_map (fun fname ->
            match List.assoc_opt fname slot_s.sfields_ty with
            | Some ft ->
                let lv = {
                  e = TFieldAccess { target = slot_lv; field = fname };
                  ty = ft; pos } in
                elem_drop_stmts ~structs ~enums ~container_alloc:malloc_
                  lv pos
            | None -> [])
            [ "key"; "value" ]
        in
        if kv_drops = [] then []
        else [ TIf { cond = occupied; then_body = kv_drops;
                     else_body = [] } ])
    @ [ free_via ~alloc_expr:malloc_
          ~ptr_expr:(mfield "slots" (TOwnPtr slot_ty))
          ~bytes:{ e = TBinOp (Ast.Mul, mfield "cap" u32_ty,
                               size_of_as_u32 slot_ty pos);
                   ty = u32_ty; pos }
          pos ]
  in
  mk_glue_tfunc ~name
    ~params:[ ("__m", TPtr (TStruct inst_path)) ]
    ~lets:[ ("__i", u32_ty) ] ~body

(* Drain the request queue to a fixpoint — building one glue body may
   request glue for nested types (a tree of Vec<String>, ...). *)
let drain_glue ~structs ~enums () =
  while not (Queue.is_empty glue_queue) do
    let (key, name) = Queue.pop glue_queue in
    let meta = Hashtbl.find glue_meta key in
    let tf =
      if String.length key >= 4 && String.sub key 0 4 = "ptr:" then
        build_ptr_glue ~structs ~enums ~name meta
      else if String.length key >= 4 && String.sub key 0 4 = "vec:" then
        (match meta with
         | TStruct p -> build_vec_glue ~structs ~enums ~name p
         | _ -> assert false)
      else
        (match meta with
         | TStruct p -> build_hm_glue ~structs ~enums ~name p
         | _ -> assert false)
    in
    glue_fns := tf :: !glue_fns
  done

(* ---------- provenance ---------- *)

let prov_of (value : texpr) : string option =
  let starts_with s pre =
    let ls = String.length s and lp = String.length pre in
    ls >= lp && String.sub s 0 lp = pre
  in
  match value.e with
  | TNew { alloc = Some { e = TVar a; _ }; _ }
  | TNewEnum { alloc = Some { e = TVar a; _ }; _ } -> Some a
  | TCall { mangled; args = { e = TVar a; _ } :: _ }
    when starts_with mangled "Allocator__alloc" -> Some a
  | _ -> None

let entry_for ~structs name (ty : typ) pos : entry option =
  match ty with
  | TStruct p when has_drop_deep ~structs ty ->
      Some { ename = name; status = Live; kind = StructDrop p; epos = pos }
  | TOwnPtr inner ->
      Some { ename = name; status = Live;
             kind = BareOwn { pointee = inner; prov = None }; epos = pos }
  | TArray { elem = TOwnPtr _; _ } ->
      (* Freeze-audit B8: an array of bare owned pointers has no
         allocator at hand to release the elements with — silent leak
         territory.  Reject; wrap them in a struct that carries its
         allocator, or use a Vec. *)
      Error.failf pos
        "an array of bare `own *T` cannot be auto-dropped (no \
         allocator at hand for the elements) — wrap the pointers in \
         an allocator-carrying struct, or use a Vec"
  | TArray { elem = TStruct p; size }
    when has_drop_deep ~structs (TStruct p) ->
      Some { ename = name; status = Live;
             kind = ArrayDrop { elem_path = p; size }; epos = pos }
  | _ -> None

(* ---------- per-fn temp collector (value-then-drop sequencing) ----- *)

let tmp_decls : (string * typ) list ref = ref []

(* ---------- statement driver (rewriter) ---------- *)

(* OWN-D3 merge: tracked entry states after two non-diverging branches
   must agree — static drops cannot follow a runtime-dependent owner
   state. *)
let merge_branches pos st_then st_else =
  List.map2 (fun (a : entry) (b : entry) ->
    match a.status, b.status with
    | Live, Live -> a
    | Consumed, Consumed -> a
    | Partial, Partial -> a
    | _ ->
        Error.failf pos
          "'%s' is moved out on one branch but stays owned on the \
           other — auto-drop is static (no runtime drop flags); \
           consume it on every path or on none"
          a.ename)
    st_then st_else

(* walk_stmts: rewrite a block.  [st] holds ALL tracked entries
   (enclosing scopes included; newest at head).  [defers] is the
   accumulated stack of defer bodies from this and enclosing scopes
   (newest first) — a scope exit applies their consumes before
   deciding drops.  At block end, this block's own Live locals are
   dropped and stripped; entries that existed on entry survive. *)
let rec walk_stmts ~structs ~enums ~defers (st : entry list) stmts
  : tstmt list * entry list =
  let baseline = names_of st in
  let rec go acc st defers = function
    | [] -> (acc, st, defers)
    | stmt :: rest ->
        let (stmts', st', defers') =
          walk_stmt ~structs ~enums ~defers st stmt in
        go (acc @ stmts') st' defers' rest
  in
  let (body, st_end, defers_end) = go [] st defers stmts in
  (* Block close: defer consumes land first (their bodies run at this
     exit), then this block's surviving locals drop. *)
  let st_exit =
    mark_consumed
      (consumed_by_stmts ~structs ~enums (names_of st_end)
         (List.concat defers_end))
      st_end
  in
  let locals, survivors =
    List.partition (fun e -> not (List.mem e.ename baseline)) st_exit in
  let ends_in_return =
    match List.rev body with TReturn _ :: _ -> true | _ -> false in
  let tail_drops =
    if ends_in_return then [] else drops_for_live ~structs ~enums locals in
  (body @ tail_drops, survivors)

and walk_stmt ~structs ~enums ~defers st stmt
  : tstmt list * entry list * tstmt list list =
  match stmt with
  | TLet { name; value; pos } ->
      let st = apply_expr ~structs ~enums st value in
      let st = consume_rhs_var ~structs st value in
      (* Same-scope shadowing of a still-Live tracked binding would
         silently leak the old value — release it before the new let
         takes the name over.  (If the RHS consumed it — `let s =
         eat(s)` — it is already Consumed and nothing fires.) *)
      let shadow_drops =
        List.concat_map (fun e ->
          if e.ename = name && e.status = Live
          then drop_stmts_for_entry ~structs ~enums e
          else [])
          st
      in
      let st = List.filter (fun e -> e.ename <> name) st in
      let st =
        match entry_for ~structs name value.ty pos with
        | Some e ->
            let e = match e.kind with
              | BareOwn b ->
                  { e with kind = BareOwn { b with prov = prov_of value } }
              | _ -> e
            in
            e :: st
        | None -> st
      in
      (shadow_drops @ [stmt], st, defers)
  | TAssign { path = [ n ]; value; pos }
    when List.exists (fun e -> e.ename = n) st ->
      (* L2 + L3: drop the old value if it is still Live after the RHS
         (the RHS may itself consume it — `s = next(a, s)`), then the
         binding is Live again with the new value's provenance.  A bare
         affine TVar RHS (`y = x`) must also be consumed BEFORE the
         old-value check so `x` is not freed at scope exit as well
         (double-free); the drop-old below still releases the OLD `y`. *)
      let st = apply_expr ~structs ~enums st value in
      let st = consume_rhs_var ~structs st value in
      let old = List.find (fun e -> e.ename = n) st in
      let drop_old =
        if old.status = Live then
          match old.kind with
          | BareOwn { prov = None; _ } ->
              Error.failf pos
                "assigning to '%s' would silently leak its current \
                 value (allocator provenance unknown) — free it or \
                 move it first" n
          | _ -> drop_stmts_for_entry ~structs ~enums old
        else []
      in
      let st =
        List.map (fun e ->
          if e.ename = n then
            let kind = match e.kind with
              | BareOwn b -> BareOwn { b with prov = prov_of value }
              | k -> k
            in
            { e with status = Live; kind }
          else e)
          st
      in
      (* Sequencing (mirrors the TReturn DR-044 rule): the RHS may READ the
         old binding by borrow (`x = f(&x)`), so it must be evaluated into a
         temp BEFORE the old value is dropped — otherwise `drop(x); x = f(&x)`
         reads freed storage (use-after-free).  A bare-TVar RHS needs no temp
         (it consumed the old binding, so drop_old is empty anyway). *)
      let needs_temp =
        drop_old <> [] &&
        (match value.e with TVar _ -> false | _ -> true) in
      let stmts =
        if needs_temp then
          let tmp = Printf.sprintf "__drop_old_%d_%d"
              pos.Pos.line pos.Pos.col in
          tmp_decls := (tmp, value.ty) :: !tmp_decls;
          let tmp_let = TLet { name = tmp; value; pos } in
          let tmp_var = { e = TVar tmp; ty = value.ty; pos = value.pos } in
          let assign' = TAssign { path = [ n ]; value = tmp_var; pos } in
          [ tmp_let ] @ drop_old @ [ assign' ]
        else drop_old @ [ stmt ]
      in
      (stmts, st, defers)
  | TAssign { value; _ } | TAssignDeref { value; _ }
  | TAssignField { value; _ } | TAssignIndex { value; _ } ->
      (apply_one ~structs ~enums st stmt value, defers)
      |> fun ((stmts, st), defers) -> (stmts, st, defers)
  | TExprStmt e ->
      (* Freeze-audit B12: an expression statement whose value is an
         owned pointer or a droppable struct discards ownership on the
         floor — nothing tracks it, nothing releases it (the L1
         never-consumed check only sees BINDINGS).  Reject; binding it
         puts it under the normal lifecycle. *)
      (match e.ty with
       | TOwnPtr _ ->
           Error.failf e.pos
             "this call returns an owned pointer that is silently \
              discarded (and leaked) — bind it with `let`, then free, \
              move, or return it"
       | TStruct _ when has_drop_deep ~structs e.ty ->
           Error.failf e.pos
             "this expression produces an owning value that is silently \
              discarded (and leaked) — bind it with `let` so it can be \
              dropped or consumed"
       | _ -> ());
      let st = apply_expr ~structs ~enums st e in
      ([stmt], st, defers)
  | TReturn { value; pos } ->
      (* Ownership of a returned tracked binding transfers to the
         caller; any other return value may consume bindings inside
         (`return String::build(sb)`).  Then this exit point flushes
         every active defer (their consumes count) and releases every
         remaining Live owner, innermost-first. *)
      let st =
        match value with
        | Some { e = TVar n; _ } -> mark_consumed [ n ] st
        | Some v -> apply_expr ~structs ~enums st v
        | None -> st
      in
      let st_exit =
        mark_consumed
          (consumed_by_stmts ~structs ~enums (names_of st)
             (List.concat defers))
          st
      in
      let drops = drops_for_live ~structs ~enums st_exit in
      (* Sequencing (DR-044): evaluate the return value BEFORE the
         drops — `return *buf.p` must read through `buf.p` while the
         backing storage is still alive.  Var returns and empty
         returns need no temp. *)
      let needs_temp =
        drops <> [] &&
        (match value with
         | Some { e = TVar _; _ } | None -> false
         | _ -> true)
      in
      let stmts =
        if needs_temp then
          match value with
          | Some v ->
              let tmp = Printf.sprintf "__drop_ret_%d_%d"
                  pos.Pos.line pos.Pos.col in
              tmp_decls := (tmp, v.ty) :: !tmp_decls;
              let tmp_let = TLet { name = tmp; value = v; pos } in
              let tmp_var = { e = TVar tmp; ty = v.ty; pos = v.pos } in
              [ tmp_let ] @ drops @ [ TReturn { value = Some tmp_var; pos } ]
          | None -> drops @ [ stmt ]
        else drops @ [ TReturn { value; pos } ]
      in
      (stmts, st_exit, defers)
  | TIf { cond; then_body; else_body } ->
      let st = apply_expr ~structs ~enums st cond in
      let (then', st_then) =
        walk_stmts ~structs ~enums ~defers st then_body in
      let (else', st_else) =
        walk_stmts ~structs ~enums ~defers st else_body in
      let then_dvg = Move.stmts_diverge then_body in
      let else_dvg = Move.stmts_diverge else_body in
      let st' =
        match then_dvg, else_dvg with
        | true, true -> st
        | true, false -> st_else
        | false, true -> st_then
        | false, false -> merge_branches cond.pos st_then st_else
      in
      ([ TIf { cond; then_body = then'; else_body = else' } ], st', defers)
  | TWhile { cond; body; post } ->
      (* Move.check already guarantees a tracked binding's state is
         loop-invariant (consume-without-rebind inside a loop body is
         rejected there), so one symbolic pass gives the exit state. *)
      let st = apply_expr ~structs ~enums st cond in
      let (body', st_body) = walk_stmts ~structs ~enums ~defers st body in
      let (post', st_post) =
        walk_stmts ~structs ~enums ~defers st_body post in
      ([ TWhile { cond; body = body'; post = post' } ], st_post, defers)
  | TDefer { body; pos } ->
      (* Freeze-audit B10: in the emitted C the auto-drops land BEFORE
         the defer flush, so a defer body that READS a tracked binding
         without consuming it would run against freed storage.  Reject
         up front; a defer that CONSUMES the binding (s.free(),
         free(a, p)) elides the auto-drop and stays legal. *)
      let tracked = names_of st in
      let consumed = consumed_by_stmts ~structs ~enums tracked body in
      let reads =
        List.concat_map
          (fun s ->
            List.concat_map
              (fun e ->
                Ir.fold_texpr
                  (fun acc (te : texpr) ->
                    match te.e with
                    | TVar n when List.mem n tracked -> n :: acc
                    | _ -> acc)
                  [] e)
              (stmt_exprs s))
          body
        |> List.sort_uniq compare
      in
      (match List.filter (fun n -> not (List.mem n consumed)) reads with
       | n :: _ ->
           Error.failf pos
             "defer body reads '%s', but '%s' is auto-dropped before \
              deferred code runs — consume it inside the defer \
              (free/drop) or restructure without defer" n n
       | [] -> ());
      ([ stmt ], st, body :: defers)
  | TLetTuple _ | TFor _ | TForEach _ | TBreak _ | TContinue _ ->
      ([ stmt ], st, defers)

and apply_one ~structs ~enums st stmt value =
  let st = apply_expr ~structs ~enums st value in
  let st = consume_rhs_var ~structs st value in
  ([ stmt ], st)

(* ---------- per-fn / whole-program ---------- *)

let rewrite_fn ~structs ~enums (tf : tfunc) : tfunc =
  (* The prelude predates auto-drop and frees by hand (String::free,
     Vec::grow's old-buffer release, ...) — inserting drops there
     would double-fire.  Only user code gets the pass. *)
  if tf.tf_func.Ast.pos.Pos.file = "<prelude>" then tf
  else begin
    tmp_decls := [];
    (* Affine by-value params are owned by the callee: seed them Live
       so an unconsumed one is released at exit (S3) — or rejected at
       exit when it is a bare own with no provenance (L1). *)
    let param_entries =
      List.filter_map (fun (p, ty) ->
        entry_for ~structs p.Ast.pname ty tf.tf_func.Ast.pos)
        (List.combine tf.tf_func.Ast.params tf.tf_param_tys)
    in
    let (body', st_end) =
      walk_stmts ~structs ~enums ~defers:[] param_entries tf.tf_body in
    (* walk_stmts dropped the body's own locals; params (the baseline)
       survive — release them unless the body already ended in a
       return (that exit dropped everything). *)
    let ends_in_return =
      match List.rev body' with TReturn _ :: _ -> true | _ -> false in
    let body_final =
      if ends_in_return then body'
      else body' @ drops_for_live ~structs ~enums st_end
    in
    { tf with
      tf_body = body_final;
      tf_lets = tf.tf_lets @ List.rev !tmp_decls }
  end

let insert (tp : tprogram) : tprogram =
  glue_reset ();
  let structs = tp.tp_struct_index and enums = tp.tp_enum_index in
  let funcs = List.map (rewrite_fn ~structs ~enums) tp.tp_funcs in
  (* Build every requested deep-drop glue fn (fixpoint: a body may
     request glue for nested types). *)
  drain_glue ~structs ~enums ();
  { tp with tp_funcs = funcs @ List.rev !glue_fns }
