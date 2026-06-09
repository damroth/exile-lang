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
let consumed_by ~structs ~enums names (te : texpr) : string list =
  if names = [] then []
  else
    let live = List.map (fun n -> (n, Move.Live)) names in
    let after = Move.walk_expr ~structs ~enums live te in
    List.filter_map
      (function (n, Move.Consumed _) -> Some n | _ -> None) after
    |> List.sort_uniq compare

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

type status = Live | Consumed

type kind =
  | StructDrop of string list                  (* droppable struct path *)
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
    if e.status = Live && List.mem e.ename names
    then { e with status = Consumed }
    else e)
    st

let apply_expr ~structs ~enums st (te : texpr) =
  mark_consumed (consumed_by ~structs ~enums (names_of st) te) st

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

(* Release everything [base] (a droppable struct value) owns: free its
   direct `own *T` fields through the SAME level's `alloc` field, then
   recurse into droppable struct-value fields (S5 transitivity —
   `Person { name: String }` releases `name.ptr` via `name.alloc`). *)
let rec drop_stmts_for_struct ~structs (base : texpr) path pos : tstmt list =
  match find_struct ~structs path with
  | None -> []
  | Some s ->
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
      (* Byte-count heuristic (DR-046): size-tracking allocators get
         the real footprint back.  Shape from sibling fields:
           cap → cap * size_of(elem)      (growable: Vec/SB/HashMap)
           len → len + 1                  (NUL-terminated: String)
           else → size_of(elem)           (single-element ownership) *)
      let bytes_expr inner =
        if has_field "cap" then
          { e = TBinOp (Ast.Mul, field_u32 "cap", size_of_as_u32 inner pos);
            ty = u32_ty; pos }
        else if has_field "len" then
          { e = TBinOp (Ast.Add, field_u32 "len",
                        { e = TIntLit 1; ty = u32_ty; pos });
            ty = u32_ty; pos }
        else size_of_as_u32 inner pos
      in
      List.concat_map (fun (fname, fty) ->
        match fty with
        | TOwnPtr inner ->
            let field_access = {
              e = TFieldAccess { target = base; field = fname };
              ty = fty; pos;
            } in
            [ free_via ~alloc_expr:alloc_field ~ptr_expr:field_access
                ~bytes:(bytes_expr inner) pos ]
        | TStruct p2 when has_drop_deep ~structs fty ->
            let sub_base = {
              e = TFieldAccess { target = base; field = fname };
              ty = fty; pos;
            } in
            drop_stmts_for_struct ~structs sub_base p2 pos
        | _ -> [])
        s.sfields_ty

let drop_stmts_for_entry ~structs (e : entry) : tstmt list =
  match e.kind with
  | StructDrop path ->
      let base = { e = TVar e.ename; ty = TStruct path; pos = e.epos } in
      drop_stmts_for_struct ~structs base path e.epos
  | BareOwn { pointee; prov = Some a } ->
      let alloc_expr = { e = TVar a; ty = allocator_ty; pos = e.epos } in
      let ptr_expr =
        { e = TVar e.ename; ty = TOwnPtr pointee; pos = e.epos } in
      [ free_via ~alloc_expr ~ptr_expr
          ~bytes:(size_of_as_u32 pointee e.epos) e.epos ]
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
let drops_for_live ~structs st : tstmt list =
  List.concat_map (fun e ->
    match e.status with
    | Live -> drop_stmts_for_entry ~structs e
    | Consumed -> [])
    st

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
    if ends_in_return then [] else drops_for_live ~structs locals in
  (body @ tail_drops, survivors)

and walk_stmt ~structs ~enums ~defers st stmt
  : tstmt list * entry list * tstmt list list =
  match stmt with
  | TLet { name; value; pos } ->
      let st = apply_expr ~structs ~enums st value in
      (* Same-scope shadowing of a still-Live tracked binding would
         silently leak the old value — release it before the new let
         takes the name over.  (If the RHS consumed it — `let s =
         eat(s)` — it is already Consumed and nothing fires.) *)
      let shadow_drops =
        List.concat_map (fun e ->
          if e.ename = name && e.status = Live
          then drop_stmts_for_entry ~structs e
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
         binding is Live again with the new value's provenance. *)
      let st = apply_expr ~structs ~enums st value in
      let old = List.find (fun e -> e.ename = n) st in
      let drop_old =
        if old.status = Live then
          match old.kind with
          | BareOwn { prov = None; _ } ->
              Error.failf pos
                "assigning to '%s' would silently leak its current \
                 value (allocator provenance unknown) — free it or \
                 move it first" n
          | _ -> drop_stmts_for_entry ~structs old
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
      (drop_old @ [stmt], st, defers)
  | TAssign { value; _ } | TAssignDeref { value; _ }
  | TAssignField { value; _ } | TAssignIndex { value; _ } ->
      (apply_one ~structs ~enums st stmt value, defers)
      |> fun ((stmts, st), defers) -> (stmts, st, defers)
  | TExprStmt e ->
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
      let drops = drops_for_live ~structs st_exit in
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
  | TDefer { body; _ } ->
      (* Registered, not walked: the body's consumes apply at each
         scope exit from here on (codegen runs defer bodies there). *)
      ([ stmt ], st, body :: defers)
  | TLetTuple _ | TFor _ | TForEach _ | TBreak _ | TContinue _ ->
      ([ stmt ], st, defers)

and apply_one ~structs ~enums st stmt value =
  let st = apply_expr ~structs ~enums st value in
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
      else body' @ drops_for_live ~structs st_end
    in
    { tf with
      tf_body = body_final;
      tf_lets = tf.tf_lets @ List.rev !tmp_decls }
  end

let insert (tp : tprogram) : tprogram =
  { tp with
    tp_funcs =
      List.map
        (rewrite_fn ~structs:tp.tp_struct_index ~enums:tp.tp_enum_index)
        tp.tp_funcs }
