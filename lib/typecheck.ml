(* Semantic / type-checking layer.  Validates an Ast.program and produces
   the typed Ir.program that codegen consumes.  The typed-AST data
   structures and pure type-domain utilities (typ, mangle, mangle_typ,
   typ_eq, tuple_struct_name, ...) live in `Ir`; this module owns the
   validation rules: lookups with visibility, builtin signatures,
   expression typing, and elaboration. *)

open Ir

(* Resolution context: every function and every struct in the program
   (keyed by module path and name), plus a flat list of every module with
   its pub flag, plus the path of the function we are currently emitting
   code for.  Local references (path = [name]) resolve within the current
   scope; qualified ones (path > 1) resolve from the root with per-segment
   visibility checks.  Mono-instance accumulator threads through
   `instances` (state + helpers live in `Mono`); the enclosing fn's
   return type rides on `ret_ty` so `try` can validate and construct
   the early-return value. *)

type fn_ctx = {
  global : (string list * string * fn_sig) list;
  structs : struct_sig list;
  enums : enum_sig list;
  modules : (string list * bool) list;   (* full path -> is_pub *)
  scope : string list;
  tparams : string list;                 (* generic type params in scope:
                                            ["T"; "U"] inside a generic
                                            decl's body, [] otherwise *)
  tbounds : (string * string list * (string * typ) list) list;
                                          (* per-tparam trait bounds in
                                             scope.  `<F: Fn1>` populates
                                             `("F", ["Fn1"], [])`; the
                                             DR-021 sugar `<F: |int|->int>`
                                             populates `("F", ["Fn1"],
                                             [("Arg", TInt …);
                                             ("Output", TInt …)])`.  Read
                                             by the assoc-type projection
                                             resolver: (a) to disambiguate
                                             `F::Output` when several
                                             traits define the assoc name
                                             (Fn1 and Fn2 both have
                                             `Output`) — the bound's trait
                                             list pins the choice; (b) to
                                             shortcut the projection
                                             directly to a bound's assoc
                                             binding without needing the
                                             concrete impl's assoc table. *)
  tvar_bindings : (string * typ) list;   (* substitute these into every
                                            type produced by resolve_type_ann.
                                            Empty for skeleton elab; populated
                                            with concrete bindings when
                                            re-elaborating a generic fn
                                            instance.  TVars matching keys
                                            in here are replaced after
                                            resolve so body code mentioning
                                            T sees the instance's concrete
                                            type. *)
  instances : Mono.state;
  ext_structs : string list;             (* names of `extern struct Foo`
                                            decls (opaque or exposed) —
                                            resolve_type_ann uses this to
                                            map a single-segment path to
                                            TExtStruct.  Field lookup goes
                                            through ext_struct_fields. *)
  ext_struct_fields : (string * (string * typ) list) list;
                                          (* For exposed extern struct
                                             decls (`extern struct Foo { f:
                                             T, ... }`): name → resolved
                                             fields.  Opaque structs are
                                             absent from this list.
                                             FieldAccess on a TExtStruct
                                             consults this for field
                                             types; absence = opaque
                                             access rejected. *)
  ext_types : string list;               (* names of `extern type Foo;`
                                            aliases — resolve_type_ann
                                            maps a single-segment path
                                            to TExtAlias *)
  ext_consts : (string * typ) list;      (* `extern const NAME: T;` —
                                            looked up in Var expr after
                                            local env miss; codegen
                                            emits raw NAME at use sites *)
  consts : (string list * string * (typ * string * int option)) list;
                                          (* user `const NAME: T = e;` index:
                                             (module path, name, (type,
                                             mangled C name, folded int value
                                             — None for a bool const)).
                                             Resolved with scope walk-up like
                                             fns; a use site becomes `TVar
                                             mangled`, which codegen emits as
                                             the `#define`d macro.  The value
                                             feeds array-size evaluation. *)
  ext_vars : (string * typ) list;        (* `extern var NAME: T;` —
                                            mutable global counterpart of
                                            ext_consts.  Looked up in Var
                                            expr (read) and in Assign
                                            (write).  Codegen emits raw
                                            NAME at use sites. *)
  ret_ty : typ option;                   (* enclosing fn's return type;
                                            None for void.  Used by `try`
                                            to validate / construct the
                                            early-return Err/None value. *)
  fn_asts : (string * (string list * Ast.func)) list;
                                          (* mangled C name → (parent path,
                                             original AST).  Lets generic
                                             fn-instance bodies be re-elaborated
                                             from the source AST after
                                             monomorphization picks args. *)
  aliases : (string list * string * string list * Pos.t) list;
                                          (* `pub use foo::bar;` re-exports.
                                             Each: (scope, local_name, target,
                                             decl_pos).  Lookup at scope for
                                             local_name redirects to target
                                             path resolved
                                             against the same scope. *)
  type_aliases : (string list * Ast.type_alias_decl) list;
                                          (* User-defined `type Name<T...> =
                                             Type;` aliases with their
                                             enclosing module path.  Looked
                                             up by `resolve_type_ann_raw`
                                             on `Ast.TyStruct { path; args }`
                                             before the struct/enum lookup;
                                             a match substitutes args into
                                             tparams of `tatarget` and
                                             recurses.  Cycle-guard is
                                             managed inline via a visited
                                             list parameter. *)
}

(* Skeleton ctx with every list field defaulted to [] and ret_ty None.
   [instances] is required because Mono.state has no zero value (it
   carries the mutable mono-job queue and visited set).  Use as
   `{ (empty_ctx ~instances) with structs = ...; modules; scope; ... }`
   — each callsite lists only its non-default fields, and adding a new
   field to fn_ctx requires editing this constant only. *)
let empty_ctx ~instances = {
  global = []; structs = []; enums = []; modules = [];
  scope = []; tparams = []; tbounds = []; tvar_bindings = []; fn_asts = [];
  aliases = []; type_aliases = []; ext_vars = []; ext_struct_fields = [];
  ext_structs = []; ext_types = []; ext_consts = []; consts = [];
  instances; ret_ty = None;
}

let rec is_prefix xs ys =
  match xs, ys with
  | [], _ -> true
  | _, [] -> false
  | x :: xs', y :: ys' -> x = y && is_prefix xs' ys'

(* Drop the last segment of a module path; [] -> []. *)
let parent_path = function
  | [] -> []
  | xs -> List.rev (List.tl (List.rev xs))

(* Walk up the current scope, trying [resolve] at each ancestor level.
   [path] is split into a relative [(mod_path, name)] pair; for each
   prefix we try [resolve (prefix @ mod_path) name].  Used by both fn
   and struct lookup, which only differ in [resolve].

   Re-exports (`pub use foo::bar;`) are checked at each ancestor too:
   if the lookup name matches an alias declared in this scope, retry
   the resolution with the alias's target path.  Cycles through aliases
   are guarded by a visited-paths set. *)
let walk_scope_up ~resolve ctx (path : string list) =
  let (suggested_mod, name) =
    match List.rev path with
    | [] -> failwith "empty path"
    | n :: rest -> (List.rev rest, n)
  in
  let try_alias prefix lookup_name =
    (* Single-segment lookups (e.g. `Window`) at scope `prefix` may
       redirect through `pub use foo::bar;` declared at `prefix`.
       Returns target path if matched. *)
    if suggested_mod <> [] then None
    else
      List.find_map (fun (alias_scope, local_name, target_path, _) ->
        if alias_scope = prefix && local_name = lookup_name
        then Some target_path
        else None)
        ctx.aliases
  in
  let visited = ref [] in
  let rec walk prefix =
    let key = (prefix, suggested_mod, name) in
    if List.mem key !visited then None
    else begin
      visited := key :: !visited;
      match resolve (prefix @ suggested_mod) name with
      | Some r -> Some r
      | None ->
          (match try_alias prefix name with
           | Some target ->
               (* Recurse with the alias target as the new path; keep
                  walking from the same prefix so the target resolves
                  against scopes visible from the alias declaration. *)
               let (alias_mod, alias_name) =
                 match List.rev target with
                 | n :: rest -> (List.rev rest, n)
                 | [] -> ([], "")
               in
               (match resolve (prefix @ alias_mod) alias_name with
                | Some r -> Some r
                | None ->
                    (* Loaded file-modules are hoisted to the program root,
                       so an alias target whose path is rooted there resolves
                       absolutely (not only relative to the current prefix). *)
                    (match resolve alias_mod alias_name with
                     | Some r -> Some r
                     | None ->
                         (match prefix with
                          | [] -> None
                          | _ -> walk (parent_path prefix))))
           | None ->
               (match prefix with
                | [] -> None
                | _ -> walk (parent_path prefix)))
    end
  in
  walk ctx.scope

(* Resolve a call path to a function.  Walks the current scope from the
   deepest ancestor down to the root, trying [prefix @ suggested_mod] at
   each level. *)
let lookup_fn ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_map
      (fun (p, n, s) ->
        if p = mod_path && n = name then Some (mod_path, s) else None)
      ctx.global)

(* Monotonic counter for `for`-loop gensym names — bumped per loop so two
   sequential `for i in ...` blocks don't collide on let-hoisting. *)
let for_gensym = ref 0

(* DR-009 — registry of view names.  Filled by [expand_views] so
   [elab_expr] on `Ast.Match` can spot patterns like `Sign::Positive`
   against a scrutinee of the view's input type (rather than the
   synthesised enum) and wrap the scrutinee in the view-fn call. *)
let view_names : (string, unit) Hashtbl.t = Hashtbl.create 8

(* Monotonic counter for the temp names that `println(x)` Display dispatch
   introduces — bumped per print so two prints in the same fn don't
   collide on the hoisted StringBuilder/String slots. *)
let display_dispatch_gensym = ref 0

(* Monotonic counter for DR-012 `with` gensym names — bumped per `with`
   so two sequential blocks reusing the user-visible binding name don't
   collide on the function-top decl list. *)
let with_gensym = ref 0

(* Substitute free `Ast.Var (from, _)` references with `Ast.Var (to_, _)`
   throughout an expression / statement / block.  Used by the `for` desugar
   to give each loop's user-facing variable a unique gensym name in the
   emitted C, so multiple sequential `for i in ...` blocks in the same
   function don't collide on let-hoisting.  Exile disallows shadowing, so
   no scope tracking is needed — `from` cannot be re-bound inside the body. *)
let rec subst_var_expr ~from ~to_ (e : Ast.expr) : Ast.expr =
  let sub = subst_var_expr ~from ~to_ in
  let subo = function None -> None | Some e -> Some (sub e) in
  match e with
  | Ast.Var (n, p) when n = from -> Ast.Var (to_, p)
  | Ast.IntLit _ | Ast.FloatLit _ | Ast.BoolLit _ | Ast.StringLit _ | Ast.NullLit _
  | Ast.Var _ | Ast.SizeOf _ -> e
  | Ast.Neg (sub_e, p) -> Ast.Neg (sub sub_e, p)
  | Ast.BitNot (sub_e, p) -> Ast.BitNot (sub sub_e, p)
  | Ast.Not (sub_e, p) -> Ast.Not (sub sub_e, p)
  | Ast.BinOp (op, l, r, p) -> Ast.BinOp (op, sub l, sub r, p)
  | Ast.Call { callee; args; pos } ->
      Ast.Call { callee; args = List.map sub args; pos }
  | Ast.Cast (e', t, p) -> Ast.Cast (sub e', t, p)
  | Ast.TupleLit (es, p) -> Ast.TupleLit (List.map sub es, p)
  | Ast.ArrayLit (es, p) -> Ast.ArrayLit (List.map sub es, p)
  | Ast.ArrayRepeat { value; count; pos } ->
      Ast.ArrayRepeat { value = sub value; count = sub count; pos }
  | Ast.Index { base; index; pos } ->
      Ast.Index { base = sub base; index = sub index; pos }
  | Ast.Range { lo; hi; inclusive; pos } ->
      Ast.Range { lo = sub lo; hi = sub hi; inclusive; pos }
  | Ast.Lambda { params; ret_ty; body; captures; pos } ->
      (* The body is its own scope (lifted to a top-level fn); a
         substitution of `from -> to_` in an enclosing context
         shouldn't reach in.  Lambdas that shadow `from` as a
         param keep the param.  Pre-typecheck pass walks lambdas
         separately so this case is mostly a safe no-op pass-through. *)
      Ast.Lambda { params; ret_ty; body = sub body; captures; pos }
  | Ast.StructLit { tname; fields; base; pos } ->
      Ast.StructLit { tname;
                      fields = List.map (fun (n, e) -> (n, sub e)) fields;
                      base = subo base; pos }
  | Ast.FieldAccess (e', n, p) -> Ast.FieldAccess (sub e', n, p)
  | Ast.Ref (e', p) -> Ast.Ref (sub e', p)
  | Ast.Deref (e', p) -> Ast.Deref (sub e', p)
  | Ast.New { tname; fields; base; alloc; pos } ->
      Ast.New { tname;
                fields = List.map (fun (n, e) -> (n, sub e)) fields;
                base = subo base;
                alloc = Option.map sub alloc;
                pos }
  | Ast.NewEnum { tname; args; alloc; pos } ->
      Ast.NewEnum { tname;
                    args = List.map sub args;
                    alloc = Option.map sub alloc;
                    pos }
  | Ast.MethodCall { receiver; name; args; pos } ->
      Ast.MethodCall { receiver = sub receiver; name;
                       args = List.map sub args; pos }
  | Ast.EnumLit { tname; variant; args; pos } ->
      let args = match args with
        | Ast.EATuple es -> Ast.EATuple (List.map sub es)
        | Ast.EAStruct fs -> Ast.EAStruct (List.map (fun (n, e) -> (n, sub e)) fs)
      in
      Ast.EnumLit { tname; variant; args; pos }
  | Ast.Match { scrutinee; arms; pos } ->
      let arms = List.map (fun (a : Ast.match_arm) ->
        { a with
          guard = (match a.guard with None -> None | Some g -> Some (sub g));
          body = sub a.body }) arms in
      Ast.Match { scrutinee = sub scrutinee; arms; pos }
  | Ast.Block (stmts, p) ->
      Ast.Block (List.map (subst_var_stmt ~from ~to_) stmts, p)
  | Ast.Orelse (a, b, p) -> Ast.Orelse (sub a, sub b, p)
  | Ast.Try (e', p) -> Ast.Try (sub e', p)
  | Ast.If { cond; then_blk; else_blk; pos } ->
      Ast.If { cond = sub cond;
               then_blk = List.map (subst_var_stmt ~from ~to_) then_blk;
               else_blk =
                 (match else_blk with
                  | None -> None
                  | Some b -> Some (List.map (subst_var_stmt ~from ~to_) b));
               pos }

and subst_var_stmt ~from ~to_ (s : Ast.stmt) : Ast.stmt =
  let sub = subst_var_expr ~from ~to_ in
  let sub_block = List.map (subst_var_stmt ~from ~to_) in
  match s with
  | Ast.Let { name; value; ty_ann; is_mut; pos } ->
      Ast.Let { name; value = sub value; ty_ann; is_mut; pos }
  | Ast.LetTuple { names; value; is_mut; pos } ->
      Ast.LetTuple { names; value = sub value; is_mut; pos }
  | Ast.LetElse { pat; value; else_body; pos } ->
      Ast.LetElse { pat; value = sub value;
                    else_body = sub_block else_body; pos }
  | Ast.Assign { path; value; pos } ->
      (* A bare `i = ...` reassign should rename to `gensym = ...` too,
         since the body's `i` is the gensym in the emitted code.  Single-
         segment paths matching `from` are remapped; qualified paths stay. *)
      let path = match path with [n] when n = from -> [to_] | p -> p in
      Ast.Assign { path; value = sub value; pos }
  | Ast.AssignField { target; field; value; pos } ->
      Ast.AssignField { target = sub target; field; value = sub value; pos }
  | Ast.AssignIndex { base; index; value; pos } ->
      Ast.AssignIndex { base = sub base; index = sub index; value = sub value; pos }
  | Ast.AssignDeref { target; value; pos } ->
      Ast.AssignDeref { target = sub target; value = sub value; pos }
  | Ast.Return (eo, p) -> Ast.Return ((match eo with None -> None | Some e -> Some (sub e)), p)
  | Ast.ExprStmt e -> Ast.ExprStmt (sub e)
  | Ast.Tail e -> Ast.Tail (sub e)
  | Ast.While { cond; body } ->
      Ast.While { cond = sub cond; body = sub_block body }
  | Ast.For { var; range; body; pos } ->
      (* If a nested `for` declares the same name, it shadows and we stop
         substituting inside.  Exile disallows shadowing, but the rewriter
         stays robust. *)
      if var = from then
        Ast.For { var; range = sub range; body; pos }
      else
        Ast.For { var; range = sub range; body = sub_block body; pos }
  | Ast.Defer { body; pos } ->
      Ast.Defer { body = sub_block body; pos }
  | Ast.With { target; name; body; pos } ->
      Ast.With { target = sub target; name; body = sub_block body; pos }
  | Ast.Break _ | Ast.Continue _ -> s

let lookup_const ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_map
      (fun (p, n, info) ->
        if p = mod_path && n = name then Some info else None)
      ctx.consts)

(* Evaluate an array size / repeat count: an integer literal, a reference
   to an int `const` (bare or qualified, value already folded), or a
   constant expression over those.  Shared by `[T; N]` type resolution and
   `[v; N]` repeat literals. *)
let eval_const_size ctx (e0 : Ast.expr) : int =
  let rec sz (e : Ast.expr) : int =
    match e with
    | Ast.IntLit (n, _) -> n
    | Ast.Neg (a, _) -> - (sz a)
    | Ast.BitNot (a, _) -> lnot (sz a)
    | Ast.BinOp (op, l, r, p) ->
        let a = sz l and b = sz r in
        (match op with
         | Ast.Add -> a + b | Ast.Sub -> a - b | Ast.Mul -> a * b
         | Ast.Div -> if b = 0 then Error.failf p "division by zero" else a / b
         | Ast.Mod -> if b = 0 then Error.failf p "modulo by zero" else a mod b
         | Ast.BitAnd -> a land b | Ast.BitOr -> a lor b
         | Ast.BitXor -> a lxor b
         | Ast.Shl -> a lsl b | Ast.Shr -> a asr b
         | _ -> Error.failf p "array size must be a constant integer expression")
    | Ast.Var (n, p) ->
        (match lookup_const ctx [n] with
         | Some (_, _, Some v) -> v
         | Some (_, _, None) ->
             Error.failf p
               "array size '%s' is not a known integer at exile time (a \
                bool const, or a `sizeof`/`as`-based value that folds to \
                a C expression)" n
         | None ->
             Error.failf p
               "array size must be an integer literal or `const`, got '%s'" n)
    | Ast.EnumLit { tname; variant; args = Ast.EATuple []; pos = p }
      when tname <> [] ->
        (match lookup_const ctx (tname @ [variant]) with
         | Some (_, _, Some v) -> v
         | Some (_, _, None) ->
             Error.failf p
               "array size is not a known integer at exile time (a bool \
                const, or a `sizeof`/`as`-based value that folds to a C \
                expression)"
         | None -> Error.failf p "array size must be an integer literal or `const`")
    | other ->
        Error.failf (Ast.expr_pos other)
          "array size must be a constant integer expression"
  in
  sz e0

let lookup_struct ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_opt
      (fun s -> s.sname_path = mod_path @ [name])
      ctx.structs)

let lookup_enum ctx path =
  walk_scope_up ctx path ~resolve:(fun mod_path name ->
    List.find_opt
      (fun e -> e.ename_path = mod_path @ [name])
      ctx.enums)

(* Look up a struct by its absolute path among the program's skeleton
   structs and any monomorphic instances generated so far.  Used
   after type_of has produced `TStruct path` (path may be mangled). *)
let resolve_struct_by_path ctx path =
  match Mono.find_struct ctx.instances path with
  | Some is -> Some is
  | None ->
      List.find_opt (fun s -> s.sname_path = path) ctx.structs

let resolve_enum_by_path ctx path =
  match Mono.find_enum ctx.instances path with
  | Some ie -> Some ie
  | None ->
      List.find_opt (fun e -> e.ename_path = path) ctx.enums

(* `extern struct Foo;` is opaque — exile knows the name but not the
   layout, so the only legal shape is a pointer.  Same goes for the
   `c_void` type: there is no value of type `void` in C, only `void
   *`.  This walks a resolved type and rejects bare `TExtStruct` /
   `TCVoid` outside any `TPtr` wrapper.  Anywhere under `TPtr` is
   fine; tuples are recursed into. *)
(* Walk a resolved type and reject bare `TExtStruct` (opaque) / `TCVoid`
   outside any `TPtr` wrapper.  Exposed extern struct names live in
   [~exposed]; they're treated as ordinary value types with a known
   layout (matching the C header pulled in via @c_include). *)
let rec forbid_naked_opaque ?(exposed = []) pos = function
  | TExtStruct n when not (List.mem n exposed) ->
      Error.failf pos
        "opaque type '%s' can only be used through a pointer (`*%s`) — \
         exile doesn't know its layout (use `extern struct %s { ... }` \
         to expose fields)" n n n
  | TExtStruct _ -> ()
  | TCVoid ->
      Error.failf pos
        "'c_void' has no values — only `*c_void` is usable as a type"
  | TPtr _ -> ()
  | TTuple ts -> List.iter (forbid_naked_opaque ~exposed pos) ts
  | TArray { elem; _ } -> forbid_naked_opaque ~exposed pos elem
  | _ -> ()


(* Find a variant by name in an enum's variant list, returning its
   (tag, variant_sig).  Tag is the index — codegen reads positions
   from the same list, so this is the canonical numbering. *)
let find_variant (e : enum_sig) name : (int * variant_sig) option =
  let rec walk i = function
    | [] -> None
    | (vs : variant_sig) :: _ when vs.vsname = name -> Some (i, vs)
    | _ :: rest -> walk (i + 1) rest
  in
  walk 0 e.evariants

(* Scope-aware analogue of `Ir.type_of_ann`: rewrites `TyStruct path`
   so the resulting `TStruct` carries the *absolute* struct path,
   resolved against the surrounding scope.  Without this, a fn or
   field declared as `: Point` inside `mod foo` would carry the
   relative path `["Point"]`, while values of the same type carry
   the absolute `["foo"; "Point"]` — and `typ_eq` would reject the
   match.

   Unknown type names are a hard error: the struct and enum indexes are
   fully built before any annotation is resolved, so a name that misses
   every lookup is a genuine typo (or a reference to an undeclared type),
   not a forward reference.  [pos] anchors that error at the annotation's
   decl site — callers pass it; the public wrapper defaults to Pos.zero
   for the few sites without a handy position. *)
(* Registry of `impl Trait for Type` pairs, as (trait-name, target
   abs-path).  Populated by [expand_impls] (which resolves both the trait
   and the target), read by [resolve_call_dispatch] to enforce a generic
   fn's `<T: Trait>` bounds at instantiation.  Trait names are matched by
   their last path segment — sufficient while traits are uniquely named
   (the conformance check already rejects ambiguity). *)
let trait_impl_table : (string * string list) list ref = ref []

(* `((trait-name, target-abs-path), [assoc-name, projected-typ])` for every
   `impl Trait for Foo { type Item = T; ... }` block.  Populated by the same
   pre-pass that fills [trait_impl_table], so projection lookups resolve
   order-independently with conformance.  Read by:
   - [resolve_type_ann_raw]'s hook on a `[head; assoc]` path — produces a
     [TAssocProj] node whose [normalize_apps] projection follows this table.
   - [normalize_apps] on [TAssocProj] with a concrete head — projects to the
     recorded typ once the head is monomorphic.
   Same naming model as [trait_impl_table]: trait matched by last segment. *)
let trait_assoc_table
  : ((string * string list) * (string * typ) list) list ref = ref []

(* DR-025 trait-decl assoc registry: maps each declared trait's last
   segment to its `trassoc` list.  Populated from `flat.traits` at the
   start of [expand_impls] (BEFORE any impl is processed) so the
   resolver can answer "does this bound's trait declare this assoc?"
   without needing an impl entry.  Read by [try_resolve_assoc_proj]'s
   TVar-bound shortcut: when head=`F` and ctx.tbounds has `F: Trait`
   and `Trait` declares `assoc`, return `TAssocProj` even when no
   `impl Trait for X` is registered yet — defers concrete
   resolution to mono time.  This unblocks prelude-synthesised
   adapter impls (`impl<I: Iterator, F: Fn1> Iterator for Map<I,
   F> { type Item = F::Output; ... }`) where Fn1 impls land in
   user code after the synth runs. *)
let trait_decl_assocs : (string * string list) list ref = ref []

let typ_head_path = function
  | TStruct p | TEnum p -> Some p
  | TStructApp { path; _ } | TEnumApp { path; _ } -> Some path
  | _ -> None

(* Try to resolve a 2-segment path `[head; assoc]` as an associated-type
   projection.  Returns [Some t] when [head] is a tparam (→ `TAssocProj`
   over the tparam, projected at instance time by [normalize_apps]) or a
   concrete struct/enum (→ direct projection if the impl is registered,
   else [None]).  Returns [None] when [head] is neither in scope nor a
   declared type — caller falls through to the standard "unknown type"
   error.  Raises [Error] on ambiguity (≥2 traits define the same assoc
   for the same head). *)
let try_resolve_assoc_proj ~pos ctx path : typ option =
  match path with
  | [ head; assoc ] ->
      let head_typ_opt : typ option =
        if List.mem head ctx.tparams then Some (Ir.TVar head)
        else
          match lookup_struct ctx [head] with
          | Some s when s.stparams = [] -> Some (Ir.TStruct s.sname_path)
          | _ ->
              (match lookup_enum ctx [head] with
               | Some e when e.etparams = [] -> Some (Ir.TEnum e.ename_path)
               | _ -> None)
      in
      (match head_typ_opt with
       | None -> None
       | Some head_typ ->
           let head_path =
             match head_typ with
             | TVar _ -> None
             | _ -> typ_head_path head_typ
           in
           (* Ambiguity is per trait NAME, not per (trait, target):
              `impl Iterator for Count` + `impl Iterator for Words`
              still resolve `I::Item` unambiguously (one trait, one
              `type Item`).  Only two distinct traits both naming
              `Item` are ambiguous.  For TVar head, every impl is a
              candidate (the binding is unknown); for concrete head,
              restrict to impls whose target matches. *)
           let candidate_traits =
             List.sort_uniq compare
               (List.filter_map
                  (fun ((trait, target), assocs) ->
                    let head_matches = match head_path with
                      | None -> true
                      | Some hp -> target = hp
                    in
                    if head_matches && List.mem_assoc assoc assocs
                    then Some trait else None)
                  !trait_assoc_table)
           in
           (* For TVar head with bound `<F: Trait>`, restrict candidates
              to the trait names on F's bound list — handles cases like
              `F: Fn1` where both Fn1 and Fn2 define `Output`, but the
              bound pins which one applies.  Trait identity here uses
              the last path segment, mirroring [trait_impl_table]'s
              lookup convention. *)
           let candidate_traits =
             match head_typ with
             | TVar n ->
                 let bound_trait_names =
                   List.filter_map
                     (fun (tp, trait_path, _assocs) ->
                       if tp = n then
                         match List.rev trait_path with
                         | last :: _ -> Some last
                         | [] -> None
                       else None)
                     ctx.tbounds
                 in
                 (match bound_trait_names with
                  | [] -> candidate_traits
                  | _ ->
                      List.filter (fun t -> List.mem t bound_trait_names)
                        candidate_traits)
             | _ -> candidate_traits
           in
           (* DR-021 — bound-side assoc shortcut.  When head is a tparam
              `F` and its bound carries an assoc binding for the
              requested `assoc` name (e.g. `<F: |int|->int>` provides
              ("Arg", TInt) and ("Output", TInt)), return that bound
              type directly — no impl-side lookup needed.  Tried
              BEFORE the candidate_traits dispatch so a sugar bound
              shortcuts even when the impl isn't reachable from the
              local scope. *)
           let bound_direct =
             match head_typ with
             | TVar n ->
                 List.find_map (fun (tp, _trait_path, assocs) ->
                   if tp = n then List.assoc_opt assoc assocs
                   else None)
                   ctx.tbounds
             | _ -> None
           in
           (* DR-025 trait-decl shortcut.  When head is a tparam `F`
              with bound `F: Trait` and `Trait` declares `assoc` in
              its `trassoc`, return `TAssocProj { head=TVar F; assoc
              }` even when no `impl Trait for X` is registered yet.
              The projection stays deferred — mono resolves it once F
              binds to a concrete type whose impl is visible.  This
              unblocks prelude-synthesised adapter impls (Map / Filter
              / ...) where Fn1 impls aren't in scope when the synth
              registers, and is also a small correctness widening:
              the user-side `<F: Trait>(...) -> F::Output` shape now
              compiles in any context, not only when an unrelated
              `impl Trait for X` happens to be reachable. *)
           let bound_decl_match =
             match head_typ with
             | TVar n ->
                 List.exists (fun (tp, trait_path, _assocs) ->
                   if tp <> n then false
                   else
                     match List.rev trait_path with
                     | last :: _ ->
                         (match List.assoc_opt last !trait_decl_assocs with
                          | Some assocs -> List.mem assoc assocs
                          | None -> false)
                     | [] -> false)
                   ctx.tbounds
             | _ -> false
           in
           (match bound_direct with
            | Some t -> Some t
            | None when bound_decl_match ->
                Some (TAssocProj { head = head_typ; assoc })
            | None ->
                (match candidate_traits with
                 | [] -> None
                 | [_one] -> Some (TAssocProj { head = head_typ; assoc })
                 | _ ->
                     (* Multiple traits define `assoc` for [head]: until
                        `<T as Trait>::Item` lands, this is unresolvable. *)
                     Error.failf pos
                       "ambiguous associated-type projection '%s::%s' \
                        (multiple traits define '%s' — qualified \
                        `<%s as Trait>::%s` is not yet supported)"
                       head assoc assoc head assoc)))
  | _ -> None

(* DR-002 FP-1 — `type Name<T...> = Type;` lookup with ancestor-
   scope walk-up matching `lookup_struct`'s shape.  Multi-segment
   paths require absolute match; single-segment paths walk up
   `ctx.scope` looking for `<scope>::<name>`. *)
let lookup_type_alias ctx path =
  let abs_match try_path =
    List.find_opt
      (fun (pmod, ta) -> pmod @ [ta.Ast.taname] = try_path)
      ctx.type_aliases
  in
  match path with
  | [n] ->
      let rec walk_up scope =
        match abs_match (scope @ [n]) with
        | Some hit -> Some hit
        | None ->
            (match scope with
             | [] -> None
             | _ -> walk_up (List.rev (List.tl (List.rev scope))))
      in
      walk_up ctx.scope
  | _ -> abs_match path

let rec resolve_type_ann_raw ?(visited = []) ~pos ctx ann =
  let recur = resolve_type_ann_raw ~visited ~pos ctx in
  match ann with
  | Ast.TyInt { signed; width } -> TInt { signed; width }
  | Ast.TyCInt { signed } -> TCInt { signed }
  | Ast.TyCShort { signed } -> TCShort { signed }
  | Ast.TyCLong { signed } -> TCLong { signed }
  | Ast.TyCChar -> TCChar
  | Ast.TyCSChar -> TCSChar
  | Ast.TyCUChar -> TCUChar
  | Ast.TyCVoid -> TCVoid
  | Ast.TyStr -> TString
  | Ast.TyBool -> TBool
  | Ast.TyFloat w -> TFloat w
  | Ast.TyTuple ts -> TTuple (List.map recur ts)
  | Ast.TyPtr t -> TPtr (recur t)
  | Ast.TyOwnPtr t -> TOwnPtr (recur t)
  | Ast.TyConstPtr t -> TConstPtr (recur t)
  | Ast.TyArray { elem; size } ->
      let elem' = recur elem in
      let n = eval_const_size ctx size in
      if n <= 0 then
        Error.failf pos "array size must be positive, got %d" n;
      TArray { elem = elem'; size = n }
  | Ast.TySelf ->
      (* parse_impl_block replaces bare-`self` with the target type, so a
         surviving TySelf means `self` was used outside an impl method. *)
      Error.failf pos
        "bare 'self' is only allowed as the receiver of an 'impl' method"
  | Ast.TyFnPtr { params; ret } ->
      TFnPtr { params = List.map recur params;
               ret = Option.map recur ret }
  | Ast.TyStruct { path; args } ->
      (* DR-002 FP-1 — `type Name<T...> = Type;` is checked FIRST so
         an alias that happens to share a path tail with a struct
         still wins (matches OCaml's `type t = u` substitution
         semantics).  Cycle-guard: each recursive step adds the
         alias's absolute path to [visited]; a hit on a path already
         there aborts with a clean error. *)
      (match lookup_type_alias ctx path with
       | Some (_pmod, ta) ->
           let abs_path = _pmod @ [ta.Ast.taname] in
           if List.mem abs_path visited then
             Error.failf pos
               "type alias cycle through '%s' — alias resolution \
                would loop"
               (String.concat "::" abs_path);
           let resolved_args = List.map recur args in
           let expected = List.length ta.Ast.tatparams in
           let got = List.length resolved_args in
           if expected <> got then
             Error.failf pos
               "type alias '%s' expects %d generic argument(s), got %d"
               (String.concat "::" abs_path) expected got;
           let bindings = List.combine ta.Ast.tatparams resolved_args in
           (* Resolve the target with the alias's tparams added to
              ctx.tparams (so TVars in the body resolve to TVar n),
              then substitute the call-site args into them. *)
           let inner_ctx =
             { ctx with tparams = ta.Ast.tatparams @ ctx.tparams } in
           let target_t =
             resolve_type_ann_raw
               ~visited:(abs_path :: visited) ~pos inner_ctx
               ta.Ast.tatarget
           in
           subst_typ bindings target_t
       | None when args = [] ->
           (* Non-generic case: tparam reference / extern type / extern
              struct / struct / enum.  ext_types / ext_structs are flat
              (single C symbol name); qualified paths like `raw::ULONG`
              accept the path as long as the last segment matches.
              Tparam ref is single-segment only. *)
           let last = match List.rev path with
             | n :: _ -> n
             | [] -> failwith "internal: resolve_type_ann_raw got empty path"
           in
           (match path with
            | [n] when List.mem n ctx.tparams -> TVar n
            | _ when List.mem last ctx.ext_types -> TExtAlias last
            | _ when List.mem last ctx.ext_structs -> TExtStruct last
            | _ ->
                (match lookup_struct ctx path with
                 | Some s -> TStruct s.sname_path
                 | None ->
                     (match lookup_enum ctx path with
                      | Some e -> TEnum e.ename_path
                      | None ->
                          (match try_resolve_assoc_proj ~pos ctx path with
                           | Some t -> t
                           | None ->
                               Error.failf pos "unknown type '%s'"
                                 (String.concat "::" path)))))
       | None ->
           (* Generic application `Foo<T1, T2>`.  We do NOT instantiate
              here: args may still contain free `TVar`s (a generic
              `impl`/fn skeleton, e.g. `self: Pair<A, B>`).  Produce a
              TStructApp / TEnumApp carrying the skeleton's absolute
              path + resolved args; `resolve_type_ann` normalises it
              to a flat instance once every arg is concrete.  Arity
              is checked here (no concreteness needed). *)
           let resolved_args = List.map recur args in
           let check_arity ~name ~tparams =
             let expected = List.length tparams in
             let got = List.length resolved_args in
             if expected <> got then
               Error.failf pos
                 "type '%s' expects %d generic argument(s), got %d"
                 name expected got
           in
           (match lookup_struct ctx path with
            | Some s ->
                check_arity ~name:(String.concat "::" s.sname_path)
                  ~tparams:s.stparams;
                TStructApp { path = s.sname_path; args = resolved_args }
            | None ->
                (match lookup_enum ctx path with
                 | Some e ->
                     check_arity ~name:(String.concat "::" e.ename_path)
                       ~tparams:e.etparams;
                     TEnumApp { path = e.ename_path; args = resolved_args }
                 | None ->
                     Error.failf pos
                       "unknown generic type '%s'"
                       (String.concat "::" path))))

(* Instantiate every fully-concrete generic application in [t] to its flat
   mono instance (registering the instance with Mono), bottom-up.  A
   TStructApp / TEnumApp that still holds a free `TVar` is left as-is —
   it belongs to a skeleton and will be normalised when re-elaborated with
   concrete bindings.  This is what keeps the rest of the pipeline seeing
   only flat `TStruct`/`TEnum` instances, never an application node. *)
let rec normalize_apps ctx t =
  match t with
  | TStructApp { path; args } ->
      let args = List.map (normalize_apps ctx) args in
      if List.for_all is_concrete args then
        match List.find_opt
                (fun (s : struct_sig) -> s.sname_path = path && s.stparams <> [])
                ctx.structs with
        | Some skel ->
            let inst =
              Mono.instantiate_struct ctx.instances
                ~normalize:(normalize_apps ctx) skel args
            in
            TStruct inst.sname_path
        | None -> TStructApp { path; args }
      else TStructApp { path; args }
  | TEnumApp { path; args } ->
      let args = List.map (normalize_apps ctx) args in
      if List.for_all is_concrete args then
        match List.find_opt
                (fun (e : enum_sig) -> e.ename_path = path && e.etparams <> [])
                ctx.enums with
        | Some skel ->
            let inst =
              Mono.instantiate_enum ctx.instances
                ~normalize:(normalize_apps ctx) skel args
            in
            TEnum inst.ename_path
        | None -> TEnumApp { path; args }
      else TEnumApp { path; args }
  | TPtr inner -> TPtr (normalize_apps ctx inner)
  | TOwnPtr inner -> TOwnPtr (normalize_apps ctx inner)
  | TConstPtr inner -> TConstPtr (normalize_apps ctx inner)
  | TTuple ts -> TTuple (List.map (normalize_apps ctx) ts)
  | TFnPtr { params; ret } ->
      TFnPtr { params = List.map (normalize_apps ctx) params;
               ret = Option.map (normalize_apps ctx) ret }
  | TAssocProj { head; assoc } ->
      (* If the head is concrete we can look up the recorded
         `type assoc = T;` from the impl and replace the projection
         with that typ (then normalise it too — the recorded typ may
         itself be a generic application).  Skeleton case (head still
         carries a `TVar`) keeps the projection node intact; it gets
         normalised once monomorphization substitutes a concrete head.
         DR-026 - mono-instance heads (`VecIter_i32`) match against
         skeleton-registered impls (`VecIter`) via Mono.is_instance_of,
         same pattern as DR-017 mono-instance trait recognition. *)
      let head = normalize_apps ctx head in
      if not (is_concrete head) then TAssocProj { head; assoc }
      else
        (match typ_head_path head with
         | None -> TAssocProj { head; assoc }
         | Some hp ->
             (* DR-026 - substitute skeleton-tparams with the mono
                instance's recorded args when the impl is registered
                under the skeleton path but the head we're projecting
                from is a mono instance.  Without this, `I::Item` on
                `I = VecIter_i32` projects through `(Iterator,
                VecIter)`'s recorded `Item = TVar T`, leaving T
                unsubstituted and reaching codegen as an unknown type. *)
             (* Args carried by the concrete head — from a registered mono
                instance's `sinstance_args`, or directly off a still-applied
                TStructApp/TEnumApp. *)
             let inst_args =
               match Mono.find_struct ctx.instances hp with
               | Some { sinstance_args = Some a; _ } -> a
               | _ ->
                   (match Mono.find_enum ctx.instances hp with
                    | Some { einstance_args = Some a; _ } -> a
                    | _ ->
                        (match head with
                         | TStructApp { args; _ } | TEnumApp { args; _ } -> args
                         | _ -> []))
             in
             let matches =
               List.filter_map
                 (fun ((_trait, target), assocs) ->
                   let target_matches =
                     target = hp || Mono.is_instance_of target hp
                   in
                   if target_matches && List.mem_assoc assoc assocs then
                     (* tparam NAMES come from the skeleton `target` — the
                        mono instance itself carries `stparams=[]`, so the
                        old `instance.stparams` read was always empty. *)
                     let skel_tps =
                       match lookup_struct ctx target with
                       | Some s -> s.stparams
                       | None ->
                           (match lookup_enum ctx target with
                            | Some e -> e.etparams | None -> [])
                     in
                     let binds =
                       if skel_tps = [] || inst_args = [] then []
                       else (try List.combine skel_tps inst_args with _ -> [])
                     in
                     let recorded = List.assoc assoc assocs in
                     Some (if binds = [] then recorded
                           else subst_typ binds recorded)
                   else None)
                 !trait_assoc_table
             in
             (match matches with
              | [t] -> normalize_apps ctx t
              | _ -> TAssocProj { head; assoc }))
  | other -> other

(* Public entry point.  Resolve the annotation, substitute the fn-instance
   tvar bindings (so a generic-instance body sees concrete types where the
   source mentions the fn's type parameters), then normalise any now-concrete
   generic application to its flat mono instance. *)
let resolve_type_ann ?(pos = Pos.zero) ctx ann =
  normalize_apps ctx
    (subst_typ ctx.tvar_bindings (resolve_type_ann_raw ~pos ctx ann))

(* Convert AST-side tbounds (`Ast.type_ann` assoc bindings, e.g. on a
   func / impl_block read from source) into ctx-side tbounds (resolved
   `typ` bindings).  Threaded into [fn_ctx.tbounds] at every body-elab
   site so the DR-021 assoc shortcut + the DR-017 bound-aware
   disambiguation read concrete types directly.  The assoc type_anns
   are resolved against the ctx-at-call-time (which already has
   tparams in scope); they may reference tparams (`<F: |T|->T>` where
   T is also a tparam) and that flows through resolve_type_ann's
   TVar handling. *)
(* Resolve bounds in source order, threading previously-resolved
   bounds back into ctx so later bounds can refer to earlier ones'
   trait-decl assocs.  Example: `impl<I: Iterator, P: |I::Item|->bool>`
   needs `I: Iterator` already in ctx.tbounds before resolving
   `I::Item` inside P's bound. *)
let resolve_ast_tbounds ~pos ctx
    (tbounds : (string * string list * (string * Ast.type_ann) list) list)
    : (string * string list * (string * typ) list) list =
  let (rev, _ctx) =
    List.fold_left (fun (acc, ctx) (tp, trait_path, ast_assocs) ->
      let assocs =
        List.map (fun (an, ann) -> (an, resolve_type_ann ~pos ctx ann))
          ast_assocs
      in
      let resolved = (tp, trait_path, assocs) in
      (resolved :: acc, { ctx with tbounds = resolved :: ctx.tbounds }))
      ([], ctx) tbounds
  in
  List.rev rev

(* Expand a (declared, actual) tparam-inference pair where the declared
   type is a generic application (`Pair<T, int>`) and the actual is its
   flat instance (`Pair_i32_int`).  Plain unification can't see the args
   a flat instance dropped, so recover them from the instance's recorded
   `sinstance_args` / `einstance_args` and pair them up element-wise.
   Recurses through pointers and nested applications.  Used wherever a
   generic application meets a concrete value: fn-call dispatch and
   struct/enum-literal construction. *)
let rec expand_inst_pair ctx (decl, act) =
  match decl, act with
  | TStructApp { args = dargs; _ }, TStruct inst_path ->
      (match Mono.find_struct ctx.instances inst_path with
       | Some { sinstance_args = Some iargs; _ }
         when List.length iargs = List.length dargs ->
           List.concat_map (expand_inst_pair ctx) (List.combine dargs iargs)
       | _ -> [ (decl, act) ])
  | TEnumApp { args = dargs; _ }, TEnum inst_path ->
      (match Mono.find_enum ctx.instances inst_path with
       | Some { einstance_args = Some iargs; _ }
         when List.length iargs = List.length dargs ->
           List.concat_map (expand_inst_pair ctx) (List.combine dargs iargs)
       | _ -> [ (decl, act) ])
  | TPtr d, TPtr a -> expand_inst_pair ctx (d, a)
  | TOwnPtr d, TOwnPtr a -> expand_inst_pair ctx (d, a)
  | TConstPtr d, TConstPtr a -> expand_inst_pair ctx (d, a)
  (* `*const T` declaration paired with a `*U` actual: pointee-immutability
     coercion is implicit, so infer `T = U` from the pointees alone (the
     coercion itself runs after instance-resolution in the caller's
     type check). *)
  | TConstPtr d, TPtr a -> expand_inst_pair ctx (d, a)
  (* DR-030 OWN-D1: `own *T` decl paired with `*U` / `*const U`
     actual — the call-site coercion narrows ownership down, so
     infer T = U from the pointees.  The coercion check runs after
     inference and ensures the lend is legal. *)
  | TOwnPtr d, TPtr a -> expand_inst_pair ctx (d, a)
  | TOwnPtr d, TConstPtr a -> expand_inst_pair ctx (d, a)
  | TPtr d, TOwnPtr a -> expand_inst_pair ctx (d, a)
  | TConstPtr d, TOwnPtr a -> expand_inst_pair ctx (d, a)
  | _ -> [ (decl, act) ]

(* Recognise an integer literal expression — bare or negated — for type-fitting
   checks at let-binding sites. *)
let rec expr_int_lit = function
  | Ast.IntLit (n, _) -> Some n
  | Ast.Neg (e, _) ->
      (match expr_int_lit e with Some n -> Some (-n) | None -> None)
  | Ast.Cast (e, _, _) ->
      (* Transparent through `as`: `255 as u8` is a literal whose value
         survives the cast unchanged when it already fits the target.
         Used by the `for ... ..=MAX` overflow detector. *)
      expr_int_lit e
  | _ -> None

(* True when [expr] is an integer literal that fits into [target] (a TInt).
   Used at assignment / field-init sites to allow `let x: i8 = 5;` even
   though `5` is `i32` by default. *)
let int_lit_fits expr target =
  match expr_int_lit expr with
  | Some n when is_int_like target -> int_fits n target
  | _ -> false

(* First duplicate key in [xs] under [key], or None.  Replaces the
   ad-hoc O(n²) `List.exists` loops scattered around the typechecker. *)
let find_dup ~key xs =
  let rec loop seen = function
    | [] -> None
    | x :: rest ->
        let k = key x in
        if List.mem k seen then Some k else loop (k :: seen) rest
  in
  loop [] xs

(* C89 reserved words.  Exile identifiers that survive into the generated C
   without mangling (locals, parameters) must not collide.  Top-level fn
   names get the `ex_` prefix from mangle, so they are also safe — but the
   check still rejects them up front for a clearer error. *)
let c_keywords = [
  "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do";
  "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if";
  "int"; "long"; "register"; "return"; "short"; "signed"; "sizeof";
  "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"; "void";
  "volatile"; "while"
]

let check_c_ident pos kind name =
  if List.mem name c_keywords then
    Error.failf pos "%s '%s' is a reserved C keyword" kind name

(* Compile-time builtin signatures.  The codegen layer carries a parallel
   table of emitters keyed by name; this module owns only the type-checking
   side.  Builtins are looked up by single-segment paths — no module
   qualification. *)
type builtin_sig = {
  bname : string;
  bcheck : ctx:fn_ctx -> pos:Pos.t -> args:texpr list -> allow_void:bool -> typ;
}

(* True when [path] names a struct/enum whose `@debug` printer the codegen
   will synthesize.  Drives the `print(v)` dispatch on aggregate types. *)
(* A generic-struct instance (`Pair_i32_bool`) lives in the Mono state,
   not in ctx.structs (which holds the skeletons), so check both — the
   instance inherits `sis_debug` from its skeleton. *)
let struct_is_debug (ctx : fn_ctx) path =
  List.exists (fun (s : struct_sig) -> s.sname_path = path && s.sis_debug)
    ctx.structs
  || (match Mono.find_struct ctx.instances path with
      | Some s -> s.sis_debug | None -> false)
let enum_is_debug (ctx : fn_ctx) path =
  List.exists (fun (e : enum_sig) -> e.ename_path = path && e.eis_debug)
    ctx.enums
  || (match Mono.find_enum ctx.instances path with
      | Some e -> e.eis_debug | None -> false)

(* `print` and `println` share the same one-printable-argument contract;
   they differ only in codegen (trailing newline).  [name] flows into the
   arity diagnostic so a misused `println(a, b)` reports `println`, not
   `print`. *)
let print_like_bcheck ~name = fun ~ctx ~pos ~args ~allow_void:_ ->
    match List.map (fun (a : texpr) -> a.ty) args with
    | [ TTuple _ ] ->
        Error.failf pos
          "cannot print a tuple; destructure with 'let (...)' first"
    | [ TStruct path ] when struct_is_debug ctx path -> t_i32
    | [ TStruct path ] ->
        Error.failf pos
          "cannot print a struct value (%s); print individual fields, \
           or mark the struct with `@debug`"
          (String.concat "::" path)
    | [ TPtr _ as t ] ->
        Error.failf pos
          "cannot print a pointer value (%s); deref or print a field"
          (typ_name t)
    | [ TConstPtr _ as t ] ->
        Error.failf pos
          "cannot print a const-pointer value (%s); deref or print a field"
          (typ_name t)
    | [ TFnPtr _ as t ] ->
        Error.failf pos
          "cannot print a function-pointer value (%s); call it or cast \
           to an int first" (typ_name t)
    | [ TArray _ as t ] ->
        Error.failf pos
          "cannot print an array value (%s); iterate and print each element"
          (typ_name t)
    | [ TNullPtr ] ->
        Error.failf pos "cannot print 'null'"
    | [ TEnum path ] when enum_is_debug ctx path -> t_i32
    | [ TEnum path ] ->
        Error.failf pos
          "cannot print an enum value (%s); match on it and print per variant, \
           or mark the enum with `@debug`"
          (String.concat "::" path)
    | [ TExtAlias n ] ->
        Error.failf pos
          "cannot print an 'extern type' value (%s); cast to a known \
           type first (e.g. `as int`)" n
    | [ TExtStruct n ] ->
        Error.failf pos
          "cannot print an 'extern struct' value (%s); only pointers \
           to opaque types are usable" n
    | [ (TCShort _ | TCLong _ | TCChar | TCSChar | TCUChar) as t ] ->
        Error.failf pos
          "cannot directly print a %s value; cast to a known type \
           first (e.g. `as int`)" (typ_name t)
    (* Allowlist: only the primitive shapes codegen actually knows how
       to format reach the codegen path (`printf_int_spec` for ints,
       `%s` for str, `%d` for bool).  Pre-fix every other singleton
       fell through to a catch-all `[_]→t_i32` and codegen asserted
       false — `println( * const T)`, `println(fn_ptr)`,
       `println([T; N])` all ICE'd.  Each rejection above already
       covers a known shape; the final catch-all here turns any
       leftover (TStructApp / TEnumApp / TAssocProj / TVar /
       TCVoid …) into a clean error instead of leaking to codegen. *)
    | [ (TInt _ | TCInt _ | TBool | TString | TFloat _) ] -> t_i32
    | [ ty ] ->
        Error.failf pos
          "cannot print a value of type %s; convert it to a printable \
           type first (int / bool / str, or `@debug`-mark the type)"
          (typ_name ty)
    | tys ->
        Error.failf pos "%s() takes exactly one argument, got %d"
          name (List.length tys)

let builtin_print = { bname = "print"; bcheck = print_like_bcheck ~name:"print" }
let builtin_println =
  { bname = "println"; bcheck = print_like_bcheck ~name:"println" }

(* GATE-3 (2026-06-10) — `free(alloc, p)` is two-arg and routes through
   the allocator seam, symmetric with `new(alloc)`.  The old one-arg
   `free(p)` lowered to libc `free()` while `new(a)` allocated through
   `a.alloc_fn` — fine on the host (default_allocator IS malloc/free),
   heap corruption with an arena or Amiga allocator.  No hidden state:
   the caller names the allocator, codegen emits
   `(a.free_fn)(a.state, p, sizeof(pointee))`.  With L1 provenance
   auto-drop in place, a manual `free(a, p)` is the rare early-release
   escape hatch — the good free is the one you don't write. *)
(* `ptr_offset(p, n)` — byte-pointer arithmetic for the arena's bump
   allocator (PORT-PREP P2, 2026-06-10).  Lowers to plain C pointer
   addition `(p + n)`; on m68k that is one ADDA — zero hidden cost.
   v1 keeps it byte-honest: the base must be a `u8` pointer (any
   ownership flavour — the arena passes its own buffer field), and
   the result is a plain BORROW `*u8` into the same storage: a
   derived pointer is a view, never a second owner. *)
let builtin_ptr_offset = {
  bname = "ptr_offset";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void:_ ->
    match args with
    | [ { ty = (TPtr p | TOwnPtr p | TConstPtr p); _ }; { ty = n; _ } ]
      when is_int_like n ->
        (match p with
         | TInt { signed = false; width = Ast.W8 } -> TPtr p
         | other ->
             Error.failf pos
               "'ptr_offset' steps in BYTES, so the base must be a u8 \
                pointer, got a pointer to %s — cast first" (typ_name other))
    | [ { ty = other; _ }; _ ] ->
        Error.failf pos
          "'ptr_offset' expects a u8 pointer base, got %s" (typ_name other)
    | xs ->
        Error.failf pos "ptr_offset(p, n) takes exactly two arguments, \
                         got %d" (List.length xs));
}

let builtin_free = {
  bname = "free";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void ->
    match args with
    | [ { ty = TStruct ["Allocator"]; _ }; { e = TRef _; _ } ] ->
        (* Syntactic guard: `free(a, &...)` is always wrong — `&`
           produces a stack-or-field address, never a heap pointer.
           Freeing it would corrupt the allocator's bookkeeping. *)
        Error.failf pos
          "'free' expects an owned pointer `own *T` (from 'new(alloc)'); \
           got '&...' which is a stack or field address — this would \
           corrupt the allocator"
    | [ { ty = TStruct ["Allocator"]; _ }; { ty = TOwnPtr _; _ } ]
      when allow_void -> t_i32
    | [ { ty = TStruct ["Allocator"]; _ }; { ty = TOwnPtr _; _ } ] ->
        Error.failf pos "'free' returns void, cannot use as a value"
    | [ { ty = TStruct ["Allocator"]; _ };
        { ty = (TPtr _ | TConstPtr _) as other; _ } ] ->
        (* Owner-sigil free-gate: a plain borrow `*T` / `*const T` is
           NOT an owned pointer.  Freeing a borrow risks a double-free
           against the owner's own `free`/auto-drop.  Only `own *T`
           (from `new(alloc)` / `Allocator.alloc`) is freeable. *)
        Error.failf pos
          "'free' expects an owned pointer `own *T`, got %s — a borrow \
           cannot be freed (the owner releases it).  Owned pointers come \
           from `new(alloc) T { ... }` or `Allocator.alloc`."
          (typ_name other)
    | [ { ty = TStruct ["Allocator"]; _ }; { ty = other; _ } ] ->
        Error.failf pos
          "'free' expects an owned pointer `own *T`, got %s" (typ_name other)
    | [ { ty = other; _ }; _ ] ->
        Error.failf pos
          "first argument of 'free' must be the Allocator that produced \
           the pointer (symmetric with `new(alloc)`), got %s"
          (typ_name other)
    | [ _ ] ->
        Error.failf pos
          "'free' takes the allocator and the owned pointer: \
           `free(alloc, p)` — the one-argument form is gone (it bypassed \
           the allocator seam; with an arena or Amiga allocator that \
           corrupts the heap)"
    | xs ->
        Error.failf pos "free() takes exactly two arguments, got %d"
          (List.length xs));
}

(* `type_name(expr)` yields a `str` with the Rust-style name of the
   expression's type ("i32", "*Point", "Result<i32, str>", ...).
   Compile-time only: codegen emits the name as a literal in `.rodata`,
   no runtime metadata.  Inside a generic fn body, the arg's type is
   substituted per monomorphic instance, so each instance bakes its own
   concrete name. *)
let builtin_type_name = {
  bname = "type_name";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void:_ ->
    match List.map (fun (a : texpr) -> a.ty) args with
    | [ TNullPtr ] ->
        Error.failf pos
          "type_name() needs a typed expression — 'null' has no \
           statically-known target type"
    | [_] -> TString
    | tys ->
        Error.failf pos "type_name() takes exactly one argument, got %d"
          (List.length tys));
}

(* `cstr_len(s)` — narrow `strlen` seam-op (DR-001).  Returns the byte
   length of a NUL-terminated `str` as `u32` (width-pinned at the seam,
   not C's `size_t`).  Compile-time-erased intrinsic — the call lowers
   to `(unsigned long)strlen(<expr>)` and pulls in `<string.h>`.  Used
   by `StringBuilder::push_str` for unknown-length C strings; user code
   prefers methods that already know the length.  No `mod sys::` wrap
   yet — the per-target seam mechanism is deferred. *)
let builtin_cstr_len = {
  bname = "cstr_len";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void:_ ->
    match List.map (fun (a : texpr) -> a.ty) args with
    | [ TString ] -> TInt { signed = false; width = Ast.W32 }
    | [ other ] ->
        Error.failf pos "'cstr_len' expects a `str`, got %s" (typ_name other)
    | xs ->
        Error.failf pos "cstr_len() takes exactly one argument, got %d"
          (List.length xs));
}

(* `mem_zero(ptr, n_bytes)` — fill `n_bytes` starting at `ptr` with
   zeros.  Used by HashMap (and any other slot-buffer collection) to
   initialise a freshly-alloc'd region without forging a default
   value for the embedded `K`/`V` fields — `malloc` returns garbage
   and the discriminator byte must read 0 (Empty).  Lowers to
   `memset(ptr, 0, n)` and pulls `<string.h>`. *)
let builtin_mem_zero = {
  bname = "mem_zero";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void:_ ->
    match List.map (fun (a : texpr) -> a.ty) args with
    | [ TPtr _; n_ty ] | [ TOwnPtr _; n_ty ] | [ TConstPtr _; n_ty ]
      when is_int_like n_ty -> TInt { signed = true; width = Ast.W32 }
    | [ p; _ ] when not (is_ptr p) ->
        Error.failf pos
          "'mem_zero' expects a pointer first argument, got %s"
          (typ_name p)
    | [ _; n ] ->
        Error.failf pos
          "'mem_zero' expects an integer byte count, got %s"
          (typ_name n)
    | xs ->
        Error.failf pos "mem_zero() takes exactly 2 arguments, got %d"
          (List.length xs));
}

(* `default_allocator()` — zero-arg prelude builtin that returns an
   `Allocator` wired to libc `malloc` / `free`.  Lets `println(x)`
   on a `impl Display for T` desugar to the writer pattern without
   threading an Allocator binding through every call site — the
   user gets a polymorphic print with the same ergonomics as a
   primitive print.  Codegen emits a per-program `static struct
   ex_Allocator exile_default_allocator(void)` helper plus its
   alloc/free thunks; usage triggers `#include <stdlib.h>`. *)
let builtin_default_allocator = {
  bname = "default_allocator";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void:_ ->
    match args with
    | [] -> TStruct ["Allocator"]
    | xs ->
        Error.failf pos
          "default_allocator() takes no arguments, got %d"
          (List.length xs));
}

let builtins =
  [ builtin_print; builtin_println; builtin_free; builtin_type_name;
    builtin_cstr_len; builtin_mem_zero; builtin_default_allocator;
    builtin_ptr_offset ]

let lookup_builtin = function
  | [ name ] -> List.find_opt (fun b -> b.bname = name) builtins
  | _ -> None

(* `Foo::Bar(args)` parses as `Call(["Foo"; "Bar"], args)` — the parser
   can't tell a fn call from an enum constructor.  `elab_expr` uses this
   helper to rewrite Call into EnumLit when the path resolves to an
   enum + variant. *)
let rewrite_call_as_enum_lit ctx path args pos =
  match path with
  | _ :: _ :: _ ->
      let (init, last) =
        let rev = List.rev path in
        (List.rev (List.tl rev), List.hd rev)
      in
      (match lookup_enum ctx init with
       | Some _ ->
           Some (Ast.EnumLit { tname = init; variant = last;
                               args = Ast.EATuple args; pos })
       | None -> None)
  | _ -> None

(* Mirror of `rewrite_call_as_enum_lit` for struct-variant construction:
   `Foo::Triangle { base: 1, height: 2 }` parses as a `StructLit` (the
   parser can't tell a struct from an enum struct-variant); typecheck
   redirects to `EnumLit (EAStruct fields)` when the path resolves to
   an enum + variant.  Functional update (`..base`) on enum variants
   isn't supported — error if base is set. *)
let rewrite_struct_lit_as_enum_lit ctx tname fields base pos =
  match tname with
  | _ :: _ :: _ ->
      let (init, last) =
        let rev = List.rev tname in
        (List.rev (List.tl rev), List.hd rev)
      in
      (match lookup_enum ctx init with
       | Some _ ->
           if base <> None then
             Error.failf pos
               "'..base' functional update is not supported on enum variants";
           Some (Ast.EnumLit { tname = init; variant = last;
                               args = Ast.EAStruct fields; pos })
       | None -> None)
  | _ -> None

(* Common arity + per-arg type + result-shape check shared by every
   call form (top-level fn, fn-pointer local, fn-pointer field, method).
   The four sites used to copy ~30 lines apiece varying only the
   human-readable [kind] / [name] / [variadic] / [param_tys] / [ret_ty];
   collapsing them into one helper means a new int-promotion or display
   tweak lands in one place.

   Error wording: arity uses "<kind> '<name>' expects N argument(s), got
   M" (variadic: "at least N").  Method/fn-pointer-field sites used to
   say "takes"; unified to "expects" — no test asserted on the old
   wording.  Arg/void diagnostics retain "'<name>' …" exactly as before
   (and the "got X" half of every message is preserved verbatim). *)
(* DR-052: an `own *T` value flowing into a borrow slot (`*T` /
   `*const T`) is a LOAN — restamp the value to the slot's borrow
   type so the move-pass (which keys consume on the stamped type)
   keeps the owner live for its own free/drop.  Applied uniformly at
   every borrow slot: call args, method receivers, let/assign,
   struct-literal fields, enum-variant args.  `own *T` and `*T` are
   the same `T*` in C — type-level only, zero codegen change.

   Only PLACE expressions can lend (the owner keeps living somewhere
   else).  A fresh owned RVALUE (`new(a) ...`, an own-returning call)
   in a borrow slot would leave the allocation with no owner at all —
   nothing tracks it, nothing releases it (re-audit F3 leak).  Reject
   those up front. *)
let loan_restamp ~slot_ty (te : texpr) =
  match slot_ty, te.ty with
  | (TPtr _ | TConstPtr _), TOwnPtr _ ->
      (match te.e with
       | TVar _ | TFieldAccess _ | TIndex _ | TDeref _ ->
           { te with ty = slot_ty }
       | _ ->
           Error.failf te.pos
             "this owned value is only borrowed here, so nothing would \
              own (or ever release) the allocation — bind it first \
              (`let x: own *T = ...;`), then lend `x`")
  | _ -> te

let check_call_args ~pos ~kind ~name ?(variadic=false) ~param_tys
                    ~raw_args ~targs ~ret_ty ~allow_void () : typ * texpr list =
  let expected_n = List.length param_tys in
  let got = List.length raw_args in
  let arity_ok = if variadic then got >= expected_n else got = expected_n in
  if not arity_ok then
    Error.failf pos
      (if variadic
       then "%s '%s' expects at least %d argument(s), got %d"
       else "%s '%s' expects %d argument(s), got %d")
      kind name expected_n got;
  let arg_tys = List.map (fun (a : texpr) -> a.ty) targs in
  let rec take n xs =
    if n <= 0 then [] else match xs with
      | [] -> [] | x :: rest -> x :: take (n - 1) rest
  in
  let rec drop n xs =
    if n <= 0 then xs else match xs with
      | [] -> [] | _ :: rest -> drop (n - 1) rest
  in
  let fixed_arg_tys = take expected_n arg_tys in
  List.iteri (fun i (exp, act) ->
    if not (coercible_to ~from:act ~to_:exp)
       && not (int_lit_fits (List.nth raw_args i) exp)
    then
      Error.failf pos
        "argument %d of '%s': expected %s, got %s"
        (i + 1) name (typ_name exp) (typ_name act))
    (List.combine param_tys fixed_arg_tys);
  (* DR-030 own-borrow: an `own *T` arg in a borrow slot is a loan,
     not a move — see [loan_restamp]. *)
  let targs' =
    List.map2 (fun param_ty arg -> loan_restamp ~slot_ty:param_ty arg)
      param_tys (take expected_n targs)
    @ drop expected_n targs
  in
  let result_ty =
    match ret_ty with
    | Some t -> t
    | None when allow_void -> t_i32
    | None ->
        Error.failf pos "'%s' returns void, cannot use as a value" name
  in
  (result_ty, targs')

(* Registry of `impl Trait for Type` pairs, as (trait-name, target
   abs-path).  Populated by [expand_impls] (which resolves both the trait
   and the target), read by [resolve_call_dispatch] to enforce a generic
   fn's `<T: Trait>` bounds at instantiation.  Trait names are matched by
   their last path segment — sufficient while traits are uniquely named
   (the conformance check already rejects ambiguity). *)
(* True when [ty] has an `impl <trait_name> for <ty>` registered.  The
   table keys hold the SKELETON path (`impl<T> Iterator for VecIter<T>`
   registers `["VecIter"]`), so mono-instance heads like `["VecIter_i32"]`
   match through [Mono.is_instance_of] — same pattern the `for x in iter`
   desugar uses at line ~3870 to recognise an Iterator receiver. *)
let type_impls_trait ~trait_name ty =
  match typ_head_path ty with
  | None -> false
  | Some path ->
      List.exists
        (fun (n, decl_path) ->
          n = trait_name
          && (decl_path = path || Mono.is_instance_of decl_path path))
        !trait_impl_table

(* Generic-call dispatch: if the resolved fn is generic, infer its
   tparams from the actual arg types (and from the surrounding expected
   type via a bidirectional seed pair `(skel.ret_ty, expected)`),
   instantiate, and return the instance's mangled name + concrete sig.
   For mono fns returns the skeleton's mangled name + sig unchanged.

   Caller is responsible for visibility + arity + arg-type checks; this
   helper owns only the inference / instantiation step.  When
   inference fails (under-determined T), `Mono.infer_tparams` raises
   with a hint to add a let / return type annotation. *)
let resolve_call_dispatch ~pos ~expected ?(recv_inst_args = []) ctx
    ~resolved_mod ~arg_tys
    (skel : fn_sig) : string * typ list * typ option =
  if skel.fn_tparams = [] then
    (skel.mangled, skel.param_tys, skel.ret_ty)
  else begin
    let n_fixed = List.length skel.param_tys in
    let got = List.length arg_tys in
    (* DR-002 C3 — arity check before `List.combine` over arg_tys.
       The callsite's [check_call_args] runs AFTER this function and
       would have caught the mismatch with a clean diagnostic, but
       the combine here is reached first and raises
       Invalid_argument straight up to the user as an ICE.  Reject
       under-supplied generic calls (and over-supplied non-variadic
       ones) up front so tparam inference only sees aligned pairs. *)
    let arity_ok =
      if skel.fn_variadic then got >= n_fixed else got = n_fixed in
    if not arity_ok then begin
      let display =
        let m = skel.mangled in
        if String.length m >= 3 && String.sub m 0 3 = "ex_"
        then String.sub m 3 (String.length m - 3)
        else m
      in
      Error.failf pos
        (if skel.fn_variadic
         then "'%s' expects at least %d argument(s), got %d"
         else "'%s' expects %d argument(s), got %d")
        display n_fixed got
    end;
    let take n xs =
      let rec loop n acc = function
        | _ when n <= 0 -> List.rev acc
        | [] -> List.rev acc
        | x :: rest -> loop (n - 1) (x :: acc) rest
      in
      loop n [] xs
    in
    (* A generic-struct parameter (`p: Pair<T, int>`) shows up as a
       TStructApp whose actual is a flat instance that dropped its type
       args; recover them so `T` is inferable (see [expand_inst_pair]). *)
    let inf_pairs =
      List.concat_map (expand_inst_pair ctx)
        (List.combine skel.param_tys (take n_fixed arg_tys))
    in
    let seed_pairs =
      match expected, skel.ret_ty with
      | Some exp, Some r ->
          (* Same `(TStructApp [Vec] [TVar T], TStruct ["Vec_i32"])`
             shape unification the arg-side path runs.  Without this
             the inference unifier can't peel a flat mono instance
             back into its tparam slots for the return-driven pair. *)
          expand_inst_pair ctx (r, exp)
      | _ -> []
    in
    (* Generic-impl method dispatch: a `*self` receiver pairs a `*Pair<A,B>`
       declaration against the *value* receiver type (auto-ref), which
       expand_inst_pair's strict pointer match doesn't reach — so seed the
       impl tparams (the leading fn_tparams) straight from the receiver
       instance's type args. *)
    let seed =
      List.combine (take (List.length recv_inst_args) skel.fn_tparams)
        recv_inst_args
    in
    let inferred =
      Mono.infer_tparams ~pos ~seed skel.fn_tparams (seed_pairs @ inf_pairs)
    in
    let bindings = List.combine skel.fn_tparams inferred in
    let func =
      match List.assoc_opt skel.mangled ctx.fn_asts with
      | Some (_, f) -> f
      | None ->
          Error.failf pos
            "internal: missing AST for generic fn '%s'" skel.mangled
    in
    (* Enforce `<T: Trait>` bounds: the type inferred for each bounded
       tparam must `impl Trait`.  Gives a clear error at the call site
       instead of a downstream "no method m" once the instance body is
       elaborated. *)
    List.iter (fun (tparam, trait_written, ast_assocs) ->
      match List.assoc_opt tparam bindings with
      | None -> ()
      | Some ty ->
          let trait_name =
            match List.rev trait_written with n :: _ -> n | [] -> ""
          in
          if not (type_impls_trait ~trait_name ty) then
            Error.failf pos
              "type '%s' does not implement trait '%s' (required by bound \
               '%s: %s' on '%s')"
              (typ_name ty) trait_name tparam trait_name func.Ast.name;
          (* DR-021 complement — bound-driven impl-side assoc equality.
             When the bound carries assoc bindings (`<F: |int|->int>`
             pins F's Arg=int, Output=int), check that the concrete
             type's impl has matching assoc types.  Without this check
             a mismatched impl would compile here and fail later with
             a less-specific type error downstream.  Skipped silently
             when the bound has no assocs (plain `<F: Trait>`) or when
             the impl's assoc table entry isn't visible (the trait
             check above already required the impl to exist; an
             absent assoc record means we can't be more specific so
             we trust the impl). *)
          if ast_assocs <> [] then begin
            match typ_head_path ty with
            | None -> ()
            | Some target_path ->
                let impl_assocs =
                  List.find_map (fun ((tn, target), assocs) ->
                    if tn = trait_name
                       && (target = target_path
                           || Mono.is_instance_of target target_path)
                    then Some assocs else None)
                    !trait_assoc_table
                in
                (match impl_assocs with
                 | None -> ()
                 | Some impl_assocs ->
                     List.iter (fun (an, expected_ann) ->
                       let expected_typ =
                         (* The bound's assoc-constraint may reference the
                            fn/impl tparams (`Arg = I::Item`).  Resolve with
                            those tparams in scope (→ deferred TAssocProj),
                            then substitute the inferred bindings (I :=
                            concrete) and normalise — without this the
                            call-site ctx has no `I` and `I::Item` trips
                            "unknown type". *)
                         let ctx_tp =
                           { ctx with tparams = skel.fn_tparams @ ctx.tparams } in
                         normalize_apps ctx
                           (subst_typ bindings
                              (resolve_type_ann ~pos ctx_tp expected_ann))
                       in
                       match List.assoc_opt an impl_assocs with
                       | None -> ()
                       | Some impl_typ ->
                           if not (typ_eq impl_typ expected_typ) then
                             Error.failf pos
                               "bound '%s: %s' on '%s' requires '%s::%s = \
                                %s' but type '%s' has '%s::%s = %s'"
                               tparam trait_name func.Ast.name
                               tparam an (typ_name expected_typ)
                               (typ_name ty) trait_name an
                               (typ_name impl_typ))
                       ast_assocs)
          end)
      func.Ast.tbounds;
    let inst =
      Mono.instantiate_fn ctx.instances
        ~path:resolved_mod ~func ~skel ~bindings ~origin_pos:pos
    in
    (* Mono.subst_typ substitutes the bindings but can't normalise a
       now-concrete generic application (`Pair<A,B>` -> Pair<i32,bool>)
       to its flat instance — that needs the index + Mono state.  Do it
       here so the dispatch result is flat for the caller. *)
    (inst.mangled,
     List.map (normalize_apps ctx) inst.param_tys,
     Option.map (normalize_apps ctx) inst.ret_ty)
  end


(* ===== Pattern matching: lowering + exhaustiveness/redundancy ===== *)

(* Lower an Ast.pattern against the type of the value it matches, into a
   tpattern plus the list of (bind-name, type) it introduces.  Recurses
   through nested variant patterns (`Outer::Wrap(Inner::A(n))`): each
   field sub-pattern is lowered against that field's type.  Validates
   that a variant pattern's type really is the matching enum, that the
   variant exists, and that tuple/struct bind syntax matches the variant
   kind. *)
let rec lower_pattern ?(allow_or = true) ?(top_level = true) ctx
    (value_ty : typ)
    (pat : Ast.pattern) : tpattern * (string * typ) list =
  match pat with
  | Ast.PWildcard _ -> (TPWildcard, [])
  | Ast.PVar (n, _) -> (TPVar n, [ (n, value_ty) ])
  | Ast.PLit (n, ppos) ->
      (* GATE-5a literal pattern: scrutinee must be int-like and the
         literal must fit its width (a `case 300:` against a u8
         scrutinee can never match — reject at compile time).
         Top-level only in v1: a literal nested in a variant payload
         would need value tests inside the Maranget matrix (its
         non-enum columns are bind-only), so exhaustiveness would
         silently over-claim. *)
      if not top_level then
        Error.failf ppos
          "literal patterns are only supported at the top level of a \
           match arm (v1) — bind the payload and compare in a guard";
      (match value_ty with
       | TInt _ | TCInt _ ->
           if not (int_fits n value_ty) then
             Error.failf ppos
               "literal pattern %d does not fit the matched type %s"
               n (typ_name value_ty);
           (TPLit n, [])
       | other ->
           Error.failf ppos
             "literal pattern needs an integer scrutinee, got %s"
             (typ_name other))
  | Ast.PBool (b, ppos) ->
      (* GATE-5b boolean literal pattern: scrutinee must be bool.  Like
         PLit it is a top-level scalar test (no payload binds); unlike PLit
         the value domain is finite, so `true | false` is exhaustive with
         no catch-all (handled in the completeness check). *)
      if not top_level then
        Error.failf ppos
          "literal patterns are only supported at the top level of a \
           match arm (v1) — bind the payload and compare in a guard";
      (match value_ty with
       | TBool -> (TPBool b, [])
       | other ->
           Error.failf ppos
             "boolean pattern needs a bool scrutinee, got %s"
             (typ_name other))
  | Ast.PVariant { tname; variant; binds; pos = ppos } ->
      let path =
        match value_ty with
        | TEnum p -> p
        | other ->
            Error.failf ppos
              "variant pattern '%s' but the matched value has type %s"
              variant (typ_name other)
      in
      let e_sig =
        match resolve_enum_by_path ctx path with
        | Some e -> e
        | None -> Error.failf ppos "internal: enum '%s' missing from index"
                    (String.concat "::" path)
      in
      let resolved =
        match lookup_enum ctx tname with
        | Some e' -> e'.ename_path
        | None ->
            Error.failf ppos "unknown enum '%s' in pattern"
              (String.concat "::" tname)
      in
      if not (Mono.is_instance_of resolved path) then
        Error.failf ppos
          "pattern matches '%s' but the value has type '%s'"
          (String.concat "::" resolved) (String.concat "::" path);
      let (tag, vsig) =
        match find_variant e_sig variant with
        | Some r -> r
        | None ->
            Error.failf ppos "enum '%s' has no variant '%s'"
              (String.concat "::" path) variant
      in
      (* (field-name, field-type, sub-pattern) in declaration order. *)
      let ordered =
        match binds, vsig.vsis_struct with
        | Ast.PBTuple ps, false ->
            let expected_n = List.length vsig.vsfields in
            let got = List.length ps in
            if expected_n <> got then
              Error.failf ppos
                "variant '%s' has %d field(s), pattern binds %d"
                variant expected_n got;
            List.map2 (fun (fn, ft) p -> (fn, ft, p)) vsig.vsfields ps
        | Ast.PBStruct entries, true ->
            let expected_names = List.map fst vsig.vsfields in
            (match find_dup ~key:Fun.id (List.map fst entries) with
             | Some n -> Error.failf ppos "duplicate field '%s' in pattern" n
             | None -> ());
            List.iter (fun (n, _) ->
              if not (List.mem n expected_names) then
                Error.failf ppos "variant '%s' has no field '%s'" variant n)
              entries;
            List.map (fun (fn, ft) ->
              match List.assoc_opt fn entries with
              | Some p -> (fn, ft, p)
              | None -> (fn, ft, Ast.PWildcard ppos))
              vsig.vsfields
        | Ast.PBTuple _, true ->
            Error.failf ppos
              "variant '%s' is a struct variant; match it with \
               '{ field: pat }', not '(...)'" variant
        | Ast.PBStruct _, false ->
            Error.failf ppos
              "variant '%s' is a tuple variant; match it with \
               '(...)', not '{ field: pat }'" variant
      in
      let lowered =
        List.map (fun (fn, ft, sp) ->
          let (tp, bs) =
            lower_pattern ~allow_or:false ~top_level:false ctx ft sp in
          ((fn, tp), bs))
          ordered
      in
      (TPVariant { variant; tag; binds = List.map fst lowered },
       List.concat_map snd lowered)
  | Ast.POr (_, opos) when not allow_or ->
      Error.failf opos
        "or-pattern only allowed at the top of a match arm \
         (nested `pat1 | pat2` inside a variant bind is not supported yet)"
  | Ast.POr (alts, opos) ->
      if List.length alts < 2 then
        Error.failf opos
          "internal: or-pattern parsed with %d alternative(s); \
           expected at least 2" (List.length alts);
      let lowered =
        List.map (lower_pattern ~allow_or:false ctx value_ty) alts
      in
      (* MVP: every alternative must bind zero variables.  Rust requires
         all alternatives to bind the *same set* of names; we punt that to
         a follow-up — the common case (`Foo | Bar | Baz`, all unit
         variants) wants zero binds anyway. *)
      List.iter (fun (_, bs) ->
        match bs with
        | [] -> ()
        | (n, _) :: _ ->
            Error.failf opos
              "or-pattern alternatives must bind zero variables \
               (got bind '%s'); use separate arms if you need to bind" n)
        lowered;
      (* MVP: each alternative is a wildcard or a unit variant (no
         payload).  Variants with payload — even all-wildcard payload
         like `Foo(_)` — would force the decision-chain codegen path
         and complicate duplicate-tag detection here; we defer them. *)
      let talts = List.map fst lowered in
      List.iter (function
        | TPWildcard | TPVar _ | TPLit _ | TPBool _ -> ()
        | TPVariant { binds = []; _ } -> ()
        | TPVariant { variant; _ } ->
            Error.failf opos
              "or-pattern alternative '%s(...)' has a payload; only \
               unit variants and `_` are allowed in `|` alternatives \
               (use separate arms if you need to bind the payload)" variant
        | TPOr _ -> assert false       (* allow_or:false in recursion *))
        talts;
      (* Within-or duplicate detection: `Foo | Foo` would emit duplicate
         C `case` labels.  Maranget's redundancy check looks at arm-level
         only, so we filter for this here. *)
      let tags =
        List.filter_map (function
          | TPVariant { tag; variant; _ } -> Some (tag, variant)
          | _ -> None) talts
      in
      (match find_dup ~key:fst tags with
       | Some tag ->
           let variant = List.assoc tag tags in
           Error.failf opos
             "or-pattern lists '%s' more than once" variant
       | None -> ());
      (TPOr talts, [])

(* Reduced pattern for the usefulness/exhaustiveness matrix (Maranget,
   "Warnings for pattern matching").  Variable and wildcard binders both
   match any value, so they collapse to [CWild]; a variant pattern keeps
   its tag and field sub-patterns (declaration order).  Only enum-typed
   columns carry constructors — fields of any other type can only be
   bound (CWild), since exile has no literal patterns. *)
type cpat = CWild | CCon of { tag : int; args : cpat list }

let rec cpat_of_tpat : tpattern -> cpat = function
  | TPWildcard | TPVar _ -> CWild
  | TPLit _ | TPBool _ ->
      (* Unreachable: scalar / bool matches bypass the Maranget matrix
         entirely, and nested literal patterns are rejected in
         lower_pattern (top_level gate). *)
      assert false
  | TPVariant { tag; binds; _ } ->
      CCon { tag; args = List.map (fun (_, p) -> cpat_of_tpat p) binds }
  | TPOr _ ->
      (* Or-patterns expand to multiple matrix rows at the arm level
         (see [cpat_rows_of_tpat]).  Bind-positions cannot contain TPOr
         (top-level restriction in [lower_pattern]), so this is unreachable
         from inside a variant. *)
      assert false

(* Expand an arm's pattern to the matrix rows it covers — a single row
   for non-or patterns, one row per alternative for a top-level
   `pat1 | pat2 | ...`.  Or-patterns only nest one level (enforced in
   [lower_pattern]), so the result list size = number of alternatives. *)
let cpat_rows_of_tpat : tpattern -> cpat list = function
  | TPOr alts -> List.map cpat_of_tpat alts
  | other -> [ cpat_of_tpat other ]

(* Constructors of an enum type: (tag, field-types) per variant in
   declaration order.  None for non-enum types — they have no
   constructors and so always behave as a wildcard column. *)
let enum_ctors ctx = function
  | TEnum path ->
      (match resolve_enum_by_path ctx path with
       | Some e ->
           Some (List.mapi (fun tag (vs : variant_sig) ->
               (tag, List.map snd vs.vsfields)) e.evariants)
       | None -> None)
  | _ -> None

let ctor_field_tys ctx ty tag =
  match enum_ctors ctx ty with
  | Some ctors -> (match List.assoc_opt tag ctors with Some fts -> fts | None -> [])
  | None -> []

(* Matrix row operations from the algorithm.  [specialize] keeps rows
   whose first column matches constructor [tag] (expanding its [arity]
   fields into the leading columns; a wildcard row expands to [arity]
   wildcards); [default_matrix] keeps only wildcard-led rows, dropping
   the first column. *)
let specialize ~tag ~arity matrix =
  List.filter_map (function
    | CCon { tag = t; args } :: rest when t = tag -> Some (args @ rest)
    | CCon _ :: _ -> None
    | CWild :: rest -> Some (List.init arity (fun _ -> CWild) @ rest)
    | [] -> assert false)
    matrix

let default_matrix matrix =
  List.filter_map (function
    | CWild :: rest -> Some rest
    | CCon _ :: _ -> None
    | [] -> assert false)
    matrix

let col0_tags matrix =
  List.filter_map (function CCon { tag; _ } :: _ -> Some tag | _ -> None) matrix

let rec split_at n xs =
  if n <= 0 then ([], xs)
  else match xs with
    | [] -> ([], [])
    | x :: rest -> let (a, b) = split_at (n - 1) rest in (x :: a, b)

(* U(P, q): is row [q] useful w.r.t. matrix [matrix] (does it match a
   value no row of [matrix] does)?  [types] gives each column's type.
   Drives the redundant-arm check: arm i is redundant iff its row is
   NOT useful w.r.t. the rows before it. *)
let rec useful ctx types matrix q =
  match types, q with
  | [], [] -> matrix = []
  | ty :: rest_t, CCon { tag; args } :: rest_q ->
      let arity = List.length args in
      useful ctx (ctor_field_tys ctx ty tag @ rest_t)
        (specialize ~tag ~arity matrix) (args @ rest_q)
  | ty :: rest_t, CWild :: rest_q ->
      (match enum_ctors ctx ty with
       | Some ctors when ctors <> [] ->
           let present = col0_tags matrix in
           if List.for_all (fun (tag, _) -> List.mem tag present) ctors then
             List.exists (fun (tag, fts) ->
               let arity = List.length fts in
               useful ctx (fts @ rest_t) (specialize ~tag ~arity matrix)
                 (List.init arity (fun _ -> CWild) @ rest_q))
               ctors
           else useful ctx rest_t (default_matrix matrix) rest_q
       | _ -> useful ctx rest_t (default_matrix matrix) rest_q)
  | _ -> assert false

(* Witness of non-exhaustiveness: a value-pattern row that no row of
   [matrix] matches, or None if [matrix] is exhaustive over [types]. *)
let rec missing ctx types matrix =
  match types with
  | [] -> if matrix = [] then Some [] else None
  | ty :: rest_t ->
      (match enum_ctors ctx ty with
       | Some ctors when ctors <> [] ->
           let present = col0_tags matrix in
           let absent =
             List.filter (fun (tag, _) -> not (List.mem tag present)) ctors
           in
           if absent = [] then
             let rec try_ctors = function
               | [] -> None
               | (tag, fts) :: more ->
                   let arity = List.length fts in
                   (match missing ctx (fts @ rest_t)
                            (specialize ~tag ~arity matrix) with
                    | Some w ->
                        let (args, rest_w) = split_at arity w in
                        Some (CCon { tag; args } :: rest_w)
                    | None -> try_ctors more)
             in
             try_ctors ctors
           else
             (match missing ctx rest_t (default_matrix matrix) with
              | None -> None
              | Some w ->
                  let (tag, fts) = List.hd absent in
                  Some (CCon { tag;
                               args = List.init (List.length fts)
                                        (fun _ -> CWild) } :: w))
       | _ ->
           (match missing ctx rest_t (default_matrix matrix) with
            | None -> None
            | Some w -> Some (CWild :: w)))

(* Render a witness pattern for the non-exhaustive error, e.g.
   `Some(Triangle(_))` or `Shape::Rectangle { w: _, h: _ }`. *)
let rec render_cpat ctx ty cp =
  match cp, ty with
  | CCon { tag; args }, TEnum path ->
      (match resolve_enum_by_path ctx path with
       | Some e ->
           let vs = List.nth e.evariants tag in
           if args = [] then vs.vsname
           else if vs.vsis_struct then
             vs.vsname ^ " { "
             ^ String.concat ", "
                 (List.map2 (fun (fn, ft) a ->
                      fn ^ ": " ^ render_cpat ctx ft a) vs.vsfields args)
             ^ " }"
           else
             vs.vsname ^ "("
             ^ String.concat ", "
                 (List.map2 (fun (_, ft) a -> render_cpat ctx ft a)
                    vs.vsfields args)
             ^ ")"
       | None -> "_")
  | _ -> "_"

(* Elaborate Ast.expr → texpr.  Each typed node carries its type in
   `.ty`, so codegen never has to re-run typing.  Single source of
   truth for both type computation and tree elaboration — used to
   coexist with a separate `type_of` function which produced
   O(N²) elab cost on deeply-nested expressions; that function and
   its helpers (binop_operand_types, validate_struct_lit, desugar_orelse)
   are gone, with their validation logic inlined per-case here. *)
(* A block's trailing value, if it is a single expression (`{ e }`).
   Block expressions with leading statements in branch position are
   deferred (see WORKLOG bare-block-expr), so a value-producing branch
   must be exactly one trailing expression. *)
let branch_value_expr : Ast.stmt list -> Ast.expr option = function
  | [ Ast.Tail e ] -> Some e
  | _ -> None

(* Does this `if` qualify as an *expression* (yields a value)?  Requires
   an `else` and both branches reducible to a single trailing expression.
   Otherwise it is a control-flow *statement* (guard clause, side
   effects, both-branches-return). *)
let is_value_if ~(then_blk : Ast.stmt list) ~(else_blk : Ast.stmt list option) =
  match else_blk with
  | None -> false
  | Some eblk ->
      branch_value_expr then_blk <> None && branch_value_expr eblk <> None

(* Per-body hook so [elab_expr] can elaborate `Ast.Block` (multi-stmt
   match arm bodies) by reusing the function-scoped `walk_stmt`.  The
   hook is installed by `elab_body` at entry and cleared on exit; outside
   a function body, `Ast.Block` is a parse-time/lift error. *)
let walk_stmts_hook
  : ((string * typ) list -> Ast.stmt list ->
       (string * typ) list * tstmt list) option ref
  = ref None

(* DR — receiver-mutability per design 2026-05-28.  Hook installed by
   [elab_body] so [elab_expr]'s MethodCall path can ask whether a
   receiver expression is a mutable lvalue (`let mut` binding, field/
   index reached through a mut place, or auto-deref through `*T`).
   Used to gate `*self` methods: a mutable receiver is required, and
   `*const T` receivers / immutable bindings are rejected up-front. *)
let is_mut_lvalue_hook : (texpr -> bool) option ref = ref None

let rec elab_expr ?(allow_void = false) ?expected ctx env e : texpr =
  let pos = Ast.expr_pos e in
  match e with
  | Ast.IntLit (n, _) ->
      (* Adopt an expected TInt width (bidirectional typing) so a literal
         in `let x: u32 = 1` / `... = 1 + 2` builds at the annotated width
         instead of defaulting to i32.  A non-fitting value falls back to
         i32 so the downstream fit-check reports the overflow cleanly. *)
      let ty =
        match expected with
        | Some (TInt _ as t) when int_fits n t -> t
        | _ -> t_i32
      in
      { e = TIntLit n; ty; pos }
  | Ast.FloatLit (f, w, _) -> { e = TFloatLit (f, w); ty = TFloat w; pos }
  | Ast.BoolLit (b, _) -> { e = TBoolLit b; ty = TBool; pos }
  | Ast.NullLit _ -> { e = TNullLit; ty = TNullPtr; pos }
  | Ast.Lambda { pos = lam_pos; _ } ->
      Error.failf lam_pos
        "internal: lambda expression reached elab_expr unresolved \
         (expand_lambdas pre-pass missing?)"
  | Ast.StringLit (s, _) -> { e = TStringLit s; ty = TString; pos }
  | Ast.Neg (sub, neg_pos) ->
      let sub' = elab_expr ?expected ctx env sub in
      let is_float = function TFloat _ -> true | _ -> false in
      if not (is_int_like sub'.ty || is_float sub'.ty) then
        Error.failf neg_pos
          "negation '-' requires an integer or float, got %s"
          (typ_name sub'.ty);
      { e = TNeg sub'; ty = sub'.ty; pos }
  | Ast.BitNot (sub, not_pos) ->
      let sub' = elab_expr ?expected ctx env sub in
      if not (is_int_like sub'.ty) then
        Error.failf not_pos
          "bitwise complement '~' requires an integer, got %s"
          (typ_name sub'.ty);
      { e = TBitNot sub'; ty = sub'.ty; pos }
  | Ast.Not (sub, not_pos) ->
      let sub' = elab_expr ctx env sub in
      if not (typ_eq sub'.ty TBool) then
        Error.failf not_pos
          "logical negation '!' requires a bool, got %s" (typ_name sub'.ty);
      { e = TNot sub'; ty = TBool; pos }
  | Ast.BinOp (Ast.Concat, l, r, _) ->
      (* Compile-time string concat: both sides must reduce to a
         compile-time-constant string at elab time.  That's a string
         literal, or a `type_name(expr)` call (its result is a `.rodata`
         constant — rendered here so `type_name(x) ++ "\n"` folds).
         Recursion folds bottom-up so `"a" ++ "b" ++ "c"` collapses to a
         single `TStringLit "abc"`.  For runtime concat use an Allocator
         method (`@must_use` Result return), kept separate so the alloc
         cost stays visible at the call site. *)
      let l' = elab_expr ctx env l in
      let r' = elab_expr ctx env r in
      let extract (e : texpr) =
        match e.e with
        | TStringLit s -> Some s
        | TBuiltinCall { name = "type_name"; args = [ a ] } ->
            Some (render_typ_user_facing ~structs:ctx.structs
                    ~enums:ctx.enums a.ty)
        | _ -> None
      in
      (match extract l', extract r' with
       | Some sl, Some sr ->
           { e = TStringLit (sl ^ sr); ty = TString; pos }
       | None, _ ->
           Error.failf l'.pos
             "'++' requires a compile-time string literal on both sides; \
              got %s on the left (for runtime concat use an Allocator method)"
             (typ_name l'.ty)
       | _, None ->
           Error.failf r'.pos
             "'++' requires a compile-time string literal on both sides; \
              got %s on the right (for runtime concat use an Allocator method)"
             (typ_name r'.ty))
  | Ast.BinOp (op, l, r, _) ->
      (* Smart literal coercion: when one operand is an integer
         literal, elab the other first so the literal can adopt a
         matching TInt width.  Without this, `let x: i16 = 5 + y`
         (y: i16) would build the literal as t_i32 and fail the
         later equality check. *)
      let elab_lit n other_ty other_pos =
        let lit_ty =
          match other_ty with
          | TInt _ when int_fits n other_ty -> other_ty
          | _ -> t_i32
        in
        { e = TIntLit n; ty = lit_ty; pos = other_pos }
      in
      let (l', r') =
        match op, expected with
        (* Value-preserving int ops under an int annotation: push the
           expected width into the operands so `1 + 2` (both literals)
           adopts it.  Arithmetic/bitwise → both sides; shift → left only
           (result takes the left type, the amount stays free). *)
        | (Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod
          | Ast.BitAnd | Ast.BitOr | Ast.BitXor), Some (TInt _ as t) ->
            (elab_expr ~expected:t ctx env l, elab_expr ~expected:t ctx env r)
        | (Ast.Shl | Ast.Shr), Some (TInt _ as t) ->
            (elab_expr ~expected:t ctx env l, elab_expr ctx env r)
        | _ ->
            (* No int annotation (or a comparison): smart literal coercion
               — a literal adopts its sibling operand's width. *)
            (match l, r with
             | Ast.IntLit (n, lp), _ ->
                 let r' = elab_expr ctx env r in
                 (elab_lit n r'.ty lp, r')
             | _, Ast.IntLit (n, rp) ->
                 let l' = elab_expr ctx env l in
                 (l', elab_lit n l'.ty rp)
             | _ -> (elab_expr ctx env l, elab_expr ctx env r))
      in
      let name = Ast.binop_name op in
      let need_int_operands () =
        if not (is_int_like l'.ty && is_int_like r'.ty) then
          Error.failf pos
            "operator '%s' requires integer operands, got %s and %s"
            name (typ_name l'.ty) (typ_name r'.ty)
      in
      (* For TInt × TInt with same signedness but mixed width, the
         result takes the wider operand's type (C-style promotion).
         Any other type combination is rejected — `as` casts are the
         only way to cross sign or kind boundaries (c_int ↔ TInt,
         signed ↔ unsigned). *)
      let promote_int_widen () =
        match l'.ty, r'.ty with
        | TInt a, TInt b when a.signed = b.signed ->
            if int_width_bits a.width >= int_width_bits b.width
            then TInt a else TInt b
        | _ ->
            Error.failf pos
              "operator '%s' between incompatible types %s and %s"
              name (typ_name l'.ty) (typ_name r'.ty)
      in
      (* `==` / `!=` on `str` — the C compiler would happily compare
         pointer values (giving `1` only when both sides land in the
         same `.rodata` slot after literal-dedup, `0` otherwise).  The
         user-visible contract is value-equality, so lower to a call
         to the prelude `str::eq` content compare.  NotEq wraps the
         call in `!`.  Per DR-001 / str-ops design 2026-05-31. *)
      if (op = Ast.EqEq || op = Ast.NotEq)
         && typ_eq l'.ty TString && typ_eq r'.ty TString then
        let call =
          { e = TCall { mangled = "str__eq"; args = [l'; r'] };
            ty = TBool; pos }
        in
        if op = Ast.EqEq then call
        else { e = TNot call; ty = TBool; pos }
      (* `==` / `!=` on a struct/enum — DR-002 W2.  Pre-fix the
         operator fell through to raw `a == b` in C which cc rejects
         for aggregates; `.eq()` dispatch worked but the operator
         didn't reach it.  Lower the BinOp to `T__eq(&a, &b)` when
         the trait_impl_table has `impl Eq for T` registered
         (`@derive(Eq)` or hand-written); reject upfront with a
         helpful diagnostic when it isn't.  Method takes `*const
         self, *const other`, so auto-ref both sides.  NotEq wraps
         in `!`. *)
      else if (op = Ast.EqEq || op = Ast.NotEq)
              && typ_eq l'.ty r'.ty
              && (match l'.ty with TStruct _ | TEnum _ -> true | _ -> false)
      then
        let target = match l'.ty with
          | TStruct p | TEnum p -> p
          | _ -> assert false
        in
        if not (List.mem ("Eq", target) !trait_impl_table) then
          Error.failf pos
            "type '%s' does not implement Eq, so `%s` cannot compare \
             two values of it (add `@derive(Eq)` to the decl or write \
             `impl Eq for %s` to define content equality)"
            (typ_name l'.ty) name (typ_name l'.ty)
        else
          let eq_mangled = mangle target "eq" in
          let lref =
            { e = TRef l'; ty = TConstPtr l'.ty; pos = l'.pos } in
          let rref =
            { e = TRef r'; ty = TConstPtr r'.ty; pos = r'.pos } in
          let call =
            { e = TCall { mangled = eq_mangled; args = [lref; rref] };
              ty = TBool; pos }
          in
          if op = Ast.EqEq then call
          else { e = TNot call; ty = TBool; pos }
      else
      let result_t =
        match op with
        | (Ast.Div | Ast.Mod) when expr_int_lit r = Some 0 ->
            (* Constant division / modulo by zero is undefined in C (and
               would leak `-Wdiv-by-zero`); reject it at compile time. *)
            Error.failf pos "%s by zero"
              (match op with Ast.Mod -> "modulo" | _ -> "division")
        | (Ast.Add | Ast.Sub | Ast.Mul | Ast.Div) when
            (match l'.ty, r'.ty with
             | TFloat a, TFloat b when a = b -> true
             | _ -> false) ->
            (* DR-floats: IEEE arithmetic on matching float widths.  No
               int↔float implicit mix — explicit `as` cast required (the
               user opts in to the precision change).  Mod (`%`) is
               deferred to libm `fmod` — rejected here for now. *)
            l'.ty
        | Ast.Mod when
            (match l'.ty, r'.ty with
             | TFloat _, _ | _, TFloat _ -> true
             | _ -> false) ->
            Error.failf pos
              "operator '%%' is not built-in for float (use the libm \
               `fmod`/`fmodf` extern fn when binding it lands)"
        | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
            need_int_operands ();
            if typ_eq l'.ty r'.ty then l'.ty
            else promote_int_widen ()
        | Ast.BitAnd | Ast.BitOr | Ast.BitXor ->
            (* Bitwise: integer operands of matching signedness; result
               takes the wider operand's type (C-style widening). *)
            need_int_operands ();
            if typ_eq l'.ty r'.ty then l'.ty
            else promote_int_widen ()
        | Ast.Shl | Ast.Shr ->
            (* Shift: both operands integer; the result takes the *left*
               operand's type (the amount's type is irrelevant).  A
               constant amount is range-checked against the left width. *)
            need_int_operands ();
            (match expr_int_lit r, l'.ty with
             | Some k, _ when k < 0 ->
                 Error.failf pos "shift amount %d is negative" k
             | Some k, TInt { width; _ } when k >= int_width_bits width ->
                 Error.failf pos
                   "shift amount %d is out of range for %s (%d bits)"
                   k (typ_name l'.ty) (int_width_bits width)
             | _ -> ());
            l'.ty
        | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq ->
            (* DR-floats: IEEE comparison on matching float widths
               (NaN comparisons return false in C — that's the
               built-in semantics, and exile carries it forward
               by emitting the raw operator).  No float trait Ord
               — total order would lie about NaN. *)
            (match l'.ty, r'.ty with
             | TFloat a, TFloat b when a = b -> TBool
             | TFloat _, _ | _, TFloat _ ->
                 Error.failf pos
                   "comparison '%s' between %s and %s — mixed-width \
                    float comparison requires an explicit `as` cast"
                   name (typ_name l'.ty) (typ_name r'.ty)
             | _ ->
                 need_int_operands ();
                 (if not (typ_eq l'.ty r'.ty) then
                    ignore (promote_int_widen ()));
                 TBool)
        | Ast.EqEq | Ast.NotEq ->
            if not (typ_eq l'.ty r'.ty) then
              Error.failf pos
                "equality '%s' between incompatible types %s and %s"
                name (typ_name l'.ty) (typ_name r'.ty);
            TBool
        | Ast.And | Ast.Or ->
            if not (typ_eq l'.ty TBool) || not (typ_eq r'.ty TBool) then
              Error.failf pos
                "logical '%s' requires bool operands, got %s and %s"
                name (typ_name l'.ty) (typ_name r'.ty);
            TBool
        | Ast.Concat ->
            failwith "internal: Concat reached scalar BinOp dispatch; \
                      the outer Concat arm in elab_expr should have \
                      caught it"
      in
      { e = TBinOp (op, l', r'); ty = result_t; pos }
  | Ast.Cast (sub, ann, cast_pos) ->
      let sub' = elab_expr ctx env sub in
      let tgt = resolve_type_ann ~pos:cast_pos ctx ann in
      let is_float = function TFloat _ -> true | _ -> false in
      let kind_ok =
        (is_int_like sub'.ty && is_int_like tgt)
        || (is_ptr sub'.ty && is_ptr tgt)
        || (is_int_like sub'.ty && is_ptr tgt)
        (* DR-floats: int↔float and float↔float casts (truncation /
           widening as in C).  No ptr↔float cast — kinds stay
           separate. *)
        || (is_int_like sub'.ty && is_float tgt)
        || (is_float sub'.ty && is_int_like tgt)
        || (is_float sub'.ty && is_float tgt)
      in
      if kind_ok then { e = TCast (sub', ann); ty = tgt; pos }
      else if typ_eq sub'.ty tgt then
        (* Identity cast on a non-scalar (struct / enum / tuple): a
           semantic no-op.  C89 cannot cast to an aggregate type, so
           the node is elided entirely instead of emitted.  Scalar and
           pointer identities stay on the kind_ok path above — the
           prelude's vec_grow W4 cast relies on the emitted pointer
           cast to strip the slice-read const qualifier in C, and an
           instantiated generic body (`src[i] as T` with T = struct)
           must not error just because the cast became an identity. *)
        sub'
      else
        Error.failf cast_pos
          "cannot cast %s to %s (supported: int↔int, int↔float, \
           float↔float, ptr↔ptr, int→ptr)"
          (typ_name sub'.ty) (typ_name tgt)
  | Ast.Ref (sub, _) ->
      let sub' = elab_expr ctx env sub in
      { e = TRef sub'; ty = TPtr sub'.ty; pos }
  | Ast.Deref (sub, deref_pos) ->
      let sub' = elab_expr ctx env sub in
      let ty =
        match sub'.ty with
        | TPtr t | TOwnPtr t | TConstPtr t -> t
        | TNullPtr -> Error.failf deref_pos "cannot deref 'null'"
        | other ->
            Error.failf deref_pos "deref '*' requires a pointer, got %s"
              (typ_name other)
      in
      { e = TDeref sub'; ty; pos }
  | Ast.TupleLit (es, _) ->
      let es' = List.map (elab_expr ctx env) es in
      let ty = TTuple (List.map (fun (e : texpr) -> e.ty) es') in
      { e = TTupleLit es'; ty; pos }
  | Ast.ArrayLit (es, lit_pos) ->
      if es = [] then
        Error.failf lit_pos
          "empty array literal `[]` has no element type or size";
      (* Bidirectional: a `[T; N]` annotation propagates the element type to
         each element (so literal widths agree) and pins the expected size. *)
      let elem_expected =
        match expected with Some (TArray { elem; _ }) -> Some elem | _ -> None
      in
      let tes = List.map (fun e -> elab_expr ?expected:elem_expected ctx env e) es in
      let elem_ty = (List.hd tes).ty in
      List.iter2 (fun (te : texpr) e ->
        if not (typ_eq te.ty elem_ty) && not (int_lit_fits e elem_ty) then
          Error.failf te.pos
            "array elements must share one type: %s vs %s"
            (typ_name elem_ty) (typ_name te.ty))
        (List.tl tes) (List.tl es);
      let n = List.length tes in
      (match expected with
       | Some (TArray { size; _ }) when size <> n ->
           Error.failf lit_pos
             "array literal has %d element(s) but type expects %d" n size
       | _ -> ());
      { e = TArrayLit tes; ty = TArray { elem = elem_ty; size = n }; pos }
  | Ast.ArrayRepeat { value; count; pos = rep_pos } ->
      let elem_expected =
        match expected with Some (TArray { elem; _ }) -> Some elem | _ -> None
      in
      let tvalue = elab_expr ?expected:elem_expected ctx env value in
      let n = eval_const_size ctx count in
      if n <= 0 then
        Error.failf rep_pos "array repeat count must be positive, got %d" n;
      (match expected with
       | Some (TArray { size; _ }) when size <> n ->
           Error.failf rep_pos
             "array repeat produces %d element(s) but type expects %d" n size
       | _ -> ());
      { e = TArrayRepeat { value = tvalue; count = n };
        ty = TArray { elem = tvalue.ty; size = n }; pos }
  | Ast.Index { base; index; pos = idx_pos } ->
      let tbase = elab_expr ctx env base in
      (* DR-012 — auto-deref one level of `*T` for `[]`.  Mirrors the
         field-access auto-deref so `with s in v.as_slice() {s[0]}`
         reads through the borrow without forcing `( *s )[0]`.  Only
         peel a single layer; deeper indirection still surfaces the
         original error. *)
      let tbase =
        match tbase.ty with
        | TPtr inner | TOwnPtr inner | TConstPtr inner ->
            { e = TDeref tbase; ty = inner; pos = tbase.pos }
        | _ -> tbase
      in
      let elem_ty =
        match tbase.ty with
        | TArray { elem; _ } -> elem
        | TStruct path when Mono.is_instance_of ["Slice"] path ->
            (* `Slice<T>` instance — element type is the resolved T from
               the monomorphized struct fields.  `.ptr` is `*const T`,
               so read it back through there. *)
            (match resolve_struct_by_path ctx path with
             | Some s ->
                 (match List.assoc_opt "ptr" s.sfields_ty with
                  | Some (TConstPtr t) -> t
                  | _ ->
                      Error.failf idx_pos
                        "internal: `Slice` instance %s has no `ptr` of \
                         *const shape" (String.concat "::" path))
             | None ->
                 Error.failf idx_pos
                   "internal: `Slice` instance %s not in struct_index"
                   (String.concat "::" path))
        | other ->
            Error.failf idx_pos
              "indexing `[...]` requires an array or Slice, got %s"
              (typ_name other)
      in
      let tindex = elab_expr ctx env index in
      (* DR-011 sub-slicing: `s[lo..hi]` / `s[lo..=hi]` desugars to a
         `Slice<T>` view rather than a scalar index.  We recognise it
         by `tindex.ty` being a mono instance of `Range` /
         `RangeInclusive`, then build `Slice { ptr: &base[lo],
         len: (hi - lo) [+ 1] as u32 }`.  Bounds-check is omitted in
         v1 (consistent with the rest of `[i]`); the lo/hi types are
         the bound's integer type (cast to u32 for `Slice.len`). *)
      let range_inclusive_opt = match tindex.ty with
        | TStruct rp when Mono.is_instance_of ["Range"] rp -> Some (false, rp)
        | TStruct rp when Mono.is_instance_of ["RangeInclusive"] rp -> Some (true, rp)
        | _ -> None
      in
      (match range_inclusive_opt with
       | Some (inclusive, range_path) ->
           let range_sig = match resolve_struct_by_path ctx range_path with
             | Some s -> s
             | None ->
                 Error.failf idx_pos
                   "internal: Range instance %s not resolved"
                   (String.concat "::" range_path)
           in
           let bound_ty =
             try List.assoc "lo" range_sig.sfields_ty
             with Not_found ->
               Error.failf idx_pos "internal: Range missing 'lo' field"
           in
           let u32_t = TInt { signed = false; width = Ast.W32 } in
           let u32_ann = Ast.TyInt { signed = false; width = Ast.W32 } in
           (* Inline-fold when the Range was constructed in place
              (`s[1..4]`): we can read lo/hi directly off the
              StructLit without forcing a temp.  Variable-Range
              (`let r = 1..4; s[r]`) falls back to field access on
              the lifted temp. *)
           let (lo_expr, hi_expr) =
             match tindex.e with
             | TStructLit { fields; _ } ->
                 (try
                    (List.assoc "lo" fields, List.assoc "hi" fields)
                  with Not_found ->
                    ({ e = TFieldAccess { target = tindex; field = "lo" };
                       ty = bound_ty; pos = idx_pos },
                     { e = TFieldAccess { target = tindex; field = "hi" };
                       ty = bound_ty; pos = idx_pos }))
             | _ ->
                 ({ e = TFieldAccess { target = tindex; field = "lo" };
                    ty = bound_ty; pos = idx_pos },
                  { e = TFieldAccess { target = tindex; field = "hi" };
                    ty = bound_ty; pos = idx_pos })
           in
           let diff =
             { e = TBinOp (Ast.Sub, hi_expr, lo_expr);
               ty = bound_ty; pos = idx_pos } in
           let span =
             if inclusive then
               let one = { e = TIntLit 1; ty = bound_ty; pos = idx_pos } in
               { e = TBinOp (Ast.Add, diff, one);
                 ty = bound_ty; pos = idx_pos }
             else diff
           in
           let len_expr =
             { e = TCast (span, u32_ann); ty = u32_t; pos = idx_pos } in
           let elem_indexed =
             { e = TIndex { base = tbase; index = lo_expr };
               ty = elem_ty; pos = idx_pos } in
           let ptr_ty = TConstPtr elem_ty in
           let ptr_expr =
             { e = TRef elem_indexed; ty = ptr_ty; pos = idx_pos } in
           let slice_skel = match List.find_opt
             (fun (s : struct_sig) -> s.sname_path = ["Slice"]
                                       && s.stparams <> [])
             ctx.structs with
             | Some s -> s
             | None ->
                 Error.failf idx_pos
                   "internal: prelude Slice<T> skeleton not registered"
           in
           let slice_inst =
             Mono.instantiate_struct ctx.instances
               ~normalize:(normalize_apps ctx) slice_skel [elem_ty]
           in
           let slice_ty = TStruct slice_inst.sname_path in
           { e = TStructLit
               { sname_path = slice_inst.sname_path;
                 fields = [("ptr", ptr_expr); ("len", len_expr)];
                 base = None };
             ty = slice_ty; pos }
       | None ->
           if not (is_int_like tindex.ty) then
             Error.failf idx_pos
               "index must be an integer or a Range, got %s"
               (typ_name tindex.ty);
           { e = TIndex { base = tbase; index = tindex };
             ty = elem_ty; pos })
  | Ast.Range { lo; hi; inclusive; pos = rng_pos } ->
      (* `a..b` / `a..=b` desugars to a literal of the prelude struct
         `Range<T>` / `RangeInclusive<T>`.  `T` flows bidirectionally from
         the bounds; an explicit annotation on the enclosing binding still
         pins it via the StructLit elaboration path. *)
      let tname = if inclusive then ["RangeInclusive"] else ["Range"] in
      let lit = Ast.StructLit
        { tname; fields = [("lo", lo); ("hi", hi)];
          base = None; pos = rng_pos } in
      elab_expr ?expected ~allow_void ctx env lit
  | Ast.SizeOf (ann, sz_pos) ->
      (* `size_of(T)` yields a c_uint constant. *)
      let t = resolve_type_ann ~pos:sz_pos ctx ann in
      { e = TSizeOf t; ty = TCInt { signed = false }; pos }
  | Ast.Var (n, var_pos) ->
      (* Local / extern const / extern var → TVar; top-level fn name →
         TFnRef (auto-converts to fn-ptr in C).  Matches the lookup order
         used by type_of's Var case for consistency. *)
      (match List.assoc_opt n env with
       | Some t -> { e = TVar n; ty = t; pos }
       | None ->
           (match List.assoc_opt n ctx.ext_consts with
            | Some t -> { e = TVar n; ty = t; pos }
            | None ->
              (match lookup_const ctx [n] with
               | Some (t, mangled, _) -> { e = TVar mangled; ty = t; pos }
               | None ->
                (match List.assoc_opt n ctx.ext_vars with
                 | Some t -> { e = TVar n; ty = t; pos }
                 | None ->
                     (match lookup_fn ctx [n] with
                      | Some (_, { mangled; param_tys; ret_ty; _ }) ->
                          { e = TFnRef mangled;
                            ty = TFnPtr { params = param_tys; ret = ret_ty };
                            pos }
                      | None ->
                          Error.failf var_pos "undefined variable '%s'" n)))))
  | Ast.Orelse (value, default, or_pos) ->
      (* `value orelse default` is a 2-arm match over Option/Result.
         We build the TMatch directly from elab'd children to avoid
         a desugar→re-elab roundtrip (which used to be a second
         traversal of `value`). *)
      let tvalue = elab_expr ctx env value in
      let (ename_path, ok_name, err_name, err_binds) =
        match tvalue.ty with
        | TEnum p when Mono.is_instance_of ["Option"] p ->
            (p, "Some", "None", [])
        | TEnum p when Mono.is_instance_of ["Result"] p ->
            (p, "Ok", "Err", [ ("_0", TPWildcard) ])
        | other ->
            Error.failf or_pos
              "'orelse' requires an Option or Result value, got %s"
              (typ_name other)
      in
      let e_sig =
        match resolve_enum_by_path ctx ename_path with
        | Some e -> e
        | None -> Error.failf or_pos
                    "internal: Option/Result instance missing"
      in
      let (ok_tag, ok_vsig) =
        match find_variant e_sig ok_name with
        | Some r -> r
        | None -> Error.failf or_pos
                    "internal: '%s' missing on '%s'"
                    ok_name (String.concat "::" ename_path)
      in
      let (err_tag, _) =
        match find_variant e_sig err_name with
        | Some r -> r
        | None -> Error.failf or_pos
                    "internal: '%s' missing on '%s'"
                    err_name (String.concat "::" ename_path)
      in
      let ok_payload_ty =
        match ok_vsig.vsfields with
        | [(_, t)] -> t
        | _ -> Error.failf or_pos
                 "internal: Ok/Some payload shape"
      in
      let bind_name = "__orelse_v" in
      let ok_arm = {
        tpat = TPVariant {
          variant = ok_name; tag = ok_tag;
          binds = [ ("_0", TPVar bind_name) ];
        };
        tguard = None;
        tbody = { e = TVar bind_name; ty = ok_payload_ty; pos = or_pos };
        tdiverges = false;
        tarm_pos = or_pos;
      } in
      let tdefault =
        elab_expr ~allow_void ?expected ctx env default
      in
      if not (typ_eq tdefault.ty ok_payload_ty)
         && not (int_lit_fits default ok_payload_ty)
      then
        Error.failf or_pos
          "orelse arms have inconsistent types: %s vs %s"
          (typ_name ok_payload_ty) (typ_name tdefault.ty);
      let err_arm = {
        tpat = TPVariant {
          variant = err_name; tag = err_tag; binds = err_binds;
        };
        tguard = None;
        tbody = tdefault;
        tdiverges = false;
        tarm_pos = or_pos;
      } in
      { e = TMatch {
          scrutinee = tvalue; ename_path; arms = [ ok_arm; err_arm ];
        };
        ty = ok_payload_ty; pos }
  | Ast.Match { scrutinee; arms; pos = match_pos } ->
      if arms = [] then
        Error.failf match_pos "'match' must have at least one arm";
      (* DR-009 — match-site view-call insertion.  If any arm pattern
         names `Vname::Case` where `Vname` is a registered view, the
         scrutinee is the view's INPUT (not the synthesised enum), so
         wrap it in the view-fn call: `match scr { Vname::A => ... }`
         becomes `match Vname(scr) { Vname::A => ... }`.  Done before
         the scrutinee is elaborated so the rest of the match path
         sees a normal enum match.  v1 constraint: all arms must use
         the same view name; multiple view-tagged arms with
         different view names are not supported in v1 (would require
         splitting the match — design says: keep Maranget simple). *)
      let rec view_in_pat = function
        | Ast.PVariant { tname = [vn]; _ }
            when Hashtbl.mem view_names vn -> Some vn
        | Ast.POr (alts, _) -> List.find_map view_in_pat alts
        | _ -> None
      in
      let view_target =
        List.find_map (fun (a : Ast.match_arm) -> view_in_pat a.pat) arms
      in
      let tscrut = elab_expr ctx env scrutinee in
      (* Only insert the view-fn call when the scrutinee isn't already
         of the view's enum type — otherwise `let s = Sign(7); match s
         { Sign::A => ... }` would double-wrap.  Re-elab the wrapped
         AST through the call path so check_call_args validates the
         input type matches the view's param. *)
      let tscrut =
        match view_target with
        | Some vname
          when (match tscrut.ty with
                | TEnum [v] when v = vname -> false
                | _ -> true) ->
            let wrapped =
              Ast.Call { callee = [vname]; args = [scrutinee];
                         pos = match_pos } in
            elab_expr ctx env wrapped
        | _ -> tscrut
      in
      let ename_path =
        match tscrut.ty with
        | TEnum p -> p
        | TInt _ | TCInt _ ->
            (* GATE-5a scalar literal match — `match b { 'a' => ... }`.
               The empty path is the scalar sentinel: codegen switches
               on the value itself instead of a tag, and the
               exhaustiveness rule below replaces Maranget (integer
               domains are not enumerated; a final catch-all arm is
               required instead). *)
            []
        | TBool ->
            (* GATE-5b bool match — same scalar sentinel / codegen path
               (switch on 0/1), but a finite domain, so `true | false`
               is exhaustive without a catch-all (see [bool_match]). *)
            []
        | other ->
            Error.failf match_pos
              "'match' requires an enum, integer, or bool value, got %s"
              (typ_name other)
      in
      let scalar_match = ename_path = [] in
      let bool_match = (match tscrut.ty with TBool -> true | _ -> false) in
      (* Freeze-audit B11: pattern binds reached THROUGH A BORROW are
         loans, not transfers — `match *e` with `e: *const E` must not
         hand out `own *T` children (a stolen child double-frees
         against the real owner's drop).  Walk the scrutinee's access
         chain: any non-own pointer hop makes the context borrowed, and
         own-typed binds demote to the hop's borrow flavour.  The
         move-pass applies the same rule to its bind seeding. *)
      let scrutinee_borrow_mode =
        let rec mode (te : texpr) =
          match te.e with
          | TDeref inner ->
              (match inner.ty with
               | TConstPtr _ -> Some `Const
               | TPtr _ -> Some `Mut
               | _ -> mode inner)
          | TFieldAccess { target; _ } ->
              (match target.ty with
               | TConstPtr _ -> Some `Const
               | TPtr _ -> Some `Mut
               | _ -> mode target)
          | TIndex { base; _ } -> mode base
          | _ -> None
        in
        mode tscrut
      in
      let demote_bind_ty t =
        match scrutinee_borrow_mode, t with
        | Some `Const, TOwnPtr inner -> TConstPtr inner
        | Some `Mut, TOwnPtr inner -> TPtr inner
        | _ -> t
      in
      let tarms =
        List.map (fun (a : Ast.match_arm) ->
          let (tpat, binds) = lower_pattern ctx tscrut.ty a.pat in
          let binds = List.map (fun (n, t) -> (n, demote_bind_ty t)) binds in
          (* Bind names must be unique across the whole (possibly nested)
             pattern, not just one level. *)
          (match find_dup ~key:fst binds with
           | Some n ->
               Error.failf a.arm_pos "duplicate bind name '%s' in pattern" n
           | None -> ());
          let arm_env = binds @ env in
          let tguard =
            match a.guard with
            | None -> None
            | Some g ->
                let tg = elab_expr ctx arm_env g in
                if not (typ_eq tg.ty TBool) then
                  Error.failf tg.pos
                    "match-arm guard `if ...` must be of type bool, got %s"
                    (typ_name tg.ty);
                Some tg
          in
          (* DR-020 propagate the match's expected type into each arm
             body.  This lets nested generic enum constructions
             (`Option::None` in a fn returning `Option<F::Output>`)
             pick up their tparams from the bidirectional seed, so the
             user can write a natural match instead of a `try`
             workaround.  Forwarded as-is — guards still typecheck as
             bool independently. *)
          let tbody = elab_expr ~allow_void ?expected ctx arm_env a.body in
          { tpat; tguard; tbody; tdiverges = false; tarm_pos = a.arm_pos })
          arms
      in
      if scalar_match && bool_match then begin
        (* Bool exhaustiveness (GATE-5b): the domain is exactly
           {true, false}, so unguarded coverage of BOTH closes the match
           with no catch-all; a catch-all also closes it.  Duplicate
           values and post-coverage arms are unreachable. *)
        let bools_of = function
          | TPBool b -> [ b ]
          | TPOr alts ->
              List.filter_map
                (function TPBool b -> Some b | _ -> None) alts
          | _ -> []
        in
        let is_catch_all = function
          | TPVar _ | TPWildcard -> true
          | TPOr alts ->
              List.exists
                (function TPVar _ | TPWildcard -> true | _ -> false) alts
          | _ -> false
        in
        let seen_t = ref false and seen_f = ref false in
        let covered = ref false in
        List.iter (fun (a : tmatch_arm) ->
          if !covered then
            Error.failf a.tarm_pos
              "unreachable match arm: an earlier arm already covers \
               every value";
          List.iter (fun b ->
            if (if b then !seen_t else !seen_f) then
              Error.failf a.tarm_pos
                "unreachable match arm: '%b' is already covered" b;
            if a.tguard = None then
              (if b then seen_t := true else seen_f := true))
            (bools_of a.tpat);
          if a.tguard = None
             && (is_catch_all a.tpat || (!seen_t && !seen_f)) then
            covered := true)
          tarms;
        if not !covered then
          Error.failf match_pos
            "non-exhaustive 'match' on bool: cover both 'true' and \
             'false' (or add a catch-all arm)"
      end else if scalar_match then begin
        (* Scalar exhaustiveness (GATE-5a): the integer domain is too
           wide to enumerate, so the rule is simpler than Maranget —
           every literal may appear once (per unguarded coverage), and
           an UNGUARDED catch-all arm (`_` or a binding) must close
           the match.  Guarded arms never prove coverage (same rule as
           the enum path below). *)
        let lits_of = function
          | TPLit n -> [ n ]
          | TPOr alts ->
              List.filter_map
                (function TPLit n -> Some n | _ -> None) alts
          | _ -> []
        in
        let is_catch_all = function
          | TPVar _ | TPWildcard -> true
          | TPOr alts ->
              List.exists
                (function TPVar _ | TPWildcard -> true | _ -> false) alts
          | _ -> false
        in
        let seen = Hashtbl.create 16 in
        let covered = ref false in
        List.iter (fun (a : tmatch_arm) ->
          if !covered then
            Error.failf a.tarm_pos
              "unreachable match arm: an earlier catch-all already \
               covers every value";
          List.iter (fun n ->
            if Hashtbl.mem seen n then
              Error.failf a.tarm_pos
                "unreachable match arm: literal %d is already covered" n;
            if a.tguard = None then Hashtbl.add seen n ())
            (lits_of a.tpat);
          if is_catch_all a.tpat && a.tguard = None then covered := true)
          tarms;
        if not !covered then
          Error.failf match_pos
            "non-exhaustive 'match' on an integer: add a final \
             catch-all arm ('_' or a binding) — the integer domain \
             cannot be enumerated"
      end else begin
      (* Redundancy + exhaustiveness via the usefulness matrix (Maranget).
         Each arm expands to one or more single-column rows (one per
         or-pattern alternative); nested variant patterns expand columns
         internally during specialization.  Guards complicate both checks:
         a guarded arm's pattern may match a value but the body still not
         run, so the arm doesn't prove coverage.  Standard treatment:
         - Redundancy of arm i uses only the rows of UNGUARDED prior arms
           (guarded priors might leak their value through to arm i).
         - Exhaustiveness uses only the rows of UNGUARDED arms (guarded
           arms contribute nothing to "must cover" reasoning). *)
      let rows_per_arm =
        List.map (fun (a : tmatch_arm) ->
          List.map (fun cp -> [ cp ]) (cpat_rows_of_tpat a.tpat)) tarms
      in
      let is_unguarded i = (List.nth tarms i).tguard = None in
      (* DR-002 W3 — per-alternative redundancy.  Pre-fix the check
         asked whether the arm as a whole had any useful row, so an
         arm like `B | C` after a `A | B` arm passed even though `B`
         in the second arm is covered by the first.  Codegen then
         emitted a flat switch with duplicate `case ex_T_B:` labels
         and cc rejected the output.  Walk alternatives in source
         order, accumulating accepted rows; reject the FIRST row a
         given arm contributes that no surviving combination of
         earlier rows leaves uncovered.  Guarded arms still check
         against earlier-arm rows but don't seed the cross-arm
         seen-set (their body may not run even when the pattern
         matches — same rule the exhaustiveness pass already uses
         below). *)
      let seen_cross_arm = ref [] in
      List.iteri (fun i (a : Ast.match_arm) ->
        let arm_rows = List.nth rows_per_arm i in
        let seen_intra_arm = ref [] in
        List.iter (fun row ->
          let earlier = !seen_cross_arm @ !seen_intra_arm in
          if not (useful ctx [ tscrut.ty ] earlier row) then
            Error.failf a.arm_pos
              "unreachable match arm: earlier arms already cover this case";
          seen_intra_arm := !seen_intra_arm @ [ row ])
          arm_rows;
        if is_unguarded i then
          seen_cross_arm := !seen_cross_arm @ !seen_intra_arm)
        arms;
      let rows =
        List.concat
          (List.filteri (fun i _ -> is_unguarded i) rows_per_arm)
      in
      (match missing ctx [ tscrut.ty ] rows with
       | Some (w :: _) ->
           Error.failf match_pos
             "non-exhaustive 'match': pattern '%s' is not covered \
              (add an arm or '_')"
             (render_cpat ctx tscrut.ty w)
       | _ -> ())
      end;
      (* All non-diverging arm bodies must agree on a type — pick the
         first as the witness. *)
      let non_div =
        List.filter_map (fun (a : tmatch_arm) ->
          if a.tdiverges then None else Some a.tbody)
          tarms
      in
      let result_ty =
        match non_div with
        | [] -> t_i32   (* all diverge — placeholder; outer caller discards *)
        | b0 :: rest ->
            List.iter (fun (b : texpr) ->
              if not (typ_eq b.ty b0.ty) then
                Error.failf match_pos
                  "match arms have inconsistent types: %s vs %s"
                  (typ_name b0.ty) (typ_name b.ty))
              rest;
            b0.ty
      in
      { e = TMatch { scrutinee = tscrut; ename_path; arms = tarms };
        ty = result_ty; pos }
  | Ast.If { cond; then_blk; else_blk; pos } ->
      (* `if` in value position: requires `else`, and both branches must
         be a single trailing expression of the same type.  Branch blocks
         with leading statements are deferred. *)
      let eblk =
        match else_blk with
        | Some b -> b
        | None ->
            Error.failf pos
              "`if` used as a value needs an `else` branch (a value must \
               exist on every path)"
      in
      let branch_expr what blk =
        match branch_value_expr blk with
        | Some e -> e
        | None ->
            Error.failf pos
              "`if` %s branch must be a single expression to yield a value \
               (block expressions in branches are not yet supported)" what
      in
      let then_e = branch_expr "then" then_blk in
      let else_e = branch_expr "else" eblk in
      let tcond = elab_expr ctx env cond in
      let tthen = elab_expr ?expected ~allow_void ctx env then_e in
      let telse = elab_expr ?expected ~allow_void ctx env else_e in
      if not (typ_eq tthen.ty telse.ty) then
        Error.failf pos
          "`if` branches have inconsistent types: %s vs %s"
          (typ_name tthen.ty) (typ_name telse.ty);
      { e = TIfExpr { cond = tcond; then_val = tthen; else_val = telse };
        ty = tthen.ty; pos }
  | Ast.Try (value, try_pos) ->
      (* Lower to a TMatch with one yielding Ok/Some arm and one
         diverging Err/None arm.  The diverging arm carries the
         early-return value (an EnumLit on the *outer* fn's ret
         instance); codegen emits `return tbody;` for it.  ret_ty
         must be set and match the inner enum (Option↔Option,
         Result with same E↔Result with same E). *)
      let tinner = elab_expr ctx env value in
      let inner_path =
        match tinner.ty with
        | TEnum p -> p
        | _ ->
            Error.failf try_pos
              "'try' requires Option or Result, got %s" (typ_name tinner.ty)
      in
      let inner_skel =
        if Mono.is_instance_of ["Option"] inner_path then ["Option"]
        else if Mono.is_instance_of ["Result"] inner_path then ["Result"]
        else
          Error.failf try_pos
            "'try' requires Option or Result, got %s" (typ_name tinner.ty)
      in
      let outer_path =
        match ctx.ret_ty with
        | Some (TEnum p) when Mono.is_instance_of inner_skel p -> p
        | Some other ->
            Error.failf try_pos
              "'try' on %s value but enclosing fn returns %s — they must \
               share the same Option/Result shape"
              (typ_name tinner.ty) (typ_name other)
        | None ->
            Error.failf try_pos
              "'try' is only allowed in fns that return Option or Result \
               (this fn has no return type)"
      in
      let inner_e =
        match resolve_enum_by_path ctx inner_path with
        | Some e -> e
        | None -> Error.failf try_pos "internal: inner enum missing"
      in
      let outer_e =
        match resolve_enum_by_path ctx outer_path with
        | Some e -> e
        | None -> Error.failf try_pos "internal: outer enum missing"
      in
      let find_v e name =
        match find_variant e name with
        | Some r -> r
        | None -> Error.failf try_pos "internal: variant '%s' missing" name
      in
      let (ok_name, err_name) =
        if inner_skel = ["Option"] then ("Some", "None")
        else ("Ok", "Err")
      in
      let (ok_tag, ok_vs) = find_v inner_e ok_name in
      let (err_tag_in, err_vs_in) = find_v inner_e err_name in
      let (outer_err_tag, outer_err_vs) = find_v outer_e err_name in
      let payload_compat (in_fs : (string * typ) list)
                         (out_fs : (string * typ) list) =
        List.length in_fs = List.length out_fs
        && List.for_all2 (fun (_, a) (_, b) -> typ_eq a b) in_fs out_fs
      in
      if not (payload_compat err_vs_in.vsfields outer_err_vs.vsfields) then
        Error.failf try_pos
          "'try' Err/None payload mismatch: inner %s vs outer %s"
          (typ_name (TEnum inner_path)) (typ_name (TEnum outer_path));
      let ok_payload_ty =
        match ok_vs.vsfields with
        | [(_, t)] -> t
        | _ -> Error.failf try_pos "internal: Ok/Some payload shape"
      in
      let try_bind = "__try_v" in
      let ok_arm_pat =
        TPVariant {
          variant = ok_name;
          tag = ok_tag;
          binds = [ ("_0", TPVar try_bind) ];
        }
      in
      let ok_body = { e = TVar try_bind; ty = ok_payload_ty; pos = try_pos } in
      let ok_arm = {
        tpat = ok_arm_pat; tguard = None; tbody = ok_body;
        tdiverges = false; tarm_pos = try_pos;
      } in
      let try_err_bind = "__try_e" in
      let (err_arm_pat, outer_err_args) =
        if inner_skel = ["Option"] then
          (TPVariant { variant = err_name; tag = err_tag_in; binds = [] }, [])
        else
          (TPVariant {
              variant = err_name;
              tag = err_tag_in;
              binds = [ ("_0", TPVar try_err_bind) ];
            },
           let e_ty =
             match err_vs_in.vsfields with
             | [(_, t)] -> t
             | _ -> Error.failf try_pos "internal: Err payload shape"
           in
           [ ("_0", { e = TVar try_err_bind; ty = e_ty; pos = try_pos }) ])
      in
      let err_body = {
        e = TEnumLit {
          ename_path = outer_path;
          variant = err_name;
          tag = outer_err_tag;
          args = outer_err_args;
        };
        ty = TEnum outer_path;
        pos = try_pos;
      } in
      let err_arm = {
        tpat = err_arm_pat; tguard = None; tbody = err_body;
        tdiverges = true; tarm_pos = try_pos;
      } in
      let node = TMatch {
        scrutinee = tinner;
        ename_path = inner_path;
        arms = [ ok_arm; err_arm ];
      } in
      { e = node; ty = ok_payload_ty; pos }
  | (Ast.StructLit { tname; fields; base; pos = lit_pos } as raw_lit)
  | (Ast.New { tname; fields; base; alloc = _; pos = lit_pos } as raw_lit) ->
      (* Enum struct-variant ctor: parser emits StructLit for
         `Foo::V { f: e }`; if the path resolves to an enum variant,
         re-elab via the EnumLit branch. *)
      let as_struct_lit = match raw_lit with
        | Ast.StructLit _ -> true | _ -> false
      in
      (* DR-046: extract optional allocator expression for `new(alloc)` form.
         When present, ret_ty becomes `own *T` (sanctioned ownership origin)
         and codegen uses `alloc.alloc_fn` instead of raw `malloc`. *)
      let alloc_ast = match raw_lit with
        | Ast.New { alloc = Some a; _ } -> Some a
        | _ -> None
      in
      (match if as_struct_lit
             then rewrite_struct_lit_as_enum_lit ctx tname fields base lit_pos
             else None with
       | Some e -> elab_expr ~allow_void ?expected ctx env e
       | None ->
           let display = String.concat "::" tname in
           let s = match lookup_struct ctx tname with
             | Some s -> s
             | None -> Error.failf lit_pos "unknown struct '%s'" display
           in
           let parent = parent_path s.sname_path in
           if (not s.sis_pub) && not (is_prefix parent ctx.scope) then
             Error.failf lit_pos "struct '%s' is private to module '%s'"
               display
               (if parent = [] then "<root>"
                else String.concat "::" parent);
           (match find_dup ~key:fst fields with
            | Some n ->
                Error.failf lit_pos
                  "duplicate field '%s' in struct literal '%s'" n display
            | None -> ());
           let provided = List.map fst fields in
           let expected_names = List.map fst s.sfields_ty in
           let missing =
             List.filter (fun n -> not (List.mem n provided)) expected_names
           in
           let extra =
             List.filter (fun n -> not (List.mem n expected_names)) provided
           in
           if extra <> [] then
             Error.failf lit_pos
               "struct literal '%s' has unknown field(s): %s"
               display (String.concat ", " extra);
           (* Elab fields (preserve source order) and optional base. *)
           let tfields =
             List.map (fun (fn, fe) -> (fn, elab_expr ctx env fe)) fields
           in
           let tbase = Option.map (elab_expr ctx env) base in
           (* Generic struct instantiation: infer tparams from elab'd
              field types, build a fresh monomorphic struct_sig. *)
           let s_concrete =
             if s.stparams = [] then s
             else match tbase with
               | Some be ->
                   (* `..base` pins the concrete instance directly: the
                      base value already has type `Pair<i32, bool>`, so the
                      tparams need no inference from the (possibly partial)
                      provided fields.  Missing fields are filled from base. *)
                   (match be.ty with
                    | TStruct path when Mono.is_instance_of s.sname_path path ->
                        (match Mono.find_struct ctx.instances path with
                         | Some inst -> inst
                         | None ->
                             (match resolve_struct_by_path ctx path with
                              | Some inst -> inst
                              | None ->
                                  Error.failf lit_pos
                                    "internal: '..base' instance '%s' missing \
                                     from index" (String.concat "::" path)))
                    | _ ->
                        Error.failf lit_pos
                          "'..base' in struct literal '%s' expects a value of \
                           type '%s', got %s"
                          display display (typ_name be.ty))
               | None ->
                   if missing <> [] then
                     Error.failf lit_pos
                       "struct literal '%s' missing field(s): %s"
                       display (String.concat ", " missing);
                   let pairs =
                     List.concat_map (expand_inst_pair ctx)
                       (List.map (fun (fn, (fe : texpr)) ->
                          let decl_t = List.assoc fn s.sfields_ty in
                          (decl_t, fe.ty))
                          tfields)
                   in
                   let inferred =
                     Mono.infer_tparams ~pos:lit_pos s.stparams pairs
                   in
                   Mono.instantiate_struct ctx.instances
                     ~normalize:(normalize_apps ctx) s inferred
           in
           (match tbase with
            | None ->
                if missing <> [] && s.stparams = [] then
                  Error.failf lit_pos
                    "struct literal '%s' missing field(s): %s"
                    display (String.concat ", " missing)
            | Some be ->
                (match be.ty with
                 | TStruct path when path = s_concrete.sname_path -> ()
                 | _ ->
                     Error.failf lit_pos
                       "'..base' in struct literal '%s' expects a value \
                        of type %s, got %s"
                       display display (typ_name be.ty)));
           (* Per-field type check against the concrete struct_sig.
              int_lit_fits stays on the original Ast literal so the
              compatibility check matches the existing behavior. *)
           List.iter2 (fun (fn, fe) (fn', (te : texpr)) ->
             assert (fn = fn');
             let fty = List.assoc fn s_concrete.sfields_ty in
             if not (coercible_to ~from:te.ty ~to_:fty)
                && not (int_lit_fits fe fty) then
               Error.failf lit_pos
                 "field '%s' of struct '%s': expected %s, got %s"
                 fn display (typ_name fty) (typ_name te.ty))
             fields tfields;
           (* DR-052: an own value in a borrow-typed field is a loan. *)
           let tfields =
             List.map (fun (fn, (te : texpr)) ->
               (fn, loan_restamp
                      ~slot_ty:(List.assoc fn s_concrete.sfields_ty) te))
               tfields
           in
           let sname_path = s_concrete.sname_path in
           (* DR-046: elab the allocator expression (if `new(alloc)` was
              used) and verify it carries a value-shaped Allocator.
              The presence of an allocator flips the ret_ty from raw
              `*T` to `own *T` (sanctioned ownership origin). *)
           if (not as_struct_lit) && alloc_ast = None then
             Error.failf lit_pos
               "bare `new %s { ... }` requires an explicit allocator: \
                write `new(alloc) %s { ... }` (obtain one via \
                `default_allocator()`).  Heap allocation is always \
                explicit-allocator in exile."
               display display;
           let talloc =
             Option.map (fun ae ->
               let te = elab_expr ctx env ae in
               (match te.ty with
                | TStruct ["Allocator"] -> ()
                | other ->
                    Error.failf lit_pos
                      "`new(...)` allocator expression must have type \
                       Allocator, got %s" (typ_name other));
               te) alloc_ast
           in
           let result_node =
             if as_struct_lit
             then TStructLit { sname_path; fields = tfields; base = tbase }
             else TNew { sname_path;
                         fields = tfields;
                         base = tbase;
                         alloc = talloc }
           in
           let result_ty =
             let st = TStruct sname_path in
             if as_struct_lit then st
             else if talloc <> None then TOwnPtr st
             else TPtr st
           in
           { e = result_node; ty = result_ty; pos })
  | Ast.NewEnum { tname; args; alloc; pos = lit_pos } ->
      (* DR-031 heap-boxed enum tuple-variant: split `Path::Variant`,
         elaborate as an EnumLit tuple-variant first, then rewrap the
         resulting IR node as TNewEnum with `*Enum` type.
         DR-046: optional `new(alloc) Path::V(args)` flips the ret_ty
         to `own *Enum` (sanctioned ownership origin). *)
      let (enum_path, variant) =
        match List.rev tname with
        | last :: rev_init -> (List.rev rev_init, last)
        | [] ->
            Error.failf lit_pos
              "'new ...' needs a qualified path (`Enum::Variant`)"
      in
      if alloc = None then
        Error.failf lit_pos
          "bare `new %s(...)` requires an explicit allocator: \
           write `new(alloc) %s(...)` (obtain one via \
           `default_allocator()`).  Heap allocation is always \
           explicit-allocator in exile."
          (String.concat "::" tname) (String.concat "::" tname);
      let talloc =
        Option.map (fun ae ->
          let te = elab_expr ctx env ae in
          (match te.ty with
           | TStruct ["Allocator"] -> ()
           | other ->
               Error.failf lit_pos
                 "`new(...)` allocator expression must have type \
                  Allocator, got %s" (typ_name other));
          te) alloc
      in
      let lit_ast = Ast.EnumLit {
        tname = enum_path; variant;
        args = Ast.EATuple args; pos = lit_pos } in
      let tlit = elab_expr ~allow_void ctx env lit_ast in
      (match tlit.e, tlit.ty with
       | TEnumLit { ename_path; variant = v; tag; args = targs }, TEnum p ->
           let result_ty =
             if talloc <> None then TOwnPtr (TEnum p) else TPtr (TEnum p)
           in
           { e = TNewEnum {
               ename_path; variant = v; tag; args = targs;
               alloc = talloc };
             ty = result_ty; pos = lit_pos }
       | _ ->
           Error.failf lit_pos
             "'new %s::%s' must name an enum tuple-variant"
             (String.concat "::" enum_path) variant)
  | Ast.EnumLit { tname; variant; args; pos = lit_pos } ->
      (* Fall-back BEFORE enum lookup: qualified `raw::DOSBase` with no
         payload is a reference to an extern var/const, not a unit
         variant.  Last segment is the C symbol name; ext_* tables are
         flat. *)
      let qualified_global = match args with
        | Ast.EATuple [] when tname <> [] ->
            (* user `const` (scope-resolved, mangled name) takes priority,
               then flat extern var / const (raw C name = last segment). *)
            (match lookup_const ctx (tname @ [variant]) with
             | Some (t, mangled, _) -> Some (mangled, t)
             | None ->
                 (match List.assoc_opt variant ctx.ext_vars with
                  | Some t -> Some (variant, t)
                  | None ->
                      (match List.assoc_opt variant ctx.ext_consts with
                       | Some t -> Some (variant, t)
                       | None -> None)))
        | _ -> None
      in
      (match qualified_global with
       | Some (cname, t) -> { e = TVar cname; ty = t; pos }
       | None ->
           let e_sig = match lookup_enum ctx tname with
             | Some e -> e
             | None ->
                 Error.failf lit_pos "unknown enum '%s'"
                   (String.concat "::" tname)
           in
           let v = match List.find_opt
                     (fun (vs : variant_sig) -> vs.vsname = variant)
                     e_sig.evariants with
             | Some v -> v
             | None ->
                 Error.failf lit_pos "enum '%s' has no variant '%s'"
                   (String.concat "::" e_sig.ename_path) variant
           in
           let display =
             String.concat "::" e_sig.ename_path ^ "::" ^ variant
           in
           (* Pre-elab args once; subsequent generic inference and
              type-checking read .ty directly. *)
           let elab_args =
             match args with
             | Ast.EATuple es -> `Tuple (List.map (elab_expr ctx env) es, es)
             | Ast.EAStruct fs ->
                 `Struct (List.map (fun (n, e) ->
                            (n, elab_expr ctx env e, e)) fs)
           in
           (* Generic enum: infer tparams from elab'd payload types,
              instantiate, then validate against the concrete shape. *)
           let final_enum =
             if e_sig.etparams = [] then e_sig
             else begin
               let pairs =
                 match elab_args with
                 | `Tuple (telabs, _) when not v.vsis_struct ->
                     if List.length telabs <> List.length v.vsfields then []
                     else
                       List.map2 (fun (_, decl_t) (te : texpr) ->
                         (decl_t, te.ty))
                         v.vsfields telabs
                 | `Struct fs when v.vsis_struct ->
                     List.filter_map (fun (n, (te : texpr), _) ->
                       match List.assoc_opt n v.vsfields with
                       | Some decl_t -> Some (decl_t, te.ty)
                       | None -> None)
                       fs
                 | _ -> []
               in
               let seed =
                 match expected with
                 | Some (TEnum exp_path)
                   when Mono.is_instance_of e_sig.ename_path exp_path ->
                     (match resolve_enum_by_path ctx exp_path with
                      | Some exp_inst ->
                          (match exp_inst.einstance_args with
                           | Some inst_args ->
                               List.combine e_sig.etparams inst_args
                           | None -> [])
                      | None -> [])
                 (* DR-020 bidirectional seed for non-concrete generic
                    Option / Result expected types.  When the enclosing
                    fn's ret type is `Option<F::Output>` (or any other
                    generic shape where args carry TVars / TAssocProj),
                    the expected type stays as TEnumApp rather than
                    collapsing to a flat TEnum mono-instance.  Match
                    the skeleton path against `e_sig.ename_path` and
                    seed `etparams` directly from `args` — keeps the
                    bare `Option::None => Option::None` arm working
                    when the surrounding match's expected type carries
                    a tparam-projected payload. *)
                 | Some (TEnumApp { path; args = exp_args })
                   when path = e_sig.ename_path
                        && List.length exp_args
                           = List.length e_sig.etparams ->
                     List.combine e_sig.etparams exp_args
                 | _ -> []
               in
               let inferred =
                 Mono.infer_tparams ~pos:lit_pos ~seed e_sig.etparams
                   (List.concat_map (expand_inst_pair ctx) pairs)
               in
               Mono.instantiate_enum ctx.instances
                 ~normalize:(normalize_apps ctx) e_sig inferred
             end
           in
           let (tag, v_used) =
             match find_variant final_enum variant with
             | Some r -> r
             | None ->
                 Error.failf lit_pos
                   "internal: variant '%s' missing from enum '%s' \
                    after instantiation"
                   variant (String.concat "::" final_enum.ename_path)
           in
           let result_path = final_enum.ename_path in
           (* Validate args against the concrete v_used + build TEnumLit
              args list. *)
           let targs =
             match elab_args with
             | `Tuple (telabs, raws) ->
                 if v_used.vsis_struct then
                   Error.failf lit_pos
                     "variant '%s' is a struct variant; construct it \
                      with '{ field: ... }', not with '(...)'" display;
                 let expected_n = List.length v_used.vsfields in
                 let got = List.length telabs in
                 if expected_n <> got then
                   Error.failf lit_pos
                     "variant '%s' takes %d argument(s), got %d"
                     display expected_n got;
                 List.iteri (fun i ((_, exp), (te : texpr)) ->
                   if not (coercible_to ~from:te.ty ~to_:exp)
                      && not (int_lit_fits (List.nth raws i) exp)
                   then
                     Error.failf lit_pos
                       "argument %d of '%s': expected %s, got %s"
                       (i + 1) display (typ_name exp) (typ_name te.ty))
                   (List.combine v_used.vsfields telabs);
                 (* DR-052: own value in a borrow-typed payload = loan. *)
                 List.map2 (fun (n, fty) te ->
                     (n, loan_restamp ~slot_ty:fty te))
                   v_used.vsfields telabs
             | `Struct fs ->
                 if not v_used.vsis_struct then
                   Error.failf lit_pos
                     "variant '%s' is a tuple variant; construct it \
                      with '(...)', not with '{ field: ... }'" display;
                 let expected_names = List.map fst v_used.vsfields in
                 let got_names = List.map (fun (n, _, _) -> n) fs in
                 (match find_dup ~key:Fun.id got_names with
                  | Some n ->
                      Error.failf lit_pos
                        "duplicate field '%s' in '%s' construction"
                        n display
                  | None -> ());
                 List.iter (fun (n, _, _) ->
                   if not (List.mem n expected_names) then
                     Error.failf lit_pos
                       "variant '%s' has no field '%s'" display n) fs;
                 List.iter (fun n ->
                   if not (List.exists (fun (m, _, _) -> m = n) fs) then
                     Error.failf lit_pos
                       "missing field '%s' in '%s' construction" n display)
                   expected_names;
                 List.iter (fun (n, (te : texpr), raw) ->
                   let exp = List.assoc n v_used.vsfields in
                   if not (typ_eq exp te.ty) && not (int_lit_fits raw exp)
                   then
                     Error.failf lit_pos
                       "field '%s' of '%s': expected %s, got %s"
                       n display (typ_name exp) (typ_name te.ty)) fs;
                 (* Emit args in variant's field-declaration order.
                    DR-052: own value in a borrow-typed field = loan. *)
                 List.map (fun (n, fty) ->
                   let (_, te, _) =
                     List.find (fun (m, _, _) -> m = n) fs
                   in
                   (n, loan_restamp ~slot_ty:fty te))
                   v_used.vsfields
           in
           { e = TEnumLit { ename_path = result_path; variant; tag;
                            args = targs };
             ty = TEnum result_path; pos })
  | Ast.Call { callee = path; args; pos = call_pos } ->
      (* DR-018 call-desugar — `f(x)` where `f` is a local variable
         whose type is a tparam bounded by a Fn-trait (Fn1, Fn2, ...)
         rewrites to `f.call(x)` so the standard trait-method dispatch
         takes over.  Without this the next branch would fall into
         `lookup_fn` and report "unknown function 'f'".  Only fires
         when `f` is a SINGLE-segment path that resolves to a local
         Var, not a real fn — qualified paths or shadowed identifiers
         keep their existing meaning.  Concrete struct types whose
         `impl Fn1 for X` is registered also dispatch through this
         path (they have `X` head, not a TVar, but the same Fn-trait
         lookup applies). *)
      let fn_call_desugar =
        match path with
        | [n] ->
            let lookup_ty =
              match List.assoc_opt n env with
              | Some t -> Some t
              | None ->
                  (match List.assoc_opt n ctx.ext_consts with
                   | Some t -> Some t
                   | None -> List.assoc_opt n ctx.ext_vars)
            in
            (match lookup_ty with
             | Some (TVar tp) ->
                 let bound_traits =
                   List.filter_map
                     (fun (q, trait_path, _assocs) ->
                       if q = tp then
                         match List.rev trait_path with
                         | last :: _ -> Some last
                         | [] -> None
                       else None)
                     ctx.tbounds
                 in
                 let is_fn_trait t =
                   String.length t >= 2
                   && String.sub t 0 2 = "Fn"
                 in
                 if List.exists is_fn_trait bound_traits
                    && lookup_fn ctx path = None
                 then Some n
                 else None
             | Some (TStruct p | TEnum p)
                 when (List.exists (fun (t, target) ->
                          target = p
                          && String.length t >= 2
                          && String.sub t 0 2 = "Fn")
                          !trait_impl_table)
                      && lookup_fn ctx path = None ->
                 Some n
             | _ -> None)
        | _ -> None
      in
      (match fn_call_desugar with
       | Some n ->
           elab_expr ~allow_void ?expected ctx env
             (Ast.MethodCall {
                receiver = Ast.Var (n, call_pos);
                name = "call"; args; pos = call_pos })
       | None ->
      (* Enum-ctor dispatch first: a Call whose path resolves to an
         enum variant is rewritten to an EnumLit and elab'd again. *)
      (match rewrite_call_as_enum_lit ctx path args call_pos with
       | Some lowered -> elab_expr ~allow_void ?expected ctx env lowered
       | None when path = ["len"] ->
           (* `len(a)` — array length, folded to a compile-time literal
              (the array's static size N).  Pairs with `for i in 0..len(a)`. *)
           (match args with
            | [arg] ->
                let ta = elab_expr ctx env arg in
                (match ta.ty with
                 | TArray { size; _ } ->
                     (* `int` (not c_uint) so `i < len(a)` type-checks
                        directly — len is meant to pair with index loops. *)
                     { e = TIntLit size; ty = t_i32; pos }
                 | other ->
                     Error.failf call_pos
                       "len(...) requires an array, got %s" (typ_name other))
            | _ ->
                Error.failf call_pos "len(...) takes exactly one argument")
       | None ->
           let targs = List.map (elab_expr ctx env) args in
           let arg_tys = List.map (fun (a : texpr) -> a.ty) targs in
           (match lookup_builtin path with
            | Some b ->
                (* DR-002 — `println(x)` / `print(x)` on a struct/enum
                   value `x` whose type has an `impl Display for T`
                   registered: desugar to the writer pattern (alloc a
                   temp StringBuilder via `default_allocator()`, ask
                   x to render itself into it, build a String, print
                   `as_str()`, free the String).  Without this the
                   `print_like_bcheck` allowlist rejects every
                   aggregate that isn't `@debug`-marked — Display is
                   the user-facing manual surface, but bare `println`
                   never reached it.  Single-arg only; the
                   `print_like_bcheck` arity diagnostic still applies
                   to other shapes. *)
                let print_name =
                  match path with [n] -> n | _ -> "" in
                let display_dispatch_ok =
                  (print_name = "print" || print_name = "println")
                  && (match targs with
                      | [ a ] ->
                          (match a.ty with
                           | TStruct p | TEnum p ->
                               List.mem ("Display", p)
                                 !trait_impl_table
                           | _ -> false)
                      | _ -> false)
                in
                if display_dispatch_ok then begin
                  let user_arg = List.hd args in
                  let n = !display_dispatch_gensym in
                  incr display_dispatch_gensym;
                  let sb_name = Printf.sprintf "__disp_sb_%d" n in
                  let s_name = Printf.sprintf "__disp_s_%d" n in
                  let p = call_pos in
                  let sb_ann =
                    Ast.TyStruct { path = ["StringBuilder"];
                                   args = [] } in
                  let string_ann =
                    Ast.TyStruct { path = ["String"]; args = [] } in
                  let u32_ann =
                    Ast.TyInt { signed = false; width = Ast.W32 } in
                  let make_alloc =
                    Ast.Call { callee = ["default_allocator"];
                               args = []; pos = p } in
                  let cap_arg =
                    Ast.Cast (Ast.IntLit (32, p), u32_ann, p) in
                  let sb_init =
                    Ast.Call {
                      callee = ["StringBuilder"; "with_capacity"];
                      args = [ make_alloc; cap_arg ];
                      pos = p } in
                  let fmt_call =
                    Ast.MethodCall {
                      receiver = user_arg; name = "fmt";
                      args = [ Ast.Ref (Ast.Var (sb_name, p), p) ];
                      pos = p } in
                  let build_call =
                    Ast.Call {
                      callee = ["String"; "build"];
                      args = [ Ast.Var (sb_name, p) ];
                      pos = p } in
                  let as_str_call =
                    Ast.MethodCall {
                      receiver = Ast.Var (s_name, p);
                      name = "as_str"; args = []; pos = p } in
                  let inner_print =
                    Ast.Call { callee = [print_name];
                               args = [ as_str_call ];
                               pos = p } in
                  let free_call =
                    Ast.MethodCall {
                      receiver = Ast.Var (s_name, p);
                      name = "free"; args = []; pos = p } in
                  let block_stmts = [
                    Ast.Let { name = sb_name; value = sb_init;
                              ty_ann = Some sb_ann; is_mut = true;
                              pos = p };
                    Ast.ExprStmt fmt_call;
                    Ast.Let { name = s_name; value = build_call;
                              ty_ann = Some string_ann;
                              is_mut = false; pos = p };
                    Ast.ExprStmt inner_print;
                    Ast.ExprStmt free_call;
                    Ast.Tail (Ast.IntLit (0, p));
                  ] in
                  elab_expr ~allow_void ?expected ctx env
                    (Ast.Block (block_stmts, p))
                end else
                let result_ty =
                  b.bcheck ~ctx ~pos:call_pos ~args:targs ~allow_void
                in
                let name =
                  match path with
                  | [n] -> n
                  | _ ->
                      failwith ("internal: builtin call resolved to \
                                 multi-segment path " ^ String.concat "::" path
                                ^ " — lookup_builtin should only match \
                                   single-segment names")
                in
                { e = TBuiltinCall { name; args = targs };
                  ty = result_ty; pos }
            | None ->
                (* Indirect call through a fn-ptr value (local / extern
                   const / extern var).  Single-segment path only. *)
                let fnptr_local =
                  match path with
                  | [n] ->
                      (match List.assoc_opt n env with
                       | Some (TFnPtr { params; ret }) ->
                           Some (n, params, ret)
                       | _ ->
                           (match List.assoc_opt n ctx.ext_consts with
                            | Some (TFnPtr { params; ret }) ->
                                Some (n, params, ret)
                            | _ ->
                                (match List.assoc_opt n ctx.ext_vars with
                                 | Some (TFnPtr { params; ret }) ->
                                     Some (n, params, ret)
                                 | _ -> None)))
                  | _ -> None
                in
                (match fnptr_local with
                 | Some (n, params, ret) ->
                     let (result_ty, targs) =
                       check_call_args ~pos:call_pos
                         ~kind:"function pointer" ~name:n
                         ~param_tys:params ~raw_args:args ~targs
                         ~ret_ty:ret ~allow_void ()
                     in
                     { e = TCall { mangled = n; args = targs };
                       ty = result_ty; pos }
                 | None ->
                     let display = String.concat "::" path in
                     (match lookup_fn ctx path with
                      | None ->
                          Error.failf call_pos "unknown function '%s'" display
                      | Some (resolved_mod,
                              ({ fn_pub; fn_variadic; _ } as skel)) ->
                          let (mangled, param_tys, ret_ty) =
                            resolve_call_dispatch ~pos:call_pos ~expected ctx
                              ~resolved_mod ~arg_tys skel
                          in
                          (* Qualified call: each module segment must be
                             visible from the current scope.  Walks the
                             resolved fn's module path (where we actually
                             found the fn). *)
                          (match path with
                           | [_] -> ()
                           | _ ->
                               let rec walk_segments parent = function
                                 | [] -> ()
                                 | seg :: rest ->
                                     let mod_path = parent @ [seg] in
                                     let pub =
                                       match List.assoc_opt mod_path
                                               ctx.modules with
                                       | Some b -> b
                                       | None ->
                                           Error.failf call_pos
                                             "unknown module '%s'"
                                             (String.concat "::" mod_path)
                                     in
                                     if (not pub)
                                        && not (is_prefix parent ctx.scope)
                                     then
                                       Error.failf call_pos
                                         "module '%s' is private (not \
                                          visible from '%s')"
                                         (String.concat "::" mod_path)
                                         (if ctx.scope = [] then "<root>"
                                          else String.concat "::" ctx.scope);
                                     walk_segments mod_path rest
                               in
                               walk_segments [] resolved_mod;
                               if (not fn_pub) && ctx.scope <> resolved_mod
                               then
                                 Error.failf call_pos
                                   "function '%s' is private to module '%s'"
                                   display
                                   (String.concat "::" resolved_mod));
                          let (result_ty, targs) =
                            check_call_args ~pos:call_pos
                              ~kind:"function" ~name:display
                              ~variadic:fn_variadic ~param_tys
                              ~raw_args:args ~targs ~ret_ty ~allow_void ()
                          in
                          { e = TCall { mangled; args = targs };
                            ty = result_ty; pos })))))
  | Ast.MethodCall { receiver; name; args; pos = mc_pos } ->
      let trecv = elab_expr ctx env receiver in
      (* Built-in `eq` / `ne` on primitive receivers (int-like / bool / str
         / ptr).  Lets `@derive(Eq)` recurse uniformly through primitive
         fields (`self.x.eq(other.x)`) without an `impl Eq for int`.  Maps
         to `==` / `!=`.  Structs / enums fall through to their real impl
         method (a derived or hand-written `Foo__eq`). *)
      let is_primitive =
        match trecv.ty with
        | TInt _ | TCInt _ | TCShort _ | TCLong _ | TCChar | TCSChar
        | TCUChar | TBool | TString | TExtAlias _
        | TPtr _ | TOwnPtr _ | TConstPtr _ | TNullPtr -> true
        | _ -> false
      in
      (match is_primitive, name, args with
       | true, ("eq" | "ne"), [ arg ] ->
           let targ = elab_expr ctx env arg in
           if not (typ_eq trecv.ty targ.ty) then
             Error.failf mc_pos
               "'.%s' on %s expects a %s argument, got %s"
               name (typ_name trecv.ty) (typ_name trecv.ty) (typ_name targ.ty);
           (* DR-007: `str.eq(other)` / `str.ne(other)` must content-
              compare so `HashMap<str, _>` lookups hit the same slot
              for equal-content keys.  Dispatch to the prelude
              `str::eq` (same path the `==` / `!=` operator takes).
              Other primitives use the direct C comparison. *)
           if typ_eq trecv.ty TString then
             let call =
               { e = TCall { mangled = "str__eq"; args = [trecv; targ] };
                 ty = TBool; pos }
             in
             if name = "eq" then call
             else { e = TNot call; ty = TBool; pos }
           else
             let op = if name = "eq" then Ast.EqEq else Ast.NotEq in
             { e = TBinOp (op, trecv, targ); ty = TBool; pos }
       | true, "hash", [] ->
           (* Built-in `hash` on primitives.  Integer / bool / c-int
              widths reinterpret as `u32` (cheap C cast) and let
              `@derive(Hash)` fold them; `str` dispatches to the
              prelude `str::hash` content hash (DR-007 prereq for
              `HashMap<str, _>`). *)
           (match trecv.ty with
            | TInt _ | TCInt _ | TCShort _ | TCLong _ | TCChar | TCSChar
            | TCUChar | TBool | TExtAlias _ ->
                let u32_ann = Ast.TyInt { signed = false; width = Ast.W32 } in
                { e = TCast (trecv, u32_ann);
                  ty = TInt { signed = false; width = Ast.W32 }; pos }
            | TString ->
                { e = TCall { mangled = "str__hash"; args = [trecv] };
                  ty = TInt { signed = false; width = Ast.W32 }; pos }
            | _ ->
                Error.failf mc_pos
                  "`hash` is not built-in for %s (pointer content \
                   hashing is not supported yet)" (typ_name trecv.ty))
       | true, "clone", [] ->
           (* Built-in `clone` on primitives → identity value-copy.
              Lets `@derive(Clone)` recurse through primitive fields
              uniformly (`self.x.clone()` on `int` returns `self.x`).
              Structs / enums fall through to their real `T__clone`. *)
           trecv
       | _ ->
      let struct_path =
        match trecv.ty with
        | TStruct p -> p
        | TPtr (TStruct p) -> p
        | TOwnPtr (TStruct p) -> p     (* DR-030: own *T dispatches methods
                                          like any pointer-to-struct; the
                                          auto-ref matrix below borrows the
                                          owner for `*self`/`*const self`. *)
        | TConstPtr (TStruct p) -> p   (* methods can mutate through self;
                                          MVP doesn't gate this — `&self`
                                          vs `&mut self` is future work *)
        | TEnum p | TPtr (TEnum p) | TOwnPtr (TEnum p)
        | TConstPtr (TEnum p) -> p
                                       (* methods on an enum (e.g. derived
                                          `Eq`); same lowering as structs *)
        | other ->
            Error.failf mc_pos
              "method call '.%s()' requires a struct or enum value (or a \
               pointer to one), got %s"
              name (typ_name other)
      in
      (* Methods of a generic struct are registered under the skeleton
         path (`Pair`), not the instance path (`Pair_i32_bool`).  Map the
         receiver instance back to its skeleton and remember its type
         args — those pin the impl's type parameters at dispatch. *)
      let (method_struct_path, recv_inst_args) =
        match Mono.find_struct ctx.instances struct_path with
        | Some { sinstance_args = Some inst_args; _ } when inst_args <> [] ->
            (match List.find_opt
                     (fun (s : struct_sig) ->
                        s.stparams <> []
                        && Mono.is_instance_of s.sname_path struct_path)
                     ctx.structs with
             | Some skel -> (skel.sname_path, inst_args)
             | None -> (struct_path, []))
        | _ -> (struct_path, [])
      in
      let mpath = method_struct_path @ [name] in
      let display = String.concat "::" mpath in
      let targs = List.map (elab_expr ctx env) args in
      let arg_tys = List.map (fun (a : texpr) -> a.ty) targs in
      let fnptr_field =
        match resolve_struct_by_path ctx struct_path with
        | Some s ->
            (match List.assoc_opt name s.sfields_ty with
             | Some (TFnPtr { params; ret }) -> Some (params, ret)
             | _ -> None)
        | None -> None
      in
      (match lookup_fn ctx mpath with
       | None ->
           (match fnptr_field with
            | Some (params, ret) ->
                let display = String.concat "::" struct_path ^ "." ^ name in
                let (result_ty, targs) =
                  check_call_args ~pos:mc_pos
                    ~kind:"fn-pointer field" ~name:display
                    ~param_tys:params ~raw_args:args ~targs
                    ~ret_ty:ret ~allow_void ()
                in
                let field_ty =
                  match resolve_struct_by_path ctx struct_path with
                  | Some s ->
                      (match List.assoc_opt name s.sfields_ty with
                       | Some t -> t
                       | None ->
                           failwith ("internal: struct '"
                                     ^ String.concat "::" struct_path
                                     ^ "' lost field '" ^ name
                                     ^ "' between fnptr_field probe and \
                                        re-lookup"))
                  | None ->
                      failwith ("internal: struct '"
                                ^ String.concat "::" struct_path
                                ^ "' vanished from index after fnptr_field \
                                   probe succeeded")
                in
                let fn_expr = {
                  e = TFieldAccess { target = trecv; field = name };
                  ty = field_ty; pos = trecv.pos;
                } in
                { e = TIndirectCall { fn_expr; args = targs };
                  ty = result_ty; pos }
            | None ->
                (* DR-019 FieldAccess call-desugar — `recv.f(args)` where
                   `recv` has a field `f` whose type implements a Fn-trait
                   (Fn1 / Fn2 / ...) rewrites to `(recv.f).call(args)`.
                   Extends DR-018's single-Var call-desugar to FieldAccess
                   heads — the common `self.f(v)` shape inside an adapter
                   method body.  Detection is via `trait_impl_table`
                   (concrete struct/enum field type) or `ctx.tbounds`
                   (tparam-typed field via generic adapter). *)
                let is_fn_trait_name t =
                  String.length t >= 2 && String.sub t 0 2 = "Fn"
                in
                let field_carries_fn_impl =
                  match resolve_struct_by_path ctx struct_path with
                  | None -> false
                  | Some s ->
                      (match List.assoc_opt name s.sfields_ty with
                       | Some (TStruct p | TEnum p) ->
                           List.exists
                             (fun (t, target) ->
                               target = p && is_fn_trait_name t)
                             !trait_impl_table
                       | Some (TVar tp) ->
                           List.exists
                             (fun (q, trait_path, _assocs) ->
                               q = tp
                               && (match List.rev trait_path with
                                   | last :: _ -> is_fn_trait_name last
                                   | [] -> false))
                             ctx.tbounds
                       | _ -> false)
                in
                if field_carries_fn_impl then
                  elab_expr ~allow_void ?expected ctx env
                    (Ast.MethodCall {
                       receiver =
                         Ast.FieldAccess (receiver, name, mc_pos);
                       name = "call"; args; pos = mc_pos })
                else
                  Error.failf mc_pos "no method '%s' on type '%s'"
                    name (String.concat "::" struct_path))
       | Some (resolved_mod, ({ fn_pub; _ } as skel)) ->
           let rec walk_segments parent = function
             | [] -> ()
             | seg :: rest ->
                 let mod_path = parent @ [seg] in
                 (match List.assoc_opt mod_path ctx.modules with
                  | Some pub ->
                      if (not pub) && not (is_prefix parent ctx.scope) then
                        Error.failf mc_pos
                          "type '%s' is private (not visible from '%s')"
                          (String.concat "::" mod_path)
                          (if ctx.scope = [] then "<root>"
                           else String.concat "::" ctx.scope)
                  | None -> ());
                 walk_segments mod_path rest
           in
           walk_segments [] resolved_mod;
           if (not fn_pub) && ctx.scope <> resolved_mod then
             Error.failf mc_pos "method '%s' is private to '%s'"
               name (String.concat "::" resolved_mod);
           let arg_tys_for_dispatch = trecv.ty :: arg_tys in
           let (mangled, inst_param_tys, ret_ty) =
             resolve_call_dispatch ~pos:mc_pos ~expected ~recv_inst_args ctx
               ~resolved_mod ~arg_tys:arg_tys_for_dispatch skel
           in
           (match inst_param_tys with
            | self_ty :: rest_params ->
                (* Auto-ref regular args at `*const T` slots so
                   `a.eq(b)` works for an Eq trait that now borrows
                   `other: *const Self` — same ergonomic the receiver
                   already gets.  Only `value T → *const T` is upgraded;
                   other shapes flow through the regular type check. *)
                let targs =
                  let n = List.length rest_params in
                  let r = List.length targs in
                  if r = n then
                    List.map2 (fun (te : texpr) pt ->
                      match pt, te.ty with
                      | TConstPtr inner, ty
                        when (not (typ_eq ty pt))
                          && typ_eq inner ty ->
                          { e = TRef te; ty = pt; pos = te.pos }
                      | _ -> te)
                      targs rest_params
                  else targs
                in
                let (result_ty, targs) =
                  check_call_args ~pos:mc_pos
                    ~kind:"method" ~name:display
                    ~param_tys:rest_params ~raw_args:args ~targs
                    ~ret_ty ~allow_void ()
                in
                (* DR — receiver-mutability (design 2026-05-28).
                   `*self` (mutable receiver) requires either a mut
                   pointer or a mut place; reject `*const T` receivers
                   and immutable bindings up-front so an inadvertent
                   `let c = ...; c.bump()` doesn't silently mutate
                   through the auto-ref.  `*const self` keeps the
                   permissive "callable on everything" semantics —
                   the auto-ref/deref matrix below handles the
                   shape coercion. *)
                (match self_ty with
                 | TPtr _ ->
                     (match trecv.ty with
                      | TConstPtr _ ->
                          Error.failf mc_pos
                            "method '%s' takes a mutable receiver \
                             (`*self`) but receiver is %s (read-only) \
                             — call a `*const self` method, or pass a \
                             `*T` to the value"
                            display (typ_name trecv.ty)
                      | TPtr _ -> ()
                      | _ ->
                          let mut_ok = match !is_mut_lvalue_hook with
                            | Some h -> h trecv
                            | None -> false
                          in
                          if not mut_ok then
                            Error.failf mc_pos
                              "method '%s' takes a mutable receiver \
                               (`*self`); the call expression is not a \
                               mutable place — declare the binding \
                               `let mut` (or mark the parameter `mut`), \
                               or use a `*const self` method if no \
                               mutation is needed"
                              display)
                 | _ -> ());
                (* Auto-ref / auto-deref: align receiver shape with the
                   method's self-param shape. *)
                let recv_is_ptr =
                  match trecv.ty with
                  | TPtr _ | TOwnPtr _ | TConstPtr _ -> true | _ -> false
                in
                let self_is_ptr =
                  match self_ty with
                  | TPtr _ | TOwnPtr _ | TConstPtr _ -> true | _ -> false
                in
                let trecv_adj =
                  match self_is_ptr, recv_is_ptr with
                  | true, true ->
                      (* DR-030 own-borrow: an `own *T` receiver calling a
                         `*self` / `*const self` method borrows — it does
                         NOT move ownership; see [loan_restamp]. *)
                      loan_restamp ~slot_ty:self_ty trecv
                  | false, false -> trecv
                  | true, false ->
                      { e = TRef trecv; ty = self_ty; pos = trecv.pos }
                  | false, true ->
                      { e = TDeref trecv; ty = self_ty; pos = trecv.pos }
                in
                { e = TCall { mangled; args = trecv_adj :: targs };
                  ty = result_ty; pos }
            | [] ->
                failwith ("internal: method '" ^ display
                          ^ "' has empty inst_param_tys — typecheck should \
                             have rejected the impl without a self param"))))
  | Ast.FieldAccess (target, fname, fa_pos) ->
      (* `.field` auto-derefs one level of pointer-to-struct.  Works
         for ordinary structs (consulting struct_index) and for exposed
         extern structs (consulting ext_struct_fields). *)
      let target' = elab_expr ctx env target in
      let field_ty =
        match target'.ty with
        | TStruct p | TPtr (TStruct p) | TOwnPtr (TStruct p)
        | TConstPtr (TStruct p) ->
            let s = match resolve_struct_by_path ctx p with
              | Some s -> s
              | None -> Error.failf fa_pos "unknown struct '%s'"
                          (String.concat "::" p)
            in
            (match List.assoc_opt fname s.sfields_ty with
             | Some t -> t
             | None ->
                 Error.failf fa_pos "struct '%s' has no field '%s'"
                   (String.concat "::" p) fname)
        | TExtStruct n | TPtr (TExtStruct n) | TConstPtr (TExtStruct n) ->
            (match List.assoc_opt n ctx.ext_struct_fields with
             | None ->
                 Error.failf fa_pos
                   "field access '.%s' on opaque type '%s' — declare \
                    fields with `extern struct %s { ... }` to access them"
                   fname n n
             | Some fs ->
                 (match List.assoc_opt fname fs with
                  | Some t -> t
                  | None ->
                      Error.failf fa_pos "extern struct '%s' has no field '%s'"
                        n fname))
        | other ->
            Error.failf fa_pos
              "field access '.%s' requires a struct value or pointer to \
               struct, got %s"
              fname (typ_name other)
      in
      { e = TFieldAccess { target = target'; field = fname };
        ty = field_ty; pos }
  | Ast.Block (stmts, block_pos) ->
      (* `{ s1; s2; [trailing] }` — multi-stmt block expression.  Only
         legal inside a function body (parse_arm_body emits it today);
         requires the per-body `walk_stmts_hook` to be installed so we
         can route let/let-tuple/assign/... through the function's
         decl table.  Trailing `Tail e` produces the block's value;
         without one the block is void (only valid when allow_void). *)
      let walk =
        match !walk_stmts_hook with
        | Some w -> w
        | None ->
            Error.failf block_pos
              "internal: block expression outside a function body"
      in
      let (init, trailing_ast) =
        match List.rev stmts with
        | Ast.Tail e :: rest -> (List.rev rest, Some e)
        | _ -> (stmts, None)
      in
      let (env', tstmts) = walk env init in
      (match trailing_ast with
       | Some e ->
           let ttrailing = elab_expr ?expected ~allow_void ctx env' e in
           { e = TBlock { stmts = tstmts; trailing = Some ttrailing };
             ty = ttrailing.ty; pos = block_pos }
       | None ->
           if not allow_void then
             Error.failf block_pos
               "block expression `{ ... }` must end with a trailing \
                value expression (no `;` after the last expression) \
                when used in a value position";
           { e = TBlock { stmts = tstmts; trailing = None };
             ty = t_i32; pos = block_pos })
                                              (* placeholder ty; never
                                                 read in void position *)

(* Single-walk variant of the old `collect_lets`: it both validates the
   body (mirroring the per-stmt type checks that lived there) and produces
   the elaborated `tstmt list`, alongside the hoisted let-decl list
   that the function-top declarations need.  Replaces `collect_lets` —
   `check_program` calls this once per function. *)
let elab_body ?(ret_ty : typ option = None) ?(is_main = false)
    ?(mut_params = []) ctx param_env stmts
    : (string * typ) list * tstmt list =
  let decls = ref [] in
  (* Loop nesting depth — `break` / `continue` are legal only when > 0.
     Bumped around `while` / `for` / `loop` bodies as they are walked. *)
  let loop_depth = ref 0 in
  (* Per-binding mutability.  Names are unique across a function body
     (add_decl rejects shadowing/redeclaration and params are unique), so
     one flat set keyed by name is correct without scoping concerns.
     Seeded with `mut`-marked parameters; `let mut` bindings register as
     they are walked.  Read only at assignment sites. *)
  let mut_names = Hashtbl.create 16 in
  List.iter (fun n -> Hashtbl.replace mut_names n ()) mut_params;
  (* Root local of an l-value path that touches an *owned value* (not a
     pointee): `x` / `x.f` / `x.f.g` -> Some "x"; anything crossing a
     `*` deref, or rooted in a non-variable, -> None (pointee mutability
     is a separate, deferred axis — see `AssignDeref`, which is never
     gated). *)
  let rec root_local : Ast.expr -> string option = function
    | Ast.Var (n, _) -> Some n
    | Ast.FieldAccess (t, _, _) -> root_local t
    | Ast.Index { base; _ } -> root_local base
    | _ -> None
  in
  let require_mut name pos ~what =
    if not (Hashtbl.mem mut_names name) then
      Error.failf pos
        "cannot %s immutable '%s' — declare it with `let mut`%s"
        what name
        (if List.mem_assoc name param_env
         then " (or mark the parameter `mut`)" else "")
  in
  let add_decl name t pos =
    check_c_ident pos "variable" name;
    if List.mem_assoc name param_env then
      Error.failf pos "variable '%s' shadows a parameter" name;
    if List.mem_assoc name !decls then
      Error.failf pos "variable '%s' already declared in this function" name;
    decls := (name, t) :: !decls
  in
  let rec walk env = function
    | [] -> (env, [])
    | s :: rest ->
        let (env', ts) = walk_stmt env s in
        let (final_env, rest_ts) = walk env' rest in
        (final_env, ts :: rest_ts)
  and walk_stmt env stmt : (string * typ) list * tstmt =
    match stmt with
    | Ast.Let { name; value; ty_ann; is_mut; pos } ->
        (* When the let has a type annotation, resolve it first so we
           can pass it as the expected type to elab_expr — that lets
           generic ctors like `Result::Ok(n)` infer all their tparams
           even when the payload alone doesn't determine them. *)
        let expected =
          match ty_ann with
          | Some ann -> Some (resolve_type_ann ~pos ctx ann)
          | None -> None
        in
        Option.iter
          (forbid_naked_opaque ~exposed:(List.map fst ctx.ext_struct_fields) pos)
          expected;
        let tvalue = elab_expr ?expected ctx env value in
        let t_inferred = tvalue.ty in
        let t_actual =
          match expected with
          | None ->
              (match t_inferred with
               | TNullPtr ->
                   Error.failf pos
                     "cannot infer pointer type for 'null'; add a type \
                      annotation like 'let %s: *T = null;'" name
               | _ -> t_inferred)
          | Some t_ann ->
              (match expr_int_lit value, t_ann with
               | Some n, TInt _ when int_fits n t_ann -> t_ann
               | Some n, TInt { signed = false; _ } when n < 0 ->
                   Error.failf pos
                     "negative literal %d cannot fit in %s" n (typ_name t_ann)
               | Some n, TInt _ ->
                   Error.failf pos
                     "literal %d does not fit in %s" n (typ_name t_ann)
               | _ ->
                   if coercible_to ~from:t_inferred ~to_:t_ann then t_ann
                   else
                     Error.failf pos
                       "variable '%s' declared as %s but initializer has type %s"
                       name (typ_name t_ann) (typ_name t_inferred))
        in
        (* DR-055: an owning binding cannot start as `null`.  An `own
           *T` must own an allocation; a null-initialized owner owns
           nothing, and (because the value type is TNullPtr, not
           TOwnPtr) the drop pass never tracks it — a later reassignment
           from `new(a)` then leaks at scope exit.  Reject here, aligned
           with the L1 rule that an owner of unknown provenance is an
           error, never a silent leak.  A `null` terminator belongs in a
           field (the tail of a list), not in an owning binding. *)
        (match t_actual, tvalue.e with
         | TOwnPtr _, TNullLit ->
             Error.failf pos
               "owning binding '%s' cannot start as `null` — an `own *T` \
                must own an allocation; initialize it from `new(a) ...` \
                (use a `Nil` enum variant, or a sentinel node, for an \
                empty list)" name
         | _ -> ());
        (* DR-052: `let b: *const T = q` with `q: own *T` is a loan —
           q stays the owner (and keeps its auto-drop); b is a borrow.
           Without the restamp the move-pass saw an affine value-ty and
           consumed q, while the drop-pass replay did not — q was both
           "moved" and auto-dropped, and returning b was a silent UAF. *)
        let tvalue = loan_restamp ~slot_ty:t_actual tvalue in
        add_decl name t_actual pos;
        if is_mut then Hashtbl.replace mut_names name ();
        ((name, t_actual) :: env, TLet { name; value = tvalue; pos })
    | Ast.LetElse { pat; value; else_body; pos } ->
        (* FP-2 — `let <refutable-pat> = expr else { divergent };`.
           Desugar to TMatch wrapped in TLet / TLetTuple so the
           pattern's binds escape into the enclosing scope (bind-
           hoisting per design 2026-05-28).  MVP rules: pattern is a
           non-or PVariant whose binds are all single-segment PVar
           (no nested patterns), enclosing enum has ≥2 variants
           (else-branch otherwise unreachable), and the else-block
           diverges (return / break / continue, etc.). *)
        let (tname, variant_name, binds_ast) = match pat with
          | Ast.PVariant { tname; variant; binds; _ } ->
              (tname, variant, binds)
          | Ast.PWildcard _ | Ast.PVar _ ->
              Error.failf pos
                "let-else pattern must be refutable (a qualified \
                 variant constructor like `Option::Some(v)`); use a \
                 plain `let` for irrefutable bindings"
          | Ast.PLit _ | Ast.PBool _ ->
              Error.failf pos
                "let-else MVP does not support literal patterns; use \
                 a `match` or an `if`"
          | Ast.POr _ ->
              Error.failf pos
                "let-else MVP does not support or-patterns; spell out \
                 one variant or use a `match`"
        in
        let _ = tname in
        let bind_pairs = match binds_ast with
          | Ast.PBTuple ps ->
              List.mapi (fun i p ->
                let local = Printf.sprintf "__le_a%d" i in
                let field = Printf.sprintf "_%d" i in
                match p with
                | Ast.PVar (n, _) -> (field, n, local)
                | _ ->
                    Error.failf pos
                      "let-else MVP only supports flat name binds in \
                       the pattern — got a nested sub-pattern")
                ps
          | Ast.PBStruct fields ->
              List.map (fun (fname, p) ->
                let local = "__le_a_" ^ fname in
                match p with
                | Ast.PVar (n, _) -> (fname, n, local)
                | _ ->
                    Error.failf pos
                      "let-else MVP only supports flat name binds in \
                       the pattern — got a nested sub-pattern")
                fields
        in
        if bind_pairs = [] then
          Error.failf pos
            "let-else on a unit variant binds nothing — use a plain \
             `match` instead";
        let tvalue = elab_expr ctx env value in
        let enum_path = match tvalue.ty with
          | TEnum p -> p
          | other ->
              Error.failf pos
                "let-else scrutinee must be enum-typed, got %s"
                (typ_name other)
        in
        let esig =
          match resolve_enum_by_path ctx enum_path with
          | Some e -> e
          | None ->
              Error.failf pos "internal: enum '%s' not in scope"
                (String.concat "::" enum_path)
        in
        if List.length esig.evariants < 2 then
          Error.failf pos
            "let-else else-branch is unreachable: enum '%s' has only \
             one variant — use a plain `let` instead"
            (String.concat "::" enum_path);
        let (variant_tag, vsig) =
          let rec find i = function
            | [] ->
                Error.failf pos
                  "variant '%s' not found in enum '%s'"
                  variant_name (String.concat "::" enum_path)
            | (v : variant_sig) :: rest ->
                if v.vsname = variant_name then (i, v)
                else find (i + 1) rest
          in
          find 0 esig.evariants
        in
        let lookup_field_ty fname =
          match List.assoc_opt fname vsig.vsfields with
          | Some t -> t
          | None ->
              Error.failf pos
                "field '%s' not found in variant '%s::%s'"
                fname (String.concat "::" enum_path) variant_name
        in
        let bind_typed = List.map (fun (fname, bname, local) ->
            (fname, bname, local, lookup_field_ty fname))
            bind_pairs in
        List.iter (fun (_, n, _, _) ->
          if List.mem_assoc n env then
            Error.failf pos "duplicate name '%s' in let-else" n)
          bind_typed;
        List.iter (fun (_, n, _, t) -> add_decl n t pos) bind_typed;
        let env' = List.fold_left (fun env (_, n, _, t) -> (n, t) :: env)
                     env bind_typed in
        (* Walk the else body in the OUTER env (binds are not in scope
           inside else — the divergent branch never sees them). *)
        let (_, t_else_stmts) = walk env else_body in
        if not (Move.stmts_diverge t_else_stmts) then
          Error.failf pos
            "let-else else-block must diverge (return / break / \
             continue / never-returning fn)";
        (* Build the desugared TMatch.  Success arm extracts each bind
           into a local name, returns a tuple (or single value); else
           arm wraps the divergent stmts in a TBlock and is flagged
           tdiverges so the match's value type comes from the success
           arm alone. *)
        let success_binds = List.map (fun (fname, _, local, _) ->
            (fname, TPVar local))
            bind_typed in
        let success_tpat =
          TPVariant { variant = variant_name; tag = variant_tag;
                      binds = success_binds } in
        let success_body, result_ty =
          match bind_typed with
          | [(_, _, local, t)] ->
              ({ e = TVar local; ty = t; pos }, t)
          | _ ->
              let ts = List.map (fun (_, _, _, t) -> t) bind_typed in
              let es = List.map (fun (_, _, local, t) ->
                           { e = TVar local; ty = t; pos })
                         bind_typed in
              ({ e = TTupleLit es; ty = TTuple ts; pos }, TTuple ts)
        in
        let success_arm =
          { tpat = success_tpat; tguard = None;
            tbody = success_body;
            tdiverges = false; tarm_pos = pos } in
        let else_block_expr =
          { e = TBlock { stmts = t_else_stmts; trailing = None };
            ty = result_ty; pos } in
        let else_arm =
          { tpat = TPWildcard; tguard = None;
            tbody = else_block_expr;
            tdiverges = true; tarm_pos = pos } in
        let tmatch =
          { e = TMatch { scrutinee = tvalue; ename_path = enum_path;
                         arms = [success_arm; else_arm] };
            ty = result_ty; pos } in
        (match bind_typed with
         | [(_, bname, _, _)] ->
             (env', TLet { name = bname; value = tmatch; pos })
         | _ ->
             let names = List.map (fun (_, n, _, _) -> n) bind_typed in
             (env', TLetTuple { names; value = tmatch; pos }))
    | Ast.LetTuple { names; value; is_mut; pos } ->
        let tvalue = elab_expr ctx env value in
        let elem_tys =
          match tvalue.ty with
          | TTuple ts -> ts
          | other ->
              Error.failf pos
                "destructuring 'let (...)' expects a tuple value, got %s"
                (typ_name other)
        in
        let n_names = List.length names in
        let n_elems = List.length elem_tys in
        if n_names <> n_elems then
          Error.failf pos
            "destructuring 'let (...)' has %d names but value is a %d-tuple"
            n_names n_elems;
        let pairs = List.combine names elem_tys in
        (match find_dup ~key:fst pairs with
         | Some n -> Error.failf pos "duplicate name '%s' in 'let (...)'" n
         | None -> ());
        List.iter (fun (n, ty) -> add_decl n ty pos) pairs;
        if is_mut then
          List.iter (fun n -> Hashtbl.replace mut_names n ()) names;
        (List.rev_append pairs env,
         TLetTuple { names; value = tvalue; pos })
    | Ast.Assign { path; value; pos } ->
        let display = String.concat "::" path in
        let target_ty =
          match path with
          | [name] ->
              (match List.assoc_opt name env with
               | Some t -> Some t
               | None ->
                   (* Single-segment can also be an unqualified extern var
                      (e.g. when bare `DOSBase` is in scope through the
                      flat ext_vars list). *)
                   List.assoc_opt name ctx.ext_vars)
          | _ ->
              (* Qualified path: must resolve to an extern var.  Today
                 ext_vars is flat (last segment is the C symbol name);
                 require the last segment to match an entry. *)
              (match List.rev path with
               | last :: _ -> List.assoc_opt last ctx.ext_vars
               | [] -> None)
        in
        (match target_ty with
         | None ->
             let last_name = match List.rev path with
               | n :: _ -> n
               | [] -> ""
             in
             if List.mem_assoc last_name ctx.ext_consts then
               Error.failf pos
                 "cannot assign to '%s' — it's an `extern const` (use \
                  `extern var` for mutable globals)" display
             else if lookup_const ctx path <> None then
               Error.failf pos
                 "cannot assign to '%s' — it's a `const` (compile-time \
                  constant)" display
             else
               Error.failf pos "assignment to undefined variable '%s'" display
         | Some _ -> ());
        (* A local binding must be `mut` to be reassigned.  Qualified
           paths and unqualified extern vars are mutable globals — not
           gated. *)
        (match path with
         | [name] when List.mem_assoc name env ->
             require_mut name pos ~what:"assign to"
         | _ -> ());
        let tvalue = elab_expr ?expected:target_ty ctx env value in
        (* Re-audit 2026-06-12: assignment never type-checked its RHS —
           `b = 5` with `b: *const int` sailed through into invalid C.
           Same contract as the annotated-let site. *)
        (match target_ty with
         | Some tty ->
             if not (coercible_to ~from:tvalue.ty ~to_:tty)
                && not (int_lit_fits value tty) then
               Error.failf pos
                 "cannot assign %s to '%s' (declared as %s)"
                 (typ_name tvalue.ty) display (typ_name tty)
         | None -> ());
        let tvalue =
          match target_ty with
          | Some tty -> loan_restamp ~slot_ty:tty tvalue
          | None -> tvalue
        in
        (env, TAssign { path; value = tvalue; pos })
    | Ast.AssignField { target; field; value; pos } ->
        let ttarget = elab_expr ctx env target in
        (* Reject field-write through a `*const` pointer up-front — the
           pointee is read-only, even via field auto-deref. *)
        (match ttarget.ty with
         | TConstPtr _ ->
             Error.failf pos
               "cannot assign field '%s' through '*const' pointer %s \
                (pointee is read-only)" field (typ_name ttarget.ty)
         | _ -> ());
        let fty =
          match ttarget.ty with
          | TStruct p | TPtr (TStruct p) | TOwnPtr (TStruct p) ->
              let s = match resolve_struct_by_path ctx p with
                | Some s -> s
                | None ->
                    Error.failf pos "unknown struct '%s'"
                      (String.concat "::" p)
              in
              (match List.assoc_opt field s.sfields_ty with
               | Some t -> t
               | None ->
                   Error.failf pos "struct '%s' has no field '%s'"
                     (String.concat "::" p) field)
          | TExtStruct n | TPtr (TExtStruct n) ->
              (match List.assoc_opt n ctx.ext_struct_fields with
               | None ->
                   Error.failf pos
                     "assignment to '.%s' on opaque type '%s' — \
                      declare fields with `extern struct %s { ... }`"
                     field n n
               | Some fs ->
                   (match List.assoc_opt field fs with
                    | Some t -> t
                    | None ->
                        Error.failf pos
                          "extern struct '%s' has no field '%s'"
                          n field))
          | other ->
              Error.failf pos
                "assignment to field '.%s' requires a struct value or \
                 pointer to struct, got %s"
                field (typ_name other)
        in
        (* Mutating a field of an *owned value* (TStruct/TExtStruct, not a
           pointer) requires a mutable root binding.  Through a pointer it
           is pointee mutation — a separate, deferred axis, left ungated
           like AssignDeref. *)
        (match ttarget.ty with
         | TStruct _ | TExtStruct _ ->
             (match root_local target with
              | Some n when List.mem_assoc n env ->
                  require_mut n pos ~what:"mutate field of"
              | _ -> ())
         | _ -> ());
        let tvalue = elab_expr ctx env value in
        if not (coercible_to ~from:tvalue.ty ~to_:fty)
           && not (int_lit_fits value fty) then
          Error.failf pos
            "field '%s': expected %s, got %s"
            field (typ_name fty) (typ_name tvalue.ty);
        let tvalue = loan_restamp ~slot_ty:fty tvalue in
        (env, TAssignField { target = ttarget; field;
                                  value = tvalue; pos })
    | Ast.AssignIndex { base; index; value; pos } ->
        let tbase = elab_expr ctx env base in
        let through_ptr, elem_ty =
          match tbase.ty with
          | TArray { elem; _ } -> false, elem
          | TPtr elem | TOwnPtr elem ->
              (* Raw write-through-pointer `p[i] = v` (Delta B): the
                 element store routes to C `p[i]`.  Read-side index on
                 a bare `*T` stays rejected — reads go through `Slice`,
                 writes through this path.  DR-030 Faza-1a: `own *T`
                 also allows write-through-pointer indexing (codegen
                 erases the sigil). *)
              true, elem
          | TConstPtr _ ->
              Error.failf pos
                "cannot assign through '*const' pointer %s (pointee \
                 is read-only)" (typ_name tbase.ty)
          | other ->
              Error.failf pos
                "indexed assignment `a[i] = ...` requires an array \
                 or '*T' pointer, got %s" (typ_name other)
        in
        let tindex = elab_expr ctx env index in
        if not (is_int_like tindex.ty) then
          Error.failf pos
            "array index must be an integer, got %s" (typ_name tindex.ty);
        (* Writing an element of an owned array needs a mutable root
           binding.  Through a pointer it is pointee mutation — the
           same axis as `AssignDeref`, intentionally left ungated. *)
        if not through_ptr then
          (match root_local base with
           | Some n when List.mem_assoc n env ->
               require_mut n pos ~what:"assign into"
           | _ -> ());
        let tvalue = elab_expr ctx env value in
        if not (coercible_to ~from:tvalue.ty ~to_:elem_ty)
           && not (int_lit_fits value elem_ty) then
          Error.failf pos
            "array element: expected %s, got %s"
            (typ_name elem_ty) (typ_name tvalue.ty);
        let tvalue = loan_restamp ~slot_ty:elem_ty tvalue in
        (env, TAssignIndex { base = tbase; index = tindex;
                             value = tvalue; pos })
    | Ast.AssignDeref { target; value; pos } ->
        let ttarget = elab_expr ctx env target in
        let inner =
          match ttarget.ty with
          | TPtr t -> t
          | TOwnPtr t -> t
          | TConstPtr _ ->
              Error.failf pos
                "cannot assign through '*const' pointer %s (pointee is \
                 read-only)" (typ_name ttarget.ty)
          | other ->
              Error.failf pos
                "assignment through '*' requires a pointer, got %s"
                (typ_name other)
        in
        let tvalue = elab_expr ctx env value in
        if not (coercible_to ~from:tvalue.ty ~to_:inner)
           && not (int_lit_fits value inner) then
          Error.failf pos
            "deref assignment: expected %s, got %s"
            (typ_name inner) (typ_name tvalue.ty);
        let tvalue = loan_restamp ~slot_ty:inner tvalue in
        (env, TAssignDeref { target = ttarget; value = tvalue; pos })
    | Ast.Return (Some e, pos) ->
        let tvalue = elab_expr ?expected:ret_ty ctx env e in
        (* The bidirectional `expected` seeds inner inference (literal
           widths, generic params, tuple/struct shape) but does not
           constrain the result — the equality check enforces the
           contract at the source pos. *)
        (match ret_ty with
         | Some expected_ty ->
             if not (typ_eq tvalue.ty expected_ty) then
               Error.failf pos
                 "return: expected %s, got %s"
                 (typ_name expected_ty) (typ_name tvalue.ty)
         | None ->
             (* No declared return type = void fn (and `main` is given an
                effective `int` ret_ty earlier, so it never lands here).
                A void fn returns nothing — `return <expr>;` has no slot
                for the value. *)
             Error.failf pos
               "cannot return a value from a function with no return \
                type (declare `-> %s` if the value is intended)"
               (typ_name tvalue.ty));
        (env, TReturn { value = Some tvalue; pos })
    | Ast.Return (None, pos) ->
        (* Bare `return;`.  Legal as an early exit from a void fn, or
           from `main` (where it means exit code 0 — desugared to
           `return 0;` so the emitted `int main` stays valid under
           -pedantic).  In a value-returning fn it's an error. *)
        (match ret_ty with
         | None -> (env, TReturn { value = None; pos })
         | Some _ when is_main ->
             let zero = { e = TIntLit 0; ty = t_i32; pos } in
             (env, TReturn { value = Some zero; pos })
         | Some t ->
             Error.failf pos
               "`return` needs a value — function returns %s" (typ_name t))
    | Ast.ExprStmt e | Ast.Tail e ->
        (* `e;` discards the value; a trailing `e` (no `;`) inside a void
           branch is likewise a void statement (the function-body trailing
           value is peeled off before walking, so it never reaches here). *)
        elab_stmt_position env e
    | Ast.While { cond; body } ->
        let tcond = elab_expr ctx env cond in
        if not (typ_eq tcond.ty TBool) then
          Error.failf tcond.pos
            "'while' condition must be of type bool, got %s"
            (typ_name tcond.ty);
        incr loop_depth;
        let (_, tbody) = walk env body in
        decr loop_depth;
        (List.rev !decls @ env,
         TWhile { cond = tcond; body = tbody; post = [] })
    | Ast.Break bpos ->
        if !loop_depth = 0 then
          Error.failf bpos "'break' outside a loop";
        (env, TBreak bpos)
    | Ast.Continue cpos ->
        if !loop_depth = 0 then
          Error.failf cpos "'continue' outside a loop";
        (env, TContinue cpos)
    | Ast.For { var; range; body; pos } ->
        (* Gensym base so multiple sequential `for var in ...` blocks in one
           function don't collide on let-hoisting.  The user-facing `var`
           is renamed in the body before elaboration. *)
        let k = !for_gensym in
        incr for_gensym;
        (* A non-literal range is elaborated once and dispatched on its
           type: `Range`/`RangeInclusive` take the integer counter path;
           a type that `impl Iterator` takes the iterator (loop + next)
           path.  A literal `a..b` / `a..=b` always takes the counter fast
           path (no struct alloc). *)
        let pre_elab =
          match range with Ast.Range _ -> None | _ -> Some (elab_expr ctx env range)
        in
        let is_iterator =
          match pre_elab with
          | Some tr ->
              (match typ_head_path tr.ty with
               | Some p ->
                   (* Match generic-impl entries (`impl<T> Iterator for
                      VecIter<T>` registers `["VecIter"]`, but the
                      receiver here is a `["VecIter_i32"]` mono
                      instance). *)
                   List.exists
                     (fun (n, decl_path) ->
                       n = "Iterator" && Mono.is_instance_of decl_path p)
                     !trait_impl_table
               | None -> false)
          | None -> false
        in
        if is_iterator then begin
          (* `for x in iter` over an `impl Iterator` value.  Desugar to a
             mutable iterator temp + `loop { match it.next() { Some(x) =>
             body | None => break } }`.  The element type flows from
             `next()`'s `Option<…>` return (no assoc-type table needed). *)
          let trange = Option.get pre_elab in
          let it_var = Printf.sprintf "__it%d" k in
          let elem_var = Printf.sprintf "__fv%d" k in
          add_decl it_var trange.ty pos;
          Hashtbl.replace mut_names it_var ();
          let it_env = (it_var, trange.ty) :: env in
          let next_call =
            Ast.MethodCall { receiver = Ast.Var (it_var, pos); name = "next";
                             args = []; pos } in
          let some_pat =
            Ast.PVariant { tname = ["Option"]; variant = "Some";
                           binds = Ast.PBTuple [ Ast.PVar (elem_var, pos) ];
                           pos } in
          let none_pat =
            Ast.PVariant { tname = ["Option"]; variant = "None";
                           binds = Ast.PBTuple []; pos } in
          let renamed_body =
            List.map (subst_var_stmt ~from:var ~to_:elem_var) body in
          let some_arm =
            Ast.{ pat = some_pat; guard = None;
                  body = Ast.Block (renamed_body, pos); arm_pos = pos } in
          let none_arm =
            Ast.{ pat = none_pat; guard = None;
                  body = Ast.Block ([ Ast.Break pos ], pos); arm_pos = pos } in
          let match_ast =
            Ast.Match { scrutinee = next_call;
                        arms = [ some_arm; none_arm ]; pos } in
          incr loop_depth;
          let (_, match_tstmt) = elab_stmt_position it_env match_ast in
          decr loop_depth;
          (List.rev !decls @ env,
           TForEach { it_var; it_init = trange; body = [ match_tstmt ]; pos })
        end else begin
        let counter = Printf.sprintf "__fv%d" k in
        let end_var = Printf.sprintf "__fe%d" k in
        let (counter_ty, inclusive, tlo, thi, range_temp, hi_for_overflow) =
          match range, pre_elab with
          | Ast.Range { lo; hi; inclusive; _ }, _ ->
              let tlo = elab_expr ctx env lo in
              if not (is_int_like tlo.ty) then
                Error.failf pos
                  "'for' loop bound must be an integer, got %s"
                  (typ_name tlo.ty);
              let cty = tlo.ty in
              let thi = elab_expr ~expected:cty ctx env hi in
              if not (typ_eq thi.ty cty) then
                Error.failf pos
                  "'for' bounds disagree: lo is %s, hi is %s"
                  (typ_name cty) (typ_name thi.ty);
              (cty, inclusive, tlo, thi, None, Some hi)
          | _, Some trange ->
              let (cty, incl) =
                match trange.ty with
                | TStruct path when Mono.is_instance_of ["Range"] path ->
                    let s = match resolve_struct_by_path ctx path with
                      | Some s -> s
                      | None ->
                          Error.failf pos
                            "internal: `Range` instance %s not resolved"
                            (String.concat "::" path)
                    in
                    let lo_ty =
                      try List.assoc "lo" s.sfields_ty
                      with Not_found ->
                        Error.failf pos "internal: Range missing 'lo'"
                    in
                    (lo_ty, false)
                | TStruct path
                  when Mono.is_instance_of ["RangeInclusive"] path ->
                    let s = match resolve_struct_by_path ctx path with
                      | Some s -> s
                      | None ->
                          Error.failf pos
                            "internal: `RangeInclusive` instance %s not \
                             resolved" (String.concat "::" path)
                    in
                    let lo_ty =
                      try List.assoc "lo" s.sfields_ty
                      with Not_found ->
                        Error.failf pos "internal: RangeInclusive missing 'lo'"
                    in
                    (lo_ty, true)
                | other ->
                    Error.failf pos
                      "`for v in ...` needs a `..` / `..=` range, a \
                       `Range<T>` / `RangeInclusive<T>` value, or a type \
                       that `impl Iterator`, got %s"
                      (typ_name other)
              in
              if not (is_int_like cty) then
                Error.failf pos
                  "'for' counter must be an integer, got %s" (typ_name cty);
              let tmp = Printf.sprintf "__fr%d" k in
              add_decl tmp trange.ty pos;
              let tmp_var = { e = TVar tmp; ty = trange.ty; pos } in
              let tlo = { e = TFieldAccess { target = tmp_var; field = "lo" };
                          ty = cty; pos } in
              let thi = { e = TFieldAccess { target = tmp_var; field = "hi" };
                          ty = cty; pos } in
              (cty, incl, tlo, thi, Some (tmp, trange), None)
          | _, None -> assert false
        in
        (* Inclusive range at the type maximum overflows the counter step
           (`i + 1` wraps and the loop never ends).  Catch when hi is a
           literal at the type maximum — push the check to compile-time.
           Only meaningful for the literal-range fast path; value-range
           paths can't be statically inspected. *)
        (if inclusive then
           match hi_for_overflow with
           | Some hi_expr ->
               (match expr_int_lit hi_expr, counter_ty with
                | Some k, TInt { signed; width } ->
                    let bits = int_width_bits width in
                    let maxv =
                      if signed then (1 lsl (bits - 1)) - 1
                      else (1 lsl bits) - 1
                    in
                    if k = maxv then
                      Error.failf pos
                        "inclusive `for ... ..=%d` reaches the maximum of \
                         %s — `%s + 1` wraps and the loop never ends; \
                         widen the counter type" k (typ_name counter_ty) var
                | _ -> ())
           | None -> ());
        (* Register the gensym names in fn-level decls; mark counter mut. *)
        add_decl counter counter_ty pos;
        add_decl end_var counter_ty pos;
        Hashtbl.replace mut_names counter ();
        (* Walk the body with the renamed counter binding. *)
        let renamed_body = List.map (subst_var_stmt ~from:var ~to_:counter) body in
        let body_env = (counter, counter_ty) :: env in
        incr loop_depth;
        let (_, tbody) = walk body_env renamed_body in
        decr loop_depth;
        (List.rev !decls @ env,
         TFor { counter; end_var; range_temp; lo = tlo; hi = thi;
                inclusive; body = tbody; pos })
        end
    | Ast.Defer { body; pos } ->
        let (_, tbody) = walk env body in
        (env, TDefer { body = tbody; pos })
    | Ast.With { target; name; body; pos } ->
        (* DR-012 scoped projection.  Desugar to
             { let <internal> = &target; body[name -> internal]... }
           — a TBlock-shaped TExprStmt isolates the binding to the
           body block so referencing the name after the `with` is
           an out-of-scope error.  The internal gensym name lets
           multiple `with x in ...` blocks in the same fn each
           register their own decl without colliding on the fn-top
           decl list; `subst_var_stmt` rewrites the user-visible
           `x` inside the body to point at the gensym. *)
        let t_target = elab_expr ctx env target in
        (* `with` needs an lvalue — `&<rvalue>` is invalid in C and
           would silently take the address of a temporary that
           expires at the end of the expression.  Restrict to the
           shapes the field/index/deref machinery accepts. *)
        let is_lvalue (te : texpr) =
          match te.e with
          | TVar _ | TFieldAccess _ | TIndex _ | TDeref _ -> true
          | _ -> false
        in
        if not (is_lvalue t_target) then
          Error.failf pos
            "`with` target must be an lvalue — a local binding, a \
             field, an index, or a deref.  Got an expression that \
             produces a fresh value (e.g. a fn call returning by \
             value); bind it to a `let mut` first and `with` over \
             that.";
        (* If the target is reached through a const-ptr source
           (Slice's `.ptr` is `*const T`, indexing through it yields
           a `const T &` in C), the borrow must inherit read-only-ness
           or `cc` warns `-Wdiscarded-qualifiers`. *)
        let const_inherited =
          match t_target.e with
          | TIndex { base; _ } ->
              (match base.ty with
               | TStruct path
                 when Mono.is_instance_of ["Slice"] path -> true
               | _ -> false)
          | _ -> false
        in
        let ptr_ty =
          if const_inherited
          then TConstPtr t_target.ty
          else TPtr t_target.ty
        in
        let ref_e = { e = TRef t_target; ty = ptr_ty; pos } in
        let internal =
          let n = !with_gensym in
          incr with_gensym;
          Printf.sprintf "%s__with%d" name n
        in
        let body =
          List.map (subst_var_stmt ~from:name ~to_:internal) body in
        add_decl internal ptr_ty pos;
        let body_env = (internal, ptr_ty) :: env in
        let (_, t_body) = walk body_env body in
        let let_st = TLet { name = internal; value = ref_e; pos } in
        let block_expr =
          { e = TBlock { stmts = let_st :: t_body; trailing = None };
            ty = TBool; pos }
        in
        (env, TExprStmt block_expr)
  (* An expression used in statement (void) position.  `if` lowers to the
     void TIf statement (guard clause / side effects, `else` optional,
     branches are full blocks); everything else must carry a side effect.
     Returns the post-stmt env with the same hoisting behaviour as the
     old `If`/`While` cases (branch-local decls stay visible — function-
     scoped C hoisting). *)
  and elab_stmt_position env e =
    match e with
    | Ast.If { cond; then_blk; else_blk; _ } ->
        let tcond = elab_expr ctx env cond in
        let (_, t_then) = walk env then_blk in
        let (_, t_else) = walk env (Option.value ~default:[] else_blk) in
        (List.rev !decls @ env,
         TIf { cond = tcond; then_body = t_then; else_body = t_else })
    | _ ->
        let tvalue = elab_expr ~allow_void:true ctx env e in
        (* Only side-effecting expressions are meaningful when their value
           is discarded — a call, an effectful builtin, or a `match`.
           Anything else would emit `-Wunused-value`; reject it here.
           Escape hatch: bind with `let _x = ...`.  `TBlock` is treated
           as effectful unconditionally — the only block-producers we
           emit (multi-stmt match arm bodies, `Display`-dispatch
           desugar) are intrinsically sequences of effects with a
           trailing value that we already produced for typing. *)
        let has_effect =
          match tvalue.e with
          | TCall _ | TIndirectCall _ | TMatch _ | TBlock _ -> true
          | TBuiltinCall { name; _ } -> name <> "type_name"
          | _ -> false
        in
        if not has_effect then
          Error.failf tvalue.pos
            "expression statement has no effect — its result is \
             discarded; remove it, or bind it with `let _x = ...`";
        (env, TExprStmt tvalue)
  in
  (* Peel a trailing block value (`{ ...; e }` with no `;` on `e`) off the
     function body before walking.  In a fn with a *declared* return type it
     becomes the return value; a control-flow `if` tail (no `else`, or with
     statement branches) stays a statement and relies on the exhaustive-
     return check.  In a void fn — and in `main`, whose return type is
     implicit and whose exit code is set only by an explicit `return <int>;`
     — a trailing expression is a void statement. *)
  let body_stmts, tail =
    match List.rev stmts with
    | Ast.Tail e :: rest -> (List.rev rest, Some e)
    | _ -> (stmts, None)
  in
  (* Install the per-body walk_stmts hook so [elab_expr] can elaborate
     `Ast.Block` (multi-stmt match arm bodies) by routing their stmts
     through the same `walk` machinery that handles fn-body stmts (let/
     let-tuple register into [decls], etc.).  Cleared at function exit
     below so calls to `elab_expr` outside a body don't see stale
     state.  We DON'T use try/finally — any exception aborts compilation
     entirely so the leak is moot. *)
  let prev_hook = !walk_stmts_hook in
  walk_stmts_hook := Some walk;
  (* receiver-mutability hook: walks a typed receiver and decides
     whether it backs a mutable lvalue.  TVar checks `mut_names`;
     TFieldAccess / TIndex follow the chain — a step through a `*T`
     target is mutable (mut pointer's pointee is mut), `*const T` is
     not.  Used by MethodCall elab to gate `*self` calls. *)
  let rec is_mut_recv (te : texpr) =
    match te.e with
    | TVar n -> Hashtbl.mem mut_names n
    | TFieldAccess { target; _ } ->
        (match target.ty with
         | TPtr _ -> true
         | TConstPtr _ -> false
         | _ -> is_mut_recv target)
    | TIndex { base; _ } ->
        (match base.ty with
         | TPtr _ -> true
         | TConstPtr _ -> false
         | _ -> is_mut_recv base)
    | TDeref sub ->
        (match sub.ty with TPtr _ -> true | _ -> false)
    | _ -> false
  in
  let prev_mut_hook = !is_mut_lvalue_hook in
  is_mut_lvalue_hook := Some is_mut_recv;
  let (env_after, walked) = walk param_env body_stmts in
  let tail_tstmts =
    match tail with
    | None -> []
    | Some e ->
        (match ret_ty with
         | None ->
             (* void fn: trailing expr is a void statement. *)
             [ snd (elab_stmt_position env_after e) ]
         | Some _ when is_main ->
             (* `main`: trailing expr is a side-effecting statement, not an
                exit code (use explicit `return <int>;` for that). *)
             [ snd (elab_stmt_position env_after e) ]
         | Some t ->
             (match e with
              | Ast.If { then_blk; else_blk; _ }
                when not (is_value_if ~then_blk ~else_blk) ->
                  (* Control-flow `if` at the tail (both-branches-return,
                     or no else) — a statement, not a value. *)
                  [ snd (elab_stmt_position env_after e) ]
              | _ ->
                  let pos = Ast.expr_pos e in
                  let tvalue = elab_expr ?expected:(Some t) ctx env_after e in
                  if not (typ_eq tvalue.ty t) then
                    Error.failf pos
                      "trailing expression: expected %s, got %s"
                      (typ_name t) (typ_name tvalue.ty);
                  [ TReturn { value = Some tvalue; pos } ]))
  in
  let tstmts = walked @ tail_tstmts in
  ignore env_after;
  (* Post-elab: lift block-shaped sub-expressions (TStructLit, TTupleLit,
     TNew, TEnumLit, TMatch) that appear in positions where codegen
     emits via `gen_expr` (which can't handle them) into preceding
     `__lift_N` let-bindings.  Top-level value of let / return / assign
     stays block-shaped — codegen already handles it via
     `emit_value_into_temp`.  Match arm bodies are NOT walked: lifting
     a sub-expression of an arm body would hoist its evaluation above
     the match, breaking conditional semantics. *)
  let lift_counter = ref 0 in
  let fresh_lift () =
    let n = !lift_counter in
    incr lift_counter;
    Printf.sprintf "__lift_%d" n
  in
  let is_block (e : texpr) =
    match e.e with
    | TStructLit _ | TTupleLit _ | TNew _ | TNewEnum _ | TEnumLit _ | TMatch _
    | TIfExpr _ | TArrayLit _ | TArrayRepeat _ -> true
    | _ -> false
  in
  let rec walk_expr ~allow_top (te : texpr) : texpr * tstmt list =
    let walked, prelude = walk_subs te in
    if is_block walked && not allow_top then
      let n = fresh_lift () in
      (* A heap-init temp (`new(a) E::V(...)` straight into a
         `*const T` slot) carries the SLOT's const-qualified type
         after the borrow restamp, but its construction writes
         through the temp — declare it mutable (`T *`) and let the
         use site read it as const (always-legal direction in C).
         Freeze-audit B3: the const decl made codegen emit
         write-through-const, rejected at -ansi -pedantic. *)
      let decl_ty = match walked.e, walked.ty with
        | (TNew _ | TNewEnum _), TConstPtr inner -> TPtr inner
        | _ -> walked.ty
      in
      decls := (n, decl_ty) :: !decls;
      let lift_let = TLet { name = n; value = walked; pos = walked.pos } in
      ({ te with e = TVar n }, prelude @ [ lift_let ])
    else
      (walked, prelude)
  and walk_subs (te : texpr) : texpr * tstmt list =
    let map_args args =
      let pairs = List.map (walk_expr ~allow_top:false) args in
      (List.map fst pairs, List.concat_map snd pairs)
    in
    let map_fields fields =
      let pairs =
        List.map (fun (n, e) ->
          let (e', p) = walk_expr ~allow_top:false e in
          ((n, e'), p)) fields
      in
      (List.map fst pairs, List.concat_map snd pairs)
    in
    let map_opt = function
      | None -> (None, [])
      | Some e ->
          let (e', p) = walk_expr ~allow_top:false e in
          (Some e', p)
    in
    match te.e with
    | TIntLit _ | TFloatLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _
    | TFnRef _ | TSizeOf _ ->
        (te, [])
    | TNeg sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TNeg sub' }, p)
    | TBitNot sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TBitNot sub' }, p)
    | TNot sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TNot sub' }, p)
    | TBinOp (op, l, r) ->
        let (l', pl) = walk_expr ~allow_top:false l in
        let (r', pr) = walk_expr ~allow_top:false r in
        ({ te with e = TBinOp (op, l', r') }, pl @ pr)
    | TCall { mangled; args } ->
        let (args', p) = map_args args in
        ({ te with e = TCall { mangled; args = args' } }, p)
    | TBuiltinCall { name; args } ->
        let (args', p) = map_args args in
        ({ te with e = TBuiltinCall { name; args = args' } }, p)
    | TIndirectCall { fn_expr; args } ->
        let (fn_expr', pf) = walk_expr ~allow_top:false fn_expr in
        let (args', pa) = map_args args in
        ({ te with e = TIndirectCall { fn_expr = fn_expr'; args = args' } },
         pf @ pa)
    | TCast (sub, ann) ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TCast (sub', ann) }, p)
    | TFieldAccess { target; field } ->
        let (t', p) = walk_expr ~allow_top:false target in
        ({ te with e = TFieldAccess { target = t'; field } }, p)
    | TRef sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        (* GATE-4 rvalue-receiver lift: `&` of a non-lvalue (a call
           result, cast, arithmetic...) is invalid C — `&(ex_make())`
           — and shows up whenever a `*self`-shaped method auto-refs
           an rvalue receiver (`make().get()`).  Pin the value in a
           `__lift_N` temp and take the temp's address. *)
        let is_lvalue = match sub'.e with
          | TVar _ | TFieldAccess _ | TIndex _ | TDeref _ -> true
          | _ -> false
        in
        if is_lvalue then ({ te with e = TRef sub' }, p)
        else begin
          let n = fresh_lift () in
          decls := (n, sub'.ty) :: !decls;
          let lift_let = TLet { name = n; value = sub'; pos = sub'.pos } in
          ({ te with e = TRef { sub' with e = TVar n } }, p @ [ lift_let ])
        end
    | TDeref sub ->
        let (sub', p) = walk_expr ~allow_top:false sub in
        ({ te with e = TDeref sub' }, p)
    | TTupleLit es ->
        let (es', p) = map_args es in
        ({ te with e = TTupleLit es' }, p)
    | TStructLit { sname_path; fields; base } ->
        let (fields', pf) = map_fields fields in
        let (base', pb) = map_opt base in
        ({ te with e = TStructLit { sname_path; fields = fields';
                                    base = base' } }, pf @ pb)
    | TNew { sname_path; fields; base; alloc } ->
        let (fields', pf) = map_fields fields in
        let (base', pb) = map_opt base in
        let (alloc', pa) = map_opt alloc in
        ({ te with e = TNew { sname_path; fields = fields';
                              base = base'; alloc = alloc' } },
         pf @ pb @ pa)
    | TEnumLit { ename_path; variant; tag; args } ->
        let (args', p) = map_fields args in
        ({ te with e = TEnumLit { ename_path; variant; tag;
                                  args = args' } }, p)
    | TNewEnum { ename_path; variant; tag; args; alloc } ->
        let (args', p) = map_fields args in
        let (alloc', pa) = map_opt alloc in
        ({ te with e = TNewEnum { ename_path; variant; tag;
                                  args = args'; alloc = alloc' } },
         p @ pa)
    | TMatch { scrutinee; ename_path; arms } ->
        let (scr', p) = walk_expr ~allow_top:false scrutinee in
        (* Arm bodies lift too (GATE-4) — otherwise a `for` inside an
           arm block never expands and block-shaped sub-expressions
           never get temps.  Any prelude produced for a non-block arm
           body must only run when that arm matches, so it wraps into
           a TBlock around the body instead of escaping upward. *)
        let arms' =
          List.map (fun (a : tmatch_arm) ->
            let (body', pb) = walk_expr ~allow_top:true a.tbody in
            let tbody =
              if pb = [] then body'
              else { body' with
                     e = TBlock { stmts = pb; trailing = Some body' } }
            in
            { a with tbody })
            arms
        in
        ({ te with e = TMatch { scrutinee = scr'; ename_path;
                                arms = arms' } }, p)
    | TIfExpr { cond; then_val; else_val } ->
        (* Only the cond is lifted: hoisting a branch value above the `if`
           would evaluate it unconditionally (same rule as match arms). *)
        let (cond', p) = walk_expr ~allow_top:false cond in
        ({ te with e = TIfExpr { cond = cond'; then_val; else_val } }, p)
    | TArrayLit es ->
        let (es', p) = map_args es in
        ({ te with e = TArrayLit es' }, p)
    | TArrayRepeat { value; count } ->
        let (value', p) = walk_expr ~allow_top:false value in
        ({ te with e = TArrayRepeat { value = value'; count } }, p)
    | TIndex { base; index } ->
        let (base', pb) = walk_expr ~allow_top:false base in
        let (index', pi) = walk_expr ~allow_top:false index in
        ({ te with e = TIndex { base = base'; index = index' } }, pb @ pi)
    | TBlock { stmts; trailing } ->
        (* Arm-body block (GATE-4): the block's OWN stmt list is a
           legal statement context, so it lifts in place —
           TFor/TForEach expand to TWhile and block-shaped
           sub-expressions become local `__lift_N` temps INSIDE the
           block.  Conditional evaluation is preserved (the block only
           runs when its arm matches); nothing is hoisted above the
           match, which is why the arm used to be skipped entirely —
           skipping also left `for` in an arm body unexpanded and
           crashed codegen.  The trailing value's lift prelude joins
           the block's stmts; its top-level shape stays for
           emit_arm_result. *)
        let stmts' = lift_stmts stmts in
        (match trailing with
         | None ->
             ({ te with e = TBlock { stmts = stmts'; trailing = None } },
              [])
         | Some tr ->
             let (tr', p) = walk_expr ~allow_top:true tr in
             ({ te with
                e = TBlock { stmts = stmts' @ p; trailing = Some tr' } },
              []))
  and lift_stmts stmts = List.concat_map lift_stmt stmts
  and lift_stmt = function
    | TLet { name; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TLet { name; value = v'; pos } ]
    | TLetTuple { names; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TLetTuple { names; value = v'; pos } ]
    | TAssign { path; value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TAssign { path; value = v'; pos } ]
    | TAssignField { target; field; value; pos } ->
        let (t', pt) = walk_expr ~allow_top:false target in
        let (v', pv) = walk_expr ~allow_top:false value in
        pt @ pv @ [ TAssignField { target = t'; field; value = v'; pos } ]
    | TAssignDeref { target; value; pos } ->
        let (t', pt) = walk_expr ~allow_top:false target in
        let (v', pv) = walk_expr ~allow_top:false value in
        pt @ pv @ [ TAssignDeref { target = t'; value = v'; pos } ]
    | TAssignIndex { base; index; value; pos } ->
        let (b', pb) = walk_expr ~allow_top:false base in
        let (i', pi) = walk_expr ~allow_top:false index in
        let (v', pv) = walk_expr ~allow_top:false value in
        pb @ pi @ pv @ [ TAssignIndex { base = b'; index = i'; value = v'; pos } ]
    | TReturn { value = Some value; pos } ->
        let (v', p) = walk_expr ~allow_top:true value in
        p @ [ TReturn { value = Some v'; pos } ]
    | TReturn { value = None; _ } as s -> [ s ]
    | (TBreak _ | TContinue _) as s -> [ s ]
    | TExprStmt e ->
        (* Top-level Match in expr-stmt position is handled by
           emit_match_stmt; everything else must be simple. *)
        let allow_top = match e.e with TMatch _ -> true | _ -> false in
        let (e', p) = walk_expr ~allow_top e in
        p @ [ TExprStmt e' ]
    | TIf { cond; then_body; else_body } ->
        let (c', p) = walk_expr ~allow_top:false cond in
        p @ [ TIf { cond = c'; then_body = lift_stmts then_body;
                    else_body = lift_stmts else_body } ]
    | TWhile { cond; body; post } ->
        let (c', p) = walk_expr ~allow_top:false cond in
        (* Block-shaped cond (`while match st {...}`) — its lift
           prelude must re-run EVERY iteration, not once before the
           loop (hoisting froze the condition: freeze-audit B2,
           infinite loop).  Rewrite to `while (1) { <prelude>;
           if (!c) break; body }`; `continue` still hits `post`
           first and then re-enters at the prelude, so the
           re-evaluation order is preserved. *)
        if p = [] then
          [ TWhile { cond = c'; body = lift_stmts body;
                     post = lift_stmts post } ]
        else
          let pos = c'.pos in
          let true_cond = { e = TBoolLit true; ty = TBool; pos } in
          let break_if_done =
            TIf { cond = { e = TNot c'; ty = TBool; pos };
                  then_body = [ TBreak pos ];
                  else_body = [] }
          in
          [ TWhile { cond = true_cond;
                     body = p @ [ break_if_done ] @ lift_stmts body;
                     post = lift_stmts post } ]
    | TDefer { body; pos } ->
        [ TDefer { body = lift_stmts body; pos } ]
    | TFor { counter; end_var; range_temp; lo; hi; inclusive; body; pos } ->
        (* Expand the transient `TFor`.  If `range_temp` is Some, prepend a
           `TLet tmp = <range value>` so the `.lo` / `.hi` field reads in
           [lo]/[hi] have a binding.  Then pin the end bound to a
           `let __fe = hi` (evaluated once), initialise the counter with
           `let mut __fv = lo`, then a while-loop whose body is the user
           body followed by `__fv = __fv + 1`. *)
        let temp_let, ptemp =
          match range_temp with
          | None -> ([], [])
          | Some (tmp, trange) ->
              let (trange', pt) = walk_expr ~allow_top:true trange in
              ([ TLet { name = tmp; value = trange'; pos } ], pt)
        in
        let (lo', plo) = walk_expr ~allow_top:true lo in
        let (hi', phi) = walk_expr ~allow_top:true hi in
        let cty = lo'.ty in
        let cvar = { e = TVar counter; ty = cty; pos } in
        let evar = { e = TVar end_var; ty = cty; pos } in
        let one = { e = TIntLit 1; ty = cty; pos } in
        let cond_op = if inclusive then Ast.LtEq else Ast.Lt in
        let cond = { e = TBinOp (cond_op, cvar, evar); ty = TBool; pos } in
        let step_rhs =
          { e = TBinOp (Ast.Add, cvar, one); ty = cty; pos } in
        let step =
          TAssign { path = [counter]; value = step_rhs; pos } in
        (* The counter step goes in `post`, not at the end of the body —
           so `continue` runs it (C `for (; cond; step)` semantics) and
           doesn't spin forever. *)
        let body' = lift_stmts body in
        let let_end = TLet { name = end_var; value = hi'; pos } in
        let let_cnt = TLet { name = counter; value = lo'; pos } in
        let while_ = TWhile { cond; body = body'; post = [ step ] } in
        ptemp @ temp_let @ plo @ phi @ [ let_end; let_cnt; while_ ]
    | TForEach { it_var; it_init; body; pos } ->
        (* `for x in <iterator>` → bind the iterator once into a mutable
           temp, then `loop { match it.next() { Some(x) => body | None =>
           break } }`.  The match body already references [it_var] and
           carries the break, built typed during walk. *)
        let (init', pinit) = walk_expr ~allow_top:true it_init in
        let let_it = TLet { name = it_var; value = init'; pos } in
        let cond = { e = TBoolLit true; ty = TBool; pos } in
        let while_ =
          TWhile { cond; body = lift_stmts body; post = [] } in
        pinit @ [ let_it; while_ ]
  in
  let lifted = lift_stmts tstmts in
  walk_stmts_hook := prev_hook;
  is_mut_lvalue_hook := prev_mut_hook;
  (List.rev !decls, lifted)

(* Result of one walk over the program tree: every function (with its
   absolute module path and mangled C name), every struct, and every
   module (with its pub flag).  Order matches source order at each
   nesting level — preserves the user-visible declaration order in the
   emitted C. *)
type flat = {
  funcs : (string list * Ast.func * string) list;
  structs : (string list * Ast.struct_decl) list;
  ext_structs : Ast.extern_struct list;   (* always top-level, raw C names *)
  ext_types : Ast.extern_type list;       (* `extern type Foo;` aliases *)
  ext_consts : Ast.extern_const list;     (* `extern const NAME: T;` *)
  consts : (string list * Ast.const_decl) list;  (* `const NAME: T = e;`
                                                    with enclosing module
                                                    path *)
  ext_vars : Ast.extern_var list;         (* `extern var NAME: T;` —
                                             mutable global, l-value *)
  enums : (string list * Ast.enum_decl) list;
  modules : (string list * bool) list;
  c_includes : string list;               (* `@c_include("...")` paths *)
  (* Each `impl` block keeps its enclosing module path; target struct
     resolution (relative-to-scope, ancestor walk-up) happens later
     once the struct index is built. *)
  impls : (string list * Ast.impl_block) list;
  (* `trait` declarations with their enclosing module path. *)
  traits : (string list * Ast.trait_decl) list;
  (* `pub use foo::bar;` re-exports.  Each entry is
     (scope, local_name, target_relative_path, decl_pos).  Lookup at
     `scope` for `local_name` redirects to `target_relative_path`
     resolved against the same scope; `decl_pos` points at the `pub
     use` statement and is used by `validate_aliases` to anchor
     unresolved-target errors at the decl site rather than at first
     usage.  Multi-segment local_name not supported (the parser only
     emits single-segment Use names today). *)
  aliases : (string list * string * string list * Pos.t) list;
  (* `type Name<T...> = Type;` user-defined aliases with their
     enclosing module path.  Resolved by `resolve_type_ann_raw`
     before struct/enum lookup — alias hits substitute targs into
     `tatarget` and recurse.  Cycle-guard prevents infinite
     `type A = B; type B = A;` loops. *)
  type_aliases : (string list * Ast.type_alias_decl) list;
}

let flatten_items program =
  let funcs = ref [] in
  let structs = ref [] in
  let ext_structs = ref [] in
  let ext_types = ref [] in
  let ext_consts = ref [] in
  let consts = ref [] in
  let ext_vars = ref [] in
  let enums = ref [] in
  let modules = ref [] in
  let impls = ref [] in
  let traits = ref [] in
  let type_aliases = ref [] in
  let c_includes = ref [] in
  (* Uniform "must be at top level" reject — `extern struct/type/const`
     and `@c_include` all share the same constraint with the same
     wording.  Path captured by walk and threaded in. *)
  (* `mod raw` is the user-facing FFI quarantine; `mod sys` is the
     compiler-shipped target seam (DR-006) — its extern fns are
     hand-vetted, target-portable, and don't need the raw-module
     hygiene gate. *)
  let in_raw_module path = match List.rev path with
    | "raw" :: _ -> true
    | "sys" :: _ -> true
    | _ -> false
  in
  let require_raw ~current_path ~kind ~name ~pos =
    if not (in_raw_module current_path) then
      Error.failf pos
        "'extern %s %s' must live inside a `mod raw { ... }` block \
         (FFI hygiene rule); wrap with `mod raw { ... }` and call as \
         `raw::%s` or import via `use raw::*;`"
        kind name name
  in
  let aliases = ref [] in
  let rec walk path items =
    List.iter
      (fun item -> match item with
        | Ast.Function (f : Ast.func) ->
            if f.is_extern then
              require_raw ~current_path:path ~kind:"fn"
                ~name:f.name ~pos:f.pos;
            (* extern fn uses its C-side name (set via `as` rename or
               default to f.name); main stays unprefixed; everything
               else gets ex_/mod__. *)
            let m =
              if f.is_extern then f.c_name
              else if f.name = "main" then "main"
              else mangle path f.name
            in
            funcs := (path, f, m) :: !funcs
        | Ast.Struct s ->
            structs := (path, s) :: !structs
        | Ast.ExternStruct es ->
            require_raw ~current_path:path ~kind:"struct"
              ~name:es.Ast.esname ~pos:es.Ast.espos;
            ext_structs := es :: !ext_structs
        | Ast.ExternType et ->
            require_raw ~current_path:path ~kind:"type"
              ~name:et.Ast.xtname ~pos:et.Ast.xtpos;
            ext_types := et :: !ext_types
        | Ast.ExternConst ec ->
            require_raw ~current_path:path ~kind:"const"
              ~name:ec.Ast.ecname ~pos:ec.Ast.ecpos;
            ext_consts := ec :: !ext_consts
        | Ast.ExternVar ev ->
            require_raw ~current_path:path ~kind:"var"
              ~name:ev.Ast.evname ~pos:ev.Ast.evpos;
            ext_vars := ev :: !ext_vars
        | Ast.Enum e ->
            enums := (path, e) :: !enums
        | Ast.Const c ->
            consts := (path, c) :: !consts
        | Ast.Module m ->
            let mod_path = path @ [m.Ast.mname] in
            modules := (mod_path, m.Ast.mis_pub) :: !modules;
            walk mod_path m.Ast.mitems
        | Ast.Impl ib ->
            impls := (path, ib) :: !impls
        | Ast.Trait td ->
            traits := (path, td) :: !traits
        | Ast.TypeAlias ta ->
            (* Reject shadowing primitive type names — `type int = u32`
               would silently rewrite every `int` annotation in scope
               into u32, breaking the language-spec types.  Apply at
               the flatten pass so the error surfaces before any
               annotation-resolution.  Generics OK (`type Box<T> = ...`)
               — the body uses tparams, not primitives. *)
            let reserved = [
              "int"; "bool"; "str"; "u8"; "u16"; "u32"; "u64";
              "i8"; "i16"; "i32"; "i64"; "c_int"; "c_uint";
              "c_short"; "c_ushort"; "c_long"; "c_ulong";
              "c_char"; "c_schar"; "c_uchar"; "c_void"; "Self";
            ] in
            if List.mem ta.Ast.taname reserved then
              Error.failf ta.Ast.tapos
                "'type %s' shadows a built-in type — pick a different \
                 alias name"
                ta.Ast.taname;
            type_aliases := (path, ta) :: !type_aliases
        | Ast.CInclude { path = inc_path; pos } ->
            if path <> [] then
              Error.failf pos
                "'@c_include' must be at top level, not inside a module";
            c_includes := inc_path :: !c_includes
        | Ast.Use { path = use_path; is_pub; pos; is_wildcard = false }
            when is_pub ->
            (* `pub use foo::bar;` — single-name re-export.  Record as
               (scope = current walk path, local_name = last segment of
               use_path, target = full use_path).  Lookup at this scope
               for `local_name` will redirect to `use_path` resolved
               against the same scope. *)
            (match List.rev use_path with
             | local_name :: _ ->
                 aliases := (path, local_name, use_path, pos) :: !aliases
             | [] ->
                 Error.failf pos "internal: empty 'pub use' path")
        | Ast.Use { pos; _ } ->
            Error.failf pos
              "internal: 'use' declaration reached codegen unresolved \
               (loader pass missing?)"
        | Ast.View { vpos; _ } ->
            Error.failf vpos
              "internal: 'view' declaration reached flatten unresolved \
               (expand_views pre-pass missing?)")
      items
  in
  walk [] program;
  { funcs = List.rev !funcs;
    structs = List.rev !structs;
    ext_structs = List.rev !ext_structs;
    ext_types = List.rev !ext_types;
    ext_consts = List.rev !ext_consts;
    ext_vars = List.rev !ext_vars;
    c_includes = List.rev !c_includes;
    enums = List.rev !enums;
    modules = List.rev !modules;
    impls = List.rev !impls;
    traits = List.rev !traits;
    consts = List.rev !consts;
    aliases = List.rev !aliases;
    type_aliases = List.rev !type_aliases }

(* Build the global function index: every function with its module path,
   exile-side name, and signature.  main() is excluded — it is not callable. *)
let build_global_index ~instances ~ext_structs ~ext_types ~ext_consts ~consts ~ext_struct_fields ~struct_index ~enum_index ~modules ~aliases ~type_aliases flat =
  List.filter_map
    (fun (p, (f : Ast.func), mangled) ->
      if f.name = "main" then None
      else
        let ctx0 = { (empty_ctx ~instances) with
          structs = struct_index; enums = enum_index;
          modules; scope = p; tparams = f.tparams;
          aliases; type_aliases; ext_struct_fields;
          ext_structs; ext_types; ext_consts; consts;
        } in
        let ctx = { ctx0 with
          tbounds = resolve_ast_tbounds ~pos:f.pos ctx0 f.tbounds } in
        Some
          (p, f.name,
           { param_tys =
               List.map (fun pp ->
                 resolve_type_ann ~pos:f.pos ctx pp.Ast.pty) f.params;
             ret_ty = Option.map (resolve_type_ann ~pos:f.pos ctx) f.ret_ty;
             mangled;
             fn_pub = f.is_pub;
             fn_tparams = f.tparams;
             fn_variadic = f.is_variadic }))
    flat

(* Build the struct registry from the flattened struct declarations.
   Two-pass: first collect every struct's absolute path with empty
   fields, then resolve each declaration's field types against that
   skeleton (and against the enum index, which has no struct deps).
   Two passes are necessary because field types can refer to other
   structs declared in any order, and `resolve_type_ann` needs to see
   them all to rewrite relative paths to absolute. *)
let build_struct_index ~instances ~ext_structs ~ext_types ~ext_consts
    ~modules ~enums ~type_aliases ~aliases struct_flat =
  let skeleton =
    List.map
      (fun (p, (s : Ast.struct_decl)) ->
        { sname_path = p @ [s.sname];
          sfields_ty = [];
          sis_pub = s.sis_pub;
          stparams = s.stparams;
          sinstance_args = None;
          sis_debug = s.sis_debug;
          ss_is_move = s.sis_move })
      struct_flat
  in
  List.map2
    (fun (p, (s : Ast.struct_decl)) skel ->
      let ctx = { (empty_ctx ~instances) with
        structs = skeleton; enums; type_aliases; aliases;
        modules; scope = p; tparams = s.stparams;
        ext_structs; ext_types; ext_consts;
      } in
      let sfields_ty =
        List.map (fun (n, t) ->
          (n, resolve_type_ann ~pos:s.spos ctx t)) s.sfields
      in
      (* DR-030 Faza-1a: a struct with at least one `own *T` field
         must carry an `alloc: Allocator` field so the synthesised
         drop knows which allocator to release the storage through.
         Without an allocator on hand, ownership has no end-of-life
         action and the language can't guarantee the free pairs
         the original allocation (libc-`free` on Amiga-arena memory
         is undefined behaviour).  Faza-1b will add `new(alloc)`
         as the second sanctioned origin which gives an alternate
         drop-source (carry-the-allocator-by-fat-ptr); until then
         the alloc-field requirement is the simplest sound rule. *)
      let has_own_field =
        List.exists
          (fun (_, ft) -> match ft with TOwnPtr _ -> true | _ -> false)
          sfields_ty
      in
      let has_alloc_field =
        List.exists
          (fun (_, ft) -> match ft with
             | TStruct ["Allocator"] -> true
             | _ -> false)
          sfields_ty
      in
      if has_own_field && not has_alloc_field then
        Error.failf s.spos
          "struct '%s' has an `own *T` field but no `alloc: Allocator` \
           field — the owner-drop synthesis needs an allocator to \
           release the storage through; add `alloc: Allocator` to the \
           struct so the drop pass knows where to send the free"
          s.sname;
      { skel with sfields_ty })
    struct_flat skeleton

(* Build the enum registry.  Like `build_struct_index` we go two-pass:
   first collect every enum's absolute path with empty variants (so
   payload types in any enum can refer to any other enum), then
   resolve each variant's `vfields` against the struct + enum
   skeleton.  Tuple variants synthesise `_0`/`_1`/... names so all
   three forms look the same to codegen; struct variants keep their
   user-given names.  `vsis_struct` lets the constructor type-check
   reject `Foo::V(args)` for a struct variant and vice versa. *)
let build_enum_index ~instances ~ext_structs ~ext_types ~ext_consts
    ~modules ~struct_index ~type_aliases ~aliases enum_flat =
  let skeleton =
    List.map
      (fun (p, (e : Ast.enum_decl)) ->
        let variants =
          List.map (fun (v : Ast.enum_variant) ->
            { vsname = v.vname; vsfields = []; vsis_struct = false })
            e.evariants
        in
        { ename_path = p @ [e.ename]; evariants = variants;
          eis_pub = e.eis_pub; etparams = e.etparams;
          einstance_args = None;
          eis_must_use = e.emust_use;
          eis_debug = e.eis_debug })
      enum_flat
  in
  List.map2
    (fun (p, (e : Ast.enum_decl)) skel ->
      let ctx = { (empty_ctx ~instances) with
        structs = struct_index; enums = skeleton; type_aliases; aliases;
        modules; scope = p; tparams = e.etparams;
        ext_structs; ext_types; ext_consts;
      } in
      let variants =
        List.map (fun (v : Ast.enum_variant) ->
          let (vsfields, vsis_struct) =
            match v.vkind with
            | Ast.VUnit -> ([], false)
            | Ast.VTuple tys ->
                (List.mapi (fun i t ->
                   ("_" ^ string_of_int i,
                    resolve_type_ann ~pos:e.epos ctx t)) tys,
                 false)
            | Ast.VStruct fs ->
                (List.map (fun (n, t) ->
                   (n, resolve_type_ann ~pos:e.epos ctx t)) fs,
                 true)
          in
          { vsname = v.vname; vsfields; vsis_struct })
          e.evariants
      in
      { skel with evariants = variants })
    enum_flat skeleton

(* Reject value-type reference cycles among structs/enums.  A struct
   field or enum-variant payload of value type (TStruct / TEnum, a
   TTuple thereof, or a generic application TStructApp / TEnumApp like
   `b: B<T>`) embeds the whole aggregate inline; a cycle through such
   fields is an infinitely-sized C type ("field has incomplete type"
   from the C compiler).  A pointer (`*T`) breaks the cycle — it embeds
   only an address.  Runs over the skeletons, so a generic application
   counts as a value edge to the applied type (`A<T> { b: B<T> }` and
   `B<T> { a: A<T> }` is a cycle); the args are walked too for nested
   references.  [pos_of] anchors the error at the decl. *)
let detect_value_cycles ~structs ~enums ~pos_of =
  let rec refs_of_typ = function
    | TStruct p | TEnum p -> [ p ]
    | TStructApp { path; args } | TEnumApp { path; args } ->
        path :: List.concat_map refs_of_typ args
    | TTuple ts -> List.concat_map refs_of_typ ts
    | TArray { elem; _ } -> refs_of_typ elem  (* embeds elem by value *)
    | _ -> []
  in
  let name path = String.concat "::" path in
  let edges path =
    match List.find_opt (fun (s : struct_sig) -> s.sname_path = path) structs with
    | Some s -> List.concat_map (fun (_, t) -> refs_of_typ t) s.sfields_ty
    | None ->
        (match List.find_opt (fun (e : enum_sig) -> e.ename_path = path) enums with
         | Some e ->
             List.concat_map (fun (v : variant_sig) ->
               List.concat_map (fun (_, t) -> refs_of_typ t) v.vsfields)
               e.evariants
         | None -> [])
  in
  let cleared = Hashtbl.create 32 in
  let rec dfs stack path =
    if List.mem path stack then begin
      let chain =
        let rec upto = function
          | x :: _ when x = path -> [ x ]
          | x :: rest -> x :: upto rest
          | [] -> []
        in
        List.rev (path :: upto stack)
      in
      Error.failf (pos_of path)
        "recursive value type '%s' (cycle: %s) — a field embeds the \
         type by value, making it infinitely sized; break the cycle \
         with a pointer (`*T`)"
        (name path) (String.concat " -> " (List.map name chain))
    end else if not (Hashtbl.mem cleared path) then begin
      List.iter (dfs (path :: stack)) (edges path);
      Hashtbl.replace cleared path ()
    end
  in
  List.iter (fun (s : struct_sig) -> dfs [] s.sname_path) structs;
  List.iter (fun (e : enum_sig) -> dfs [] e.ename_path) enums

(* Walk a typed function body looking for tuple types in use, deduplicating
   by mangled name; codegen later emits one C struct per unique shape.
   Reads the `.ty` field on each typed expression — no `type_of` dispatch
   needed. *)
let collect_tuple_types_of ?(structs = []) ?(enums = []) tfuncs =
  let tup_seen = ref [] in
  let fnptr_seen = ref [] in
  let arr_seen = ref [] in
  (* DR-002 C2 — skip aggregates that still hold an unresolved TVar
     (or a generic TStructApp / TEnumApp / TAssocProj head).  Pre-fix
     a generic skeleton's signature (`fn wrap<T>(x:T) -> (T, int)`)
     or field type (`struct Pair<T> { p: (T, int) }`) flowed straight
     into `mangle_typ`, which has no encoding for TVar and asserted
     false on the declaration alone.  Skeletons never reach codegen
     — only their monomorphic instances do, and those carry concrete
     args verbatim. *)
  let rec contains_tvar (t : typ) =
    match t with
    | TVar _ | TAssocProj _ -> true
    | TTuple ts -> List.exists contains_tvar ts
    | TFnPtr { params; ret } ->
        List.exists contains_tvar params
        || (match ret with Some t -> contains_tvar t | None -> false)
    | TArray { elem; _ } -> contains_tvar elem
    | TPtr inner | TOwnPtr inner | TConstPtr inner -> contains_tvar inner
    | TStructApp { args; _ } | TEnumApp { args; _ } ->
        List.exists contains_tvar args
    | _ -> false
  in
  let add_tuple t =
    if contains_tvar t then ()
    else
      let name = mangle_typ t in
      if not (List.exists (fun (n, _) -> n = name) !tup_seen) then
        tup_seen := (name, t) :: !tup_seen
  in
  let add_fnptr t =
    if contains_tvar t then ()
    else
      let name = mangle_typ t in
      if not (List.exists (fun (n, _) -> n = name) !fnptr_seen) then
        fnptr_seen := (name, t) :: !fnptr_seen
  in
  let add_array t =
    if contains_tvar t then ()
    else
      let name = mangle_typ t in
      if not (List.exists (fun (n, _) -> n = name) !arr_seen) then
        arr_seen := (name, t) :: !arr_seen
  in
  let rec walk_typ t =
    match t with
    | TTuple ts -> add_tuple t; List.iter walk_typ ts
    | TFnPtr { params; ret } ->
        add_fnptr t;
        List.iter walk_typ params;
        Option.iter walk_typ ret
    | TArray { elem; _ } ->
        (* Post-order: an array-of-array's inner shape must be defined
           first, so add the element shapes before self. *)
        walk_typ elem; add_array t
    | TPtr inner | TOwnPtr inner | TConstPtr inner -> walk_typ inner
    | _ -> ()
  in
  let visit_expr (te : texpr) =
    walk_typ te.ty;
    match te.e with TSizeOf t -> walk_typ t | _ -> ()
  in
  let visit_stmt s =
    List.iter (iter_texpr visit_expr) (tstmt_own_exprs s)
  in
  List.iter
    (fun tf ->
      (* Walk the *resolved* signature types (array sizes / generic
         instances already evaluated) rather than the raw AST anns. *)
      Option.iter walk_typ tf.tf_ret_ty;
      List.iter walk_typ tf.tf_param_tys;
      List.iter (iter_tstmt visit_stmt) tf.tf_body)
    tfuncs;
  (* Struct fields and enum variants may carry types (fn-ptrs, arrays,
     tuples) that nothing in the body references — e.g. an `Allocator`
     whose `alloc_fn` / `free_fn` fields are the only mention of those
     fn-ptr shapes when a caller alloc's but never frees.  Walk them
     so the typedefs make it into the output. *)
  List.iter (fun (s : struct_sig) ->
    List.iter (fun (_, t) -> walk_typ t) s.sfields_ty) structs;
  List.iter (fun (e : enum_sig) ->
    List.iter (fun (v : variant_sig) ->
      List.iter (fun (_, t) -> walk_typ t) v.vsfields) e.evariants) enums;
  (List.rev !tup_seen, List.rev !fnptr_seen, List.rev !arr_seen)

(* Detect heap usage by scanning the typed bodies for `TNew` expressions or
   builtin `free(p)` calls — both are emitted in C only when one of them is
   present, so codegen can conditionally include `<stdlib.h>`.
   `default_allocator()` also needs `<stdlib.h>` because its alloc/free
   thunks call libc `malloc`/`free`. *)
let uses_heap_of tfuncs =
  let expr_is_heap (te : texpr) = match te.e with
    | TNew _ | TNewEnum _ -> true
    | TBuiltinCall { name = "free"; _ }
    | TBuiltinCall { name = "default_allocator"; _ } -> true
    | _ -> false
  in
  let stmt_is_heap s =
    List.exists (exists_texpr expr_is_heap) (tstmt_own_exprs s)
  in
  List.exists (fun tf ->
    List.exists (exists_tstmt stmt_is_heap) tf.tf_body)
    tfuncs

(* Detect calls to the prelude `default_allocator()` builtin so codegen
   can emit the libc-backed thunks + helper fn at the top of the C
   output.  Forwarded via `tp_uses_default_allocator`. *)
let uses_default_allocator_of tfuncs =
  let expr_uses (te : texpr) = match te.e with
    | TBuiltinCall { name = "default_allocator"; _ } -> true
    | _ -> false
  in
  let stmt_uses s =
    List.exists (exists_texpr expr_uses) (tstmt_own_exprs s)
  in
  List.exists (fun tf ->
    List.exists (exists_tstmt stmt_uses) tf.tf_body)
    tfuncs

(* `<string.h>` is pulled in only when something in the program needs
   it.  Today the only trigger is `cstr_len(s)` (lowers to `strlen()`).
   Same shape as [uses_heap_of] — checked once over the typed program
   and forwarded to codegen via [tp_uses_string_h]. *)
let uses_string_h_of tfuncs =
  let expr_uses (te : texpr) = match te.e with
    | TBuiltinCall { name = "cstr_len"; _ }
    | TBuiltinCall { name = "mem_zero"; _ } -> true
    | _ -> false
  in
  let stmt_uses s =
    List.exists (exists_texpr expr_uses) (tstmt_own_exprs s)
  in
  List.exists (fun tf ->
    List.exists (exists_tstmt stmt_uses) tf.tf_body)
    tfuncs

(* Resolve `impl` blocks against the struct registry, validate each method
   (self-param shape, name clash with fields, dup methods across blocks),
   and lower them to ordinary fn entries plus virtual-module entries.

   Lowering: a method on `Foo` becomes a regular fn in the global index
   under path = absolute struct path, with mangled name `Foo__method`
   (or `mod__Foo__method` for `Foo` inside a module).  The struct's
   absolute path is registered as a virtual module so qualified call
   visibility walks (`Foo::method(p, ...)`) resolve naturally. *)
(* Substitute `Self` and associated-type projections in a type annotation
   with concrete types — used when comparing a trait method's declared
   signature against a concrete `impl Trait for Foo`.
   - `Self` (bare receiver `TySelf`, or written as a type name) → [target].
   - `Self::Item` (a 2-segment `["Self"; "Item"]`) → its [assoc] binding.
   Recurses through `TyStruct.args` (so `Option<Self::Item>` works) and
   pointer wrappers.  The previous shallow version left args untouched —
   load-bearing fix for associated types. *)
let rec subst_assoc ~assoc target ann =
  let recur = subst_assoc ~assoc target in
  match ann with
  | Ast.TySelf -> target
  | Ast.TyStruct { path = ["Self"]; args = [] } -> target
  | Ast.TyStruct { path = ["Self"; item]; args = [] } ->
      (match List.assoc_opt item assoc with
       | Some t -> t
       | None -> ann)   (* unbound assoc — completeness check reports it *)
  | Ast.TyStruct { path; args } ->
      Ast.TyStruct { path; args = List.map recur args }
  | Ast.TyPtr t -> Ast.TyPtr (recur t)
  | Ast.TyConstPtr t -> Ast.TyConstPtr (recur t)
  | other -> other

(* Resolve a written trait path to its (module, decl).  Unique name → use
   it; multiple → prefer an exact absolute-path match; else ambiguous. *)
let resolve_trait ~flat ~pos trait_written =
  let last p = match List.rev p with n :: _ -> n | [] -> "" in
  let tname = last trait_written in
  let candidates =
    List.filter (fun (_, (td : Ast.trait_decl)) -> td.trname = tname)
      flat.traits
  in
  match candidates with
  | [] -> Error.failf pos "unknown trait '%s'" (String.concat "::" trait_written)
  | [ one ] -> one
  | many ->
      (match List.find_opt
               (fun (p, td) -> p @ [td.Ast.trname] = trait_written) many with
       | Some one -> one
       | None ->
           Error.failf pos
             "ambiguous trait '%s' — multiple traits with that name; \
              qualify the path" (String.concat "::" trait_written))

(* Validate `impl Trait for Foo`: trait exists, orphan rule holds, all
   supertraits are also implemented for the target, every required trait
   method is implemented with a matching signature, and no method outside
   the trait sneaks into the impl block. *)
let check_trait_conformance ~ctx ~flat ~parent_path ~target_path ~itparams
    ~iassoc ~trait_written ~methods ~pos =
  let (trait_mod, trait_decl) = resolve_trait ~flat ~pos trait_written in
  let trait_path = trait_mod @ [trait_decl.Ast.trname] in
  (* Associated-type completeness: every `type X;` in the trait must have a
     `type X = ...;` in the impl, and no impl binding may name a non-trait
     assoc. *)
  List.iter (fun an ->
    if not (List.mem_assoc an iassoc) then
      Error.failf pos
        "missing associated type 'type %s = ...;' required by trait '%s'"
        an (String.concat "::" trait_path))
    trait_decl.Ast.trassoc;
  List.iter (fun (an, _) ->
    if not (List.mem an trait_decl.Ast.trassoc) then
      Error.failf pos
        "associated type '%s' is not a member of trait '%s'"
        an (String.concat "::" trait_path))
    iassoc;
  (* Supertraits (`trait B: A`): the target must also implement every
     supertrait.  Reads the full impl table (populated in a pre-pass), so
     the supertrait's impl may appear before or after this one in source. *)
  List.iter (fun super_written ->
    let (_, super_decl) = resolve_trait ~flat ~pos super_written in
    if not (List.mem (super_decl.Ast.trname, target_path) !trait_impl_table)
    then
      Error.failf pos
        "'%s' requires supertrait '%s', but '%s' does not implement it \
         (add `impl %s for %s`)"
        (String.concat "::" trait_path) super_decl.Ast.trname
        (String.concat "::" target_path) super_decl.Ast.trname
        (String.concat "::" target_path))
    trait_decl.Ast.trsupers;
  (* Orphan rule (D3): the impl must live in the trait's module or the
     target type's module (direct, not an ancestor). *)
  let target_mod = match List.rev target_path with _ :: rest -> List.rev rest | [] -> [] in
  if parent_path <> trait_mod && parent_path <> target_mod then
    Error.failf pos
      "orphan 'impl %s for %s': a trait impl must live in the trait's \
       module or the target type's module"
      (String.concat "::" trait_path) (String.concat "::" target_path);
  (* The target type as a type annotation, for substituting `Self`. *)
  let target_ann =
    Ast.TyStruct { path = target_path;
                   args = List.map (fun n -> Ast.TyStruct { path = [n]; args = [] })
                            itparams }
  in
  (* DR-014 — method-tparams must be in scope for both sides during
     comparison.  Without them `cmp_ann` would call resolve_type_ann
     on a `T` neither known to the impl context (struct-scope) nor a
     real type, and trip "unknown type T".  Splice them into ctx
     just for the resolution; the names match position-for-position
     in trait vs impl, so a `T` on each side resolves to the same
     `TVar T`. *)
  let cmp_ann ~tparams tm_ann im_ann =
    let ctx' = { ctx with tparams = tparams @ ctx.tparams } in
    let t1 = resolve_type_ann ~pos ctx'
               (subst_assoc ~assoc:iassoc target_ann tm_ann) in
    let t2 = resolve_type_ann ~pos ctx' im_ann in
    typ_eq t1 t2
  in
  let cmp_ret ~tparams tm_ret im_ret =
    match tm_ret, im_ret with
    | None, None -> true
    | Some a, Some b -> cmp_ann ~tparams a b
    | _ -> false
  in
  (* Substitute `Self` in a (default) method's signature so the synthesised
     impl method has the concrete target type for its receiver / params. *)
  let sub_ann = subst_assoc ~assoc:iassoc target_ann in
  let rec sub_stmts ss = List.map sub_stmt ss
  and sub_stmt (s : Ast.stmt) : Ast.stmt = match s with
    | Ast.Let { name; is_mut; ty_ann; value; pos } ->
        Ast.Let { name; is_mut;
                  ty_ann = Option.map sub_ann ty_ann;
                  value; pos }
    | Ast.LetElse { pat; value; else_body; pos } ->
        Ast.LetElse { pat; value;
                      else_body = sub_stmts else_body; pos }
    | Ast.LetTuple _ | Ast.Assign _ | Ast.AssignField _
    | Ast.AssignIndex _ | Ast.AssignDeref _
    | Ast.Return _ | Ast.ExprStmt _ | Ast.Tail _
    | Ast.Break _ | Ast.Continue _ -> s
    | Ast.While { cond; body } ->
        Ast.While { cond; body = sub_stmts body }
    | Ast.For { var; range; body; pos } ->
        Ast.For { var; range; body = sub_stmts body; pos }
    | Ast.Defer { body; pos } ->
        Ast.Defer { body = sub_stmts body; pos }
    | Ast.With { target; name; body; pos } ->
        Ast.With { target; name; body = sub_stmts body; pos }
  in
  let specialise_default (tm : Ast.func) : Ast.func =
    { tm with
      Ast.params = List.map (fun (p : Ast.param) ->
        { p with Ast.pty = sub_ann p.pty })
        tm.params;
      Ast.ret_ty = Option.map sub_ann tm.ret_ty;
      Ast.body = sub_stmts tm.body }
  in
  (* Walk every trait method.  Provided → check signature.  Omitted and
     defaulted → synthesise from the default body.  Omitted and required →
     error.  Collect synthesised defaults to add to the impl. *)
  let synthesised =
    List.filter_map (fun (tm : Ast.func) ->
      match List.find_opt (fun (im : Ast.func) -> im.name = tm.name) methods with
      | Some im ->
          if List.length im.params <> List.length tm.params then
            Error.failf im.pos
              "method '%s' has %d parameter(s) but trait '%s' declares %d"
              tm.name (List.length im.params)
              (String.concat "::" trait_path) (List.length tm.params);
          (* DR-014 — generic-method tparam arity must match too.
             Same-position names get treated as the same TVar by
             cmp_ann's scope-splice; differing arity is a flat
             mismatch. *)
          if List.length im.tparams <> List.length tm.tparams then
            Error.failf im.pos
              "method '%s' has %d type parameter(s) but trait '%s' declares %d"
              tm.name (List.length im.tparams)
              (String.concat "::" trait_path) (List.length tm.tparams);
          let m_tparams = tm.tparams in
          let ctx' = { ctx with tparams = m_tparams @ ctx.tparams } in
          List.iter2 (fun (tp : Ast.param) (ip : Ast.param) ->
            if not (cmp_ann ~tparams:m_tparams tp.pty ip.pty) then begin
              let t1 = resolve_type_ann ~pos ctx'
                         (subst_assoc ~assoc:iassoc target_ann tp.pty) in
              let t2 = resolve_type_ann ~pos ctx' ip.pty in
              Error.failf im.pos
                "method '%s': parameter '%s' type does not match trait '%s' \
                 (expected %s, got %s)"
                tm.name ip.pname (String.concat "::" trait_path)
                (typ_name t1) (typ_name t2)
            end)
            tm.params im.params;
          if not (cmp_ret ~tparams:m_tparams tm.ret_ty im.ret_ty) then
            Error.failf im.pos
              "method '%s' return type does not match trait '%s'"
              tm.name (String.concat "::" trait_path);
          None
      | None ->
          if List.mem tm.name trait_decl.Ast.trdefaults then
            (* Defaulted method omitted by the impl — synthesise it from the
               trait's default body, with `Self` specialised to the target. *)
            Some (specialise_default tm)
          else
            Error.failf pos
              "missing method '%s' required by trait '%s'"
              tm.name (String.concat "::" trait_path))
      trait_decl.Ast.trmethods
  in
  (* No method outside the trait's surface. *)
  List.iter (fun (im : Ast.func) ->
    if not (List.exists (fun (tm : Ast.func) -> tm.name = im.name)
              trait_decl.Ast.trmethods) then
      Error.failf im.pos
        "method '%s' is not a member of trait '%s'"
        im.name (String.concat "::" trait_path))
    methods;
  synthesised

(* Resolve an `impl` target to `(abs-path, is-pub, field-names)`.  Works
   for both struct and enum targets; field-names is [] for an enum (the
   method-name-vs-field clash check is struct-only). *)
let resolve_impl_target ctx (path : string list) =
  match lookup_struct ctx path with
  | Some s -> Some (s.sname_path, s.sis_pub, List.map fst s.sfields_ty)
  | None ->
      (match lookup_enum ctx path with
       | Some e -> Some (e.ename_path, e.eis_pub, [])
       | None -> None)

let expand_impls ~instances ~ext_structs ~ext_types ~ext_consts flat struct_index enum_index modules =
  (* DR-025 — populate the trait-decl assoc registry up front so the
     resolver can answer `<F: Trait>(...) -> F::assoc` shortcut
     queries without needing any `impl Trait for X` to be registered
     yet.  Trait identity uses the last path segment, mirroring
     [trait_impl_table] / [trait_assoc_table]. *)
  trait_decl_assocs :=
    List.map (fun (_, (td : Ast.trait_decl)) ->
      (td.Ast.trname, td.Ast.trassoc))
      flat.traits;
  (* Pre-pass: register every `impl Trait for Foo` as (trait-name, target)
     BEFORE any conformance check runs, so generic `<T: Trait>` bounds and
     supertrait requirements resolve order-independently (a supertrait's
     impl may be written after the impl that depends on it). *)
  List.iter
    (fun (parent_path, ib) ->
      match ib.Ast.itrait with
      | None -> ()
      | Some trait_written ->
          let ctx0 = { (empty_ctx ~instances) with
            structs = struct_index; enums = enum_index;
            modules; scope = parent_path;
            tparams = ib.Ast.itparams;
            ext_structs; ext_types; ext_consts } in
          let ctx = { ctx0 with
            tbounds = resolve_ast_tbounds ~pos:ib.Ast.ipos ctx0 ib.Ast.itbounds } in
          (match resolve_impl_target ctx ib.Ast.itarget with
           | None -> ()  (* the conformance pass reports the unknown target *)
           | Some (target_path, _, _) ->
               let (_, td) = resolve_trait ~flat ~pos:ib.Ast.ipos trait_written in
               trait_impl_table :=
                 (td.Ast.trname, target_path) :: !trait_impl_table;
               (* Same pre-pass populates the assoc table — resolving each
                  `type X = T;` against the impl's tparam scope.  Skip on
                  resolution failure (conformance pass will report). *)
               let assoc_resolved =
                 List.filter_map
                   (fun (an, ann) ->
                     try
                       Some (an,
                             resolve_type_ann ~pos:ib.Ast.ipos ctx ann)
                     with _ -> None)
                   ib.Ast.iassoc
               in
               if assoc_resolved <> [] then
                 trait_assoc_table :=
                   ((td.Ast.trname, target_path), assoc_resolved)
                   :: !trait_assoc_table))
    flat.impls;
  let resolved =
    List.map
      (fun (parent_path, ib) ->
        let ctx0 = { (empty_ctx ~instances) with
          structs = struct_index; enums = enum_index;
          modules; scope = parent_path;
          (* The impl's type parameters are in scope while resolving the
             methods' self/param/ret types (`self: Pair<A, B>`). *)
          tparams = ib.Ast.itparams;
          ext_structs; ext_types; ext_consts;
        } in
        let ctx = { ctx0 with
          tbounds = resolve_ast_tbounds ~pos:ib.Ast.ipos ctx0 ib.Ast.itbounds } in
        let (target_path, target_pub, field_names) =
          match resolve_impl_target ctx ib.Ast.itarget with
          | Some t -> t
          | None ->
              Error.failf ib.Ast.ipos
                "unknown type '%s' in 'impl' block"
                (String.concat "::" ib.Ast.itarget)
        in
        let in_block_seen = ref [] in
        List.iter
          (fun (m : Ast.func) ->
            if List.mem m.name field_names then
              Error.failf m.pos
                "method name '%s' clashes with a field on '%s'"
                m.name (String.concat "::" target_path);
            if List.mem m.name !in_block_seen then
              Error.failf m.pos
                "method '%s' already defined in this 'impl' block" m.name;
            in_block_seen := m.name :: !in_block_seen;
            (* When the first param is named `self`, its annotation must
               match `Self` or `*Self`; any other type is a configuration
               error.  Other names for the receiver are allowed (and the
               method becomes a static method — no auto-ref/-deref later). *)
            (match m.params with
             | { pname = "self"; pty = ann } :: _ ->
                 let self_t = resolve_type_ann ~pos:m.pos ctx ann in
                 (* Mono impl: self is `Foo` / `*Foo` (struct or enum).
                    Generic impl: self is the still-applied `Pair<A, B>`
                    (a TStructApp / TEnumApp on the target path). *)
                 (match self_t with
                  | TStruct p when p = target_path -> ()
                  | TPtr (TStruct p) when p = target_path -> ()
                  | TConstPtr (TStruct p) when p = target_path -> ()
                  | TStructApp { path; _ } when path = target_path -> ()
                  | TPtr (TStructApp { path; _ }) when path = target_path -> ()
                  | TConstPtr (TStructApp { path; _ })
                      when path = target_path -> ()
                  | TEnum p when p = target_path -> ()
                  | TPtr (TEnum p) when p = target_path -> ()
                  | TConstPtr (TEnum p) when p = target_path -> ()
                  | TEnumApp { path; _ } when path = target_path -> ()
                  | TPtr (TEnumApp { path; _ }) when path = target_path -> ()
                  | TConstPtr (TEnumApp { path; _ })
                      when path = target_path -> ()
                  | _ ->
                      Error.failf m.pos
                        "first parameter 'self' must have type '%s', \
                         '*%s', or '*const %s', got %s"
                        (String.concat "::" target_path)
                        (String.concat "::" target_path)
                        (String.concat "::" target_path)
                        (typ_name self_t))
             | _ -> ()))
          ib.Ast.iitems;
        (* Trait conformance: when this is `impl Trait for Foo`, check the
           trait exists, every required method is present with a matching
           signature, no extra methods sneak in, and the orphan rule holds.
           Returns the synthesised default methods to add to the impl. *)
        let synthesised =
          match ib.Ast.itrait with
          | None -> []
          | Some trait_written ->
              check_trait_conformance ~ctx ~flat ~parent_path
                ~target_path ~itparams:ib.Ast.itparams
                ~iassoc:ib.Ast.iassoc
                ~trait_written ~methods:ib.Ast.iitems ~pos:ib.Ast.ipos
        in
        (target_path, target_pub, ib.Ast.itparams, ib.Ast.itbounds,
         ib.Ast.iitems @ synthesised))
      flat.impls
  in
  (* Cross-block dup check: same method name on the same struct in two
     different impl blocks. *)
  let seen_methods = Hashtbl.create 16 in
  List.iter
    (fun (target_path, _, _, _, methods) ->
      List.iter
        (fun (m : Ast.func) ->
          let key = (target_path, m.name) in
          match Hashtbl.find_opt seen_methods key with
          | Some _ ->
              Error.failf m.pos
                "method '%s' on '%s' already defined in another 'impl' block"
                m.name (String.concat "::" target_path)
          | None -> Hashtbl.add seen_methods key m.pos)
        methods)
    resolved;
  let virtual_modules =
    let seen = ref [] in
    List.filter_map
      (fun (target_path, sis_pub, _, _, _) ->
        if List.mem target_path !seen then None
        else (seen := target_path :: !seen;
              Some (target_path, sis_pub)))
      resolved
  in
  let impl_funcs =
    List.concat_map
      (fun (target_path, _, itparams, itbounds, methods) ->
        List.map
          (fun (m : Ast.func) ->
            (* A generic impl's tparams become the method's own tparams,
               so the method is treated as a generic fn (inferred /
               instantiated per concrete receiver at the call site).
               Bounds on those tparams (`impl<T: Bound>`) also splice
               into the method's tbounds — the existing
               instantiation-time `type_impls_trait` check
               (typecheck.ml:1267) then covers them with no separate
               enforcement path. *)
            let m = { m with
              Ast.tparams = itparams @ m.tparams;
              Ast.tbounds = itbounds @ m.tbounds;
            } in
            let mangled = mangle target_path m.name in
            (target_path, m, mangled))
          methods)
      resolved
  in
  (impl_funcs, virtual_modules)

(* Synthetic items prepended to every program before flatten_items.  Gives
   `Option<T>` and `Result<T, E>` for free without a `use` or explicit
   declaration.  Both live at top level (path = []) so users write
   `Option::None`, `Result::Ok(x)` directly.  If a user redeclares one
   at top level we skip our copy — the user's definition wins. *)
let prelude_pos = { Pos.zero with file = "<prelude>" }

(* Mono structs synthesised by `prelude_items`.  Listed here so the
   post-typecheck DCE pass in `check_program` can drop them when no
   user code mentions the type — keeps unrelated programs (hello_world,
   etc.) from carrying an unused `struct ex_Allocator` decl.  Add a
   name when introducing a new mono prelude struct. *)
let prelude_mono_struct_names =
  ["Allocator"; "StringBuilder"; "String"; "Arena"]

let prelude_items () =
  let mk_unit name = {
    Ast.vname = name; vkind = Ast.VUnit; vpos = prelude_pos;
  } in
  let mk_tuple name tys = {
    Ast.vname = name; vkind = Ast.VTuple tys; vpos = prelude_pos;
  } in
  let tvar n = Ast.TyStruct { path = [n]; args = [] } in
  let option_decl = {
    Ast.ename = "Option";
    etparams = ["T"];
    evariants = [ mk_unit "None"; mk_tuple "Some" [tvar "T"] ];
    epos = prelude_pos;
    eis_pub = true;
    etier_hint = Some "core";
    emust_use = true;
    eis_debug = false;
    ederives = [];
  } in
  let result_decl = {
    Ast.ename = "Result";
    etparams = ["T"; "E"];
    evariants = [ mk_tuple "Ok" [tvar "T"]; mk_tuple "Err" [tvar "E"] ];
    epos = prelude_pos;
    eis_pub = true;
    etier_hint = Some "core";
    emust_use = true;
    eis_debug = false;
    ederives = [];
  } in
  (* Allocator — uniform pluggable memory interface.  `state` rides on
     every call so allocators with backing data (arenas, pools) can
     reach it; stateless ones (libc) ignore.  Generic methods alloc/
     free are monomorphized per T at call site, so the typed cast +
     `size_of(T)` expand to compile-time constants in the emitted C.
     User code prefers `alloc.alloc::<Foo>()` over raw pointer maths. *)
  let cvoid_ptr = Ast.TyPtr Ast.TyCVoid in
  (* `Range<T>` / `RangeInclusive<T>` — generic prelude structs carrying a
     pair of bounds.  `a..b` and `a..=b` desugar to a struct literal of the
     respective type; `for v in r` extracts `.lo` and `.hi` when `r` is a
     value of such a type (literal `..`/`..=` in for-head still take the
     fast path). *)
  let range_struct = {
    Ast.sname = "Range"; stparams = ["T"];
    sfields = [("lo", tvar "T"); ("hi", tvar "T")];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "core"; sis_debug = false; sderives = []; sis_move = false;
  } in
  let range_inclusive_struct = {
    Ast.sname = "RangeInclusive"; stparams = ["T"];
    sfields = [("lo", tvar "T"); ("hi", tvar "T")];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "core"; sis_debug = false; sderives = []; sis_move = false;
  } in
  (* `Slice<T>` — bounded view (read-only pointer + length).  MVP scope:
     indexing (`s[i]` lowers to `s.ptr[i]`), `.len` / `.ptr` field
     access, by-value pass through fn args (struct copy of two words).
     Sub-slicing and mutable variant deferred. *)
  let slice_struct = {
    Ast.sname = "Slice"; stparams = ["T"];
    sfields = [("ptr", Ast.TyConstPtr (tvar "T"));
               ("len", Ast.TyInt { signed = false; width = Ast.W32 })];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "core"; sis_debug = false; sderives = []; sis_move = false;
  } in
  let alloc_struct = {
    Ast.sname = "Allocator";
    stparams = [];
    sfields = [
      ("state", cvoid_ptr);
      (* Byte-count width-pinned to `u32` at the seam (DR-001 §6(ii) /
         DR-003 defect #5): on the Amiga m68k toolchain `c_uint` is
         16-bit and `cap as c_uint` truncates buffers >64KB.  The stub
         takes the responsibility for `u32 → size_t` on its side. *)
      ("alloc_fn",
       Ast.TyFnPtr {
         params = [ cvoid_ptr;
                    Ast.TyInt { signed = false; width = Ast.W32 } ];
         ret = Some cvoid_ptr });
      (* `free_fn` carries the byte-count back to the allocator (DR-004
         size-on-free): libc free ignores it but Amiga FreeMem / arena /
         pool / kernel ward-region need the size to reclaim — without
         the size param every non-libc allocator breaks.  Cheap-now
         (size_of(T) is a compile-time constant for typed free; the
         buffer types track their own cap/len), expensive-later
         (every Allocator consumer locks the signature). *)
      ("free_fn",
       Ast.TyFnPtr {
         params = [ cvoid_ptr; cvoid_ptr;
                    Ast.TyInt { signed = false; width = Ast.W32 } ];
         ret = None });
    ];
    spos = prelude_pos;
    sis_pub = true;
    stier_hint = Some "core";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  (* Method bodies use the fn-ptr-field call syntax (`self.alloc_fn(...)`)
     directly — typecheck routes it through TIndirectCall when the
     receiver's matching field is a TFnPtr. *)
  let pos = prelude_pos in
  let var n = Ast.Var (n, pos) in
  let alloc_body = [
    (* DR-030 Faza-1a: Allocator.alloc is the sanctioned origin of
       `own *T`.  The body casts the raw `*c_void` from `alloc_fn`
       to `own *T` — that cast is "fabricate ownership" but explicit
       (the seam between unsafe extern memory and the OWN-tracked
       value world).  Plain `*T → own *T` implicit coercions remain
       forbidden by OWN-D1; this cast is the single exception that
       lets the rest of the language stay strict. *)
    Ast.Return (
      Some (Ast.Cast (
        Ast.MethodCall {
          receiver = var "self"; name = "alloc_fn";
          args = [ Ast.FieldAccess (var "self", "state", pos);
                   Ast.Cast (Ast.SizeOf (tvar "T", pos),
                             Ast.TyInt { signed = false; width = Ast.W32 },
                             pos) ];
          pos;
        },
        Ast.TyOwnPtr (tvar "T"), pos)),
      pos);
  ] in
  let free_body = [
    (* self.free_fn(self.state, p as *c_void, size_of(T) as u32);
       size_of(T) is a compile-time constant for typed free, so the
       seam carries the byte-count back to the allocator without
       any runtime tracking on the caller side. *)
    Ast.ExprStmt (Ast.MethodCall {
      receiver = var "self"; name = "free_fn";
      args = [ Ast.FieldAccess (var "self", "state", pos);
               Ast.Cast (var "p", cvoid_ptr, pos);
               Ast.Cast (Ast.SizeOf (tvar "T", pos),
                         Ast.TyInt { signed = false; width = Ast.W32 },
                         pos) ];
      pos;
    });
  ] in
  let mk_method name tparams params ret body = {
    Ast.name; c_name = name; tparams; tbounds = []; params; ret_ty = ret; body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false; escapes_hatch = false; pos;
  } in
  let self_param =
    { Ast.pname = "self";
      pty = Ast.TyStruct { path = ["Allocator"]; args = [] };
      preg = None; is_mut = false }
  in
  let alloc_method =
    mk_method "alloc" ["T"] [self_param]
      (Some (Ast.TyOwnPtr (tvar "T"))) alloc_body
  in
  let free_method =
    (* DR-030 own-borrow: `Allocator.free` RELEASES the pointee, so it
       takes ownership — its parameter is `own *T`, not a borrow `*T`.
       Passing an `own *T` here is therefore a MOVE (the move-pass
       consumes the binding), which is what blocks a double-free.  A
       borrow `*T` cannot be passed at all (mirrors the builtin
       `free`'s own-only gate). *)
    mk_method "free" ["T"]
      [ self_param;
        { Ast.pname = "p"; pty = Ast.TyOwnPtr (tvar "T");
          preg = None; is_mut = false } ]
      None free_body
  in
  let alloc_impl = {
    Ast.itparams = []; itbounds = [];
    itrait = None;
    iassoc = [];
    itarget = ["Allocator"];
    iitems = [alloc_method; free_method];
    ipos = pos;
  } in
  (* `StringBuilder` — mutable growable u8 buffer over the prelude
     `Allocator`.  Keystone of the writer pattern: `Display`/`Debug`
     impls call methods on a `*StringBuilder` to assemble output.
     Per DR-001 the read side (`as_slice`) and growth math are pure
     exile; alloc/free go through `Allocator`'s seam.  v1 ships
     constructor + `push_byte` + `length` + `as_slice` + private
     `grow`; `push_str` and `push_int` will land in follow-up commits.
     No `build()` — that consumes the buffer and depends on
     OwnedStr/String (deferred until minimal-move lands). *)
  let u32_t = Ast.TyInt { signed = false; width = Ast.W32 } in
  let u8_t = Ast.TyInt { signed = false; width = Ast.W8 } in
  let _u8_ptr = Ast.TyPtr u8_t in
  let u8_cptr = Ast.TyConstPtr u8_t in
  let sb_struct = {
    Ast.sname = "StringBuilder";
    stparams = [];
    sfields = [
      (* DR-045: SB owns its buffer through `own *u8`. *)
      ("buf", Ast.TyOwnPtr u8_t);
      ("len", u32_t);
      ("cap", u32_t);
      ("alloc", Ast.TyStruct { path = ["Allocator"]; args = [] });
    ];
    spos = prelude_pos;
    sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let int_lit n = Ast.IntLit (n, pos) in
  let int_lit_as n ann = Ast.Cast (Ast.IntLit (n, pos), ann, pos) in
  let u32_lit n = int_lit_as n u32_t in
  let bin op a b = Ast.BinOp (op, a, b, pos) in
  let field e f = Ast.FieldAccess (e, f, pos) in
  let methcall recv name args =
    Ast.MethodCall { receiver = recv; name; args; pos } in
  let sb_self_ptr_param =
    { Ast.pname = "self";
      pty = Ast.TyPtr (Ast.TyStruct { path = ["StringBuilder"]; args = [] });
      preg = None; is_mut = false }
  in
  let sb_self_const_ptr_param =
    { Ast.pname = "self";
      pty = Ast.TyConstPtr (Ast.TyStruct { path = ["StringBuilder"]; args = [] });
      preg = None; is_mut = false }
  in
  let mk_sb_method ?(is_pub = true) name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = []; params; ret_ty = ret;
    body; is_pub; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false; escapes_hatch = false; pos;
  } in
  let sb_struct_ann = Ast.TyStruct { path = ["StringBuilder"]; args = [] } in
  let alloc_ann = Ast.TyStruct { path = ["Allocator"]; args = [] } in
  (* with_capacity(a, hint): clamp `cap = max(hint, 16)`, alloc bytes
     through the allocator seam, return a builder with len = 0. *)
  let with_capacity_body = [
    Ast.Let { name = "cap"; is_mut = false;
              ty_ann = Some u32_t;
              value =
                Ast.If { cond = bin Ast.Lt (Ast.Var ("hint", pos)) (u32_lit 16);
                         then_blk = [ Ast.Tail (u32_lit 16) ];
                         else_blk = Some [ Ast.Tail (Ast.Var ("hint", pos)) ];
                         pos };
              pos };
    (* DR-045: SB::buf is `own *u8`. *)
    Ast.Let { name = "buf"; is_mut = false;
              ty_ann = Some (Ast.TyOwnPtr u8_t);
              value = Ast.Cast (
                methcall (Ast.Var ("a", pos)) "alloc_fn"
                  [ field (Ast.Var ("a", pos)) "state";
                    Ast.Var ("cap", pos) ],
                Ast.TyOwnPtr u8_t, pos);
              pos };
    Ast.Return (Some (Ast.StructLit {
      tname = ["StringBuilder"];
      fields = [
        ("buf", Ast.Var ("buf", pos));
        ("len", u32_lit 0);
        ("cap", Ast.Var ("cap", pos));
        ("alloc", Ast.Var ("a", pos));
      ];
      base = None; pos }), pos);
  ] in
  let with_capacity_method =
    mk_sb_method "with_capacity"
      [ { Ast.pname = "a"; pty = alloc_ann; preg = None; is_mut = false };
        { Ast.pname = "hint"; pty = u32_t; preg = None; is_mut = false } ]
      (Some sb_struct_ann) with_capacity_body
  in
  (* length( * self) -> u32: trivial accessor.  Named `length` (NOT `len`)
     to avoid the method-vs-field name clash the impl-pass rejects. *)
  let length_method =
    mk_sb_method "length" [ sb_self_const_ptr_param ] (Some u32_t)
      [ Ast.Return (Some (field (Ast.Var ("self", pos)) "len"), pos) ]
  in
  (* as_slice( * self) -> Slice<u8>: read-only view backed by buf/len.
     Drops mutability via the `*u8 → *const u8` coercion. *)
  let slice_u8_ann =
    Ast.TyStruct { path = ["Slice"]; args = [ u8_t ] } in
  let as_slice_method =
    mk_sb_method "as_slice" [ sb_self_const_ptr_param ] (Some slice_u8_ann)
      [ Ast.Return (Some (Ast.StructLit {
          tname = ["Slice"];
          fields = [
            ("ptr", Ast.Cast (field (Ast.Var ("self", pos)) "buf",
                              u8_cptr, pos));
            ("len", field (Ast.Var ("self", pos)) "len");
          ];
          base = None; pos }), pos) ]
  in
  (* grow( * self, new_cap): alloc new buffer, copy existing bytes
     through Slice<u8>+Delta-B, free old buffer, swap pointers.
     Private to the prelude — push_* call it through self-method
     resolution; user code touches buf/cap directly only at its
     own risk. *)
  let grow_body =
    let self_v = Ast.Var ("self", pos) in
    let self_alloc = field self_v "alloc" in
    [
      (* DR-045: SB grow allocates a new owned buffer. *)
      Ast.Let { name = "new_buf"; is_mut = false;
                ty_ann = Some (Ast.TyOwnPtr u8_t);
                value = Ast.Cast (
                  methcall self_alloc "alloc_fn"
                    [ field self_alloc "state";
                      Ast.Var ("new_cap", pos) ],
                  Ast.TyOwnPtr u8_t, pos);
                pos };
      Ast.Let { name = "src"; is_mut = false; ty_ann = Some slice_u8_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", Ast.Cast (field self_v "buf", u8_cptr, pos));
                    ("len", field self_v "len");
                  ]; base = None; pos };
                pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While { cond = bin Ast.Lt (Ast.Var ("i", pos)) (field self_v "len");
                  body = [
                    Ast.AssignIndex {
                      base = Ast.Var ("new_buf", pos);
                      index = Ast.Var ("i", pos);
                      value = Ast.Index { base = Ast.Var ("src", pos);
                                          index = Ast.Var ("i", pos);
                                          pos }; pos };
                    Ast.Assign { path = ["i"];
                                 value = bin Ast.Add (Ast.Var ("i", pos))
                                           (u32_lit 1); pos };
                  ] };
      (* free old buffer: alloc.free_fn(alloc.state, buf, OLD cap).
         cap is updated AFTER the free so the seam receives the
         byte-count of the buffer being released. *)
      Ast.ExprStmt (methcall self_alloc "free_fn"
        [ field self_alloc "state";
          Ast.Cast (field self_v "buf", cvoid_ptr, pos);
          field self_v "cap" ]);
      Ast.AssignField { target = self_v; field = "buf";
                        value = Ast.Var ("new_buf", pos); pos };
      Ast.AssignField { target = self_v; field = "cap";
                        value = Ast.Var ("new_cap", pos); pos };
    ]
  in
  let grow_method =
    mk_sb_method ~is_pub:false "grow"
      [ sb_self_ptr_param;
        { Ast.pname = "new_cap"; pty = u32_t; preg = None; is_mut = false } ]
      None grow_body
  in
  (* push_byte( * self, b): grow if full (geometric doubling), then
     write the byte through Delta-B and bump len. *)
  let push_byte_body =
    let self_v = Ast.Var ("self", pos) in
    [
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.Gt
                 (bin Ast.Add (field self_v "len") (u32_lit 1))
                 (field self_v "cap");
        then_blk = [
          Ast.ExprStmt (methcall self_v "grow"
            [ bin Ast.Mul (field self_v "cap") (u32_lit 2) ]);
        ];
        else_blk = None; pos });
      Ast.AssignIndex {
        base = field self_v "buf";
        index = field self_v "len";
        value = Ast.Var ("b", pos); pos };
      Ast.AssignField { target = self_v; field = "len";
                        value = bin Ast.Add (field self_v "len") (u32_lit 1);
                        pos };
    ]
  in
  let push_byte_method =
    mk_sb_method "push_byte"
      [ sb_self_ptr_param;
        { Ast.pname = "b"; pty = u8_t; preg = None; is_mut = false } ]
      None push_byte_body
  in
  (* push_str( * self, s): take the str's length via the cstr_len
     seam, grow to len+n (geometric doubling), then copy through a
     `Slice<u8>` read + Delta-B writes.  `s as *const u8` is the
     same pointer reinterpreted (str is `const char *` in C). *)
  let push_str_body =
    let self_v = Ast.Var ("self", pos) in
    let s_v = Ast.Var ("s", pos) in
    [
      Ast.Let { name = "n"; is_mut = false; ty_ann = Some u32_t;
                value = Ast.Call { callee = ["cstr_len"]; args = [s_v]; pos };
                pos };
      Ast.Let { name = "need"; is_mut = false; ty_ann = Some u32_t;
                value = bin Ast.Add (field self_v "len") (Ast.Var ("n", pos));
                pos };
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.Gt (Ast.Var ("need", pos)) (field self_v "cap");
        then_blk = [
          Ast.Let { name = "new_cap"; is_mut = true; ty_ann = Some u32_t;
                    value = field self_v "cap"; pos };
          Ast.While {
            cond = bin Ast.Lt (Ast.Var ("new_cap", pos))
                              (Ast.Var ("need", pos));
            body = [
              Ast.Assign { path = ["new_cap"];
                           value = bin Ast.Mul (Ast.Var ("new_cap", pos))
                                                (u32_lit 2);
                           pos };
            ] };
          Ast.ExprStmt (methcall self_v "grow"
            [ Ast.Var ("new_cap", pos) ]);
        ];
        else_blk = None; pos });
      Ast.Let { name = "src"; is_mut = false; ty_ann = Some slice_u8_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", Ast.Cast (s_v, u8_cptr, pos));
                    ("len", Ast.Var ("n", pos));
                  ]; base = None; pos };
                pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While {
        cond = bin Ast.Lt (Ast.Var ("i", pos)) (Ast.Var ("n", pos));
        body = [
          Ast.AssignIndex {
            base = field self_v "buf";
            index = bin Ast.Add (field self_v "len")
                                (Ast.Var ("i", pos));
            value = Ast.Index { base = Ast.Var ("src", pos);
                                index = Ast.Var ("i", pos); pos };
            pos };
          Ast.Assign { path = ["i"];
                       value = bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1);
                       pos };
        ] };
      Ast.AssignField { target = self_v; field = "len";
                        value = bin Ast.Add (field self_v "len")
                                            (Ast.Var ("n", pos));
                        pos };
    ]
  in
  let push_str_method =
    mk_sb_method "push_str"
      [ sb_self_ptr_param;
        { Ast.pname = "s"; pty = Ast.TyStr; preg = None; is_mut = false } ]
      None push_str_body
  in
  (* push_int( * self, n): decimal render through push_byte.  Negative
     values prefix '-' and work in u32 so i32::min (whose negation
     would overflow signed) still round-trips correctly — `0u - n_u`
     wraps to the right absolute value.  Digits go to a 10-byte stack
     buffer in reverse, then pushed in forward order. *)
  let i32_t = Ast.TyInt { signed = true; width = Ast.W32 } in
  let push_int_body =
    let self_v = Ast.Var ("self", pos) in
    let n_v = Ast.Var ("n", pos) in
    let push_byte_b b =
      Ast.ExprStmt (methcall self_v "push_byte" [ int_lit_as b u8_t ])
    in
    [
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.EqEq n_v (int_lit 0);
        then_blk = [
          push_byte_b 48;   (* '0' *)
          Ast.Return (None, pos);
        ];
        else_blk = None; pos });
      Ast.Let { name = "u"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.Lt n_v (int_lit 0);
        then_blk = [
          push_byte_b 45;   (* '-' *)
          Ast.Assign { path = ["u"];
                       value = bin Ast.Sub (u32_lit 0)
                                            (Ast.Cast (n_v, u32_t, pos));
                       pos };
        ];
        else_blk = Some [
          Ast.Assign { path = ["u"];
                       value = Ast.Cast (n_v, u32_t, pos);
                       pos };
        ]; pos });
      Ast.Let { name = "digits"; is_mut = true;
                ty_ann = Some (Ast.TyArray
                                 { elem = u8_t; size = Ast.IntLit (10, pos) });
                value = Ast.ArrayRepeat { value = int_lit_as 0 u8_t;
                                          count = Ast.IntLit (10, pos); pos };
                pos };
      Ast.Let { name = "idx"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While {
        cond = bin Ast.Gt (Ast.Var ("u", pos)) (u32_lit 0);
        body = [
          Ast.AssignIndex {
            base = Ast.Var ("digits", pos);
            index = Ast.Var ("idx", pos);
            value = Ast.Cast (
              bin Ast.Add (u32_lit 48)
                          (bin Ast.Mod (Ast.Var ("u", pos)) (u32_lit 10)),
              u8_t, pos);
            pos };
          Ast.Assign { path = ["u"];
                       value = bin Ast.Div (Ast.Var ("u", pos)) (u32_lit 10);
                       pos };
          Ast.Assign { path = ["idx"];
                       value = bin Ast.Add (Ast.Var ("idx", pos)) (u32_lit 1);
                       pos };
        ] };
      Ast.While {
        cond = bin Ast.Gt (Ast.Var ("idx", pos)) (u32_lit 0);
        body = [
          Ast.Assign { path = ["idx"];
                       value = bin Ast.Sub (Ast.Var ("idx", pos)) (u32_lit 1);
                       pos };
          Ast.ExprStmt (methcall self_v "push_byte"
            [ Ast.Index { base = Ast.Var ("digits", pos);
                          index = Ast.Var ("idx", pos); pos } ]);
        ] };
    ]
  in
  let push_int_method =
    mk_sb_method "push_int"
      [ sb_self_ptr_param;
        { Ast.pname = "n"; pty = i32_t; preg = None; is_mut = false } ]
      None push_int_body
  in
  let sb_impl = {
    Ast.itparams = []; itbounds = []; itrait = None; iassoc = [];
    itarget = ["StringBuilder"];
    iitems = [
      with_capacity_method;
      grow_method;
      push_byte_method;
      push_str_method;
      push_int_method;
      length_method;
      as_slice_method;
    ];
    ipos = pos;
  } in
  (* `String` — owned NUL-terminated buffer, Faza 1 (deep-copy ctors,
     sound today without a move pass).  Mirrors StringBuilder's
     allocator-by-value shape minus `cap` (frozen owner; no growth).
     The trailing NUL is written by every constructor so `as_str()`
     hands out a libc-`%s`-safe pointer by construction (graft G1
     from the design).  Faza 2 will add `build(sb) -> String` once
     minimal-move lands.  `@move` attribute hooks here when DR-002
     materialises — until then user must thread `*String` borrows
     (every by-value copy aliases ptr and double-free would compile
     silently). *)
  let string_struct = {
    Ast.sname = "String";
    stparams = [];
    sfields = [
      (* DR-030 Faza-1a Step E: `ptr` is now `own *u8` instead of
         `*u8`, so the affineness derives structurally from the
         own field.  `sis_move = false` since the predicate-swap
         in move.ml (DR-042 Step B) reads ownership off the
         field type. *)
      ("ptr", Ast.TyOwnPtr u8_t);
      ("len", u32_t);
      ("alloc", Ast.TyStruct { path = ["Allocator"]; args = [] });
    ];
    spos = prelude_pos;
    sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let string_struct_ann = Ast.TyStruct { path = ["String"]; args = [] } in
  (* `*self` for the destructive ops (`free` mutates ownership) and
     `*const self` for the pure accessors (`length` / `as_slice` /
     `as_str` borrow without aliasing) so user-written `let s = ...;
     defer s.free(); println(s.length())` reads through the immutable
     borrow while keeping the same call sites the original `*self`
     allowed (`*const` accepts a `*T` by coercion). *)
  let string_self_const_ptr_param =
    { Ast.pname = "self";
      pty = Ast.TyConstPtr string_struct_ann;
      preg = None; is_mut = false }
  in
  (* By-value self for `free` — destructor needs to consume the
     binding under the move-pass (parallels `String::build(sb)`).
     `*self` would auto-ref to `TPtr String` and `consume_var`
     only seeds Consumed for bare-TVar by-value args, so the legal
     `s.free(); s.length();` use-after-free would compile +
     segfault.  By-value receiver flows the TVar straight through
     TCall → consume. *)
  let string_self_value_param =
    { Ast.pname = "self";
      pty = string_struct_ann;
      preg = None; is_mut = false }
  in
  let mk_string_method ?(is_pub = true) name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = []; params; ret_ty = ret;
    body; is_pub; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false; escapes_hatch = false; pos;
  } in
  (* with_str(a, s): allocate len+1 bytes through the allocator seam,
     copy `s` through a Slice<u8> view + Delta-B, write the NUL
     terminator at [len].  Same growth-math idiom as
     StringBuilder::push_str minus the grow check (size known up front). *)
  let with_str_body =
    let s_v = Ast.Var ("s", pos) in
    let a_v = Ast.Var ("a", pos) in
    [
      Ast.Let { name = "n"; is_mut = false; ty_ann = Some u32_t;
                value = Ast.Call { callee = ["cstr_len"]; args = [s_v]; pos };
                pos };
      (* DR-030 Faza-1a Step E: cast the raw allocator return to
         `own *u8` so the buffer flows into the String's own field
         without an extra fabrication site. *)
      Ast.Let { name = "buf"; is_mut = false;
                ty_ann = Some (Ast.TyOwnPtr u8_t);
                value = Ast.Cast (
                  methcall a_v "alloc_fn"
                    [ field a_v "state";
                      bin Ast.Add (Ast.Var ("n", pos)) (u32_lit 1) ],
                  Ast.TyOwnPtr u8_t, pos);
                pos };
      Ast.Let { name = "src"; is_mut = false; ty_ann = Some slice_u8_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", Ast.Cast (s_v, u8_cptr, pos));
                    ("len", Ast.Var ("n", pos));
                  ]; base = None; pos };
                pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While {
        cond = bin Ast.Lt (Ast.Var ("i", pos)) (Ast.Var ("n", pos));
        body = [
          Ast.AssignIndex {
            base = Ast.Var ("buf", pos);
            index = Ast.Var ("i", pos);
            value = Ast.Index { base = Ast.Var ("src", pos);
                                index = Ast.Var ("i", pos); pos };
            pos };
          Ast.Assign { path = ["i"];
                       value = bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1);
                       pos };
        ] };
      Ast.AssignIndex {
        base = Ast.Var ("buf", pos);
        index = Ast.Var ("n", pos);
        value = int_lit_as 0 u8_t; pos };
      Ast.Return (Some (Ast.StructLit {
        tname = ["String"];
        fields = [
          ("ptr", Ast.Var ("buf", pos));
          ("len", Ast.Var ("n", pos));
          ("alloc", a_v);
        ]; base = None; pos }), pos);
    ]
  in
  let with_str_method =
    mk_string_method "with_str"
      [ { Ast.pname = "a";
          pty = Ast.TyStruct { path = ["Allocator"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "s"; pty = Ast.TyStr; preg = None; is_mut = false } ]
      (Some string_struct_ann) with_str_body
  in
  (* empty(a): 1-byte buffer holding just NUL, so as_str() is still
     `%s`-safe.  Avoids malloc(0) which is implementation-defined. *)
  let empty_body =
    let a_v = Ast.Var ("a", pos) in
    [
      (* DR-030 Faza-1a Step E: own *u8 for String.ptr. *)
      Ast.Let { name = "buf"; is_mut = false;
                ty_ann = Some (Ast.TyOwnPtr u8_t);
                value = Ast.Cast (
                  methcall a_v "alloc_fn"
                    [ field a_v "state"; u32_lit 1 ],
                  Ast.TyOwnPtr u8_t, pos);
                pos };
      Ast.AssignIndex {
        base = Ast.Var ("buf", pos);
        index = u32_lit 0;
        value = int_lit_as 0 u8_t; pos };
      Ast.Return (Some (Ast.StructLit {
        tname = ["String"];
        fields = [
          ("ptr", Ast.Var ("buf", pos));
          ("len", u32_lit 0);
          ("alloc", a_v);
        ]; base = None; pos }), pos);
    ]
  in
  let empty_method =
    mk_string_method "empty"
      [ { Ast.pname = "a";
          pty = Ast.TyStruct { path = ["Allocator"]; args = [] };
          preg = None; is_mut = false } ]
      (Some string_struct_ann) empty_body
  in
  (* length, as_slice, as_str: trivial accessors on the live buffer.
     `as_str` reinterprets `*u8` as `str` (`const char *` at the C
     level) — the NUL written by every constructor is what makes the
     reinterpret libc-`%s`-safe. *)
  let string_length_method =
    mk_string_method "length" [ string_self_const_ptr_param ] (Some u32_t)
      [ Ast.Return (Some (field (Ast.Var ("self", pos)) "len"), pos) ]
  in
  let string_as_slice_method =
    mk_string_method "as_slice" [ string_self_const_ptr_param ] (Some slice_u8_ann)
      [ Ast.Return (Some (Ast.StructLit {
          tname = ["Slice"];
          fields = [
            ("ptr", Ast.Cast (field (Ast.Var ("self", pos)) "ptr",
                              u8_cptr, pos));
            ("len", field (Ast.Var ("self", pos)) "len");
          ]; base = None; pos }), pos) ]
  in
  let string_as_str_method =
    mk_string_method "as_str" [ string_self_const_ptr_param ] (Some Ast.TyStr)
      [ Ast.Return (Some (Ast.Cast (
          field (Ast.Var ("self", pos)) "ptr", Ast.TyStr, pos)), pos) ]
  in
  (* free(self): hand the owned buffer back through the allocator
     seam.  Named `free` not `drop` — matches Allocator::free's
     convention and pairs idiomatically with `defer s.free();`.
     By-value self (not `*self`) so the move-pass consumes the
     binding through TCall — `s.free(); s.length();` rejected at
     compile-time instead of segfaulting at runtime. *)
  let string_free_method =
    let self_v = Ast.Var ("self", pos) in
    let self_alloc = field self_v "alloc" in
    (* The buffer was allocated as `len + 1` bytes (NUL terminator),
       so the seam carries the matching size back to the allocator. *)
    mk_string_method "free" [ string_self_value_param ] None
      [ Ast.ExprStmt (methcall self_alloc "free_fn"
          [ field self_alloc "state";
            Ast.Cast (field self_v "ptr", cvoid_ptr, pos);
            bin Ast.Add (field self_v "len") (u32_lit 1) ]) ]
  in
  (* build(sb) — Faza A3 of OwnedStr/String.  Consumes the
     StringBuilder by value (legal now that @move + Move.check are
     in: the by-value param marks the caller's sb Consumed, so a
     subsequent `sb.push_*` would error), writes the NUL terminator
     at [sb.len] so `as_str()` stays %s-safe, and projects the
     remaining fields into a fresh `String`.  Zero-copy: the buffer
     ownership transfers, `cap` is dropped (accepted tail-waste). *)
  let build_body =
    let sb_v = Ast.Var ("sb", pos) in
    [
      Ast.AssignIndex {
        base = field sb_v "buf";
        index = field sb_v "len";
        value = int_lit_as 0 u8_t; pos };
      (* DR-030 Faza-1a Step E: cast the StringBuilder's `*u8` buf
         to `own *u8` for transfer into the new String.  The build
         consumes `sb` (move-pass marks sb Consumed via by-value
         param), so this is a clean ownership transfer, not a
         fabrication of duplicate ownership. *)
      Ast.Return (Some (Ast.StructLit {
        tname = ["String"];
        fields = [
          ("ptr", Ast.Cast (field sb_v "buf", Ast.TyOwnPtr u8_t, pos));
          ("len", field sb_v "len");
          ("alloc", field sb_v "alloc");
        ]; base = None; pos }), pos);
    ]
  in
  let build_method =
    mk_string_method "build"
      [ { Ast.pname = "sb"; pty = sb_struct_ann;
          preg = None; is_mut = false } ]
      (Some string_struct_ann) build_body
  in
  let string_impl = {
    Ast.itparams = []; itbounds = []; itrait = None; iassoc = [];
    itarget = ["String"];
    iitems = [
      with_str_method;
      empty_method;
      build_method;
      string_length_method;
      string_as_slice_method;
      string_as_str_method;
      string_free_method;
    ];
    ipos = pos;
  } in
  (* String's `Eq` / `Hash` / `Clone` are HAND-WRITTEN content-equal
     impls that delegate to the prelude `str::*` ops.  `@derive` is
     wrong here (the `alloc: Allocator` field would force `Allocator
     impl Eq` and `ptr: *u8` would yield pointer-eq instead of
     content-eq).  `Clone` deep-copies the buffer so each owner
     manages its own allocation. *)
  let string_self_const_ptr =
    { Ast.pname = "self";
      pty = Ast.TyConstPtr string_struct_ann;
      preg = None; is_mut = false }
  in
  let string_other_const_ptr =
    { Ast.pname = "other";
      pty = Ast.TyConstPtr string_struct_ann;
      preg = None; is_mut = false }
  in
  (* eq( * self, * other) = str::eq(self.as_str(), other.as_str()) *)
  let string_eq_body =
    let self_v = Ast.Var ("self", pos) in
    let other_v = Ast.Var ("other", pos) in
    [
      Ast.Return (Some (Ast.Call {
        callee = ["str"; "eq"];
        args = [
          methcall self_v "as_str" [];
          methcall other_v "as_str" [];
        ]; pos }), pos);
    ]
  in
  let string_eq_method =
    mk_string_method "eq"
      [ string_self_const_ptr; string_other_const_ptr ]
      (Some Ast.TyBool) string_eq_body
  in
  let string_eq_impl = {
    Ast.itparams = []; itbounds = []; itrait = Some ["Eq"]; iassoc = [];
    itarget = ["String"];
    iitems = [ string_eq_method ];
    ipos = pos;
  } in
  let string_hash_body = [
    Ast.Return (Some (Ast.Call {
      callee = ["str"; "hash"];
      args = [ methcall (Ast.Var ("self", pos)) "as_str" [] ];
      pos }), pos);
  ] in
  let string_hash_method =
    mk_string_method "hash"
      [ string_self_const_ptr ]
      (Some u32_t) string_hash_body
  in
  let string_hash_impl = {
    Ast.itparams = []; itbounds = []; itrait = Some ["Hash"]; iassoc = [];
    itarget = ["String"];
    iitems = [ string_hash_method ];
    ipos = pos;
  } in
  (* clone( * const self) -> String: deep-copy via `String::with_str
     (self.alloc, self.as_str())` so each owner has its own buffer.
     The allocator value is copied into the new String (Allocator is
     plain by-value, not affine). *)
  let string_clone_body = [
    Ast.Return (Some (Ast.Call {
      callee = ["String"; "with_str"];
      args = [
        field (Ast.Var ("self", pos)) "alloc";
        methcall (Ast.Var ("self", pos)) "as_str" [];
      ]; pos }), pos);
  ] in
  let string_clone_method =
    mk_string_method "clone"
      [ string_self_const_ptr ]
      (Some string_struct_ann) string_clone_body
  in
  let string_clone_impl = {
    Ast.itparams = []; itbounds = []; itrait = Some ["Clone"]; iassoc = [];
    itarget = ["String"];
    iitems = [ string_clone_method ];
    ipos = pos;
  } in
  (* `Vec<T>` — growable workhorse collection (DR-003 v1 copy-out
     value-T).  `@move` because the buffer aliases under a silent
     value-copy; the move-pass forces single ownership.  `ptr` is
     writable `*T` (Delta-B writes land directly); `count` (not
     `len`) leaves the accessor `len()` free; we NEVER store a
     `Slice<T>` in the struct (defect #3 bait) — every read builds
     a local Slice-view from `ptr`/`count`.  Per-element drop of
     `@move` payloads (`Vec<String>`) lands once `pop` /
     `clear`-on-drop arrive — sound today for value-T (Token,
     AstNode), where per-element drop is a no-op. *)
  let vec_t_ann =
    Ast.TyStruct { path = ["Vec"]; args = [ tvar "T" ] } in
  let vec_iter_t_ann =
    Ast.TyStruct { path = ["VecIter"]; args = [ tvar "T" ] } in
  let option_t_ann =
    Ast.TyStruct { path = ["Option"]; args = [ tvar "T" ] } in
  let slice_t_ann =
    Ast.TyStruct { path = ["Slice"]; args = [ tvar "T" ] } in
  let alloc_ann_v =
    Ast.TyStruct { path = ["Allocator"]; args = [] } in
  let vec_struct = {
    Ast.sname = "Vec";
    stparams = ["T"];
    sfields = [
      (* DR-045: Vec owns its buffer through `own *T`. *)
      ("ptr", Ast.TyOwnPtr (tvar "T"));
      ("count", u32_t);
      ("cap", u32_t);
      ("alloc", alloc_ann_v);
    ];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let vec_iter_struct = {
    Ast.sname = "VecIter";
    stparams = ["T"];
    sfields = [
      ("data", Ast.TyConstPtr (tvar "T"));
      ("len", u32_t);
      ("pos", u32_t);
    ];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let vec_self_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyPtr vec_t_ann;
      preg = None; is_mut = false } in
  let vec_self_const_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyConstPtr vec_t_ann;
      preg = None; is_mut = false } in
  let mk_vec_method ?(is_pub = true) name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = []; params;
    ret_ty = ret; body; is_pub; is_extern = false;
    is_variadic = false; tier_hint = Some "full"; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos;
  } in
  let size_of_T_as_u32 =
    Ast.Cast (Ast.SizeOf (tvar "T", pos), u32_t, pos) in
  (* with_capacity(a, hint): clamp cap = max(hint, 8), alloc
     size_of(T)*cap bytes through the seam. *)
  let vec_with_capacity_body = [
    Ast.Let { name = "cap"; is_mut = false; ty_ann = Some u32_t;
              value = Ast.If {
                cond = bin Ast.Lt (Ast.Var ("hint", pos)) (u32_lit 8);
                then_blk = [ Ast.Tail (u32_lit 8) ];
                else_blk = Some [ Ast.Tail (Ast.Var ("hint", pos)) ];
                pos }; pos };
    Ast.Let { name = "bytes"; is_mut = false; ty_ann = Some u32_t;
              value = bin Ast.Mul size_of_T_as_u32
                                  (Ast.Var ("cap", pos)); pos };
    (* DR-045: Vec::ptr is `own *T`. *)
    Ast.Let { name = "p"; is_mut = false;
              ty_ann = Some (Ast.TyOwnPtr (tvar "T"));
              value = Ast.Cast (
                methcall (Ast.Var ("a", pos)) "alloc_fn"
                  [ field (Ast.Var ("a", pos)) "state";
                    Ast.Var ("bytes", pos) ],
                Ast.TyOwnPtr (tvar "T"), pos);
              pos };
    Ast.Return (Some (Ast.StructLit {
      tname = ["Vec"];
      fields = [
        ("ptr", Ast.Var ("p", pos));
        ("count", u32_lit 0);
        ("cap", Ast.Var ("cap", pos));
        ("alloc", Ast.Var ("a", pos));
      ]; base = None; pos }), pos);
  ] in
  let vec_with_capacity_method =
    mk_vec_method "with_capacity"
      [ { Ast.pname = "a"; pty = alloc_ann_v;
          preg = None; is_mut = false };
        { Ast.pname = "hint"; pty = u32_t;
          preg = None; is_mut = false } ]
      (Some vec_t_ann) vec_with_capacity_body in
  let vec_len_method =
    mk_vec_method "length" [vec_self_const_ptr_param] (Some u32_t)
      [ Ast.Return (Some (field (Ast.Var ("self", pos)) "count"), pos) ] in
  (* grow( * self, new_cap): alloc new buf, copy through Slice-view
     + Delta-B, free old with matching byte-count, swap fields. *)
  let vec_grow_body =
    let self_v = Ast.Var ("self", pos) in
    let self_alloc = field self_v "alloc" in
    [
      Ast.Let { name = "bytes"; is_mut = false; ty_ann = Some u32_t;
                value = bin Ast.Mul size_of_T_as_u32
                                    (Ast.Var ("new_cap", pos)); pos };
      (* DR-045: grow allocates a new owned buffer. *)
      Ast.Let { name = "new_ptr"; is_mut = false;
                ty_ann = Some (Ast.TyOwnPtr (tvar "T"));
                value = Ast.Cast (
                  methcall self_alloc "alloc_fn"
                    [ field self_alloc "state";
                      Ast.Var ("bytes", pos) ],
                  Ast.TyOwnPtr (tvar "T"), pos);
                pos };
      Ast.Let { name = "src"; is_mut = false; ty_ann = Some slice_t_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", Ast.Cast (field self_v "ptr",
                                      Ast.TyConstPtr (tvar "T"), pos));
                    ("len", field self_v "count");
                  ]; base = None; pos }; pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While {
        cond = bin Ast.Lt (Ast.Var ("i", pos)) (field self_v "count");
        body = [
          (* DR-002 W4 — cast the source element back to `T` before
             storing.  `Slice<T>` carries `*const T`, so `src[i]`
             yields a const-qualified T; assigning straight into the
             writable `new_ptr[i]` (`*T`) emitted `-Wdiscarded-
             qualifiers` for any `Vec<*U>` (LHS `U **`, RHS `U * const
             *`).  Plain-value `T` cases (`Vec<int>` etc.) drop const
             trivially and never warned; the cast is a no-op for them
             at the C level and silences the warning for pointer-T. *)
          Ast.AssignIndex {
            base = Ast.Var ("new_ptr", pos);
            index = Ast.Var ("i", pos);
            value = Ast.Cast (
              Ast.Index { base = Ast.Var ("src", pos);
                          index = Ast.Var ("i", pos); pos },
              tvar "T", pos);
            pos };
          Ast.Assign { path = ["i"];
                       value = bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1);
                       pos };
        ] };
      Ast.Let { name = "old_bytes"; is_mut = false; ty_ann = Some u32_t;
                value = bin Ast.Mul size_of_T_as_u32
                                    (field self_v "cap"); pos };
      Ast.ExprStmt (methcall self_alloc "free_fn"
        [ field self_alloc "state";
          Ast.Cast (field self_v "ptr", cvoid_ptr, pos);
          Ast.Var ("old_bytes", pos) ]);
      Ast.AssignField { target = self_v; field = "ptr";
                        value = Ast.Var ("new_ptr", pos); pos };
      Ast.AssignField { target = self_v; field = "cap";
                        value = Ast.Var ("new_cap", pos); pos };
    ]
  in
  let vec_grow_method =
    mk_vec_method ~is_pub:false "grow"
      [ vec_self_ptr_param;
        { Ast.pname = "new_cap"; pty = u32_t;
          preg = None; is_mut = false } ]
      None vec_grow_body in
  (* push( * self, x): grow if full, store via Delta-B, bump count.
     `x` is consumed for `@move T` (per the move-pass) or copied
     for value-T (one rule). *)
  let vec_push_body =
    let self_v = Ast.Var ("self", pos) in
    [
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.Gt
                 (bin Ast.Add (field self_v "count") (u32_lit 1))
                 (field self_v "cap");
        then_blk = [
          Ast.ExprStmt (methcall self_v "grow"
            [ bin Ast.Mul (field self_v "cap") (u32_lit 2) ]);
        ];
        else_blk = None; pos });
      Ast.AssignIndex {
        base = field self_v "ptr";
        index = field self_v "count";
        value = Ast.Var ("x", pos); pos };
      Ast.AssignField { target = self_v; field = "count";
                        value = bin Ast.Add (field self_v "count")
                                            (u32_lit 1);
                        pos };
    ]
  in
  let vec_push_method =
    mk_vec_method "push"
      [ vec_self_ptr_param;
        { Ast.pname = "x"; pty = tvar "T";
          preg = None; is_mut = false } ]
      None vec_push_body in
  (* get( * self, i) -> Option<T>: copy-out (DR-003 default).
     OOB → None; in-bounds builds a local Slice<T> view and
     yields `Some(s[i])`.  Value-T sound today; `Vec<@move T>`
     get aliases the backing store and lands after per-element
     drop. *)
  let vec_get_body =
    let self_v = Ast.Var ("self", pos) in
    [
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.GtEq (Ast.Var ("i", pos)) (field self_v "count");
        then_blk = [
          Ast.Return (Some (Ast.Call {
            callee = ["Option"; "None"]; args = []; pos }), pos);
        ];
        else_blk = None; pos });
      Ast.Let { name = "s"; is_mut = false; ty_ann = Some slice_t_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", Ast.Cast (field self_v "ptr",
                                      Ast.TyConstPtr (tvar "T"), pos));
                    ("len", field self_v "count");
                  ]; base = None; pos }; pos };
      Ast.Return (Some (Ast.Call {
        callee = ["Option"; "Some"];
        args = [ Ast.Index { base = Ast.Var ("s", pos);
                             index = Ast.Var ("i", pos); pos } ];
        pos }), pos);
    ]
  in
  let vec_get_method =
    mk_vec_method "get"
      [ vec_self_const_ptr_param;
        { Ast.pname = "i"; pty = u32_t;
          preg = None; is_mut = false } ]
      (Some option_t_ann) vec_get_body in
  let vec_as_slice_method =
    mk_vec_method "as_slice" [vec_self_const_ptr_param] (Some slice_t_ann)
      [ Ast.Return (Some (Ast.StructLit {
          tname = ["Slice"];
          fields = [
            ("ptr", Ast.Cast (field (Ast.Var ("self", pos)) "ptr",
                              Ast.TyConstPtr (tvar "T"), pos));
            ("len", field (Ast.Var ("self", pos)) "count");
          ]; base = None; pos }), pos) ] in
  let vec_iter_method =
    mk_vec_method "iter" [vec_self_const_ptr_param] (Some vec_iter_t_ann)
      [ Ast.Return (Some (Ast.StructLit {
          tname = ["VecIter"];
          fields = [
            ("data", Ast.Cast (field (Ast.Var ("self", pos)) "ptr",
                               Ast.TyConstPtr (tvar "T"), pos));
            ("len", field (Ast.Var ("self", pos)) "count");
            ("pos", u32_lit 0);
          ]; base = None; pos }), pos) ] in
  let vec_impl = {
    Ast.itparams = ["T"]; itbounds = []; itrait = None; iassoc = [];
    itarget = ["Vec"];
    iitems = [
      vec_with_capacity_method;
      vec_len_method;
      vec_grow_method;
      vec_push_method;
      vec_get_method;
      vec_as_slice_method;
      vec_iter_method;
    ];
    ipos = pos;
  } in
  (* Re-audit F8: uniform `.length()` across containers.  Slice keeps
     its transparent `.ptr`/`.len` layout (it IS an honest view
     struct), but answers the same method spelling as
     Vec/String/HashMap/StringBuilder. *)
  let slice_self_const_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyConstPtr slice_t_ann;
      preg = None; is_mut = false } in
  let slice_length_method =
    mk_vec_method "length" [ slice_self_const_ptr_param ] (Some u32_t)
      [ Ast.Return (Some (field (Ast.Var ("self", pos)) "len"), pos) ] in
  let slice_impl = {
    Ast.itparams = ["T"]; itbounds = []; itrait = None; iassoc = [];
    itarget = ["Slice"];
    iitems = [ slice_length_method ];
    ipos = pos;
  } in
  (* VecIter::next - by-value cursor.  `data` is a `*const T` view
     over the original vec; the iterator advances `pos` until it
     hits `len`, yielding copies (value-T sound today). *)
  let vec_iter_self_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyPtr vec_iter_t_ann;
      preg = None; is_mut = false } in
  let vec_iter_next_body =
    let self_v = Ast.Var ("self", pos) in
    [
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.GtEq (field self_v "pos") (field self_v "len");
        then_blk = [
          Ast.Return (Some (Ast.Call {
            callee = ["Option"; "None"]; args = []; pos }), pos);
        ];
        else_blk = None; pos });
      Ast.Let { name = "s"; is_mut = false; ty_ann = Some slice_t_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", field self_v "data");
                    ("len", field self_v "len");
                  ]; base = None; pos }; pos };
      Ast.Let { name = "v"; is_mut = false; ty_ann = Some (tvar "T");
                value = Ast.Index { base = Ast.Var ("s", pos);
                                    index = field self_v "pos"; pos };
                pos };
      Ast.AssignField { target = self_v; field = "pos";
                        value = bin Ast.Add (field self_v "pos")
                                            (u32_lit 1);
                        pos };
      Ast.Return (Some (Ast.Call {
        callee = ["Option"; "Some"];
        args = [ Ast.Var ("v", pos) ];
        pos }), pos);
    ]
  in
  let vec_iter_next_method = {
    Ast.name = "next"; c_name = "next"; tparams = []; tbounds = [];
    params = [ vec_iter_self_ptr_param ];
    ret_ty = Some option_t_ann;
    body = vec_iter_next_body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos;
  } in
  let vec_iter_impl = {
    Ast.itparams = ["T"]; itbounds = []; itrait = Some ["Iterator"];
    iassoc = [("Item", tvar "T")];
    itarget = ["VecIter"];
    iitems = [ vec_iter_next_method ];
    ipos = pos;
  } in
  (* `HashMap<K, V>` — open-addressing linear-probing table.
     Flat `*Slot<K,V>` buffer over `Allocator`; `@move` so the buffer
     can't be silently aliased.  Per DR-007: `K: Hash + Eq` (the
     bounds aren't recorded on the impl tparams today — mono catches
     the missing impl at instantiation), hash cached in the slot
     so probe skips look at `u32` before `K::eq`.  Full API:
     `with_capacity` / `len` / `contains` / `get` / `insert` /
     `remove` (tombstone) / `iter` (HashMapIter cursor). *)
  let slot_t_ann =
    Ast.TyStruct { path = ["Slot"]; args = [ tvar "K"; tvar "V" ] } in
  let hashmap_t_ann =
    Ast.TyStruct { path = ["HashMap"]; args = [ tvar "K"; tvar "V" ] } in
  let option_v_ann =
    Ast.TyStruct { path = ["Option"]; args = [ tvar "V" ] } in
  let slice_slot_ann =
    Ast.TyStruct { path = ["Slice"]; args = [ slot_t_ann ] } in
  let slot_struct = {
    Ast.sname = "Slot";
    stparams = ["K"; "V"];
    sfields = [
      ("state", u8_t);    (* 0=Empty, 1=Occupied, 2=Tombstone *)
      ("hash", u32_t);
      ("key", tvar "K");
      ("value", tvar "V");
    ];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let hashmap_struct = {
    Ast.sname = "HashMap";
    stparams = ["K"; "V"];
    sfields = [
      (* DR-045: HashMap owns its slot buffer through `own *Slot`. *)
      ("slots", Ast.TyOwnPtr slot_t_ann);
      ("count", u32_t);
      ("cap", u32_t);
      ("alloc", alloc_ann_v);
    ];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let hm_self_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyPtr hashmap_t_ann;
      preg = None; is_mut = false } in
  let hm_self_const_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyConstPtr hashmap_t_ann;
      preg = None; is_mut = false } in
  let mk_hm_method ?(is_pub = true) name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = []; params;
    ret_ty = ret; body; is_pub; is_extern = false;
    is_variadic = false; tier_hint = Some "full"; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos;
  } in
  let size_of_slot =
    Ast.Cast (Ast.SizeOf (slot_t_ann, pos), u32_t, pos) in
  let local_slice_of self_v =
    Ast.StructLit {
      tname = ["Slice"];
      fields = [
        ("ptr", Ast.Cast (field self_v "slots",
                          Ast.TyConstPtr slot_t_ann, pos));
        ("len", field self_v "cap");
      ]; base = None; pos }
  in
  (* with_capacity(a, hint): cap = next-pow-of-2(max(hint, 8)); alloc
     cap * size_of(Slot<K,V>) bytes and mem_zero them so every slot's
     `state` byte reads Empty (0).  K / V payloads stay zeroed too —
     unused until a slot transitions to Occupied.  The power-of-2
     invariant (M2) lets every probe compute `h & (cap - 1)` instead
     of `h % cap`, swapping 68k DIVU (~140cy) for AND (~4cy). *)
  let hm_with_capacity_body = [
    Ast.Let { name = "raw"; is_mut = false; ty_ann = Some u32_t;
              value = Ast.If {
                cond = bin Ast.Lt (Ast.Var ("hint", pos)) (u32_lit 8);
                then_blk = [ Ast.Tail (u32_lit 8) ];
                else_blk = Some [ Ast.Tail (Ast.Var ("hint", pos)) ];
                pos }; pos };
    (* Round `raw` up to the next power of 2 by doubling from 1
       until we meet it.  Tiny loop (≤ 30 iters for u32) — runs
       once at construction, not on every probe. *)
    Ast.Let { name = "cap"; is_mut = true; ty_ann = Some u32_t;
              value = u32_lit 1; pos };
    Ast.While {
      cond = bin Ast.Lt (Ast.Var ("cap", pos)) (Ast.Var ("raw", pos));
      body = [
        Ast.Assign { path = ["cap"];
                     value = bin Ast.Shl
                       (Ast.Var ("cap", pos)) (u32_lit 1); pos };
      ];
    };
    Ast.Let { name = "bytes"; is_mut = false; ty_ann = Some u32_t;
              value = bin Ast.Mul size_of_slot
                                  (Ast.Var ("cap", pos)); pos };
    (* DR-045: HashMap::slots is `own *Slot`. *)
    Ast.Let { name = "p"; is_mut = false;
              ty_ann = Some (Ast.TyOwnPtr slot_t_ann);
              value = Ast.Cast (
                methcall (Ast.Var ("a", pos)) "alloc_fn"
                  [ field (Ast.Var ("a", pos)) "state";
                    Ast.Var ("bytes", pos) ],
                Ast.TyOwnPtr slot_t_ann, pos); pos };
    Ast.ExprStmt (Ast.Call {
      callee = ["mem_zero"];
      args = [ Ast.Var ("p", pos); Ast.Var ("bytes", pos) ];
      pos });
    Ast.Return (Some (Ast.StructLit {
      tname = ["HashMap"];
      fields = [
        ("slots", Ast.Var ("p", pos));
        ("count", u32_lit 0);
        ("cap", Ast.Var ("cap", pos));
        ("alloc", Ast.Var ("a", pos));
      ]; base = None; pos }), pos);
  ] in
  let hm_with_capacity_method =
    mk_hm_method "with_capacity"
      [ { Ast.pname = "a"; pty = alloc_ann_v;
          preg = None; is_mut = false };
        { Ast.pname = "hint"; pty = u32_t;
          preg = None; is_mut = false } ]
      (Some hashmap_t_ann) hm_with_capacity_body in
  let hm_len_method =
    mk_hm_method "length" [hm_self_const_ptr_param] (Some u32_t)
      [ Ast.Return (Some (field (Ast.Var ("self", pos)) "count"), pos) ] in
  (* contains( * self, k): hash k, linear probe; Occupied + matching
     hash + key.eq(slot.key) → true.  Empty stops the probe;
     Tombstone keeps scanning. *)
  let hm_contains_body =
    let self_v = Ast.Var ("self", pos) in
    let k_v = Ast.Var ("k", pos) in
    [
      Ast.Let { name = "h"; is_mut = false; ty_ann = Some u32_t;
                value = methcall k_v "hash" []; pos };
      Ast.Let { name = "view"; is_mut = false; ty_ann = Some slice_slot_ann;
                value = local_slice_of self_v; pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = bin Ast.BitAnd (Ast.Var ("h", pos))
                                       (bin Ast.Sub
                                          (field self_v "cap")
                                          (u32_lit 1)); pos };
      Ast.While { cond = Ast.BoolLit (true, pos); body = [
        Ast.Let { name = "s"; is_mut = false; ty_ann = Some slot_t_ann;
                  value = Ast.Index { base = Ast.Var ("view", pos);
                                      index = Ast.Var ("i", pos); pos };
                  pos };
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.EqEq (field (Ast.Var ("s", pos)) "state")
                              (int_lit_as 0 u8_t);
          then_blk = [
            Ast.Return (Some (Ast.BoolLit (false, pos)), pos);
          ];
          else_blk = None; pos });
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.And
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "state")
                      (int_lit_as 1 u8_t))
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "hash")
                      (Ast.Var ("h", pos)));
          then_blk = [
            Ast.ExprStmt (Ast.If {
              cond = methcall k_v "eq"
                       [ field (Ast.Var ("s", pos)) "key" ];
              then_blk = [
                Ast.Return (Some (Ast.BoolLit (true, pos)), pos);
              ];
              else_blk = None; pos });
          ];
          else_blk = None; pos });
        Ast.Assign { path = ["i"];
                     value = bin Ast.BitAnd
                       (bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1))
                       (bin Ast.Sub (field self_v "cap") (u32_lit 1));
                     pos };
      ] };
    ]
  in
  let hm_contains_method =
    mk_hm_method "contains"
      [ hm_self_const_ptr_param;
        { Ast.pname = "k"; pty = tvar "K";
          preg = None; is_mut = false } ]
      (Some Ast.TyBool) hm_contains_body in
  (* get( * self, k): same probe; on key match return Some(slot.value),
     on Empty return None. *)
  let hm_get_body =
    let self_v = Ast.Var ("self", pos) in
    let k_v = Ast.Var ("k", pos) in
    [
      Ast.Let { name = "h"; is_mut = false; ty_ann = Some u32_t;
                value = methcall k_v "hash" []; pos };
      Ast.Let { name = "view"; is_mut = false; ty_ann = Some slice_slot_ann;
                value = local_slice_of self_v; pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = bin Ast.BitAnd (Ast.Var ("h", pos))
                                       (bin Ast.Sub
                                          (field self_v "cap")
                                          (u32_lit 1)); pos };
      Ast.While { cond = Ast.BoolLit (true, pos); body = [
        Ast.Let { name = "s"; is_mut = false; ty_ann = Some slot_t_ann;
                  value = Ast.Index { base = Ast.Var ("view", pos);
                                      index = Ast.Var ("i", pos); pos };
                  pos };
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.EqEq (field (Ast.Var ("s", pos)) "state")
                              (int_lit_as 0 u8_t);
          then_blk = [
            Ast.Return (Some (Ast.Call {
              callee = ["Option"; "None"]; args = []; pos }), pos);
          ];
          else_blk = None; pos });
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.And
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "state")
                      (int_lit_as 1 u8_t))
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "hash")
                      (Ast.Var ("h", pos)));
          then_blk = [
            Ast.ExprStmt (Ast.If {
              cond = methcall k_v "eq"
                       [ field (Ast.Var ("s", pos)) "key" ];
              then_blk = [
                Ast.Return (Some (Ast.Call {
                  callee = ["Option"; "Some"];
                  args = [ field (Ast.Var ("s", pos)) "value" ]; pos }),
                  pos);
              ];
              else_blk = None; pos });
          ];
          else_blk = None; pos });
        Ast.Assign { path = ["i"];
                     value = bin Ast.BitAnd
                       (bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1))
                       (bin Ast.Sub (field self_v "cap") (u32_lit 1));
                     pos };
      ] };
    ]
  in
  let hm_get_method =
    mk_hm_method "get"
      [ hm_self_const_ptr_param;
        { Ast.pname = "k"; pty = tvar "K";
          preg = None; is_mut = false } ]
      (Some option_v_ann) hm_get_body in
  (* insert( * self, k, v): probe; first Empty/Tombstone or matching
     Occupied → write slot.  No grow yet — v1 hard-errors via debug
     assertion if you fill the table (count == cap).  Grow lands in
     the follow-up. *)
  (* grow( * self, new_cap): alloc a fresh zeroed buffer, walk the
     old slots and re-probe every Occupied entry into the new layout
     (cached `hash` lets us skip a re-hash), free the old buffer,
     swap the pointer + cap.  Tombstones are dropped — only live
     entries are re-inserted.  `count` stays accurate. *)
  let hm_grow_body =
    let self_v = Ast.Var ("self", pos) in
    let self_alloc = field self_v "alloc" in
    let size_of_slot_u32 =
      bin Ast.Mul size_of_slot (Ast.Var ("new_cap", pos)) in
    [
      Ast.Let { name = "bytes"; is_mut = false; ty_ann = Some u32_t;
                value = size_of_slot_u32; pos };
      (* DR-045: HashMap grow allocates an owned slot buffer. *)
      Ast.Let { name = "new_p"; is_mut = false;
                ty_ann = Some (Ast.TyOwnPtr slot_t_ann);
                value = Ast.Cast (
                  methcall self_alloc "alloc_fn"
                    [ field self_alloc "state";
                      Ast.Var ("bytes", pos) ],
                  Ast.TyOwnPtr slot_t_ann, pos); pos };
      Ast.ExprStmt (Ast.Call {
        callee = ["mem_zero"];
        args = [ Ast.Var ("new_p", pos); Ast.Var ("bytes", pos) ];
        pos });
      Ast.Let { name = "old_cap"; is_mut = false; ty_ann = Some u32_t;
                value = field self_v "cap"; pos };
      Ast.Let { name = "old_view"; is_mut = false;
                ty_ann = Some slice_slot_ann;
                value = local_slice_of self_v; pos };
      Ast.Let { name = "j"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While {
        cond = bin Ast.Lt (Ast.Var ("j", pos))
                          (Ast.Var ("old_cap", pos));
        body = [
          Ast.Let { name = "s"; is_mut = false; ty_ann = Some slot_t_ann;
                    value = Ast.Index { base = Ast.Var ("old_view", pos);
                                        index = Ast.Var ("j", pos); pos };
                    pos };
          Ast.ExprStmt (Ast.If {
            cond = bin Ast.EqEq (field (Ast.Var ("s", pos)) "state")
                                (int_lit_as 1 u8_t);
            then_blk = [
              (* Re-probe `s` into the new buffer using its cached hash. *)
              Ast.Let { name = "new_view"; is_mut = false;
                        ty_ann = Some slice_slot_ann;
                        value = Ast.StructLit {
                          tname = ["Slice"];
                          fields = [
                            ("ptr", Ast.Cast (Ast.Var ("new_p", pos),
                                              Ast.TyConstPtr slot_t_ann, pos));
                            ("len", Ast.Var ("new_cap", pos));
                          ]; base = None; pos }; pos };
              Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                        value = bin Ast.BitAnd
                          (field (Ast.Var ("s", pos)) "hash")
                          (bin Ast.Sub (Ast.Var ("new_cap", pos))
                                       (u32_lit 1));
                        pos };
              Ast.While { cond = Ast.BoolLit (true, pos); body = [
                Ast.Let { name = "n"; is_mut = false; ty_ann = Some slot_t_ann;
                          value = Ast.Index {
                            base = Ast.Var ("new_view", pos);
                            index = Ast.Var ("i", pos); pos };
                          pos };
                Ast.ExprStmt (Ast.If {
                  cond = bin Ast.NotEq
                           (field (Ast.Var ("n", pos)) "state")
                           (int_lit_as 1 u8_t);
                  then_blk = [
                    Ast.AssignIndex {
                      base = Ast.Var ("new_p", pos);
                      index = Ast.Var ("i", pos);
                      value = Ast.StructLit {
                        tname = ["Slot"];
                        fields = [
                          ("state", int_lit_as 1 u8_t);
                          ("hash", field (Ast.Var ("s", pos)) "hash");
                          ("key",   field (Ast.Var ("s", pos)) "key");
                          ("value", field (Ast.Var ("s", pos)) "value");
                        ]; base = None; pos };
                      pos };
                    Ast.Break pos;
                  ];
                  else_blk = None; pos });
                Ast.Assign { path = ["i"];
                             value = bin Ast.BitAnd
                               (bin Ast.Add (Ast.Var ("i", pos))
                                            (u32_lit 1))
                               (bin Ast.Sub (Ast.Var ("new_cap", pos))
                                            (u32_lit 1));
                             pos };
              ] };
            ];
            else_blk = None; pos });
          Ast.Assign { path = ["j"];
                       value = bin Ast.Add (Ast.Var ("j", pos))
                                           (u32_lit 1); pos };
        ] };
      (* Release the old buffer; element heap was already moved into
         new slots above, so byte-level free is enough. *)
      Ast.Let { name = "old_bytes"; is_mut = false; ty_ann = Some u32_t;
                value = bin Ast.Mul size_of_slot
                                    (Ast.Var ("old_cap", pos)); pos };
      Ast.ExprStmt (methcall self_alloc "free_fn"
        [ field self_alloc "state";
          Ast.Cast (field self_v "slots", cvoid_ptr, pos);
          Ast.Var ("old_bytes", pos) ]);
      Ast.AssignField { target = self_v; field = "slots";
                        value = Ast.Var ("new_p", pos); pos };
      Ast.AssignField { target = self_v; field = "cap";
                        value = Ast.Var ("new_cap", pos); pos };
    ]
  in
  let hm_grow_method =
    mk_hm_method ~is_pub:false "grow"
      [ hm_self_ptr_param;
        { Ast.pname = "new_cap"; pty = u32_t;
          preg = None; is_mut = false } ]
      None hm_grow_body in
  let hm_insert_body =
    let self_v = Ast.Var ("self", pos) in
    let k_v = Ast.Var ("k", pos) in
    let v_v = Ast.Var ("v", pos) in
    [
      (* Load-factor check: grow when (count + 1) * 4 > cap * 3
         (load > 0.75).  Doubles the cap and rehashes every Occupied
         slot — keeps the linear-probe cluster lengths bounded. *)
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.Gt
                 (bin Ast.Mul
                    (bin Ast.Add (field self_v "count") (u32_lit 1))
                    (u32_lit 4))
                 (bin Ast.Mul (field self_v "cap") (u32_lit 3));
        then_blk = [
          Ast.ExprStmt (methcall self_v "grow"
            [ bin Ast.Mul (field self_v "cap") (u32_lit 2) ]);
        ];
        else_blk = None; pos });
      Ast.Let { name = "h"; is_mut = false; ty_ann = Some u32_t;
                value = methcall k_v "hash" []; pos };
      Ast.Let { name = "view"; is_mut = false; ty_ann = Some slice_slot_ann;
                value = local_slice_of self_v; pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = bin Ast.BitAnd (Ast.Var ("h", pos))
                                       (bin Ast.Sub
                                          (field self_v "cap")
                                          (u32_lit 1)); pos };
      Ast.While { cond = Ast.BoolLit (true, pos); body = [
        Ast.Let { name = "s"; is_mut = false; ty_ann = Some slot_t_ann;
                  value = Ast.Index { base = Ast.Var ("view", pos);
                                      index = Ast.Var ("i", pos); pos };
                  pos };
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.NotEq (field (Ast.Var ("s", pos)) "state")
                               (int_lit_as 1 u8_t);
          then_blk = [
            (* Empty or Tombstone → put k,v here and bump count if it
               was Empty (Tombstone means count is already incremented
               from a prior alive entry, but we removed it… handled
               more carefully once `remove` lands; v1 always bumps). *)
            Ast.AssignIndex {
              base = field self_v "slots";
              index = Ast.Var ("i", pos);
              value = Ast.StructLit {
                tname = ["Slot"];
                fields = [
                  ("state", int_lit_as 1 u8_t);
                  ("hash", Ast.Var ("h", pos));
                  ("key", k_v);
                  ("value", v_v);
                ]; base = None; pos };
              pos };
            Ast.AssignField {
              target = self_v; field = "count";
              value = bin Ast.Add (field self_v "count") (u32_lit 1);
              pos };
            Ast.Return (None, pos);
          ];
          else_blk = None; pos });
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.And
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "hash")
                      (Ast.Var ("h", pos)))
                   (methcall k_v "eq"
                      [ field (Ast.Var ("s", pos)) "key" ]);
          then_blk = [
            Ast.AssignIndex {
              base = field self_v "slots";
              index = Ast.Var ("i", pos);
              value = Ast.StructLit {
                tname = ["Slot"];
                fields = [
                  ("state", int_lit_as 1 u8_t);
                  ("hash", Ast.Var ("h", pos));
                  ("key", k_v);
                  ("value", v_v);
                ]; base = None; pos };
              pos };
            Ast.Return (None, pos);
          ];
          else_blk = None; pos });
        Ast.Assign { path = ["i"];
                     value = bin Ast.BitAnd
                       (bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1))
                       (bin Ast.Sub (field self_v "cap") (u32_lit 1));
                     pos };
      ] };
    ]
  in
  let hm_insert_method =
    mk_hm_method "insert"
      [ hm_self_ptr_param;
        { Ast.pname = "k"; pty = tvar "K";
          preg = None; is_mut = false };
        { Ast.pname = "v"; pty = tvar "V";
          preg = None; is_mut = false } ]
      None hm_insert_body in
  (* remove( * self, k): find slot with matching key, mark it
     Tombstone, decrement count.  Returns silently if the key isn't
     present.  v1 keeps the old key/value bytes in the tombstoned
     slot (state byte alone changes) — fine for value K/V; @move K
     leaves its heap stranded in the slot until the buffer is freed
     wholesale (parity with the insert/grow caveat). *)
  let hm_remove_body =
    let self_v = Ast.Var ("self", pos) in
    let k_v = Ast.Var ("k", pos) in
    [
      Ast.Let { name = "h"; is_mut = false; ty_ann = Some u32_t;
                value = methcall k_v "hash" []; pos };
      Ast.Let { name = "view"; is_mut = false; ty_ann = Some slice_slot_ann;
                value = local_slice_of self_v; pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = bin Ast.BitAnd (Ast.Var ("h", pos))
                                       (bin Ast.Sub
                                          (field self_v "cap")
                                          (u32_lit 1)); pos };
      Ast.While { cond = Ast.BoolLit (true, pos); body = [
        Ast.Let { name = "s"; is_mut = false; ty_ann = Some slot_t_ann;
                  value = Ast.Index { base = Ast.Var ("view", pos);
                                      index = Ast.Var ("i", pos); pos };
                  pos };
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.EqEq (field (Ast.Var ("s", pos)) "state")
                              (int_lit_as 0 u8_t);
          then_blk = [ Ast.Return (None, pos) ];
          else_blk = None; pos });
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.And
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "state")
                      (int_lit_as 1 u8_t))
                   (bin Ast.EqEq
                      (field (Ast.Var ("s", pos)) "hash")
                      (Ast.Var ("h", pos)));
          then_blk = [
            Ast.ExprStmt (Ast.If {
              cond = methcall k_v "eq"
                       [ field (Ast.Var ("s", pos)) "key" ];
              then_blk = [
                Ast.AssignIndex {
                  base = field self_v "slots";
                  index = Ast.Var ("i", pos);
                  value = Ast.StructLit {
                    tname = ["Slot"];
                    fields = [
                      ("state", int_lit_as 2 u8_t);
                      ("hash", field (Ast.Var ("s", pos)) "hash");
                      ("key",   field (Ast.Var ("s", pos)) "key");
                      ("value", field (Ast.Var ("s", pos)) "value");
                    ]; base = None; pos };
                  pos };
                Ast.AssignField {
                  target = self_v; field = "count";
                  value = bin Ast.Sub (field self_v "count") (u32_lit 1);
                  pos };
                Ast.Return (None, pos);
              ];
              else_blk = None; pos });
          ];
          else_blk = None; pos });
        Ast.Assign { path = ["i"];
                     value = bin Ast.BitAnd
                       (bin Ast.Add (Ast.Var ("i", pos)) (u32_lit 1))
                       (bin Ast.Sub (field self_v "cap") (u32_lit 1));
                     pos };
      ] };
    ]
  in
  let hm_remove_method =
    mk_hm_method "remove"
      [ hm_self_ptr_param;
        { Ast.pname = "k"; pty = tvar "K";
          preg = None; is_mut = false } ]
      None hm_remove_body in
  (* HashMapIter<K, V> — by-value cursor over Occupied slots.
     `data` aliases the live slot buffer (read-only); `pos`/`len`
     advance / cap the walk.  Yields `(K, V)` tuples; user can
     destructure with `let (k, v) = ...;`. *)
  let hashmap_iter_t_ann =
    Ast.TyStruct { path = ["HashMapIter"];
                   args = [ tvar "K"; tvar "V" ] } in
  let kv_tuple_ann = Ast.TyTuple [ tvar "K"; tvar "V" ] in
  let option_kv_ann =
    Ast.TyStruct { path = ["Option"]; args = [ kv_tuple_ann ] } in
  let hashmap_iter_struct = {
    Ast.sname = "HashMapIter";
    stparams = ["K"; "V"];
    sfields = [
      ("data", Ast.TyConstPtr slot_t_ann);
      ("len", u32_t);
      ("pos", u32_t);
    ];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let hm_iter_method =
    mk_hm_method "iter" [hm_self_const_ptr_param]
      (Some hashmap_iter_t_ann)
      [ Ast.Return (Some (Ast.StructLit {
          tname = ["HashMapIter"];
          fields = [
            ("data", Ast.Cast (field (Ast.Var ("self", pos)) "slots",
                               Ast.TyConstPtr slot_t_ann, pos));
            ("len", field (Ast.Var ("self", pos)) "cap");
            ("pos", u32_lit 0);
          ]; base = None; pos }), pos) ] in
  let hashmap_iter_self_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyPtr hashmap_iter_t_ann;
      preg = None; is_mut = false } in
  (* next( * self): walk forward from `pos` until an Occupied slot,
     emit `Some((k, v))` and advance past it; out-of-bounds returns
     `None`.  Tombstones and Emptys are skipped. *)
  let hashmap_iter_next_body =
    let self_v = Ast.Var ("self", pos) in
    [
      Ast.Let { name = "view"; is_mut = false; ty_ann = Some slice_slot_ann;
                value = Ast.StructLit {
                  tname = ["Slice"];
                  fields = [
                    ("ptr", field self_v "data");
                    ("len", field self_v "len");
                  ]; base = None; pos };
                pos };
      Ast.While {
        cond = bin Ast.Lt (field self_v "pos") (field self_v "len");
        body = [
          Ast.Let { name = "s"; is_mut = false; ty_ann = Some slot_t_ann;
                    value = Ast.Index { base = Ast.Var ("view", pos);
                                        index = field self_v "pos"; pos };
                    pos };
          Ast.AssignField { target = self_v; field = "pos";
                            value = bin Ast.Add (field self_v "pos")
                                                (u32_lit 1); pos };
          Ast.ExprStmt (Ast.If {
            cond = bin Ast.EqEq (field (Ast.Var ("s", pos)) "state")
                                (int_lit_as 1 u8_t);
            then_blk = [
              Ast.Return (Some (Ast.Call {
                callee = ["Option"; "Some"];
                args = [ Ast.TupleLit (
                  [ field (Ast.Var ("s", pos)) "key";
                    field (Ast.Var ("s", pos)) "value" ], pos) ];
                pos }), pos);
            ];
            else_blk = None; pos });
        ] };
      Ast.Return (Some (Ast.Call {
        callee = ["Option"; "None"]; args = []; pos }), pos);
    ]
  in
  let hashmap_iter_next_method = {
    Ast.name = "next"; c_name = "next"; tparams = []; tbounds = [];
    params = [ hashmap_iter_self_ptr_param ];
    ret_ty = Some option_kv_ann;
    body = hashmap_iter_next_body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos;
  } in
  let hashmap_iter_impl = {
    Ast.itparams = ["K"; "V"]; itbounds = []; itrait = Some ["Iterator"];
    iassoc = [("Item", kv_tuple_ann)];
    itarget = ["HashMapIter"];
    iitems = [ hashmap_iter_next_method ];
    ipos = pos;
  } in
  let hashmap_impl = {
    Ast.itparams = ["K"; "V"]; itbounds = []; itrait = None; iassoc = [];
    itarget = ["HashMap"];
    iitems = [
      hm_with_capacity_method;
      hm_len_method;
      hm_grow_method;
      hm_contains_method;
      hm_get_method;
      hm_insert_method;
      hm_remove_method;
      hm_iter_method;
    ];
    ipos = pos;
  } in
  (* `Iterator` — the prelude iteration protocol.  `for x in <value>`
     desugars to `loop { match value.next() { Some(x) => … | None =>
     break } }` for any type that `impl Iterator`.  `next` takes `*self`
     so it can advance the iterator's state.  Skipped by prepend_prelude
     if the user declares their own `trait Iterator`. *)
  let iter_next =
    { Ast.name = "next"; c_name = "next"; tparams = []; tbounds = [];
      params = [ { Ast.pname = "self"; pty = Ast.TyPtr Ast.TySelf;
                   preg = None; is_mut = false } ];
      ret_ty = Some (Ast.TyStruct
        { path = ["Option"];
          args = [ Ast.TyStruct { path = ["Self"; "Item"]; args = [] } ] });
      body = []; is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None; must_use = false; escapes_hatch = false; pos }
  in
  (* DR-026 Step B — Iterator.map default-method.  Builds the Map<Self,
     F> adapter from `self` (by-value, consumes the iterator) plus a
     user-supplied `f: F` callable.  The body is a plain struct literal
     — Mono picks Map's tparams per use site (Self pinned by impl
     target, F pinned by callsite arg).  Self is written as bare
     TySelf which parse_impl_block / specialise_default substitute
     with the impl target at synthesis time. *)
  let iter_map_default =
    { Ast.name = "map"; c_name = "map";
      tparams = ["F"]; tbounds = [("F", ["Fn1"], [])];
      params = [
        { Ast.pname = "self"; pty = Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "f";
          pty = Ast.TyStruct { path = ["F"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct {
        path = ["Map"];
        args = [
          Ast.TySelf;
          Ast.TyStruct { path = ["F"]; args = [] };
        ] });
      body = [
        Ast.Tail (Ast.StructLit {
          tname = ["Map"];
          fields = [
            ("inner", Ast.Var ("self", pos));
            ("f", Ast.Var ("f", pos));
          ];
          base = None; pos })
      ];
      is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  (* DR-026 Step D — Iterator.take(n) default-method.  Builds a
     Take<Self> from `self` (by-value, consumes the iterator) plus a
     `u32` cap.  Single struct-literal body; Mono pins Take's tparam
     `I` to the impl target at each callsite. *)
  let iter_take_default =
    { Ast.name = "take"; c_name = "take";
      tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self"; pty = Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "n"; pty = u32_t;
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct {
        path = ["Take"];
        args = [ Ast.TySelf ] });
      body = [
        Ast.Tail (Ast.StructLit {
          tname = ["Take"];
          fields = [
            ("inner", Ast.Var ("self", pos));
            ("remaining", Ast.Var ("n", pos));
          ];
          base = None; pos })
      ];
      is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  (* DR-026 Step D — Iterator.enumerate() default-method.  Builds an
     Enumerate<Self> with idx pre-seeded to 0; each yielded item is a
     (u32, I::Item) pair from the inner iterator's next(). *)
  let iter_enumerate_default =
    { Ast.name = "enumerate"; c_name = "enumerate";
      tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self"; pty = Ast.TySelf;
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct {
        path = ["Enumerate"];
        args = [ Ast.TySelf ] });
      body = [
        Ast.Tail (Ast.StructLit {
          tname = ["Enumerate"];
          fields = [
            ("inner", Ast.Var ("self", pos));
            ("idx", u32_lit 0);
          ];
          base = None; pos })
      ];
      is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  (* DR-026 Step E - `Iterator.fold(init, f)` consuming terminal.
     Folds the iterator into a single accumulator via Fn2:
     `acc = f.call(acc, x)` for every yielded `x`.  Generic over
     `B` (the accumulator/return type); the `F: Fn2<Arg1=B,
     Arg2=Self::Item, Output=B>` bound pins the callable's shape
     bidirectionally.  By-value `self` is the consume signal —
     after `fold` returns, the iterator is drained.

     Body uses `for v in self` which desugars through the same
     `Iterator::next` protocol the trait is defining (mono fuses
     it with the impl's next on every concrete instantiation), so
     the terminal lowers to a single `while next != None` loop
     wrapping the Fn2-call. *)
  let iter_fold_default =
    let acc_ann = Ast.TyStruct { path = ["Acc"]; args = [] } in
    { Ast.name = "fold"; c_name = "fold";
      tparams = ["Acc"; "G"];
      tbounds = [ ("G", ["Fn2"], []) ];
      params = [
        { Ast.pname = "self"; pty = Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "init"; pty = acc_ann;
          preg = None; is_mut = false };
        { Ast.pname = "f";
          pty = Ast.TyStruct { path = ["G"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some acc_ann;
      body = [
        Ast.Let { name = "acc"; is_mut = true; ty_ann = Some acc_ann;
                  value = Ast.Var ("init", pos); pos };
        Ast.For {
          var = "v";
          range = Ast.Var ("self", pos);
          body = [
            Ast.Assign {
              path = ["acc"];
              value = methcall (Ast.Var ("f", pos)) "call"
                        [ Ast.Var ("acc", pos);
                          Ast.Var ("v", pos) ];
              pos };
          ];
          pos };
        Ast.Tail (Ast.Var ("acc", pos));
      ];
      is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = true; escapes_hatch = false; pos }
  in
  (* DR-026 Step E - `Iterator.collect(a)` consuming terminal.
     Drains the iterator into a fresh `Vec<Self::Item>` allocated
     through the passed-in `Allocator`.  Initial capacity is the
     prelude floor (8); push grows on overflow.  By-value `self`
     consumes the iterator (same drain rule as fold).

     Item type plumbs through `Self::Item` — DR-027 site-1 resolves
     it at every concrete-iterator use site, so the returned
     `Vec<Self::Item>` becomes e.g. `Vec<int>` for `VecIter<int>`
     or `Vec<(u32, int)>` for `Enumerate<VecIter<int>>`. *)
  let iter_collect_default =
    let item_ann = Ast.TyStruct { path = ["Self"; "Item"]; args = [] } in
    let collect_vec_ann =
      Ast.TyStruct { path = ["Vec"]; args = [ item_ann ] } in
    { Ast.name = "collect"; c_name = "collect";
      tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self"; pty = Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "a"; pty = alloc_ann_v;
          preg = None; is_mut = false };
      ];
      ret_ty = Some collect_vec_ann;
      body = [
        Ast.Let { name = "out"; is_mut = true;
                  ty_ann = Some collect_vec_ann;
                  value = Ast.Call {
                    callee = ["Vec"; "with_capacity"];
                    args = [
                      Ast.Var ("a", pos);
                      u32_lit 8;
                    ]; pos };
                  pos };
        Ast.For {
          var = "v";
          range = Ast.Var ("self", pos);
          body = [
            Ast.ExprStmt (methcall (Ast.Var ("out", pos)) "push"
                            [ Ast.Var ("v", pos) ]);
          ];
          pos };
        Ast.Tail (Ast.Var ("out", pos));
      ];
      is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = true; escapes_hatch = false; pos }
  in
  (* DR-026 Step C - `Iterator.filter(p)` default-method.  Builds a
     `Filter<Self, P>` adapter from `self` (by-value, consumes the
     iterator) plus a predicate `p: P` callable returning bool.
     Same struct-literal shape as `take`/`enumerate` defaults; mono
     pins `Self` to the impl target + `P` to the callsite arg
     type. *)
  let iter_filter_default =
    { Ast.name = "filter"; c_name = "filter";
      tparams = ["P"]; tbounds = [("P", ["Fn1"], [])];
      params = [
        { Ast.pname = "self"; pty = Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "p";
          pty = Ast.TyStruct { path = ["P"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct {
        path = ["Filter"];
        args = [
          Ast.TySelf;
          Ast.TyStruct { path = ["P"]; args = [] };
        ] });
      body = [
        Ast.Tail (Ast.StructLit {
          tname = ["Filter"];
          fields = [
            ("inner", Ast.Var ("self", pos));
            ("p", Ast.Var ("p", pos));
          ];
          base = None; pos })
      ];
      is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  let iterator_trait = {
    Ast.trname = "Iterator"; trassoc = ["Item"]; trsupers = [];
    trmethods = [ iter_next; iter_map_default;
                  iter_take_default; iter_enumerate_default;
                  iter_filter_default;
                  iter_fold_default; iter_collect_default ];
    trdefaults = ["map"; "take"; "enumerate"; "filter";
                  "fold"; "collect"];
    trpos = pos; tris_pub = true;
  } in
  (* `Fn1` / `Fn2` — callable protocols.  Per the DR-015 reality-check
     (2026-06-04) Fn is encoded as a real per-arity trait whose argument
     and result types are associated types, not trait tparams: exile
     has no generic-trait surface and assoc-types already work in bound
     position + bodies.  `impl Fn1 for Foo { type Arg = T; type Output =
     U; fn call(self-const-ptr, a: T) -> U { ... } }` lets a generic
     adapter (`Map<I: Iterator, F: Fn1>`) drive both the in-type and
     out-type through `F::Arg` and `F::Output`.  v1 ships Fn1 (unary,
     drives map/filter/take/enumerate) and Fn2 (binary, drives fold's
     accumulator).  Higher arities ship when a combinator actually
     wants one — Fn3+/zip stay out of v1. *)
  let fn0_call =
    { Ast.name = "call"; c_name = "call"; tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self";
          pty = Ast.TyConstPtr Ast.TySelf;
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct { path = ["Self"; "Output"]; args = [] });
      body = []; is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  let fn0_trait = {
    Ast.trname = "Fn0"; trassoc = ["Output"]; trsupers = [];
    trmethods = [ fn0_call ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  let fn1_call =
    { Ast.name = "call"; c_name = "call"; tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self";
          pty = Ast.TyConstPtr Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "a";
          pty = Ast.TyStruct { path = ["Self"; "Arg"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct { path = ["Self"; "Output"]; args = [] });
      body = []; is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  let fn1_trait = {
    Ast.trname = "Fn1"; trassoc = ["Arg"; "Output"]; trsupers = [];
    trmethods = [ fn1_call ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  let fn2_call =
    { Ast.name = "call"; c_name = "call"; tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self";
          pty = Ast.TyConstPtr Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "a";
          pty = Ast.TyStruct { path = ["Self"; "Arg1"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "b";
          pty = Ast.TyStruct { path = ["Self"; "Arg2"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct { path = ["Self"; "Output"]; args = [] });
      body = []; is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  let fn2_trait = {
    Ast.trname = "Fn2"; trassoc = ["Arg1"; "Arg2"; "Output"]; trsupers = [];
    trmethods = [ fn2_call ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  (* DR-029 - Fn3 and Fn4 prelude traits, ternary and quaternary
     callable protocols.  Parser sugar `|A, B, C|->R` / `|A, B, C,
     D|->R` was already arity-agnostic (`Fn{arity}` naming, Arg{N}
     numbering), so this just lights up the bound sugar end-to-end
     for the higher-arity cases.  Useful when a combinator-style
     callback needs more than two values (zip-style accumulators,
     ternary predicates) without forcing the user to bundle args
     into a tuple. *)
  let fn3_call =
    { Ast.name = "call"; c_name = "call"; tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self";
          pty = Ast.TyConstPtr Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "a";
          pty = Ast.TyStruct { path = ["Self"; "Arg1"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "b";
          pty = Ast.TyStruct { path = ["Self"; "Arg2"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "c";
          pty = Ast.TyStruct { path = ["Self"; "Arg3"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct { path = ["Self"; "Output"]; args = [] });
      body = []; is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  let fn3_trait = {
    Ast.trname = "Fn3";
    trassoc = ["Arg1"; "Arg2"; "Arg3"; "Output"]; trsupers = [];
    trmethods = [ fn3_call ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  let fn4_call =
    { Ast.name = "call"; c_name = "call"; tparams = []; tbounds = [];
      params = [
        { Ast.pname = "self";
          pty = Ast.TyConstPtr Ast.TySelf;
          preg = None; is_mut = false };
        { Ast.pname = "a";
          pty = Ast.TyStruct { path = ["Self"; "Arg1"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "b";
          pty = Ast.TyStruct { path = ["Self"; "Arg2"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "c";
          pty = Ast.TyStruct { path = ["Self"; "Arg3"]; args = [] };
          preg = None; is_mut = false };
        { Ast.pname = "d";
          pty = Ast.TyStruct { path = ["Self"; "Arg4"]; args = [] };
          preg = None; is_mut = false };
      ];
      ret_ty = Some (Ast.TyStruct { path = ["Self"; "Output"]; args = [] });
      body = []; is_pub = true; is_extern = false; is_variadic = false;
      tier_hint = None; amiga_lib = None;
      must_use = false; escapes_hatch = false; pos }
  in
  let fn4_trait = {
    Ast.trname = "Fn4";
    trassoc = ["Arg1"; "Arg2"; "Arg3"; "Arg4"; "Output"]; trsupers = [];
    trmethods = [ fn4_call ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  (* DR-026 combinator stdlib v1 — Step A: `Map<I, F>` adapter struct
     plus its `impl Iterator`.  Manual construction shape:

       let m = Map { inner: v.iter(), f: my_closure };
       for x in m { ... }

     Iterator.map default-method (Step B) ships next, building this
     adapter behind the dot-chain `v.iter().map(|x| ...)`.

     The next-body matches Option from the inner iter; the `Some(v) =>
     Option::Some(self.f(v))` arm uses DR-019 FieldAccess call-desugar
     to lower `self.f(v)` to `(self.f).call(v)`, and the `None =>
     Option::None` arm uses DR-020 bidirectional seed to pick up its
     T from the fn's `Option<F::Output>` ret type.  `F::Output`
     projects via the DR-025 trait-decl shortcut (Fn1 declares Output,
     no impl needed at synth time). *)
  let map_struct = {
    Ast.sname = "Map";
    stparams = ["I"; "F"];
    sfields = [
      ("inner", Ast.TyStruct { path = ["I"]; args = [] });
      ("f", Ast.TyStruct { path = ["F"]; args = [] });
    ];
    spos = pos; sis_pub = true;
    stier_hint = None;
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let map_target_ty =
    Ast.TyStruct {
      path = ["Map"];
      args = [
        Ast.TyStruct { path = ["I"]; args = [] };
        Ast.TyStruct { path = ["F"]; args = [] };
      ] } in
  let map_next_self_param =
    { Ast.pname = "self"; pty = Ast.TyPtr map_target_ty;
      preg = None; is_mut = false } in
  let map_next_body =
    let scrutinee =
      Ast.MethodCall {
        receiver = Ast.FieldAccess (Ast.Var ("self", pos), "inner", pos);
        name = "next"; args = []; pos }
    in
    let some_arm = Ast.{
      pat = PVariant {
        tname = ["Option"]; variant = "Some";
        binds = PBTuple [ PVar ("v", pos) ]; pos };
      guard = None;
      body = EnumLit {
        tname = ["Option"]; variant = "Some";
        args = EATuple [
          MethodCall {
            receiver = FieldAccess (Var ("self", pos), "f", pos);
            name = "call";
            args = [ Var ("v", pos) ];
            pos }
        ]; pos };
      arm_pos = pos } in
    let none_arm = Ast.{
      pat = PVariant {
        tname = ["Option"]; variant = "None";
        binds = PBTuple []; pos };
      guard = None;
      body = EnumLit {
        tname = ["Option"]; variant = "None";
        args = EATuple []; pos };
      arm_pos = pos } in
    [ Ast.Tail (Ast.Match {
        scrutinee; arms = [ some_arm; none_arm ]; pos }) ]
  in
  let map_next_method = {
    Ast.name = "next"; c_name = "next";
    tparams = []; tbounds = [];
    params = [ map_next_self_param ];
    ret_ty = Some (Ast.TyStruct {
      path = ["Option"];
      args = [ Ast.TyStruct {
        path = ["F"; "Output"]; args = [] } ] });
    body = map_next_body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = None; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos
  } in
  let map_iter_impl = {
    Ast.itparams = ["I"; "F"];
    itbounds = [
      ("I", ["Iterator"], []);
      ("F", ["Fn1"], []);
    ];
    itrait = Some ["Iterator"];
    iassoc = [
      ("Item", Ast.TyStruct { path = ["F"; "Output"]; args = [] })
    ];
    itarget = ["Map"];
    iitems = [ map_next_method ];
    ipos = pos;
  } in
  (* DR-026 Step D — `Take<I>` adapter.  `take(n)` yields at most `n`
     items from the inner iterator, then returns None.  Field
     `remaining: u32` counts down; `next` short-circuits to None once
     it reaches 0.  Item type passes through unchanged
     (`I::Item`) — the assoc-projection multi-hop fix (Site-1, DR-027
     refinement 2026-06-06) makes this resolve at every use site. *)
  let take_struct = {
    Ast.sname = "Take";
    stparams = ["I"];
    sfields = [
      ("inner", Ast.TyStruct { path = ["I"]; args = [] });
      ("remaining", u32_t);
    ];
    spos = pos; sis_pub = true;
    stier_hint = None;
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let take_target_ty =
    Ast.TyStruct {
      path = ["Take"];
      args = [ Ast.TyStruct { path = ["I"]; args = [] } ] } in
  let take_next_self_param =
    { Ast.pname = "self"; pty = Ast.TyPtr take_target_ty;
      preg = None; is_mut = false } in
  let take_next_body =
    let self_v = Ast.Var ("self", pos) in
    [
      Ast.ExprStmt (Ast.If {
        cond = bin Ast.EqEq (field self_v "remaining") (u32_lit 0);
        then_blk = [
          Ast.Return (Some (Ast.EnumLit {
            tname = ["Option"]; variant = "None";
            args = Ast.EATuple []; pos }), pos);
        ];
        else_blk = None; pos });
      Ast.AssignField {
        target = self_v; field = "remaining";
        value = bin Ast.Sub (field self_v "remaining") (u32_lit 1);
        pos };
      Ast.Tail (methcall (field self_v "inner") "next" []);
    ]
  in
  let take_next_method = {
    Ast.name = "next"; c_name = "next";
    tparams = []; tbounds = [];
    params = [ take_next_self_param ];
    ret_ty = Some (Ast.TyStruct {
      path = ["Option"];
      args = [ Ast.TyStruct {
        path = ["I"; "Item"]; args = [] } ] });
    body = take_next_body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = None; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos
  } in
  let take_iter_impl = {
    Ast.itparams = ["I"];
    itbounds = [("I", ["Iterator"], [])];
    itrait = Some ["Iterator"];
    iassoc = [
      ("Item", Ast.TyStruct { path = ["I"; "Item"]; args = [] })
    ];
    itarget = ["Take"];
    iitems = [ take_next_method ];
    ipos = pos;
  } in
  (* DR-026 Step D — `Enumerate<I>` adapter.  Pairs every yielded
     value with its zero-based index as a `(u32, I::Item)` tuple.
     `idx: u32` advances after every successful inner `next()`; on
     inner None the index stays put and the outer next returns None.
     Match-arm with a block body (multi-stmt: bump idx + emit pair)
     keeps the assoc-projection path single-hop — the tuple item
     type `(u32, I::Item)` projects through DR-027 site-1 with no
     fresh machinery. *)
  let enumerate_struct = {
    Ast.sname = "Enumerate";
    stparams = ["I"];
    sfields = [
      ("inner", Ast.TyStruct { path = ["I"]; args = [] });
      ("idx", u32_t);
    ];
    spos = pos; sis_pub = true;
    stier_hint = None;
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let enumerate_target_ty =
    Ast.TyStruct {
      path = ["Enumerate"];
      args = [ Ast.TyStruct { path = ["I"]; args = [] } ] } in
  let enumerate_next_self_param =
    { Ast.pname = "self"; pty = Ast.TyPtr enumerate_target_ty;
      preg = None; is_mut = false } in
  let enumerate_item_ann =
    Ast.TyTuple [
      u32_t;
      Ast.TyStruct { path = ["I"; "Item"]; args = [] };
    ] in
  let enumerate_next_body =
    let self_v = Ast.Var ("self", pos) in
    let scrutinee = methcall (field self_v "inner") "next" [] in
    let pair_ann =
      Ast.TyTuple [
        u32_t;
        Ast.TyStruct { path = ["I"; "Item"]; args = [] };
      ] in
    let some_arm = Ast.{
      pat = PVariant {
        tname = ["Option"]; variant = "Some";
        binds = PBTuple [ PVar ("v", pos) ]; pos };
      guard = None;
      body = Block ([
        Let { name = "i"; is_mut = false; ty_ann = Some u32_t;
              value = field self_v "idx"; pos };
        AssignField {
          target = self_v; field = "idx";
          value = bin Ast.Add (Var ("i", pos)) (u32_lit 1);
          pos };
        Let { name = "pair"; is_mut = false; ty_ann = Some pair_ann;
              value = TupleLit ([ Var ("i", pos); Var ("v", pos) ], pos);
              pos };
        Tail (EnumLit {
          tname = ["Option"]; variant = "Some";
          args = EATuple [ Var ("pair", pos) ]; pos });
      ], pos);
      arm_pos = pos } in
    let none_arm = Ast.{
      pat = PVariant {
        tname = ["Option"]; variant = "None";
        binds = PBTuple []; pos };
      guard = None;
      body = EnumLit {
        tname = ["Option"]; variant = "None";
        args = EATuple []; pos };
      arm_pos = pos } in
    [ Ast.Tail (Ast.Match {
        scrutinee; arms = [ some_arm; none_arm ]; pos }) ]
  in
  let enumerate_next_method = {
    Ast.name = "next"; c_name = "next";
    tparams = []; tbounds = [];
    params = [ enumerate_next_self_param ];
    ret_ty = Some (Ast.TyStruct {
      path = ["Option"];
      args = [ enumerate_item_ann ] });
    body = enumerate_next_body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = None; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos
  } in
  let enumerate_iter_impl = {
    Ast.itparams = ["I"];
    itbounds = [("I", ["Iterator"], [])];
    itrait = Some ["Iterator"];
    iassoc = [
      ("Item", enumerate_item_ann)
    ];
    itarget = ["Enumerate"];
    iitems = [ enumerate_next_method ];
    ipos = pos;
  } in
  (* DR-026 Step C - `Filter<I, P>` adapter.  Two-tparam: `I:
     Iterator` provides the upstream stream, `P: Fn1` filters its
     items.  Item type is `I::Item` (single-hop assoc-projection
     through DR-027 site-1, same as Take/Enumerate).

     `next` body sidesteps the if-as-value and block-if-branch
     limitations the worklog flagged for predicate-driven `next`s:
     instead of `if (p) Some else recur`, the body uses a mut
     `keep`/`result` flag pair and a `while keep` outer loop.
     Each iteration pulls one `inner.next()`, and:
       Some(v) - if `p.call(v)` is true, store Some(v) and stop;
                 otherwise drop the value and let the loop re-fire.
       None   - record None and stop.
     The trailing `result` is the matched final value.  Every
     stmt-shaped branch lives inside a Block arm, so the codegen
     path matches Enumerate's existing block-arm flow. *)
  let filter_struct = {
    Ast.sname = "Filter";
    stparams = ["I"; "P"];
    sfields = [
      ("inner", Ast.TyStruct { path = ["I"]; args = [] });
      ("p", Ast.TyStruct { path = ["P"]; args = [] });
    ];
    spos = pos; sis_pub = true;
    stier_hint = None;
    sis_debug = false; sderives = []; sis_move = false;
  } in
  let filter_target_ty =
    Ast.TyStruct {
      path = ["Filter"];
      args = [
        Ast.TyStruct { path = ["I"]; args = [] };
        Ast.TyStruct { path = ["P"]; args = [] };
      ] } in
  let filter_next_self_param =
    { Ast.pname = "self"; pty = Ast.TyPtr filter_target_ty;
      preg = None; is_mut = false } in
  let filter_item_ann =
    Ast.TyStruct { path = ["I"; "Item"]; args = [] } in
  let filter_option_item_ann =
    Ast.TyStruct { path = ["Option"]; args = [ filter_item_ann ] } in
  let filter_next_body =
    let self_v = Ast.Var ("self", pos) in
    let scrutinee = methcall (field self_v "inner") "next" [] in
    let some_arm = Ast.{
      pat = PVariant {
        tname = ["Option"]; variant = "Some";
        binds = PBTuple [ PVar ("v", pos) ]; pos };
      guard = None;
      body = Block ([
        ExprStmt (If {
          cond = methcall (field self_v "p") "call" [ Var ("v", pos) ];
          then_blk = [
            Assign {
              path = ["result"];
              value = EnumLit {
                tname = ["Option"]; variant = "Some";
                args = EATuple [ Var ("v", pos) ]; pos };
              pos };
            Assign {
              path = ["keep"];
              value = BoolLit (false, pos);
              pos };
          ];
          else_blk = None; pos });
      ], pos);
      arm_pos = pos } in
    let none_arm = Ast.{
      pat = PVariant {
        tname = ["Option"]; variant = "None";
        binds = PBTuple []; pos };
      guard = None;
      body = Block ([
        Assign {
          path = ["keep"];
          value = BoolLit (false, pos);
          pos };
      ], pos);
      arm_pos = pos } in
    [
      Ast.Let { name = "keep"; is_mut = true; ty_ann = Some Ast.TyBool;
                value = Ast.BoolLit (true, pos); pos };
      Ast.Let { name = "result"; is_mut = true;
                ty_ann = Some filter_option_item_ann;
                value = Ast.EnumLit {
                  tname = ["Option"]; variant = "None";
                  args = Ast.EATuple []; pos };
                pos };
      Ast.While {
        cond = Ast.Var ("keep", pos);
        body = [
          Ast.ExprStmt (Ast.Match {
            scrutinee;
            arms = [ some_arm; none_arm ];
            pos });
        ]
      };
      Ast.Tail (Ast.Var ("result", pos));
    ]
  in
  let filter_next_method = {
    Ast.name = "next"; c_name = "next";
    tparams = []; tbounds = [];
    params = [ filter_next_self_param ];
    ret_ty = Some filter_option_item_ann;
    body = filter_next_body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = None; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos
  } in
  let filter_iter_impl = {
    Ast.itparams = ["I"; "P"];
    itbounds = [
      ("I", ["Iterator"], []);
      ("P", ["Fn1"], []);
    ];
    itrait = Some ["Iterator"];
    iassoc = [
      ("Item", filter_item_ann)
    ];
    itarget = ["Filter"];
    iitems = [ filter_next_method ];
    ipos = pos;
  } in
  (* `Eq` / `Clone` — prelude traits derivable via `@derive(Eq, Clone)`.
     By-value `self` (value types copy cheaply).  `ne` is a default in
     terms of `eq`.  `Clone::clone` returns a copy of self.  Primitive
     types get built-in `eq`/`ne`/`clone` (see method-call elaboration)
     so derived impls recurse through fields uniformly. *)
  let mk_param name ty = { Ast.pname = name; pty = ty; preg = None;
                           is_mut = false } in
  let self_v = mk_param "self" Ast.TySelf in
  let other_v = mk_param "other" Ast.TySelf in
  let trait_sig name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = []; params;
    ret_ty = ret; body; is_pub = true; is_extern = false;
    is_variadic = false; tier_hint = None; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos }
  in
  (* `Eq` / `Hash` / `Clone` all borrow `self` read-only so an
     `@move`-marked type (String, StringBuilder, Vec, …) can compare
     / hash / clone without the move-pass consuming the source.
     `Eq::eq` and `Eq::ne` take `( *const Self, *const Self)`;
     `Hash::hash` takes `*const Self`; `Clone::clone` returns by-value
     `Self` from a `*const Self` borrow.  Built-in primitive eq /
     hash / clone (in MethodCall elaboration) auto-ref the receiver
     so existing call sites stay `a.eq(b)` / `x.hash()` / `p.clone()`. *)
  let self_const_ptr_v = mk_param "self" (Ast.TyConstPtr Ast.TySelf) in
  let other_const_ptr_v = mk_param "other" (Ast.TyConstPtr Ast.TySelf) in
  let _ = self_v in let _ = other_v in
  let eq_sig =
    trait_sig "eq" [self_const_ptr_v; other_const_ptr_v]
      (Some Ast.TyBool) [] in
  (* `ne` defaults to `!self.eq(other)`.  Both `self` and `other` are
     now `*const Self`, so the body derefs them — otherwise method
     dispatch sees a pointer receiver, hits the built-in primitive-
     pointer eq path and silently lowers to a pointer compare. *)
  let ne_default =
    trait_sig "ne" [self_const_ptr_v; other_const_ptr_v]
      (Some Ast.TyBool)
      [ Ast.Tail (Ast.Not (Ast.MethodCall {
          receiver = Ast.Deref (Ast.Var ("self", pos), pos);
          name = "eq";
          args = [ Ast.Deref (Ast.Var ("other", pos), pos) ]; pos }, pos)) ]
  in
  let eq_trait = {
    Ast.trname = "Eq"; trassoc = []; trsupers = [];
    trmethods = [ eq_sig; ne_default ]; trdefaults = [ "ne" ];
    trpos = pos; tris_pub = true;
  } in
  let clone_sig =
    trait_sig "clone" [self_const_ptr_v] (Some Ast.TySelf) [] in
  let clone_trait = {
    Ast.trname = "Clone"; trassoc = []; trsupers = [];
    trmethods = [ clone_sig ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  let u32_ann = Ast.TyInt { signed = false; width = Ast.W32 } in
  let hash_sig =
    trait_sig "hash" [self_const_ptr_v] (Some u32_ann) [] in
  let hash_trait = {
    Ast.trname = "Hash"; trassoc = []; trsupers = [ ["Eq"] ];
    trmethods = [ hash_sig ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  (* `Display` / `Debug` — writer-pattern formatting traits.
     `fn fmt( * const self, out: *StringBuilder)` — read-only borrow
     of self, write into a builder the caller owns; threading the
     same `out` through nested fmts composes without intermediate
     allocs.  Receiver was `*self` before receiver-mutability
     landed; `*const self` (per DECYZJA #2-bis 2026-05-28) makes
     the read-only intent explicit and lets the formatter take
     immutable bindings too.  Display = user-facing (manual
     impls); Debug = developer-facing (`@derive(Debug)` synthesises
     it). *)
  let self_const_ptr_param =
    mk_param "self" (Ast.TyConstPtr Ast.TySelf) in
  let out_param =
    mk_param "out"
      (Ast.TyPtr (Ast.TyStruct { path = ["StringBuilder"]; args = [] })) in
  let display_sig =
    trait_sig "fmt" [self_const_ptr_param; out_param] None [] in
  let display_trait = {
    Ast.trname = "Display"; trassoc = []; trsupers = [];
    trmethods = [ display_sig ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  (* GATE-5d: Debug's formatter is `fmt_debug`, not `fmt` — exile has
     a flat per-type method namespace, so a type implementing BOTH
     Display and Debug would otherwise collide on `fmt` (confirmed:
     "method 'fmt' on 'P' already defined in another 'impl' block"). *)
  let debug_sig =
    trait_sig "fmt_debug" [self_const_ptr_param; out_param] None [] in
  let debug_trait = {
    Ast.trname = "Debug"; trassoc = []; trsupers = [];
    trmethods = [ debug_sig ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  (* `mod str` — pure-exile string ops over `cstr_len` + Slice<u8>.
     Per DR-001 (READ czysty) all bodies stay in exile; only
     `cstr_len` (lowered to libc `strlen`) crosses the FFI seam.
     `==` / `!=` on `str` operands dispatch to `str::eq` in the
     BinOp arm (otherwise C would compare pointers — the long-standing
     footgun this design closes). *)
  let str_var n = Ast.Var (n, pos) in
  let str_param pn =
    { Ast.pname = pn; pty = Ast.TyStr; preg = None; is_mut = false } in
  let mk_str_fn name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = [];
    params; ret_ty = ret; body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos;
  } in
  let slice_u8_ret = Ast.TyStruct { path = ["Slice"]; args = [ u8_t ] } in
  let i_lit n = Ast.IntLit (n, pos) in
  let cstr_len_of s =
    Ast.Call { callee = ["cstr_len"]; args = [s]; pos } in
  let as_bytes_call s =
    Ast.Call { callee = ["as_bytes"]; args = [s]; pos } in
  let str_len_body = [
    Ast.Return (Some (cstr_len_of (str_var "s")), pos);
  ] in
  let str_len_fn =
    (* GATE-5d completion (freeze-audit should-fix): one naming
       convention — `length` everywhere a COUNT is asked for
       (methods went in DR-049; this module fn was missed). *)
    mk_str_fn "length" [str_param "s"] (Some u32_t) str_len_body in
  let str_as_bytes_body = [
    Ast.Let { name = "n"; is_mut = false; ty_ann = Some u32_t;
              value = cstr_len_of (str_var "s"); pos };
    Ast.Return (Some (Ast.StructLit {
      tname = ["Slice"];
      fields = [
        ("ptr", Ast.Cast (str_var "s", u8_cptr, pos));
        ("len", str_var "n");
      ]; base = None; pos }), pos);
  ] in
  let str_as_bytes_fn =
    mk_str_fn "as_bytes" [str_param "s"] (Some slice_u8_ret)
      str_as_bytes_body in
  (* eq: len-check + byte-loop over Slice<u8>. *)
  let str_eq_body = [
    Ast.Let { name = "la"; is_mut = false; ty_ann = Some u32_t;
              value = cstr_len_of (str_var "a"); pos };
    Ast.Let { name = "lb"; is_mut = false; ty_ann = Some u32_t;
              value = cstr_len_of (str_var "b"); pos };
    Ast.ExprStmt (Ast.If {
      cond = bin Ast.NotEq (str_var "la") (str_var "lb");
      then_blk = [ Ast.Return (Some (Ast.BoolLit (false, pos)), pos) ];
      else_blk = None; pos });
    Ast.Let { name = "ba"; is_mut = false; ty_ann = Some slice_u8_ret;
              value = as_bytes_call (str_var "a"); pos };
    Ast.Let { name = "bb"; is_mut = false; ty_ann = Some slice_u8_ret;
              value = as_bytes_call (str_var "b"); pos };
    Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
              value = u32_lit 0; pos };
    Ast.While {
      cond = bin Ast.Lt (str_var "i") (str_var "la");
      body = [
        Ast.ExprStmt (Ast.If {
          cond = bin Ast.NotEq
                   (Ast.Index { base = str_var "ba";
                                index = str_var "i"; pos })
                   (Ast.Index { base = str_var "bb";
                                index = str_var "i"; pos });
          then_blk = [ Ast.Return (Some (Ast.BoolLit (false, pos)), pos) ];
          else_blk = None; pos });
        Ast.Assign { path = ["i"];
                     value = bin Ast.Add (str_var "i") (u32_lit 1); pos };
      ] };
    Ast.Return (Some (Ast.BoolLit (true, pos)), pos);
  ] in
  let str_eq_fn =
    mk_str_fn "eq" [str_param "a"; str_param "b"] (Some Ast.TyBool)
      str_eq_body in
  (* cmp: lexicographic; return ba[i]-bb[i] at first diff, else la-lb. *)
  let str_cmp_body =
    let i32_ann = Ast.TyInt { signed = true; width = Ast.W32 } in
    [
      Ast.Let { name = "la"; is_mut = false; ty_ann = Some u32_t;
                value = cstr_len_of (str_var "a"); pos };
      Ast.Let { name = "lb"; is_mut = false; ty_ann = Some u32_t;
                value = cstr_len_of (str_var "b"); pos };
      Ast.Let { name = "ba"; is_mut = false; ty_ann = Some slice_u8_ret;
                value = as_bytes_call (str_var "a"); pos };
      Ast.Let { name = "bb"; is_mut = false; ty_ann = Some slice_u8_ret;
                value = as_bytes_call (str_var "b"); pos };
      Ast.Let { name = "m"; is_mut = false; ty_ann = Some u32_t;
                value = Ast.If {
                  cond = bin Ast.Lt (str_var "la") (str_var "lb");
                  then_blk = [ Ast.Tail (str_var "la") ];
                  else_blk = Some [ Ast.Tail (str_var "lb") ];
                  pos }; pos };
      Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
                value = u32_lit 0; pos };
      Ast.While {
        cond = bin Ast.Lt (str_var "i") (str_var "m");
        body = [
          Ast.ExprStmt (Ast.If {
            cond = bin Ast.NotEq
                     (Ast.Index { base = str_var "ba";
                                  index = str_var "i"; pos })
                     (Ast.Index { base = str_var "bb";
                                  index = str_var "i"; pos });
            then_blk = [
              Ast.Return (Some (bin Ast.Sub
                (Ast.Cast (Ast.Index { base = str_var "ba";
                                       index = str_var "i"; pos },
                           i32_ann, pos))
                (Ast.Cast (Ast.Index { base = str_var "bb";
                                       index = str_var "i"; pos },
                           i32_ann, pos))), pos);
            ];
            else_blk = None; pos });
          Ast.Assign { path = ["i"];
                       value = bin Ast.Add (str_var "i") (u32_lit 1); pos };
        ] };
      Ast.Return (Some (bin Ast.Sub
        (Ast.Cast (str_var "la", i32_ann, pos))
        (Ast.Cast (str_var "lb", i32_ann, pos))), pos);
    ]
  in
  let str_cmp_fn =
    mk_str_fn "cmp" [str_param "a"; str_param "b"]
      (Some (Ast.TyInt { signed = true; width = Ast.W32 })) str_cmp_body in
  (* hash: multiplicative content fold over Slice<u8> — same shape
     as @derive(Hash)'s fold for byte-array fields.  Needed by
     HashMap<String,_> / HashMap<str,_> so they avoid the pointer-
     hash footgun the prelude `==` fix closes for equality. *)
  let str_hash_body = [
    Ast.Let { name = "bytes"; is_mut = false; ty_ann = Some slice_u8_ret;
              value = as_bytes_call (str_var "s"); pos };
    Ast.Let { name = "n"; is_mut = false; ty_ann = Some u32_t;
              value = field (str_var "bytes") "len"; pos };
    Ast.Let { name = "acc"; is_mut = true; ty_ann = Some u32_t;
              value = u32_lit 0; pos };
    Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
              value = u32_lit 0; pos };
    Ast.While {
      cond = bin Ast.Lt (str_var "i") (str_var "n");
      body = [
        Ast.Assign { path = ["acc"];
                     value = bin Ast.Add
                       (bin Ast.Mul (str_var "acc") (i_lit 31))
                       (Ast.Cast (Ast.Index { base = str_var "bytes";
                                              index = str_var "i"; pos },
                                  u32_t, pos)); pos };
        Ast.Assign { path = ["i"];
                     value = bin Ast.Add (str_var "i") (u32_lit 1); pos };
      ] };
    Ast.Return (Some (str_var "acc"), pos);
  ] in
  let str_hash_fn =
    mk_str_fn "hash" [str_param "s"] (Some u32_t) str_hash_body in
  (* Arena — bump allocator over the seam (PORT-PREP P2, ratified
     2026-06-10).  The AST-building idiom for the self-host port:
     thousands of nodes, one wholesale release.

     P1 decision (user-ratified): `alloc_borrowed::<T>()` returns a
     plain BORROW `*T` — the node belongs to the ARENA, not to the
     binding, so the own/L1 machinery does not track it (no per-node
     free ceremony, which is the entire point of an arena).  `new(a)`
     and `own` stay unchanged for general allocators.

     Wholesale release IS the GATE-2 auto-drop: `buf` is an own field
     with a sibling `alloc`, so the Arena struct is droppable — when
     the arena binding goes out of scope (or is consumed by an
     explicit `.drop()`), the whole backing buffer returns to the
     parent allocator in one `free_fn` call.  The byte-count
     heuristic reads the sibling `cap` field: cap * sizeof(u8) = the
     exact footprint.

     Honest v1 limits: borrows into the buffer must not outlive the
     arena binding (same class as a Slice into a Vec — escape-pass
     coverage is the DR-010 horizon); exhaustion returns `null`
     (caller picks the capacity; the port sizes generously). *)
  let arena_struct = {
    Ast.sname = "Arena";
    stparams = [];
    sfields = [
      ("buf", Ast.TyOwnPtr u8_t);
      ("cap", u32_t);
      ("off", u32_t);
      ("alloc", alloc_ann);
    ];
    spos = pos; sis_pub = true;
    stier_hint = Some "core"; sis_debug = false; sderives = [];
    sis_move = false;
  } in
  let arena_struct_ann = Ast.TyStruct { path = ["Arena"]; args = [] } in
  let arena_with_capacity_body = [
    Ast.Let { name = "buf"; is_mut = false;
              ty_ann = Some (Ast.TyOwnPtr u8_t);
              value = Ast.Cast (
                methcall (var "a") "alloc_fn"
                  [ field (var "a") "state"; var "bytes" ],
                Ast.TyOwnPtr u8_t, pos);
              pos };
    Ast.Return (Some (Ast.StructLit {
      tname = ["Arena"];
      fields = [
        ("buf", var "buf");
        ("cap", var "bytes");
        ("off", u32_lit 0);
        ("alloc", var "a");
      ];
      base = None; pos }), pos);
  ] in
  let arena_alloc_borrowed_body = [
    (* aligned = (off + 7) & ~7 — 8-byte alignment covers every
       exile-expressible field type on both host and m68k. *)
    Ast.Let { name = "aligned"; is_mut = false; ty_ann = Some u32_t;
              value = bin Ast.BitAnd
                (bin Ast.Add (field (var "self") "off") (u32_lit 7))
                (Ast.BitNot (u32_lit 7, pos));
              pos };
    Ast.Let { name = "need"; is_mut = false; ty_ann = Some u32_t;
              value = Ast.Cast (Ast.SizeOf (tvar "T", pos), u32_t, pos);
              pos };
    Ast.ExprStmt (Ast.If {
      cond = bin Ast.Gt (bin Ast.Add (var "aligned") (var "need"))
               (field (var "self") "cap");
      then_blk = [ Ast.Return (Some (Ast.NullLit pos), pos) ];
      else_blk = None; pos });
    Ast.Let { name = "p"; is_mut = false;
              ty_ann = Some (Ast.TyPtr u8_t);
              value = Ast.Call {
                callee = ["ptr_offset"];
                args = [ field (var "self") "buf"; var "aligned" ];
                pos };
              pos };
    Ast.AssignField { target = var "self"; field = "off";
                      value = bin Ast.Add (var "aligned") (var "need");
                      pos };
    Ast.Return (Some (Ast.Cast (var "p", Ast.TyPtr (tvar "T"), pos)), pos);
  ] in
  let arena_self_ptr_param =
    { Ast.pname = "self"; pty = Ast.TyPtr arena_struct_ann;
      preg = None; is_mut = false } in
  let mk_arena_method name tparams params ret body = {
    Ast.name; c_name = name; tparams; tbounds = []; params; ret_ty = ret;
    body; is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false;
    escapes_hatch = false; pos;
  } in
  let arena_impl = {
    Ast.itparams = []; itbounds = []; itrait = None; iassoc = [];
    itarget = ["Arena"];
    iitems = [
      mk_arena_method "with_capacity" []
        [ { Ast.pname = "a"; pty = alloc_ann; preg = None; is_mut = false };
          { Ast.pname = "bytes"; pty = u32_t; preg = None; is_mut = false } ]
        (Some arena_struct_ann) arena_with_capacity_body;
      mk_arena_method "alloc_borrowed" ["T"] [ arena_self_ptr_param ]
        (Some (Ast.TyPtr (tvar "T"))) arena_alloc_borrowed_body;
    ];
    ipos = pos;
  } in
  (* `str::from_slice(arena, s)` — copy [s]'s bytes into the arena,
     NUL-terminate, and return a `str` pointing at the arena copy.  The
     string-interning primitive for the self-host lexer / parser: an
     identifier or string literal is a slice of the source buffer, and
     this materialises it as a NUL-terminated `str` that lives as long as
     the arena (the AST owns its names through the same arena that owns
     its nodes).  Exhaustion returns the empty string — size the arena
     generously, the same contract as `alloc_borrowed`'s `null`. *)
  let str_from_slice_body = [
    Ast.Let { name = "n"; is_mut = false; ty_ann = Some u32_t;
              value = field (var "s") "len"; pos };
    Ast.Let { name = "start"; is_mut = false; ty_ann = Some u32_t;
              value = field (var "arena") "off"; pos };
    Ast.ExprStmt (Ast.If {
      cond = bin Ast.Gt
               (bin Ast.Add (bin Ast.Add (var "start") (var "n")) (u32_lit 1))
               (field (var "arena") "cap");
      then_blk = [ Ast.Return (Some (Ast.StringLit ("", pos)), pos) ];
      else_blk = None; pos });
    Ast.Let { name = "i"; is_mut = true; ty_ann = Some u32_t;
              value = u32_lit 0; pos };
    Ast.While {
      cond = bin Ast.Lt (var "i") (var "n");
      body = [
        Ast.AssignIndex {
          base = field (var "arena") "buf";
          index = bin Ast.Add (var "start") (var "i");
          value = Ast.Index { base = var "s"; index = var "i"; pos };
          pos };
        Ast.Assign { path = ["i"];
                     value = bin Ast.Add (var "i") (u32_lit 1); pos };
      ] };
    Ast.AssignIndex {
      base = field (var "arena") "buf";
      index = bin Ast.Add (var "start") (var "n");
      value = int_lit_as 0 u8_t;
      pos };
    Ast.AssignField { target = var "arena"; field = "off";
                      value = bin Ast.Add
                                (bin Ast.Add (var "start") (var "n")) (u32_lit 1);
                      pos };
    Ast.Let { name = "p"; is_mut = false; ty_ann = Some (Ast.TyPtr u8_t);
              value = Ast.Call { callee = ["ptr_offset"];
                                 args = [ field (var "arena") "buf"; var "start" ];
                                 pos };
              pos };
    Ast.Return (Some
      (Ast.Cast (Ast.Cast (var "p", Ast.TyConstPtr Ast.TyCChar, pos),
                 Ast.TyStr, pos)), pos);
  ] in
  let str_from_slice_fn =
    mk_str_fn "from_slice"
      [ { Ast.pname = "arena"; pty = Ast.TyPtr arena_struct_ann;
          preg = None; is_mut = false };
        { Ast.pname = "s"; pty = slice_u8_ret; preg = None; is_mut = false } ]
      (Some Ast.TyStr) str_from_slice_body in
  let str_mod = {
    Ast.mname = "str";
    mitems = [
      Ast.Function str_len_fn;
      Ast.Function str_as_bytes_fn;
      Ast.Function str_eq_fn;
      Ast.Function str_cmp_fn;
      Ast.Function str_hash_fn;
      Ast.Function str_from_slice_fn;
    ];
    mpos = pos;
    mis_pub = true;
  } in
  (* DR-006 — `pub mod sys` is the one seam between exile stdlib and
     the host platform.  Each backend (host / amiga / kernel) supplies
     the bodies behind these `extern fn`s; layer-0 code calls the
     interface verbatim and the compiler swaps the implementation by
     auto-linking the matching runtime per `--target`.  Width-pinned
     signatures (`c_ulong` byte counts, `c_int` fds) avoid the
     `size_t`/`ssize_t` portability skew DR-001 warned about. *)
  let cvoid_ptr_ann = Ast.TyPtr Ast.TyCVoid in
  let cuchar_ptr_ann = Ast.TyPtr Ast.TyCUChar in
  let cuchar_const_ptr_ann = Ast.TyConstPtr Ast.TyCUChar in
  let cchar_const_ptr_ann = Ast.TyConstPtr Ast.TyCChar in
  let cint_ann = Ast.TyCInt { signed = true } in
  let culong_ann = Ast.TyCLong { signed = false } in
  let clong_ann = Ast.TyCLong { signed = true } in
  let mk_extern name params ret = {
    Ast.name;
    c_name = name;
    tparams = []; tbounds = [];
    params;
    ret_ty = ret;
    body = [];
    is_pub = true;
    is_extern = true;
    is_variadic = false;
    tier_hint = Some "core";
    amiga_lib = None;
    must_use = false;
    escapes_hatch = false;
    pos;
  } in
  let mk_param name ty =
    { Ast.pname = name; pty = ty; preg = None; is_mut = false }
  in
  let sys_alloc_fn =
    mk_extern "sys_alloc"
      [ mk_param "state" cvoid_ptr_ann;
        mk_param "n" culong_ann ]
      (Some cvoid_ptr_ann)
  in
  let sys_free_fn =
    mk_extern "sys_free"
      [ mk_param "state" cvoid_ptr_ann;
        mk_param "p" cvoid_ptr_ann;
        mk_param "n" culong_ann ]
      None
  in
  let sys_write_fn =
    mk_extern "sys_write"
      [ mk_param "fd" cint_ann;
        mk_param "buf" cuchar_const_ptr_ann;
        mk_param "n" culong_ann ]
      (Some clong_ann)
  in
  let sys_read_fn =
    mk_extern "sys_read"
      [ mk_param "fd" cint_ann;
        mk_param "buf" cuchar_ptr_ann;
        mk_param "n" culong_ann ]
      (Some clong_ann)
  in
  (* DR-032 sys_open / sys_close — file-handle seam for module-
     loading (`use foo;` resolution in the future self-host port).
     `path` is a `*const c_char` C string; `flags` is a POSIX-style
     access mode (`O_RDONLY=0` covers the read-only port case);
     return value is a small-int file handle (`>= 0` on success,
     `-1` on failure).  Host backend wraps libc `open`/`close`;
     amiga backend stubs to -1 until `dos.library/Open` + BPTR/fd
     bookkeeping is wired up (single-file bootstrap on stdin/
     stdout doesn't need it). *)
  let sys_open_fn =
    mk_extern "sys_open"
      [ mk_param "path" cchar_const_ptr_ann;
        mk_param "flags" cint_ann ]
      (Some cint_ann)
  in
  let sys_close_fn =
    mk_extern "sys_close"
      [ mk_param "fd" cint_ann ]
      (Some cint_ann)
  in
  let sys_mod = {
    Ast.mname = "sys";
    mitems = [
      Ast.Function sys_alloc_fn;
      Ast.Function sys_free_fn;
      Ast.Function sys_write_fn;
      Ast.Function sys_read_fn;
      Ast.Function sys_open_fn;
      Ast.Function sys_close_fn;
    ];
    mpos = pos;
    mis_pub = true;
  } in
  [ Ast.Enum option_decl; Ast.Enum result_decl;
    Ast.Struct range_struct; Ast.Struct range_inclusive_struct;
    Ast.Struct slice_struct;
    Ast.Struct alloc_struct; Ast.Impl alloc_impl;
    Ast.Struct arena_struct; Ast.Impl arena_impl;
    Ast.Struct sb_struct; Ast.Impl sb_impl;
    Ast.Struct string_struct; Ast.Impl string_impl;
    Ast.Impl string_eq_impl; Ast.Impl string_hash_impl;
    Ast.Impl string_clone_impl;
    Ast.Struct vec_struct; Ast.Struct vec_iter_struct;
    Ast.Impl vec_impl;
    Ast.Impl slice_impl;
    Ast.Struct slot_struct; Ast.Struct hashmap_struct;
    Ast.Struct hashmap_iter_struct;
    Ast.Impl hashmap_impl;
    Ast.Module str_mod;
    Ast.Module sys_mod;
    Ast.Trait iterator_trait;
    Ast.Trait fn0_trait; Ast.Trait fn1_trait; Ast.Trait fn2_trait;
    Ast.Trait fn3_trait; Ast.Trait fn4_trait;
    Ast.Trait eq_trait; Ast.Trait clone_trait;
    Ast.Trait hash_trait;
    Ast.Trait display_trait; Ast.Trait debug_trait;
    Ast.Impl vec_iter_impl;
    Ast.Impl hashmap_iter_impl;
    Ast.Struct map_struct;
    Ast.Impl map_iter_impl;
    Ast.Struct take_struct;
    Ast.Impl take_iter_impl;
    Ast.Struct enumerate_struct;
    Ast.Impl enumerate_iter_impl;
    Ast.Struct filter_struct;
    Ast.Impl filter_iter_impl;
  ]

(* ===== `@derive(...)` — synthesize trait impls (DECYZJA #1/#2) =====
   Each `@derive`d trait becomes a real `impl Trait for Foo` generated as
   surface AST and run through the normal conformance + mono + codegen
   path.  Bodies use `.eq()` on fields (primitive fields resolve via the
   built-in `eq` in method-call elaboration; aggregate fields via their own
   derived/hand-written impl), so derivation recurses uniformly. *)

let derive_field_param name target_ann =
  { Ast.pname = name; pty = target_ann; preg = None; is_mut = false }

let derive_mk_method name params ret body pos =
  { Ast.name; c_name = name; tparams = []; tbounds = []; params;
    ret_ty = ret; body; is_pub = true; is_extern = false;
    is_variadic = false; tier_hint = None; amiga_lib = None;
    must_use = false; escapes_hatch = false; pos }

(* `a.eq(b)` *)
let derive_eq_call a b pos =
  Ast.MethodCall { receiver = a; name = "eq"; args = [ b ]; pos }

(* AND-fold a list of bool exprs; empty → `true`. *)
let derive_and_all pos = function
  | [] -> Ast.BoolLit (true, pos)
  | x :: rest -> List.fold_left (fun acc e -> Ast.BinOp (Ast.And, acc, e, pos)) x rest

let derive_eq_struct (s : Ast.struct_decl) : Ast.item =
  let pos = s.spos in
  let target = Ast.TyStruct { path = [s.sname]; args = [] } in
  let cmps =
    List.map (fun (fname, _) ->
      derive_eq_call
        (Ast.FieldAccess (Ast.Var ("self", pos), fname, pos))
        (Ast.FieldAccess (Ast.Var ("other", pos), fname, pos)) pos)
      s.sfields
  in
  let body = [ Ast.Tail (derive_and_all pos cmps) ] in
  let m = derive_mk_method "eq"
    [ derive_field_param "self" (Ast.TyConstPtr target);
      derive_field_param "other" (Ast.TyConstPtr target) ]
    (Some Ast.TyBool) body pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Eq"]; iassoc = [];
             itarget = [s.sname]; iitems = [m]; ipos = pos }

let derive_eq_enum (e : Ast.enum_decl) : Ast.item =
  let pos = e.epos in
  let target = Ast.TyStruct { path = [e.ename]; args = [] } in
  let nvar = List.length e.evariants in
  let outer_arms =
    List.map (fun (v : Ast.enum_variant) ->
      let vname = v.vname in
      let (self_binds, other_binds, cmp) =
        match v.vkind with
        | Ast.VUnit ->
            (Ast.PBTuple [], Ast.PBTuple [], Ast.BoolLit (true, pos))
        | Ast.VTuple tys ->
            let n = List.length tys in
            let an i = Printf.sprintf "__de_a%d" i in
            let bn i = Printf.sprintf "__de_b%d" i in
            let sp = Ast.PBTuple (List.init n (fun i -> Ast.PVar (an i, pos))) in
            let op = Ast.PBTuple (List.init n (fun i -> Ast.PVar (bn i, pos))) in
            let cmps = List.init n (fun i ->
              derive_eq_call (Ast.Var (an i, pos)) (Ast.Var (bn i, pos)) pos) in
            (sp, op, derive_and_all pos cmps)
        | Ast.VStruct fields ->
            let names = List.map fst fields in
            let an f = "__de_a_" ^ f in
            let bn f = "__de_b_" ^ f in
            let sp = Ast.PBStruct (List.map (fun f -> (f, Ast.PVar (an f, pos))) names) in
            let op = Ast.PBStruct (List.map (fun f -> (f, Ast.PVar (bn f, pos))) names) in
            let cmps = List.map (fun f ->
              derive_eq_call (Ast.Var (an f, pos)) (Ast.Var (bn f, pos)) pos) names in
            (sp, op, derive_and_all pos cmps)
      in
      let self_pat =
        Ast.PVariant { tname = [e.ename]; variant = vname; binds = self_binds; pos } in
      let inner_pat =
        Ast.PVariant { tname = [e.ename]; variant = vname; binds = other_binds; pos } in
      let inner_arms =
        let matched = Ast.{ pat = inner_pat; guard = None; body = cmp; arm_pos = pos } in
        if nvar = 1 then [ matched ]
        else [ matched;
               Ast.{ pat = Ast.PWildcard pos; guard = None;
                     body = Ast.BoolLit (false, pos); arm_pos = pos } ]
      in
      let inner_match =
        Ast.Match {
          scrutinee = Ast.Deref (Ast.Var ("other", pos), pos);
          arms = inner_arms; pos } in
      Ast.{ pat = self_pat; guard = None; body = inner_match; arm_pos = pos })
      e.evariants
  in
  let outer_match =
    Ast.Match {
      scrutinee = Ast.Deref (Ast.Var ("self", pos), pos);
      arms = outer_arms; pos } in
  let m = derive_mk_method "eq"
    [ derive_field_param "self" (Ast.TyConstPtr target);
      derive_field_param "other" (Ast.TyConstPtr target) ]
    (Some Ast.TyBool) [ Ast.Tail outer_match ] pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Eq"]; iassoc = [];
             itarget = [e.ename]; iitems = [m]; ipos = pos }

(* `Clone` — field-wise deep copy mirroring `Eq` / `Hash` derive
   shape.  Pre-fix this emitted `return *self;` (shallow memcpy);
   that aliases every heap-owning field with the source so a later
   `free()` on either side fires twice (DR-002 S4).  Now each
   primitive field copies through the built-in `.clone()` (identity
   value-copy) and each aggregate field dispatches through its
   `T__clone` impl, so a `String` field deep-copies its buffer.
   `@derive(Clone)` on a struct/enum carrying an @move field
   without `impl Clone` for that field type errors at the inner
   `.clone()` lookup. *)
let derive_clone_struct (s : Ast.struct_decl) : Ast.item =
  let pos = s.spos in
  let target = Ast.TyStruct { path = [s.sname]; args = [] } in
  let clone_call recv =
    Ast.MethodCall { receiver = recv; name = "clone"; args = []; pos } in
  let field_inits =
    List.map (fun (fname, _) ->
      (fname, clone_call
                (Ast.FieldAccess (Ast.Var ("self", pos), fname, pos))))
      s.sfields
  in
  let body =
    Ast.StructLit { tname = [s.sname]; fields = field_inits;
                    base = None; pos } in
  let m = derive_mk_method "clone"
    [ derive_field_param "self" (Ast.TyConstPtr target) ]
    (Some target) [ Ast.Tail body ] pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Clone"]; iassoc = [];
             itarget = [s.sname]; iitems = [m]; ipos = pos }

let derive_clone_enum (e : Ast.enum_decl) : Ast.item =
  let pos = e.epos in
  let target = Ast.TyStruct { path = [e.ename]; args = [] } in
  let clone_call recv =
    Ast.MethodCall { receiver = recv; name = "clone"; args = []; pos } in
  let arms = List.map (fun (v : Ast.enum_variant) ->
    let vname = v.vname in
    let (binds, ctor_args) = match v.vkind with
      | Ast.VUnit ->
          (Ast.PBTuple [], Ast.EATuple [])
      | Ast.VTuple tys ->
          let n = List.length tys in
          let bn i = Printf.sprintf "__dc_a%d" i in
          let bp =
            Ast.PBTuple
              (List.init n (fun i -> Ast.PVar (bn i, pos))) in
          let args =
            Ast.EATuple
              (List.init n (fun i ->
                 clone_call (Ast.Var (bn i, pos)))) in
          (bp, args)
      | Ast.VStruct fields ->
          let names = List.map fst fields in
          let bn f = "__dc_a_" ^ f in
          let bp =
            Ast.PBStruct
              (List.map (fun f -> (f, Ast.PVar (bn f, pos))) names) in
          let args =
            Ast.EAStruct
              (List.map (fun f ->
                 (f, clone_call (Ast.Var (bn f, pos)))) names) in
          (bp, args)
    in
    let pat =
      Ast.PVariant { tname = [e.ename]; variant = vname; binds; pos } in
    let body =
      Ast.EnumLit { tname = [e.ename]; variant = vname;
                    args = ctor_args; pos } in
    Ast.{ pat; guard = None; body; arm_pos = pos })
    e.evariants
  in
  let match_expr =
    Ast.Match { scrutinee = Ast.Deref (Ast.Var ("self", pos), pos);
                arms; pos } in
  let m = derive_mk_method "clone"
    [ derive_field_param "self" (Ast.TyConstPtr target) ]
    (Some target) [ Ast.Tail match_expr ] pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Clone"]; iassoc = [];
             itarget = [e.ename]; iitems = [m]; ipos = pos }

(* Hash — `fn hash(self) -> u32`.  Multiplicative fold `acc*31 + f.hash()`
   over fields (primitive fields fold via the built-in `.hash()`); an enum
   seeds each arm with the variant index so distinct variants hash apart. *)
let derive_u32_ann = Ast.TyInt { signed = false; width = Ast.W32 }

let derive_hash_combine acc fh pos =
  Ast.BinOp (Ast.Add,
    Ast.BinOp (Ast.Mul, acc, Ast.IntLit (31, pos), pos), fh, pos)

let derive_hash_call e pos =
  Ast.MethodCall { receiver = e; name = "hash"; args = []; pos }

let derive_hash_struct (s : Ast.struct_decl) : Ast.item =
  let pos = s.spos in
  let target = Ast.TyStruct { path = [s.sname]; args = [] } in
  let field_hash (fname, _) =
    derive_hash_call (Ast.FieldAccess (Ast.Var ("self", pos), fname, pos)) pos in
  let body =
    match s.sfields with
    | [] -> Ast.Cast (Ast.IntLit (0, pos), derive_u32_ann, pos)
    | f :: rest ->
        List.fold_left (fun acc f -> derive_hash_combine acc (field_hash f) pos)
          (field_hash f) rest
  in
  let m = derive_mk_method "hash"
    [ derive_field_param "self" (Ast.TyConstPtr target) ]
    (Some derive_u32_ann) [ Ast.Tail body ] pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Hash"]; iassoc = [];
             itarget = [s.sname]; iitems = [m]; ipos = pos }

let derive_hash_enum (e : Ast.enum_decl) : Ast.item =
  let pos = e.epos in
  let target = Ast.TyStruct { path = [e.ename]; args = [] } in
  let arms =
    List.mapi (fun idx (v : Ast.enum_variant) ->
      let base = Ast.Cast (Ast.IntLit (idx, pos), derive_u32_ann, pos) in
      let (binds, hashes) =
        match v.vkind with
        | Ast.VUnit -> (Ast.PBTuple [], [])
        | Ast.VTuple tys ->
            let n = List.length tys in
            let nm i = Printf.sprintf "__dh%d" i in
            (Ast.PBTuple (List.init n (fun i -> Ast.PVar (nm i, pos))),
             List.init n (fun i -> derive_hash_call (Ast.Var (nm i, pos)) pos))
        | Ast.VStruct fields ->
            let names = List.map fst fields in
            (Ast.PBStruct (List.map (fun f -> (f, Ast.PVar ("__dh_" ^ f, pos))) names),
             List.map (fun f -> derive_hash_call (Ast.Var ("__dh_" ^ f, pos)) pos) names)
      in
      let body = List.fold_left (fun acc fh -> derive_hash_combine acc fh pos)
        base hashes in
      let pat = Ast.PVariant { tname = [e.ename]; variant = v.vname;
                               binds; pos } in
      Ast.{ pat; guard = None; body; arm_pos = pos })
      e.evariants
  in
  let match_e =
    Ast.Match { scrutinee = Ast.Deref (Ast.Var ("self", pos), pos);
                arms; pos } in
  let m = derive_mk_method "hash"
    [ derive_field_param "self" (Ast.TyConstPtr target) ]
    (Some derive_u32_ann) [ Ast.Tail match_e ] pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Hash"]; iassoc = [];
             itarget = [e.ename]; iitems = [m]; ipos = pos }

(* @derive(Debug) helpers — writer-pattern body synthesis.  Output
   format mirrors Rust's `{:?}` shape: struct `S { f: ..., g: ... }`,
   unit / tuple / struct enum variants `E::V`, `E::V(..)`, `E::V { f:
   .. }`.  Primitive fields land inline (push_int / push_byte for
   bool / quoted push_str for str); other field types must impl Debug
   and recurse through `.fmt(out)` (conformance enforces). *)
let derive_debug_push_str s pos =
  Ast.ExprStmt (Ast.MethodCall {
    receiver = Ast.Var ("out", pos);
    name = "push_str";
    args = [ Ast.StringLit (s, pos) ];
    pos })

let derive_debug_push_byte b pos =
  Ast.ExprStmt (Ast.MethodCall {
    receiver = Ast.Var ("out", pos);
    name = "push_byte";
    args = [ Ast.Cast (Ast.IntLit (b, pos),
                       Ast.TyInt { signed = false; width = Ast.W8 }, pos) ];
    pos })

(* Render a single value expression into `out`.  Primitive int-like
   types push as decimal (i32-cast for narrower / wider integers);
   bool branches to a literal push; str pushes quoted; anything else
   recurses through the type's own Debug impl via `.fmt(out)`. *)
let derive_debug_render_value (e : Ast.expr) (ty : Ast.type_ann) pos =
  let i32_ann = Ast.TyInt { signed = true; width = Ast.W32 } in
  match ty with
  | Ast.TyInt _ | Ast.TyCInt _ | Ast.TyCShort _ | Ast.TyCLong _
  | Ast.TyCChar | Ast.TyCSChar | Ast.TyCUChar ->
      [ Ast.ExprStmt (Ast.MethodCall {
          receiver = Ast.Var ("out", pos);
          name = "push_int";
          args = [ Ast.Cast (e, i32_ann, pos) ];
          pos }) ]
  | Ast.TyBool ->
      [ Ast.ExprStmt (Ast.If {
          cond = e;
          then_blk = [ derive_debug_push_str "true" pos ];
          else_blk = Some [ derive_debug_push_str "false" pos ];
          pos }) ]
  | Ast.TyStr ->
      [ derive_debug_push_byte 34 pos;
        Ast.ExprStmt (Ast.MethodCall {
          receiver = Ast.Var ("out", pos);
          name = "push_str";
          args = [ e ]; pos });
        derive_debug_push_byte 34 pos ]
  | _ ->
      [ Ast.ExprStmt (Ast.MethodCall {
          receiver = e; name = "fmt_debug";
          args = [ Ast.Var ("out", pos) ]; pos }) ]

(* Interleave a separator stmt-list between rendered chunks. *)
let derive_debug_join sep chunks =
  match chunks with
  | [] -> []
  | x :: rest -> List.fold_left (fun acc c -> acc @ sep @ c) x rest

let derive_debug_sb_ann =
  Ast.TyPtr (Ast.TyStruct { path = ["StringBuilder"]; args = [] })

let derive_debug_mk_fmt target body pos =
  let self_p = derive_field_param "self" (Ast.TyConstPtr target) in
  let out_p =
    { Ast.pname = "out"; pty = derive_debug_sb_ann;
      preg = None; is_mut = false }
  in
  derive_mk_method "fmt_debug" [self_p; out_p] None body pos

let derive_debug_struct (s : Ast.struct_decl) : Ast.item =
  let pos = s.spos in
  let target = Ast.TyStruct { path = [s.sname]; args = [] } in
  let self_v = Ast.Var ("self", pos) in
  let body =
    match s.sfields with
    | [] -> [ derive_debug_push_str s.sname pos ]
    | _ ->
        let field_chunks =
          List.map (fun (fname, fty) ->
            let access = Ast.FieldAccess (self_v, fname, pos) in
            derive_debug_push_str (fname ^ ": ") pos
            :: derive_debug_render_value access fty pos)
            s.sfields
        in
        derive_debug_push_str (s.sname ^ " { ") pos
        :: (derive_debug_join [ derive_debug_push_str ", " pos ]
              field_chunks)
        @ [ derive_debug_push_str " }" pos ]
  in
  let m = derive_debug_mk_fmt target body pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Debug"]; iassoc = [];
             itarget = [s.sname]; iitems = [m]; ipos = pos }

let derive_debug_enum (e : Ast.enum_decl) : Ast.item =
  let pos = e.epos in
  let target = Ast.TyStruct { path = [e.ename]; args = [] } in
  let arms =
    List.map (fun (v : Ast.enum_variant) ->
      let prefix = e.ename ^ "::" ^ v.vname in
      let (binds, body_stmts) =
        match v.vkind with
        | Ast.VUnit ->
            (Ast.PBTuple [],
             [ derive_debug_push_str prefix pos ])
        | Ast.VTuple tys ->
            let n = List.length tys in
            let nm i = Printf.sprintf "__dd%d" i in
            let pats = List.init n (fun i -> Ast.PVar (nm i, pos)) in
            let chunks =
              List.mapi (fun i ty ->
                derive_debug_render_value (Ast.Var (nm i, pos)) ty pos)
                tys
            in
            (Ast.PBTuple pats,
             derive_debug_push_str (prefix ^ "(") pos
             :: (derive_debug_join [ derive_debug_push_str ", " pos ]
                   chunks)
             @ [ derive_debug_push_byte 41 pos ])  (* ')' *)
        | Ast.VStruct fields ->
            let pats =
              List.map (fun (n, _) ->
                (n, Ast.PVar ("__dd_" ^ n, pos))) fields in
            let chunks =
              List.map (fun (n, ty) ->
                derive_debug_push_str (n ^ ": ") pos
                :: derive_debug_render_value
                     (Ast.Var ("__dd_" ^ n, pos)) ty pos)
                fields
            in
            (Ast.PBStruct pats,
             derive_debug_push_str (prefix ^ " { ") pos
             :: (derive_debug_join [ derive_debug_push_str ", " pos ]
                   chunks)
             @ [ derive_debug_push_str " }" pos ])
      in
      let pat = Ast.PVariant { tname = [e.ename]; variant = v.vname;
                               binds; pos } in
      let arm_body = Ast.Block (body_stmts, pos) in
      Ast.{ pat; guard = None; body = arm_body; arm_pos = pos })
      e.evariants
  in
  (* `self` is a pointer-to-Self inside `fn fmt( * self, ...)`, so
     the match scrutinee derefs to the enum value.  Field access
     auto-derefs but pattern matching does not. *)
  let body =
    [ Ast.ExprStmt
        (Ast.Match {
          scrutinee = Ast.Deref (Ast.Var ("self", pos), pos);
          arms; pos }) ]
  in
  let m = derive_debug_mk_fmt target body pos in
  Ast.Impl { itparams = []; itbounds = []; itrait = Some ["Debug"]; iassoc = [];
             itarget = [e.ename]; iitems = [m]; ipos = pos }

(* DR-009 — expand each `view Name(p: T) -> Case1 | Case2 { body }` into
   a nominal `enum Name { Case1 | Case2 }` plus a function `Name(p: T)
   -> Name { body }`.  After this pass the rest of the pipeline sees
   the view as an ordinary enum + an ordinary fn — Maranget
   exhaustiveness on `Name::*`, mono on `Name(arg)`, codegen on tagged
   switch.  Recursion through modules so view-decls nested under
   `mod foo { ... }` get the same treatment.

   Match-arm rewrite (`match scr { Name::A => ... }` against a
   scrutinee of type `T` rather than `Name`) lands in the elab path
   for `Ast.Match`, which has the type information needed to decide
   when to insert the view-call. *)
(* DR-024 closures-A2 — lifts every `Ast.Lambda` to a synthesised
   top-level form.  Two paths:

   - Captureless (no free vars referring to enclosing locals): the
     legacy DR-008 A1 decay.  Lambda turns into a fresh top-level
     fn `__lambda_N`, the expression becomes `Var "__lambda_N"`,
     and the ordinary fn-ptr-decay in elab makes it usable as a
     callable value.

   - With captures: synthesise an env struct `__closure_N { c1: T1,
     ...}`, an `impl Fn{arity} for __closure_N` whose `call` body is
     the lambda's body with each captured name rewritten to
     `self.<name>`, and replace the expression with a struct literal
     `__closure_N { c1: c1, c2: c2, ... }` constructing the env from
     the surrounding scope.  Captures are inferred by a free-var
     walk over the lambda body; capture types come from the
     surrounding fn's params and explicitly-annotated lets (the only
     bindings whose type is known before typecheck).  An untyped
     let referenced as a capture errors with a clear message
     pointing the user at the missing annotation.

   The synthesised env struct, impl, and `call` method all flow
   through the normal typecheck / mono / codegen pipeline — codegen
   sees a real impl Fn{arity}, mono picks the right instance, and
   the rest of the DR-015 sugars (DR-018 / DR-019 call-desugar,
   DR-021 |A|->R bound, DR-022 assoc equality) Just Work on top. *)
let expand_lambdas (program : Ast.program) : Ast.program =
  let lifted = ref [] in
  let counter = ref 0 in
  let fresh_lambda_name () =
    let n = !counter in incr counter;
    Printf.sprintf "__lambda_%d" n
  in
  let closure_counter = ref 0 in
  let fresh_closure_name () =
    let n = !closure_counter in incr closure_counter;
    Printf.sprintf "__closure_%d" n
  in
  (* DR-036 follow-up - index every top-level fn's ret_ty by name.
     The mini-inferencer below uses this to handle the
     `let n = compute_value()` shape: a Call to a fn with a known
     return type can flow its annotation into the surrounding
     scope so a downstream closure sees `n` as captured.
     Methods and qualified paths are skipped - their receiver
     types aren't known pre-typecheck. *)
  let fn_ret_index : (string, Ast.type_ann) Hashtbl.t = Hashtbl.create 64 in
  (* DR-041 - method ret-ty index: (struct-name, method-name) → ret_ty.
     Walks every Impl block and records the concrete method
     signature for closure-polish #2 partial coverage — lets
     `let n = v.len()` infer `n: u32` from Vec::len's ret_ty when
     `v` carries an annotation in scope.  Generic ret types
     (`I::Item`, `T`, etc.) are recorded as-is; the closure body
     then sees the un-substituted ann, which still survives the
     A2 lift path because the closure's own elab does the real
     substitution at the use site. *)
  let method_ret_index : ((string * string), Ast.type_ann) Hashtbl.t =
    Hashtbl.create 64 in
  let rec index_item (it : Ast.item) =
    match it with
    | Ast.Function f ->
        (match f.ret_ty with
         | Some t -> Hashtbl.replace fn_ret_index f.name t
         | None -> ())
    | Ast.Impl ib ->
        (match ib.itarget with
         | [target] ->
             List.iter (fun (m : Ast.func) ->
               match m.ret_ty with
               | Some t ->
                   Hashtbl.replace method_ret_index (target, m.name) t
               | None -> ())
               ib.iitems
         | _ -> ())
    | Ast.Module m -> List.iter index_item m.mitems
    | _ -> ()
  in
  List.iter index_item program;
  (* Collect the names referenced as Ast.Var that are NOT bound
     locally inside [e] (by lambda params or internal lets/patterns).
     Used at Lambda elab to discover captures: the lambda's own
     params + any internal lets shadow names that would otherwise
     look like captures. *)
  let rec free_vars_of_expr (bound : (string, unit) Hashtbl.t) (e : Ast.expr) acc =
    match e with
    | Ast.Var (n, _) ->
        if Hashtbl.mem bound n then acc
        else if List.mem n acc then acc
        else acc @ [n]
    | Ast.IntLit _ | Ast.FloatLit _ | Ast.BoolLit _ | Ast.StringLit _
    | Ast.NullLit _ | Ast.SizeOf _ -> acc
    | Ast.Neg (sub, _) | Ast.BitNot (sub, _) | Ast.Not (sub, _)
    | Ast.Try (sub, _) | Ast.Cast (sub, _, _)
    | Ast.FieldAccess (sub, _, _)
    | Ast.Ref (sub, _) | Ast.Deref (sub, _) ->
        free_vars_of_expr bound sub acc
    | Ast.BinOp (_, l, r, _) | Ast.Orelse (l, r, _) ->
        let acc = free_vars_of_expr bound l acc in
        free_vars_of_expr bound r acc
    | Ast.TupleLit (es, _) | Ast.ArrayLit (es, _) ->
        List.fold_left (fun a e -> free_vars_of_expr bound e a) acc es
    | Ast.ArrayRepeat { value; count; _ } ->
        let acc = free_vars_of_expr bound value acc in
        free_vars_of_expr bound count acc
    | Ast.Index { base; index; _ } ->
        let acc = free_vars_of_expr bound base acc in
        free_vars_of_expr bound index acc
    | Ast.Range { lo; hi; _ } ->
        let acc = free_vars_of_expr bound lo acc in
        free_vars_of_expr bound hi acc
    | Ast.Call { args; _ } ->
        List.fold_left (fun a e -> free_vars_of_expr bound e a) acc args
    | Ast.MethodCall { receiver; args; _ } ->
        let acc = free_vars_of_expr bound receiver acc in
        List.fold_left (fun a e -> free_vars_of_expr bound e a) acc args
    | Ast.StructLit { fields; base; _ } | Ast.New { fields; base; _ } ->
        let acc =
          List.fold_left (fun a (_, e) -> free_vars_of_expr bound e a)
            acc fields
        in
        (match base with
         | Some b -> free_vars_of_expr bound b acc
         | None -> acc)
    | Ast.NewEnum { args; _ } ->
        List.fold_left (fun a e -> free_vars_of_expr bound e a) acc args
    | Ast.EnumLit { args; _ } ->
        let args =
          match args with
          | Ast.EATuple es -> es
          | Ast.EAStruct fs -> List.map snd fs
        in
        List.fold_left (fun a e -> free_vars_of_expr bound e a) acc args
    | Ast.Match { scrutinee; arms; _ } ->
        let acc = free_vars_of_expr bound scrutinee acc in
        List.fold_left (fun a (arm : Ast.match_arm) ->
          let pat_binds = pattern_bound_names arm.pat in
          let saved = List.map (fun n -> (n, Hashtbl.mem bound n)) pat_binds in
          List.iter (fun n -> Hashtbl.replace bound n ()) pat_binds;
          let a =
            match arm.guard with
            | Some g -> free_vars_of_expr bound g a
            | None -> a
          in
          let a = free_vars_of_expr bound arm.body a in
          List.iter (fun (n, was) ->
            if was then () else Hashtbl.remove bound n) saved;
          a) acc arms
    | Ast.If { cond; then_blk; else_blk; _ } ->
        let acc = free_vars_of_expr bound cond acc in
        let acc = free_vars_of_stmts bound then_blk acc in
        (match else_blk with
         | Some sl -> free_vars_of_stmts bound sl acc
         | None -> acc)
    | Ast.Block (stmts, _) -> free_vars_of_stmts bound stmts acc
    | Ast.Lambda { params; body; _ } ->
        (* Inner lambda: its own params shadow within its body.  Its
           captures are still free vars from the OUTER lambda's
           perspective if they're not in OUTER's params / lets. *)
        let saved =
          List.map (fun (n, _) -> (n, Hashtbl.mem bound n)) params
        in
        List.iter (fun (n, _) -> Hashtbl.replace bound n ()) params;
        let acc = free_vars_of_expr bound body acc in
        List.iter (fun (n, was) ->
          if was then () else Hashtbl.remove bound n) saved;
        acc
  and pattern_bound_names (p : Ast.pattern) : string list =
    let rec go acc = function
      | Ast.PWildcard _ | Ast.PLit _ | Ast.PBool _ -> acc
      | Ast.PVar (n, _) ->
          if n = "_" || List.mem n acc then acc else n :: acc
      | Ast.PVariant { binds; _ } ->
          (match binds with
           | Ast.PBTuple ps -> List.fold_left go acc ps
           | Ast.PBStruct ps -> List.fold_left (fun a (_, p) -> go a p) acc ps)
      | Ast.POr (alts, _) -> List.fold_left go acc alts
    in go [] p
  and free_vars_of_stmts bound stmts acc =
    let (acc, removed) =
      List.fold_left (fun (acc, removed) s ->
        match s with
        | Ast.Let { name; value; _ } ->
            let acc = free_vars_of_expr bound value acc in
            let was = Hashtbl.mem bound name in
            Hashtbl.replace bound name ();
            (acc, (name, was) :: removed)
        | Ast.LetTuple { names; value; _ } ->
            let acc = free_vars_of_expr bound value acc in
            let added =
              List.map (fun n ->
                let was = Hashtbl.mem bound n in
                Hashtbl.replace bound n ();
                (n, was)) names
            in
            (acc, added @ removed)
        | Ast.LetElse { pat; value; else_body; _ } ->
            let acc = free_vars_of_expr bound value acc in
            let acc = free_vars_of_stmts bound else_body acc in
            let added =
              List.map (fun n ->
                let was = Hashtbl.mem bound n in
                Hashtbl.replace bound n ();
                (n, was)) (pattern_bound_names pat)
            in
            (acc, added @ removed)
        | Ast.Assign { path = [n]; value; _ } ->
            let acc = if Hashtbl.mem bound n then acc
                      else if List.mem n acc then acc else acc @ [n] in
            (free_vars_of_expr bound value acc, removed)
        | Ast.Assign { value; _ } ->
            (free_vars_of_expr bound value acc, removed)
        | Ast.AssignField { target; value; _ }
        | Ast.AssignDeref { target; value; _ } ->
            let acc = free_vars_of_expr bound target acc in
            (free_vars_of_expr bound value acc, removed)
        | Ast.AssignIndex { base; index; value; _ } ->
            let acc = free_vars_of_expr bound base acc in
            let acc = free_vars_of_expr bound index acc in
            (free_vars_of_expr bound value acc, removed)
        | Ast.Return (Some e, _) | Ast.ExprStmt e | Ast.Tail e ->
            (free_vars_of_expr bound e acc, removed)
        | Ast.Return (None, _) -> (acc, removed)
        | Ast.While { cond; body } ->
            let acc = free_vars_of_expr bound cond acc in
            (free_vars_of_stmts bound body acc, removed)
        | Ast.For { var; range; body; _ } ->
            let acc = free_vars_of_expr bound range acc in
            let was = Hashtbl.mem bound var in
            Hashtbl.replace bound var ();
            let acc = free_vars_of_stmts bound body acc in
            if not was then Hashtbl.remove bound var;
            (acc, removed)
        | Ast.With { target; name; body; _ } ->
            let acc = free_vars_of_expr bound target acc in
            let was = Hashtbl.mem bound name in
            Hashtbl.replace bound name ();
            let acc = free_vars_of_stmts bound body acc in
            if not was then Hashtbl.remove bound name;
            (acc, removed)
        | Ast.Defer { body; _ } ->
            (free_vars_of_stmts bound body acc, removed)
        | Ast.Break _ | Ast.Continue _ -> (acc, removed))
        (acc, []) stmts
    in
    (* Pop the block-local bindings before returning so outer-scope
       references after the block stay correctly classified. *)
    List.iter (fun (n, was) ->
      if not was then Hashtbl.remove bound n) removed;
    acc
  in
  (* Substitute `Ast.Var (cap, _)` with `Ast.FieldAccess (Ast.Var
     ("self", pos), cap, pos)` (or its Deref for DR-033 by-ref
     captures) everywhere except where a tighter binding shadows
     the name.  Used to rewrite the lambda's body into the impl
     Fn1 `call` method body.  [captures] is `(name, is_byref)`. *)
  let rec subst_captures captures bound (e : Ast.expr) : Ast.expr =
    match e with
    | Ast.Var (n, p) when not (Hashtbl.mem bound n) &&
                          List.mem_assoc n captures ->
        let is_byref = List.assoc n captures in
        let access = Ast.FieldAccess (Ast.Var ("self", p), n, p) in
        if is_byref then Ast.Deref (access, p) else access
    | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.BoolLit _
    | Ast.StringLit _ | Ast.NullLit _ | Ast.SizeOf _ -> e
    | Ast.Neg (s, p) -> Ast.Neg (subst_captures captures bound s, p)
    | Ast.BitNot (s, p) -> Ast.BitNot (subst_captures captures bound s, p)
    | Ast.Not (s, p) -> Ast.Not (subst_captures captures bound s, p)
    | Ast.Try (s, p) -> Ast.Try (subst_captures captures bound s, p)
    | Ast.Cast (s, t, p) -> Ast.Cast (subst_captures captures bound s, t, p)
    | Ast.FieldAccess (s, f, p) ->
        Ast.FieldAccess (subst_captures captures bound s, f, p)
    | Ast.Ref (s, p) -> Ast.Ref (subst_captures captures bound s, p)
    | Ast.Deref (s, p) -> Ast.Deref (subst_captures captures bound s, p)
    | Ast.BinOp (op, l, r, p) ->
        Ast.BinOp (op, subst_captures captures bound l,
                       subst_captures captures bound r, p)
    | Ast.Orelse (l, r, p) ->
        Ast.Orelse (subst_captures captures bound l,
                    subst_captures captures bound r, p)
    | Ast.TupleLit (es, p) ->
        Ast.TupleLit (List.map (subst_captures captures bound) es, p)
    | Ast.ArrayLit (es, p) ->
        Ast.ArrayLit (List.map (subst_captures captures bound) es, p)
    | Ast.ArrayRepeat { value; count; pos } ->
        Ast.ArrayRepeat {
          value = subst_captures captures bound value;
          count = subst_captures captures bound count; pos }
    | Ast.Index { base; index; pos } ->
        Ast.Index { base = subst_captures captures bound base;
                    index = subst_captures captures bound index; pos }
    | Ast.Range { lo; hi; inclusive; pos } ->
        Ast.Range { lo = subst_captures captures bound lo;
                    hi = subst_captures captures bound hi; inclusive; pos }
    | Ast.Call { callee; args; pos } ->
        Ast.Call { callee;
                   args = List.map (subst_captures captures bound) args; pos }
    | Ast.MethodCall { receiver; name; args; pos } ->
        Ast.MethodCall {
          receiver = subst_captures captures bound receiver; name;
          args = List.map (subst_captures captures bound) args; pos }
    | Ast.StructLit { tname; fields; base; pos } ->
        Ast.StructLit {
          tname;
          fields = List.map (fun (n, e) ->
            (n, subst_captures captures bound e)) fields;
          base = Option.map (subst_captures captures bound) base; pos }
    | Ast.New { tname; fields; base; alloc; pos } ->
        Ast.New {
          tname;
          fields = List.map (fun (n, e) ->
            (n, subst_captures captures bound e)) fields;
          base = Option.map (subst_captures captures bound) base;
          alloc = Option.map (subst_captures captures bound) alloc;
          pos }
    | Ast.NewEnum { tname; args; alloc; pos } ->
        Ast.NewEnum {
          tname;
          args = List.map (subst_captures captures bound) args;
          alloc = Option.map (subst_captures captures bound) alloc;
          pos }
    | Ast.EnumLit { tname; variant; args; pos } ->
        let args = match args with
          | Ast.EATuple es ->
              Ast.EATuple (List.map (subst_captures captures bound) es)
          | Ast.EAStruct fs ->
              Ast.EAStruct (List.map (fun (n, e) ->
                (n, subst_captures captures bound e)) fs)
        in
        Ast.EnumLit { tname; variant; args; pos }
    | Ast.Match { scrutinee; arms; pos } ->
        let arms = List.map (fun (a : Ast.match_arm) ->
          let pat_binds = pattern_bound_names a.pat in
          let saved = List.map (fun n -> (n, Hashtbl.mem bound n)) pat_binds in
          List.iter (fun n -> Hashtbl.replace bound n ()) pat_binds;
          let arm = { a with
            Ast.guard = Option.map (subst_captures captures bound) a.guard;
            body = subst_captures captures bound a.body } in
          List.iter (fun (n, was) ->
            if not was then Hashtbl.remove bound n) saved;
          arm) arms
        in
        Ast.Match {
          scrutinee = subst_captures captures bound scrutinee; arms; pos }
    | Ast.If { cond; then_blk; else_blk; pos } ->
        Ast.If {
          cond = subst_captures captures bound cond;
          then_blk = subst_captures_stmts captures bound then_blk;
          else_blk =
            Option.map (subst_captures_stmts captures bound) else_blk;
          pos }
    | Ast.Block (stmts, p) ->
        Ast.Block (subst_captures_stmts captures bound stmts, p)
    | Ast.Lambda { params; ret_ty; body; captures = inner_caps; pos } ->
        let saved =
          List.map (fun (n, _) -> (n, Hashtbl.mem bound n)) params
        in
        List.iter (fun (n, _) -> Hashtbl.replace bound n ()) params;
        let body = subst_captures captures bound body in
        List.iter (fun (n, was) ->
          if not was then Hashtbl.remove bound n) saved;
        Ast.Lambda { params; ret_ty; body; captures = inner_caps; pos }
  and subst_captures_stmts captures bound stmts =
    let (out, removed) =
      List.fold_left (fun (acc, removed) s ->
        let (s', new_binds) =
          match s with
          | Ast.Let { name; value; ty_ann; is_mut; pos } ->
              let value = subst_captures captures bound value in
              let was = Hashtbl.mem bound name in
              Hashtbl.replace bound name ();
              (Ast.Let { name; value; ty_ann; is_mut; pos }, [(name, was)])
          | Ast.LetTuple { names; value; is_mut; pos } ->
              let value = subst_captures captures bound value in
              let added =
                List.map (fun n ->
                  let was = Hashtbl.mem bound n in
                  Hashtbl.replace bound n ();
                  (n, was)) names
              in
              (Ast.LetTuple { names; value; is_mut; pos }, added)
          | Ast.LetElse { pat; value; else_body; pos } ->
              let value = subst_captures captures bound value in
              let else_body = subst_captures_stmts captures bound else_body in
              let added =
                List.map (fun n ->
                  let was = Hashtbl.mem bound n in
                  Hashtbl.replace bound n ();
                  (n, was)) (pattern_bound_names pat)
              in
              (Ast.LetElse { pat; value; else_body; pos }, added)
          | Ast.Assign { path; value; pos } ->
              (Ast.Assign { path;
                            value = subst_captures captures bound value;
                            pos }, [])
          | Ast.AssignField { target; field; value; pos } ->
              (Ast.AssignField {
                 target = subst_captures captures bound target;
                 field;
                 value = subst_captures captures bound value;
                 pos }, [])
          | Ast.AssignIndex { base; index; value; pos } ->
              (Ast.AssignIndex {
                 base = subst_captures captures bound base;
                 index = subst_captures captures bound index;
                 value = subst_captures captures bound value;
                 pos }, [])
          | Ast.AssignDeref { target; value; pos } ->
              (Ast.AssignDeref {
                 target = subst_captures captures bound target;
                 value = subst_captures captures bound value;
                 pos }, [])
          | Ast.Return (e, p) ->
              (Ast.Return (Option.map (subst_captures captures bound) e, p),
               [])
          | Ast.ExprStmt e ->
              (Ast.ExprStmt (subst_captures captures bound e), [])
          | Ast.Tail e ->
              (Ast.Tail (subst_captures captures bound e), [])
          | Ast.While { cond; body } ->
              (Ast.While {
                 cond = subst_captures captures bound cond;
                 body = subst_captures_stmts captures bound body }, [])
          | Ast.For { var; range; body; pos } ->
              let range = subst_captures captures bound range in
              let was = Hashtbl.mem bound var in
              Hashtbl.replace bound var ();
              let body = subst_captures_stmts captures bound body in
              if not was then Hashtbl.remove bound var;
              (Ast.For { var; range; body; pos }, [])
          | Ast.With { target; name; body; pos } ->
              let target = subst_captures captures bound target in
              let was = Hashtbl.mem bound name in
              Hashtbl.replace bound name ();
              let body = subst_captures_stmts captures bound body in
              if not was then Hashtbl.remove bound name;
              (Ast.With { target; name; body; pos }, [])
          | Ast.Defer { body; pos } ->
              (Ast.Defer {
                 body = subst_captures_stmts captures bound body; pos }, [])
          | Ast.Break _ | Ast.Continue _ -> (s, [])
        in
        (s' :: acc, new_binds @ removed)) ([], []) stmts
    in
    List.iter (fun (n, was) ->
      if not was then Hashtbl.remove bound n) removed;
    List.rev out
  in
  let rec lift_e ?(fn_bound_pos = false) ~scope (e : Ast.expr)
    : Ast.expr =
    match e with
    | Ast.Lambda { params; ret_ty; body; captures = explicit_caps; pos } ->
        let body = lift_e ~scope:(scope @ params) body in
        let bound = Hashtbl.create 16 in
        List.iter (fun (n, _) -> Hashtbl.replace bound n ()) params;
        let free = free_vars_of_expr bound body [] in
        (* DR-033: validate explicit by-ref captures.  Each `&n` in the
           capture list must (a) resolve to a name in [scope] and
           (b) actually be referenced in the lambda body.  Implicit
           by-value captures (free vars not listed) keep working. *)
        List.iter (fun (n, _is_byref) ->
          if not (List.mem_assoc n scope) then
            Error.failf pos
              "by-ref capture `&%s`: name not in scope at lambda \
               (captures must be fn params or type-annotated lets)" n;
          if not (List.mem n free) then
            Error.failf pos
              "by-ref capture `&%s` has no reference in lambda body" n
        ) explicit_caps;
        let captures =
          List.filter_map (fun n ->
            match List.assoc_opt n scope with
            | None -> None
            | Some ann ->
                let is_byref =
                  match List.assoc_opt n explicit_caps with
                  | Some b -> b
                  | None -> false
                in
                Some (n, ann, is_byref)) free
        in
        if captures = [] && not fn_bound_pos then
          (* A1 path — captureless decay. *)
          let name = fresh_lambda_name () in
          let ast_params =
            List.map (fun (n, t) ->
              Ast.{ pname = n; pty = t; preg = None; is_mut = false })
              params
          in
          let body_stmts = [ Ast.Tail body ] in
          let fn = Ast.{
            name; c_name = name; tparams = []; tbounds = [];
            params = ast_params; ret_ty; body = body_stmts;
            is_pub = false; is_extern = false; is_variadic = false;
            amiga_lib = None; tier_hint = None;
            must_use = false; escapes_hatch = false; pos
          } in
          lifted := Ast.Function fn :: !lifted;
          Ast.Var (name, pos)
        else begin
          (* A2 path — synthesise env struct + impl FnN. *)
          let arity = List.length params in
          let fn_trait_name = Printf.sprintf "Fn%d" arity in
          let closure_name = fresh_closure_name () in
          let env_fields =
            List.map (fun (cap_name, cap_ann, is_byref) ->
              let ft = if is_byref then Ast.TyConstPtr cap_ann
                       else cap_ann in
              (cap_name, ft)) captures
          in
          (* GATE-5b: a captureless lambda routed here (FnN-bound
             position) has an EMPTY env — C89 forbids empty structs,
             so a one-byte pad field keeps the shape legal.  The
             struct-literal below initialises it to 0. *)
          let env_fields =
            if env_fields = [] then
              [ ("_pad", Ast.TyInt { signed = false; width = Ast.W8 }) ]
            else env_fields
          in
          let env_struct = Ast.{
            sname = closure_name;
            sis_pub = false;
            stparams = [];
            sfields = env_fields;
            spos = pos;
            sis_debug = false;
            sis_move = false;
            sderives = [];
            stier_hint = None;
          } in
          (* Build call's body — start from the lambda body, substitute
             every captured-Var reference with `self.<name>`, wrap in a
             `Tail` so the body's value flows as the call's result. *)
          let cap_name_mode =
            List.map (fun (n, _, b) -> (n, b)) captures in
          let bound_for_subst = Hashtbl.create 8 in
          List.iter (fun (n, _) -> Hashtbl.replace bound_for_subst n ())
            params;
          let body_substituted =
            subst_captures cap_name_mode bound_for_subst body
          in
          (* parse_impl_block replaces bare-`TySelf` with the target
             type at parse time; since this impl is synthesised after
             parsing, write the concrete target type straight into
             the `self` param so resolve_type_ann doesn't trip on a
             stray TySelf at body elab. *)
          let target_ty =
            Ast.TyStruct { path = [closure_name]; args = [] }
          in
          let self_ty = Ast.TyConstPtr target_ty in
          let call_params =
            { Ast.pname = "self"; pty = self_ty;
              preg = None; is_mut = false } ::
            List.map (fun (n, t) ->
              Ast.{ pname = n; pty = t; preg = None; is_mut = false })
              params
          in
          let call_method = Ast.{
            name = "call"; c_name = "call";
            tparams = []; tbounds = [];
            params = call_params;
            ret_ty;
            body = [ Ast.Tail body_substituted ];
            is_pub = true; is_extern = false; is_variadic = false;
            amiga_lib = None; tier_hint = None;
            must_use = false; escapes_hatch = false; pos
          } in
          let assoc_bindings =
            let arg_part =
              match arity with
              | 0 -> []
              | 1 ->
                  [("Arg", snd (List.hd params))]
              | _ ->
                  List.mapi (fun i (_, t) ->
                    (Printf.sprintf "Arg%d" (i + 1), t)) params
            in
            let ret_ann =
              match ret_ty with
              | Some t -> t
              | None -> Ast.TyStruct { path = ["c_void"]; args = [] }
            in
            arg_part @ [("Output", ret_ann)]
          in
          let env_impl = Ast.Impl Ast.{
            itparams = [];
            itbounds = [];
            itrait = Some [fn_trait_name];
            iassoc = assoc_bindings;
            itarget = [closure_name];
            iitems = [call_method];
            ipos = pos;
          } in
          lifted :=
            env_impl :: Ast.Struct env_struct :: !lifted;
          (* Replace the lambda expression with a struct literal
             constructing the env from the surrounding scope.  For
             by-ref captures the field is initialised with `&name`. *)
          let fields =
            List.map (fun (cap_name, _, is_byref) ->
              let v = Ast.Var (cap_name, pos) in
              let v = if is_byref then Ast.Ref (v, pos) else v in
              (cap_name, v)) captures
          in
          let fields =
            if fields = [] then [ ("_pad", Ast.IntLit (0, pos)) ]
            else fields
          in
          Ast.StructLit {
            tname = [closure_name]; fields; base = None; pos }
        end
    | Ast.IntLit _ | Ast.FloatLit _ | Ast.BoolLit _ | Ast.StringLit _
    | Ast.NullLit _ | Ast.Var _ | Ast.SizeOf _ -> e
    | Ast.Neg (sub, p) -> Ast.Neg (lift_e ~scope sub, p)
    | Ast.BitNot (sub, p) -> Ast.BitNot (lift_e ~scope sub, p)
    | Ast.Not (sub, p) -> Ast.Not (lift_e ~scope sub, p)
    | Ast.BinOp (op, l, r, p) ->
        Ast.BinOp (op, lift_e ~scope l, lift_e ~scope r, p)
    | Ast.Orelse (a, b, p) ->
        Ast.Orelse (lift_e ~scope a, lift_e ~scope b, p)
    | Ast.Try (a, p) -> Ast.Try (lift_e ~scope a, p)
    | Ast.Cast (a, t, p) -> Ast.Cast (lift_e ~scope a, t, p)
    | Ast.TupleLit (es, p) -> Ast.TupleLit (List.map (lift_e ~scope) es, p)
    | Ast.Call { callee; args; pos } ->
        Ast.Call { callee; args = List.map (lift_e ~scope) args; pos }
    | Ast.MethodCall { receiver; name; args; pos } ->
        (* GATE-5b: a captureless lambda in METHOD-ARG position routes
           through the A2 empty-env closure instead of A1 fn-ptr decay
           — method args are the combinator surface (`.map(|x| ...)`)
           where an `F: FnN` bound awaits, and a bare fn-ptr does not
           implement FnN.  Plain fn-ptr positions (lets, free-fn call
           args) keep the A1 decay. *)
        Ast.MethodCall { receiver = lift_e ~scope receiver; name;
                         args =
                           List.map (lift_e ~fn_bound_pos:true ~scope)
                             args;
                         pos }
    | Ast.StructLit { tname; fields; base; pos } ->
        Ast.StructLit { tname;
                        fields = List.map (fun (n, e) ->
                          (n, lift_e ~scope e)) fields;
                        base = Option.map (lift_e ~scope) base; pos }
    | Ast.FieldAccess (a, n, p) -> Ast.FieldAccess (lift_e ~scope a, n, p)
    | Ast.Ref (a, p) -> Ast.Ref (lift_e ~scope a, p)
    | Ast.Deref (a, p) -> Ast.Deref (lift_e ~scope a, p)
    | Ast.New { tname; fields; base; alloc; pos } ->
        Ast.New { tname;
                  fields = List.map (fun (n, e) ->
                    (n, lift_e ~scope e)) fields;
                  base = Option.map (lift_e ~scope) base;
                  alloc = Option.map (lift_e ~scope) alloc;
                  pos }
    | Ast.NewEnum { tname; args; alloc; pos } ->
        Ast.NewEnum { tname;
                      args = List.map (lift_e ~scope) args;
                      alloc = Option.map (lift_e ~scope) alloc;
                      pos }
    | Ast.EnumLit { tname; variant; args; pos } ->
        let args = match args with
          | Ast.EATuple es -> Ast.EATuple (List.map (lift_e ~scope) es)
          | Ast.EAStruct fs ->
              Ast.EAStruct (List.map (fun (n, e) ->
                (n, lift_e ~scope e)) fs)
        in
        Ast.EnumLit { tname; variant; args; pos }
    | Ast.Match { scrutinee; arms; pos } ->
        let arms = List.map (fun (a : Ast.match_arm) ->
          { a with
            Ast.guard = Option.map (lift_e ~scope) a.guard;
            body = lift_e ~scope a.body }) arms
        in
        Ast.Match { scrutinee = lift_e ~scope scrutinee; arms; pos }
    | Ast.If { cond; then_blk; else_blk; pos } ->
        Ast.If { cond = lift_e ~scope cond;
                 then_blk = lift_stmts ~scope then_blk;
                 else_blk = Option.map (lift_stmts ~scope) else_blk;
                 pos }
    | Ast.ArrayLit (es, p) -> Ast.ArrayLit (List.map (lift_e ~scope) es, p)
    | Ast.ArrayRepeat { value; count; pos } ->
        Ast.ArrayRepeat { value = lift_e ~scope value;
                          count = lift_e ~scope count; pos }
    | Ast.Index { base; index; pos } ->
        Ast.Index { base = lift_e ~scope base;
                    index = lift_e ~scope index; pos }
    | Ast.Range { lo; hi; inclusive; pos } ->
        Ast.Range { lo = lift_e ~scope lo;
                    hi = lift_e ~scope hi; inclusive; pos }
    | Ast.Block (stmts, p) ->
        Ast.Block (lift_stmts ~scope stmts, p)
  and lift_stmts ~scope (stmts : Ast.stmt list) : Ast.stmt list =
    let (out, _) =
      List.fold_left (fun (acc, scope) s ->
        let (s', scope') = lift_s ~scope s in
        (s' :: acc, scope')) ([], scope) stmts
    in
    List.rev out
  and lift_s ~scope (s : Ast.stmt) : Ast.stmt * (string * Ast.type_ann) list =
    match s with
    | Ast.Let { name; value; ty_ann; is_mut; pos } ->
        let value = lift_e ~scope value in
        (* DR-036 - mini-inferencer for untyped let RHS.  Walks the
           RHS structurally without a real typecheck and derives a
           cheap type annotation; the binding then enters scope so
           a downstream closure can capture it through the A2 path
           instead of decaying to A1 fn-ptr (which fails Fn1).
           Patterns covered:
             - IntLit / BoolLit / StringLit / FloatLit (literals)
             - Cast (_, ann, _)               (explicit annotation)
             - Var n (lookup in current scope, lets transitively
               flow through `let a = 42; let b = a;` chains)
             - BinOp (op, l, r) (the operator's result type is the
               same as the wider/known operand for arithmetic and
               always bool for comparisons / logical ops)
             - Not / Neg / BitNot (unary)
             - Call { callee = [name] } (top-level fn ret_ty lookup)
             - StructLit { tname; ... } (cheap path-only wrap)
             - EnumLit { tname; ... } (enum value)
             - Range { ... } (constructs `Range<T>` from its bounds)
           Complex RHS (method calls, generic struct lits with type
           args, container literals) still need an explicit ann -
           the long-term fix is the full POST-typecheck lift
           restructuring. *)
        let int_default = Ast.TyInt { signed = true; width = Ast.W32 } in
        let bool_ann = Ast.TyBool in
        let is_cmp_op = function
          | Ast.EqEq | Ast.NotEq | Ast.Lt | Ast.LtEq
          | Ast.Gt | Ast.GtEq -> true
          | _ -> false
        in
        let is_logical_op = function
          | Ast.And | Ast.Or -> true
          | _ -> false
        in
        let rec infer_ty_of (e : Ast.expr) : Ast.type_ann option =
          match e with
          | Ast.IntLit _ -> Some int_default
          | Ast.BoolLit _ -> Some bool_ann
          | Ast.StringLit _ -> Some Ast.TyStr
          | Ast.FloatLit (_, w, _) -> Some (Ast.TyFloat w)
          | Ast.Cast (_, ann, _) -> Some ann
          | Ast.Var (n, _) -> List.assoc_opt n scope
          | Ast.BinOp (op, l, r, _) when is_cmp_op op || is_logical_op op ->
              Some bool_ann
          | Ast.BinOp (_, l, r, _) ->
              (match infer_ty_of l with
               | Some _ as t -> t
               | None -> infer_ty_of r)
          | Ast.Neg (sub, _) | Ast.BitNot (sub, _) -> infer_ty_of sub
          | Ast.Not _ -> Some bool_ann
          | Ast.Call { callee = [single]; _ } ->
              Hashtbl.find_opt fn_ret_index single
          | Ast.MethodCall { receiver; name; _ } ->
              (* DR-041 - method ret-ty lookup.  Receiver type is
                 inferred recursively (Var-from-scope, chained
                 method calls, etc.); if it resolves to a single-
                 name struct path, look the method up in the
                 prelude / user impl table.  Generic ret types
                 (`I::Item`) flow through unsubstituted - good
                 enough for the A2 capture decision. *)
              (match infer_ty_of receiver with
               | Some (Ast.TyStruct { path = [target]; _ }) ->
                   Hashtbl.find_opt method_ret_index (target, name)
               | _ -> None)
          | Ast.StructLit { tname; _ } ->
              Some (Ast.TyStruct { path = tname; args = [] })
          | Ast.EnumLit { tname; _ } ->
              Some (Ast.TyStruct { path = tname; args = [] })
          | Ast.Range _ ->
              (* `Range<int>` is the common case; the lambda needs
                 *some* aggregate type and Range carries its tparam
                 from its bounds, but we cheaply pin to `int` since
                 untyped ranges are the typical untyped-let target. *)
              Some (Ast.TyStruct {
                path = ["Range"];
                args = [ int_default ];
              })
          | _ -> None
        in
        let inferred_ty =
          match ty_ann with
          | Some _ -> ty_ann
          | None -> infer_ty_of value
        in
        let scope' =
          match inferred_ty with
          | Some ann -> (name, ann) :: scope
          | None -> scope
        in
        (Ast.Let { name; value; ty_ann; is_mut; pos }, scope')
    | Ast.LetTuple { names; value; is_mut; pos } ->
        (Ast.LetTuple { names; value = lift_e ~scope value; is_mut; pos },
         scope)
    | Ast.LetElse { pat; value; else_body; pos } ->
        (Ast.LetElse { pat; value = lift_e ~scope value;
                       else_body = lift_stmts ~scope else_body; pos },
         scope)
    | Ast.Assign { path; value; pos } ->
        (Ast.Assign { path; value = lift_e ~scope value; pos }, scope)
    | Ast.AssignField { target; field; value; pos } ->
        (Ast.AssignField { target = lift_e ~scope target; field;
                           value = lift_e ~scope value; pos }, scope)
    | Ast.AssignIndex { base; index; value; pos } ->
        (Ast.AssignIndex { base = lift_e ~scope base;
                           index = lift_e ~scope index;
                           value = lift_e ~scope value; pos }, scope)
    | Ast.AssignDeref { target; value; pos } ->
        (Ast.AssignDeref { target = lift_e ~scope target;
                           value = lift_e ~scope value; pos }, scope)
    | Ast.Return (e, p) ->
        (Ast.Return (Option.map (lift_e ~scope) e, p), scope)
    | Ast.ExprStmt e -> (Ast.ExprStmt (lift_e ~scope e), scope)
    | Ast.Tail e -> (Ast.Tail (lift_e ~scope e), scope)
    | Ast.While { cond; body } ->
        (Ast.While { cond = lift_e ~scope cond;
                     body = lift_stmts ~scope body }, scope)
    | Ast.For { var; range; body; pos } ->
        (Ast.For { var; range = lift_e ~scope range;
                   body = lift_stmts ~scope body; pos }, scope)
    | Ast.Defer { body; pos } ->
        (Ast.Defer { body = lift_stmts ~scope body; pos }, scope)
    | Ast.With { target; name; body; pos } ->
        (Ast.With { target = lift_e ~scope target; name;
                    body = lift_stmts ~scope body; pos }, scope)
    | Ast.Break _ | Ast.Continue _ -> (s, scope)
  in
  let rec lift_item (it : Ast.item) : Ast.item =
    match it with
    | Ast.Function f ->
        let scope =
          List.map (fun (p : Ast.param) -> (p.Ast.pname, p.Ast.pty))
            f.params
        in
        Ast.Function { f with body = lift_stmts ~scope f.body }
    | Ast.Module m ->
        Ast.Module { m with mitems = List.map lift_item m.mitems }
    | Ast.View v ->
        let scope = [(v.vparam.Ast.pname, v.vparam.Ast.pty)] in
        Ast.View { v with vbody = lift_stmts ~scope v.vbody }
    | other -> other
  in
  let walked = List.map lift_item program in
  walked @ List.rev !lifted

let expand_views (program : Ast.program) : Ast.program =
  let case_to_variant (c : Ast.view_case) : Ast.enum_variant =
    let vkind =
      if c.vcis_struct then Ast.VStruct c.vcfields
      else if c.vcfields = [] then Ast.VUnit
      else Ast.VTuple (List.map snd c.vcfields)
    in
    Ast.{ vname = c.vcname; vkind; vpos = Pos.zero }
  in
  let view_to_items (v : Ast.view_decl) : Ast.item list =
    let enum_decl = Ast.{
      ename = v.vname;
      etparams = [];
      evariants = List.map case_to_variant v.vcases;
      epos = v.vpos;
      eis_pub = v.vis_pub;
      etier_hint = None;
      emust_use = false;
      eis_debug = false;
      ederives = [];
    } in
    let ret_ty = Some (Ast.TyStruct { path = [v.vname]; args = [] }) in
    let fn_decl = Ast.{
      name = v.vname;
      c_name = v.vname;
      tparams = [];
      tbounds = [];
      params = [v.vparam];
      ret_ty;
      body = v.vbody;
      is_pub = v.vis_pub;
      is_extern = false;
      is_variadic = false;
      amiga_lib = None;
      tier_hint = None;
      must_use = false;
      escapes_hatch = false;
      pos = v.vpos;
    } in
    [Ast.Enum enum_decl; Ast.Function fn_decl]
  in
  let rec walk (items : Ast.item list) : Ast.item list =
    List.concat_map (fun item ->
      match item with
      | Ast.View v ->
          Hashtbl.replace view_names v.vname ();
          view_to_items v
      | Ast.Module m ->
          [Ast.Module { m with mitems = walk m.mitems }]
      | other -> [other])
      items
  in
  Hashtbl.clear view_names;
  walk program

let expand_derives (program : Ast.program) : Ast.program =
  let one ~kind ~name ~pos ~generic
      ~gen_eq ~gen_hash ~gen_clone ~gen_debug tr =
    let needs_mono trait =
      if generic then
        Error.failf pos
          "@derive(%s) on a generic %s '%s' is not supported yet"
          trait kind name
    in
    match tr with
    | "Eq" -> needs_mono "Eq"; gen_eq ()
    | "Clone" -> needs_mono "Clone"; gen_clone ()
    | "Hash" -> needs_mono "Hash"; gen_hash ()
    | "Debug" -> needs_mono "Debug"; gen_debug ()
    | "Display" ->
        Error.failf pos
          "cannot derive 'Display' — Display is a hand-written surface; \
           use `@derive(Debug)` for an automatically-generated formatter"
    | other ->
        Error.failf pos
          "cannot derive '%s' (supported: Eq, Hash, Clone, Debug)" other
  in
  let derived =
    List.concat_map (fun item ->
      match item with
      | Ast.Struct s when s.sderives <> [] ->
          List.map (one ~kind:"struct" ~name:s.sname ~pos:s.spos
                      ~generic:(s.stparams <> [])
                      ~gen_eq:(fun () -> derive_eq_struct s)
                      ~gen_hash:(fun () -> derive_hash_struct s)
                      ~gen_clone:(fun () -> derive_clone_struct s)
                      ~gen_debug:(fun () -> derive_debug_struct s))
            s.sderives
      | Ast.Enum e when e.ederives <> [] ->
          List.map (one ~kind:"enum" ~name:e.ename ~pos:e.epos
                      ~generic:(e.etparams <> [])
                      ~gen_eq:(fun () -> derive_eq_enum e)
                      ~gen_hash:(fun () -> derive_hash_enum e)
                      ~gen_clone:(fun () -> derive_clone_enum e)
                      ~gen_debug:(fun () -> derive_debug_enum e))
            e.ederives
      | _ -> [])
      program
  in
  program @ derived

(* DR-030 Faza-1a Step C - synthesize a drop method (pointer-self)
   for every struct that carries at least one own field.  The body
   sends each own field through self.alloc.free_fn(state, ptr,
   size_of(T)), mirroring how String::free emits its manual call.
   Nested owning-struct fields recurse depth-first by name; cycles
   are honest-limit out of v1.  Runs after expand_derives so
   user-derived impls land before the drop emit (their bodies may
   touch fields the drop pass reads). *)
let expand_drop_methods (program : Ast.program) : Ast.program =
  (* Inspect the AST-level type annotation - the resolved IR isn't
     available at pass time, but we only need the structural shape
     (`Ast.TyOwnPtr _` vs anything else) to decide whether a field
     needs a drop call. *)
  let is_own_ann = function Ast.TyOwnPtr _ -> true | _ -> false in
  let synth_drop (s : Ast.struct_decl) : Ast.item option =
    let has_own = List.exists (fun (_, t) -> is_own_ann t) s.sfields in
    if not has_own then None
    else begin
      let pos = s.spos in
      let self_v = Ast.Var ("self", pos) in
      let cvoid_ptr = Ast.TyPtr Ast.TyCVoid in
      let u32_ann =
        Ast.TyInt { signed = false; width = Ast.W32 } in
      let alloc_field = Ast.FieldAccess (self_v, "alloc", pos) in
      let body_stmts =
        List.filter_map (fun (fname, ft) ->
          match ft with
          | Ast.TyOwnPtr inner ->
              (* self.alloc.free_fn(self.alloc.state,
                                    self.<fname> as *c_void,
                                    size_of(T) as u32) *)
              Some (Ast.ExprStmt (Ast.MethodCall {
                receiver = alloc_field; name = "free_fn";
                args = [
                  Ast.FieldAccess (alloc_field, "state", pos);
                  Ast.Cast (Ast.FieldAccess (self_v, fname, pos),
                            cvoid_ptr, pos);
                  Ast.Cast (Ast.SizeOf (inner, pos), u32_ann, pos);
                ]; pos }))
          | _ -> None)
          s.sfields
      in
      if body_stmts = [] then None
      else begin
        let self_struct_ann =
          Ast.TyStruct {
            path = [s.sname];
            args = List.map (fun n ->
              Ast.TyStruct { path = [n]; args = [] }) s.stparams;
          } in
        let drop_method = Ast.{
          name = "drop"; c_name = "drop";
          tparams = []; tbounds = [];
          params = [{
            pname = "self";
            pty = Ast.TyPtr self_struct_ann;
            preg = None; is_mut = false;
          }];
          ret_ty = None;
          body = body_stmts;
          is_pub = true; is_extern = false; is_variadic = false;
          tier_hint = None; amiga_lib = None;
          must_use = false; escapes_hatch = false; pos;
        } in
        Some (Ast.Impl Ast.{
          itparams = s.stparams;
          itbounds = [];
          itrait = None;
          iassoc = [];
          itarget = [s.sname];
          iitems = [drop_method];
          ipos = pos;
        })
      end
    end
  in
  let rec walk items =
    List.concat_map (fun item ->
      match item with
      | Ast.Struct s ->
          (match synth_drop s with
           | Some impl -> [item; impl]
           | None -> [item])
      | Ast.Module m ->
          [Ast.Module { m with mitems = walk m.mitems }]
      | other -> [other])
      items
  in
  walk program

(* Skip prelude items whose names collide with a user-declared top-level
   enum or struct (and skip the matching `impl` block in that case).
   Matches by name only (top-level path = []). *)
let prepend_prelude (program : Ast.program) : Ast.program =
  let user_top_enum_names =
    List.filter_map
      (fun item -> match item with
        | Ast.Enum e -> Some e.ename
        | _ -> None)
      program
  in
  let user_top_struct_names =
    List.filter_map
      (fun item -> match item with
        | Ast.Struct s -> Some s.sname
        | _ -> None)
      program
  in
  let user_top_trait_names =
    List.filter_map
      (fun item -> match item with
        | Ast.Trait t -> Some t.trname
        | _ -> None)
      program
  in
  let kept =
    List.filter
      (fun item -> match item with
        | Ast.Enum e -> not (List.mem e.ename user_top_enum_names)
        | Ast.Struct s -> not (List.mem s.sname user_top_struct_names)
        | Ast.Trait t -> not (List.mem t.trname user_top_trait_names)
        | Ast.Impl ib ->
            (* Drop a prelude impl if the user shadowed either the
               target struct (e.g. `struct Iterator { ... }`) or the
               trait (e.g. `trait Iterator { fn next(self) ... }` —
               the prelude impl would no longer match the user's
               trait signature). *)
            let target_shadowed = match ib.itarget with
              | [n] -> List.mem n user_top_struct_names
              | _ -> false
            in
            let trait_shadowed = match ib.itrait with
              | Some [n] -> List.mem n user_top_trait_names
              | _ -> false
            in
            not (target_shadowed || trait_shadowed)
        | _ -> true)
      (prelude_items ())
  in
  kept @ program

(* True when a `tstmt list` (typically a fn body) ends in a `return` on
   every control-flow path that reaches its end.  Used to enforce that
   value-returning fns can't fall off the end with an undefined result.

   Only the *last* statement of the list matters for fall-through; earlier
   statements may also `return` but if they do, code after them is dead
   and won't reach the end anyway.  Branching forms (`if`) require both
   arms to return; `while` never qualifies (the body may never run, or
   may exit normally).  `defer` registers cleanup but does not redirect
   control flow. *)
let rec always_returns (stmts : tstmt list) : bool =
  match List.rev stmts with
  | [] -> false
  | last :: _ -> stmt_returns last
and stmt_returns = function
  | TReturn _ -> true
  | TIf { then_body; else_body; _ } ->
      always_returns then_body && always_returns else_body
  | TWhile { cond; body; _ } ->
      (* `while (true) { ... }` with no `break` is an infinite loop —
         the only way out is via `return` from within.  Treating it as
         "always returns" lets prelude collections write probe loops
         (`while (true) { if ... return; }`) without a redundant
         unreachable return at the end. *)
      (match cond.e with
       | TBoolLit true when not (List.exists has_break body) -> true
       | _ -> false)
  | TLet _ | TLetTuple _ | TAssign _ | TAssignField _ | TAssignIndex _
  | TAssignDeref _ | TFor _ | TForEach _ | TDefer _ | TExprStmt _
  | TBreak _ | TContinue _ -> false

and has_break = function
  | TBreak _ -> true
  | TIf { then_body; else_body; _ } ->
      List.exists has_break then_body
      || List.exists has_break else_body
  | TWhile _ | TFor _ | TForEach _ -> false (* break inside nested loop is local *)
  | TDefer { body; _ } -> List.exists has_break body
  | _ -> false

(* Compile-time evaluation of `const NAME: T = expr;` declarations.
   Folds int/bool scalar expressions — literals, references to other
   consts (resolved by scope walk-up), `+ - * / %`, `& | ^ ~`, `<< >>`,
   comparisons, unary `-`/`~`, and `as` casts — into concrete values.
   `size_of` / `len` fold to C `sizeof(...)` expressions rather than
   literals, so they are deliberately rejected here (deferred).  Returns
   the index threaded into fn_ctx (path, name, (type, mangled)) and the
   list of `(mangled, C-literal)` pairs codegen emits as `#define`s. *)
(* Compile-time value of a `const`.  `CExpr` carries an already-
   parenthesised C expression — emitted verbatim as the `#define` body
   when the value isn't a literal integer/bool but a target-dependent
   construct like `sizeof(struct ex_Foo)`.  Such consts are NOT usable as
   array sizes (no compile-time integer for exile to fold). *)
type cval = CInt of int | CBool of bool | CExpr of string

let eval_consts ~instances ~modules ~aliases ~struct_index ~enum_index
    (consts : (string list * Ast.const_decl) list) =
  let res_ctx scope =
    { (empty_ctx ~instances) with
      structs = struct_index; enums = enum_index; modules; aliases; scope }
  in
  (* (abs_path, mod_path, decl, mangled, resolved type) per const. *)
  let entries =
    List.map (fun (mod_path, (c : Ast.const_decl)) ->
      let abs = mod_path @ [c.kname] in
      let mangled = mangle mod_path c.kname in
      let typ = resolve_type_ann ~pos:c.kpos (res_ctx mod_path) c.kty in
      (abs, mod_path, c, mangled, typ))
      consts
  in
  let find_abs abs =
    List.find_opt (fun (a, _, _, _, _) -> a = abs) entries
  in
  let resolve_ref scope ref_path =
    walk_scope_up (res_ctx scope) ref_path ~resolve:(fun mp name ->
      List.find_map
        (fun (a, _, _, _, _) -> if a = mp @ [name] then Some a else None)
        entries)
  in
  let memo : (string list, cval) Hashtbl.t = Hashtbl.create 16 in
  let in_prog : (string list, unit) Hashtbl.t = Hashtbl.create 16 in
  let truncate typ i =
    match typ with
    | TInt { signed; width } ->
        let bits = int_width_bits width in
        let m = i land ((1 lsl bits) - 1) in
        if signed && m >= (1 lsl (bits - 1)) then m - (1 lsl bits) else m
    | _ -> i   (* c_* widths are target-defined; leave the value as-is *)
  in
  let rec eval_entry abs =
    match Hashtbl.find_opt memo abs with
    | Some v -> v
    | None ->
        let (_, mod_path, (c : Ast.const_decl), _, typ) =
          match find_abs abs with Some e -> e | None -> assert false
        in
        if Hashtbl.mem in_prog abs then
          Error.failf c.kpos
            "cyclic constant definition involving '%s'"
            (String.concat "::" abs);
        Hashtbl.replace in_prog abs ();
        let v = eval mod_path c.kvalue in
        (match v, typ with
         | CInt i, _ when not (int_fits i typ) ->
             Error.failf c.kpos
               "constant '%s' = %d does not fit in %s"
               c.kname i (typ_name typ)
         | CBool _, TBool -> ()
         | CBool _, _ ->
             Error.failf c.kpos
               "constant '%s' is bool but declared %s" c.kname (typ_name typ)
         | CInt _, _ -> ()
         (* CExpr's value is a target-dependent C expression (`sizeof(...)`
            and friends); exile can't compute its integer to fit-check.
            Trust the declared type and let cc warn on narrowing. *)
         | CExpr _, _ -> ());
        Hashtbl.remove in_prog abs;
        Hashtbl.replace memo abs v;
        v
  and eval scope (e : Ast.expr) : cval =
    match e with
    | Ast.IntLit (n, _) -> CInt n
    | Ast.BoolLit (b, _) -> CBool b
    | Ast.Neg (sub, p) ->
        (match eval scope sub with
         | CInt i -> CInt (- i)
         | CExpr s -> CExpr ("(-" ^ s ^ ")")
         | CBool _ -> Error.failf p "'-' requires an integer constant")
    | Ast.BitNot (sub, p) ->
        (match eval scope sub with
         | CInt i -> CInt (lnot i)
         | CExpr s -> CExpr ("(~" ^ s ^ ")")
         | CBool _ -> Error.failf p "'~' requires an integer constant")
    | Ast.Not (sub, p) ->
        (match eval scope sub with
         | CBool b -> CBool (not b)
         | CExpr s -> CExpr ("(!" ^ s ^ ")")
         | CInt _ -> Error.failf p "'!' requires a bool constant")
    | Ast.Cast (sub, ann, p) ->
        let target = resolve_type_ann ~pos:p (res_ctx scope) ann in
        (match eval scope sub with
         | CInt i -> CInt (truncate target i)
         | CExpr s ->
             let cty = strip_trailing_space (c_type_prefix target) in
             CExpr ("((" ^ cty ^ ")" ^ s ^ ")")
         | CBool _ -> Error.failf p "cannot cast a bool constant")
    | Ast.BinOp (op, l, r, p) -> eval_binop scope op l r p
    | Ast.Var (n, p) -> resolve_eval scope [n] p
    | Ast.EnumLit { tname; variant; args = Ast.EATuple []; pos }
      when tname <> [] ->
        resolve_eval scope (tname @ [variant]) pos
    | Ast.SizeOf (ann, p) ->
        (* Folds to the C `sizeof(<c-type>)` expression — a target-dependent
           constant that cc resolves.  The result type is c_uint by exile
           convention. *)
        let t = resolve_type_ann ~pos:p (res_ctx scope) ann in
        let cty = strip_trailing_space (c_type_prefix t) in
        CExpr ("sizeof(" ^ cty ^ ")")
    | other ->
        Error.failf (Ast.expr_pos other) "not a constant expression"
  and resolve_eval scope path p =
    match resolve_ref scope path with
    | Some abs -> eval_entry abs
    | None ->
        Error.failf p "unknown constant '%s'" (String.concat "::" path)
  and eval_binop scope op l r p =
    let lv = eval scope l and rv = eval scope r in
    (* Render a cval as a C-expression fragment.  Used when at least one
       operand is a CExpr (e.g. `sizeof(...)`) — the result becomes an
       opaque-to-exile, parenthesised C expression that cc will resolve. *)
    let cstr = function
      | CInt i -> string_of_int i
      | CBool b -> if b then "1" else "0"
      | CExpr s -> s
    in
    let is_cexpr = function CExpr _ -> true | _ -> false in
    let mix_expr op_str =
      CExpr ("(" ^ cstr lv ^ " " ^ op_str ^ " " ^ cstr rv ^ ")")
    in
    let ints () =
      match lv, rv with
      | CInt a, CInt b -> (a, b)
      | _ ->
          Error.failf p "operator '%s' requires integer constants"
            (Ast.binop_name op)
    in
    let int_or_expr fold op_str =
      match lv, rv with
      | CInt a, CInt b -> CInt (fold a b)
      | (CInt _ | CExpr _), (CInt _ | CExpr _) -> mix_expr op_str
      | _ ->
          Error.failf p "operator '%s' requires integer constants"
            (Ast.binop_name op)
    in
    match op with
    | Ast.Add -> int_or_expr ( + ) "+"
    | Ast.Sub -> int_or_expr ( - ) "-"
    | Ast.Mul -> int_or_expr ( * ) "*"
    | Ast.Div ->
        (match lv, rv with
         | CInt a, CInt b ->
             if b = 0 then Error.failf p "division by zero" else CInt (a / b)
         | _ -> int_or_expr ( / ) "/")
    | Ast.Mod ->
        (match lv, rv with
         | CInt a, CInt b ->
             if b = 0 then Error.failf p "modulo by zero" else CInt (a mod b)
         | _ -> int_or_expr (mod) "%")
    | Ast.BitAnd -> int_or_expr (land) "&"
    | Ast.BitOr  -> int_or_expr (lor) "|"
    | Ast.BitXor -> int_or_expr (lxor) "^"
    | Ast.Shl    -> int_or_expr (lsl) "<<"
    | Ast.Shr    -> int_or_expr (asr) ">>"
    | Ast.Lt ->
        (match lv, rv with
         | CInt a, CInt b -> CBool (a < b)
         | _ when is_cexpr lv || is_cexpr rv -> mix_expr "<"
         | _ -> let (a, b) = ints () in CBool (a < b))
    | Ast.Gt ->
        (match lv, rv with
         | CInt a, CInt b -> CBool (a > b)
         | _ when is_cexpr lv || is_cexpr rv -> mix_expr ">"
         | _ -> let (a, b) = ints () in CBool (a > b))
    | Ast.LtEq ->
        (match lv, rv with
         | CInt a, CInt b -> CBool (a <= b)
         | _ when is_cexpr lv || is_cexpr rv -> mix_expr "<="
         | _ -> let (a, b) = ints () in CBool (a <= b))
    | Ast.GtEq ->
        (match lv, rv with
         | CInt a, CInt b -> CBool (a >= b)
         | _ when is_cexpr lv || is_cexpr rv -> mix_expr ">="
         | _ -> let (a, b) = ints () in CBool (a >= b))
    | Ast.EqEq | Ast.NotEq ->
        let op_str = if op = Ast.EqEq then "==" else "!=" in
        (match lv, rv with
         | CInt a, CInt b ->
             let eq = a = b in
             CBool (if op = Ast.EqEq then eq else not eq)
         | CBool a, CBool b ->
             let eq = a = b in
             CBool (if op = Ast.EqEq then eq else not eq)
         | _ when is_cexpr lv || is_cexpr rv -> mix_expr op_str
         | _ ->
             Error.failf p
               "'%s' compares two integers or two bools" (Ast.binop_name op))
    | Ast.And | Ast.Or ->
        let op_str = if op = Ast.And then "&&" else "||" in
        (match lv, rv with
         | CBool a, CBool b ->
             CBool (if op = Ast.And then a && b else a || b)
         | _ when is_cexpr lv || is_cexpr rv -> mix_expr op_str
         | _ ->
             Error.failf p
               "logical '%s' requires bool constants" (Ast.binop_name op))
    | Ast.Concat ->
        Error.failf p "'++' is not a constant expression"
  in
  (* Reject duplicate const paths up front (same scope + name). *)
  let rec dups seen = function
    | [] -> ()
    | (abs, _, (c : Ast.const_decl), _, _) :: rest ->
        if List.mem abs seen then
          Error.failf c.kpos "constant '%s' already defined" c.kname;
        dups (abs :: seen) rest
  in
  dups [] entries;
  let consts_index =
    List.map (fun (abs, mod_path, (c : Ast.const_decl), mangled, typ) ->
      let v = match eval_entry abs with
        | CInt i -> Some i
        | CBool _ | CExpr _ -> None  (* not a known integer at exile time *)
      in
      (mod_path, c.kname, (typ, mangled, v)))
      entries
  in
  let tp_consts =
    List.map (fun (abs, _, _, mangled, _) ->
      let lit = match eval_entry abs with
        | CInt i -> string_of_int i
        | CBool b -> if b then "1" else "0"
        | CExpr s -> "(" ^ s ^ ")"
      in
      (mangled, lit))
      entries
  in
  (consts_index, tp_consts)

let check_program program : tprogram =
  (* Per-program counter reset so gensym names (`__fv0`, `__fe0`, ...) start
     from 0 in each compilation — keeps golden output deterministic across
     test runs. *)
  for_gensym := 0;
  with_gensym := 0;
  trait_impl_table := [];
  trait_assoc_table := [];
  trait_decl_assocs := [];
  let mono_state = Mono.new_state () in
  let program = prepend_prelude program in
  (* DR-008 A1 — lift every captureless lambda to a fresh top-level
     fn before anything else looks at the program.  The replacement
     `Var "__lambda_N"` flows through ordinary fn-ptr decay; any
     reference to an enclosing local errors as "undefined variable"
     at elab time, which is exactly the captureless guarantee. *)
  let program = expand_lambdas program in
  (* DR-009 — expand `view Name(...) -> ... { ... }` into a nominal
     enum + view-fn before anything else looks at the program.
     Subsequent passes (derives, flatten, elab) see plain enums and
     fns. *)
  let program = expand_views program in
  (* Expand `@derive(...)` into real `impl Trait for Foo` blocks (after the
     prelude so the Eq / Clone traits they target are in scope). *)
  let program = expand_derives program in
  (* DR-030 Faza-1a Step C - synthesize `drop` impl for every
     struct carrying an `own *T` field.  Runs after derives so
     the synthesised drop sees any derive-generated methods
     already attached to the struct. *)
  let program = expand_drop_methods program in
  let flat = flatten_items program in
  (* main() must be at top level, not inside a module. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
      if f.name = "main" && path <> [] then
        Error.raise_ f.pos
          "'main' must be at top level, not inside a module")
    flat.funcs;
  (* Top-level function names land in C unmangled (modulo the `ex_`
     prefix), so they must not collide with C keywords.  Mod-internal fns
     get a `mod__` prefix and are safe. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
      if path = [] then check_c_ident f.pos "function" f.name)
    flat.funcs;
  (* Param names are emitted unprefixed in C parameter lists, so they
     also need the keyword check.  (Local lets are checked inside
     elab_body.) *)
  List.iter
    (fun (_, (f : Ast.func), _) ->
      List.iter
        (fun (p : Ast.param) -> check_c_ident f.pos "parameter" p.pname)
        f.params)
    flat.funcs;
  (* Order: structs need an enum skeleton (struct fields can mention
     enums); enums need a struct skeleton (variant payloads can mention
     structs).  Build a placeholder enum skeleton (paths only) first,
     pass it to build_struct_index, then build the full enum_index
     against the now-resolved struct_index. *)
  let enum_skeleton =
    List.map (fun (p, (e : Ast.enum_decl)) ->
      let variants =
        List.map (fun (v : Ast.enum_variant) ->
          { vsname = v.vname; vsfields = []; vsis_struct = false })
          e.evariants
      in
      { ename_path = p @ [e.ename]; evariants = variants;
        eis_pub = e.eis_pub; etparams = e.etparams;
        einstance_args = None;
        eis_must_use = e.emust_use;
        eis_debug = e.eis_debug })
      flat.enums
  in
  let ext_structs =
    List.map (fun (es : Ast.extern_struct) -> es.esname) flat.ext_structs
  in
  let ext_types =
    List.map (fun (et : Ast.extern_type) -> et.xtname) flat.ext_types
  in
  let ann_only_ctx = { (empty_ctx ~instances:mono_state) with
    modules = flat.modules;
    ext_structs; ext_types;
  } in
  let ext_consts =
    List.map (fun (ec : Ast.extern_const) ->
        (ec.ecname, resolve_type_ann ~pos:ec.ecpos ann_only_ctx ec.ecty))
      flat.ext_consts
  in
  let ext_vars =
    List.map (fun (ev : Ast.extern_var) ->
        (ev.evname, resolve_type_ann ~pos:ev.evpos ann_only_ctx ev.evty))
      flat.ext_vars
  in
  let ext_struct_fields =
    List.filter_map (fun (es : Ast.extern_struct) ->
        match es.esfields with
        | None -> None
        | Some fs ->
            Some (es.esname,
                  List.map (fun (n, t) ->
                      (n, resolve_type_ann ~pos:es.espos ann_only_ctx t)) fs))
      flat.ext_structs
  in
  let struct_index =
    build_struct_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~modules:flat.modules ~enums:enum_skeleton
      ~type_aliases:flat.type_aliases ~aliases:flat.aliases flat.structs
  in
  (* Defect-fix: `struct H { s: Slice<int> }` elaborates `Slice<int>`
     during pass-2 of [build_struct_index], when every skeleton still
     has `sfields_ty = []` (names-first, fields-later).  Mono caches
     that empty instance, so every later use of `Slice<int>` saw
     "no field 'ptr' / 'len'" or crashed with `Not_found`.  Refresh:
     after pass-2, walk every cached mono struct instance and, when
     it landed empty, rebuild its fields from the now-resolved
     skeleton.  Iterates because a refresh can flatten a nested
     `TStructApp` into another instance, which itself may be empty
     until the next pass. *)
  let refresh_instances () =
    let changed = ref false in
    let resolved_skel_of (inst : struct_sig) =
      List.find_opt
        (fun (s : struct_sig) ->
          s.stparams <> []
          && Mono.is_instance_of s.sname_path inst.sname_path)
        struct_index
    in
    let norm_ctx =
      { (empty_ctx ~instances:mono_state) with
        structs = struct_index } in
    let updated =
      List.map
        (fun (inst : struct_sig) ->
          if inst.sfields_ty <> [] then inst
          else
            match resolved_skel_of inst, inst.sinstance_args with
            | Some skel, Some args ->
                let bindings = List.combine skel.stparams args in
                let new_fields =
                  List.map (fun (n, t) ->
                    (n, normalize_apps norm_ctx (subst_typ bindings t)))
                    skel.sfields_ty
                in
                if new_fields <> [] then changed := true;
                { inst with sfields_ty = new_fields }
            | _ -> inst)
        mono_state.inst_structs
    in
    mono_state.inst_structs <- updated;
    !changed
  in
  let rec loop () = if refresh_instances () then loop () in
  loop ();
  let enum_index =
    build_enum_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~modules:flat.modules ~struct_index
      ~type_aliases:flat.type_aliases ~aliases:flat.aliases flat.enums
  in
  (* Same defect on the enum side: `struct H { o: Option<int> }`
     elaborated `Option<int>` against the placeholder enum_skeleton
     whose variants carried empty `vsfields`.  After enum_index
     resolves the real variants, walk cached enum instances and
     refresh any that landed empty.  Loop because a refreshed
     variant's payload type may itself be a still-empty TStructApp /
     TEnumApp. *)
  let refresh_enum_instances () =
    let changed = ref false in
    let resolved_skel_of (inst : enum_sig) =
      List.find_opt
        (fun (e : enum_sig) ->
          e.etparams <> []
          && Mono.is_instance_of e.ename_path inst.ename_path)
        enum_index
    in
    let norm_ctx =
      { (empty_ctx ~instances:mono_state) with
        structs = struct_index; enums = enum_index } in
    let needs_refresh (inst : enum_sig) =
      (* "Empty" = every variant has no fields AND the skeleton has
         at least one variant with fields (variants like `Option::None`
         legitimately have no payload). *)
      match resolved_skel_of inst with
      | None -> false
      | Some skel ->
          let skel_has_payload =
            List.exists (fun (v : variant_sig) -> v.vsfields <> []) skel.evariants
          in
          let inst_has_payload =
            List.exists (fun (v : variant_sig) -> v.vsfields <> []) inst.evariants
          in
          skel_has_payload && not inst_has_payload
    in
    let updated =
      List.map
        (fun (inst : enum_sig) ->
          if not (needs_refresh inst) then inst
          else
            match resolved_skel_of inst, inst.einstance_args with
            | Some skel, Some args ->
                let bindings = List.combine skel.etparams args in
                let new_variants =
                  List.map (fun (v : variant_sig) ->
                    { v with vsfields =
                        List.map (fun (n, t) ->
                          (n, normalize_apps norm_ctx (subst_typ bindings t)))
                          v.vsfields })
                    skel.evariants
                in
                changed := true;
                { inst with evariants = new_variants }
            | _ -> inst)
        mono_state.inst_enums
    in
    mono_state.inst_enums <- updated;
    !changed
  in
  let rec loop_enum () = if refresh_enum_instances () then loop_enum () in
  loop_enum ();
  (* Fail fast on infinitely-sized value types before any elaboration. *)
  let pos_of path =
    match List.find_opt
            (fun (p, (d : Ast.struct_decl)) -> p @ [d.sname] = path)
            flat.structs with
    | Some (_, d) -> d.spos
    | None ->
        (match List.find_opt
                 (fun (p, (d : Ast.enum_decl)) -> p @ [d.ename] = path)
                 flat.enums with
         | Some (_, d) -> d.epos
         | None -> Pos.zero)
  in
  detect_value_cycles ~structs:struct_index ~enums:enum_index ~pos_of;
  let (impl_funcs, virtual_modules) =
    expand_impls ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      flat struct_index enum_index flat.modules
  in
  let modules = flat.modules @ virtual_modules in
  let all_funcs = flat.funcs @ impl_funcs in
  (* A top-level user fn may not shadow a compiler builtin (`print`,
     `free`, `type_name`).  Builtins are looked up by single-segment
     path, so a top-level `fn print` is silently outranked at every
     call site and becomes dead code.  Methods (`impl X { fn free }`)
     and module fns are reached by qualified paths and don't collide —
     the prelude's `Allocator::free` is exactly such a case. *)
  List.iter
    (fun (path, (f : Ast.func), _) ->
       if path = []
          && List.exists (fun b -> b.bname = f.name) builtins then
         Error.failf f.pos
           "'%s' is a compiler builtin and cannot be redefined as a \
            top-level function — pick a different name" f.name)
    all_funcs;
  (* Method param names also need the C-keyword check (their first param
     is `self`, which is fine; rest are user-chosen). *)
  List.iter
    (fun (_, (f : Ast.func), _) ->
      List.iter
        (fun (p : Ast.param) -> check_c_ident f.pos "parameter" p.pname)
        f.params)
    impl_funcs;
  (* Fold every `const NAME: T = expr;` to a value first: use sites resolve
     against [consts_index] during signature + body elaboration (array
     sizes `[T; N]` in a fn signature need const values), and [tp_consts]
     becomes the `#define` block in the emitted C. *)
  let (consts_index, tp_consts) =
    eval_consts ~instances:mono_state ~modules ~aliases:flat.aliases
      ~struct_index ~enum_index flat.consts
  in
  let global =
    build_global_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~consts:consts_index ~ext_struct_fields
      ~struct_index ~enum_index ~modules ~aliases:flat.aliases
      ~type_aliases:flat.type_aliases all_funcs
  in
  (* `pub use foo::bar;` validation.  Building the global index just
     above means every fn/struct/enum has its absolute path on file;
     we can now check each alias's target resolves from the alias's
     own scope and emit a precise error at the alias decl site
     (rather than waiting for the broken short name to surface at
     a call site, where the error confusingly says "unknown function
     'bar'" with no pointer to the bad `pub use`). *)
  List.iter (fun (scope, _local_name, target_path, decl_pos) ->
    let probe_ctx = { (empty_ctx ~instances:mono_state) with
      global; structs = struct_index; enums = enum_index;
      modules; scope;
      aliases = flat.aliases; type_aliases = flat.type_aliases;
      ext_vars; ext_struct_fields;
      ext_structs; ext_types; ext_consts;
    } in
    let resolves =
      lookup_fn probe_ctx target_path <> None
      || lookup_struct probe_ctx target_path <> None
      || lookup_enum probe_ctx target_path <> None
      || lookup_type_alias probe_ctx target_path <> None
    in
    if not resolves then
      Error.failf decl_pos
        "'pub use %s' refers to unknown item — no fn, struct, enum, or \
         type alias with that path is visible from this scope"
        (String.concat "::" target_path))
    flat.aliases;
  (* Side-table for re-elaborating generic fn instances after the main
     loop: keyed by skeleton mangled name, value is (parent path, AST). *)
  let fn_asts =
    List.map (fun (p, (f : Ast.func), mangled) -> (mangled, (p, f))) all_funcs
  in
  (* Build a tfunc from one fn occurrence (skeleton or instance).  For
     generic fns at the top level we still produce a tfunc carrying
     the resolved (TVar-bearing) param/ret types but skip body elab —
     skeleton bodies can contain operations (BinOp, comparisons,
     field access) that aren't well-defined on free TVars.  Codegen
     filters the skeletons out via [is_concrete]; only the instance
     tfuncs (built later from pending jobs) carry real bodies. *)
  let elab_one_fn ~tvar_bindings (path, (f : Ast.func), mangled) =
    let ctx0_pre = { (empty_ctx ~instances:mono_state) with
      global; structs = struct_index; enums = enum_index;
      modules; scope = path; tparams = f.tparams;
      tvar_bindings; fn_asts; aliases = flat.aliases;
      type_aliases = flat.type_aliases;
      ext_vars; ext_struct_fields;
      ext_structs; ext_types; ext_consts; consts = consts_index;
    } in
    let ctx0 = { ctx0_pre with
      tbounds = resolve_ast_tbounds ~pos:f.pos ctx0_pre f.tbounds } in
    let param_tys =
      List.map (fun (p : Ast.param) ->
        resolve_type_ann ~pos:f.pos ctx0 p.pty) f.params
    in
    let exposed_extern = List.map fst ext_struct_fields in
    List.iter
      (forbid_naked_opaque ~exposed:exposed_extern f.pos)
      param_tys;
    let ret_ty = Option.map (resolve_type_ann ~pos:f.pos ctx0) f.ret_ty in
    Option.iter (forbid_naked_opaque ~exposed:exposed_extern f.pos) ret_ty;
    (* `main` carries no user-declared return type, but a `return` inside
       it sets the process exit code, so the body type-checks against
       `int` (codegen wraps it as C's `int main(void)`).  Unlike a normal
       value fn, main may still fall through — codegen appends a default
       `return 0;` — so it is exempt from the exhaustive-return check
       below. *)
    let effective_ret_ty =
      if f.name = "main" then Some t_i32 else ret_ty
    in
    let ctx = { ctx0 with ret_ty = effective_ret_ty } in
    let param_env =
      List.combine (List.map (fun (p : Ast.param) -> p.pname) f.params)
        param_tys
    in
    let is_skeleton = f.tparams <> [] && tvar_bindings = [] in
    let mut_params =
      List.filter_map (fun (p : Ast.param) ->
        if p.is_mut then Some p.pname else None) f.params
    in
    let (lets, tbody) =
      if f.is_extern || is_skeleton then ([], [])
      else elab_body ~ret_ty:effective_ret_ty ~is_main:(f.name = "main")
             ~mut_params ctx param_env f.body
    in
    (* Exhaustive-return check.  A value-returning fn must have a
       `return` on every control-flow path.  Without this, a non-
       returning path falls off the end and the caller sees whatever
       the C compiler left in the result register — UB.  Void fns and
       extern fns are skipped; skeletons too (they have no body to
       analyse — their concrete instances get checked individually). *)
    if not f.is_extern && not is_skeleton then
      (match ret_ty with
       | Some t when not (always_returns tbody) ->
           (* `;`-footgun: a trailing `e;` whose value matches the return
              type is almost always a dropped trailing expression.  Point
              at it rather than the generic "no return on every path". *)
           let dropped_tail =
             match List.rev tbody with
             | TExprStmt e :: _ when typ_eq e.ty t -> true
             | _ -> false
           in
           if dropped_tail then
             Error.failf f.pos
               "function '%s' returns %s but its last statement is a \
                discarded expression — drop the trailing `;` to return its \
                value, or add an explicit `return`"
               f.name (typ_name t)
           else
             Error.failf f.pos
               "function '%s' declared with return type %s, but not \
                every control-flow path ends in `return`"
               f.name (typ_name t)
       | _ -> ());
    { tf_path = path; tf_func = f; tf_mangled = mangled;
      tf_param_tys = param_tys; tf_ret_ty = ret_ty;
      tf_body = tbody; tf_lets = lets;
      tf_origin_pos = None }
  in
  let skeleton_tfuncs =
    List.map (elab_one_fn ~tvar_bindings:[]) all_funcs
  in
  (* Drain pending fn-instance jobs accumulated during skeleton elab.
     Each job re-runs body elaboration under a context where the fn's
     tparams substitute to concrete types.  New jobs may be queued
     during this drain (recursive generic calls); loop until the queue
     stays empty.  Mangled name on the instance tfunc is the instance's
     C-side name, set by [Mono.instantiate_fn]. *)
  let rec drain acc =
    match Mono.take_pending_fn_jobs mono_state with
    | [] -> List.rev acc
    | jobs ->
        let norm_ctx =
          { (empty_ctx ~instances:mono_state) with
            structs = struct_index; enums = enum_index }
        in
        let new_tfuncs =
          List.map (fun (job : Mono.pending_fn_job) ->
            let tf = elab_one_fn ~tvar_bindings:job.pj_bindings
              (job.pj_path, job.pj_func, job.pj_mangled)
            in
            (* Normalise any now-concrete generic application (Mono's
               subst leaves `Pair<i32,bool>` as an App; codegen needs the
               flat `Pair_i32_bool` instance). *)
            { tf with
              tf_param_tys = List.map (normalize_apps norm_ctx) job.pj_param_tys;
              tf_ret_ty = Option.map (normalize_apps norm_ctx) job.pj_ret_ty;
              tf_origin_pos = Some job.pj_origin_pos })
            jobs
        in
        drain (List.rev_append new_tfuncs acc)
  in
  let instance_tfuncs = drain [] in
  let tp_funcs = skeleton_tfuncs @ instance_tfuncs in
  (* tp_tuple_types / fnptr / array / uses_heap / uses_string_h are
     collected after the prelude-mono DCE filter below — see the
     comment at that site. *)
  (* Drain monomorphic instances accumulated during resolve_type_ann
     into the program's indexes.  Instances accumulate in reverse
     registration order; reversing puts them in roughly the order
     users wrote them.  Codegen emits them inline with regular
     non-generic decls. *)
  let mono_structs = List.rev mono_state.inst_structs in
  let mono_enums = List.rev mono_state.inst_enums in
  (* mono_state.inst_funcs intentionally unused here — the registered
     fn-instance signatures already live on the instance tfuncs, and
     callers emit instance mangled names directly. *)
  (* DCE for prelude mono structs.  Walk every mono tfunc's signature
     and let-types looking for `TStruct ["Allocator"]`; if no such
     reference exists, drop the struct decl AND the impl methods that
     came from the prelude.  Generic skeletons (`alloc<T>`/`free<T>`)
     mention `Allocator` in `self` only as a definition site, so they
     are excluded from the walk; their concrete instances carry the
     reference instead and ARE walked.  Prelude origin is identified
     by `tf_func.pos.file = "<prelude>"`, which is essential: a user's
     `mod Allocator { fn helper() }` registers `tf_path = ["Allocator"]`
     identical to the prelude impl methods, but its origin file differs. *)
  let rec typ_mentions target = function
    | TStruct p -> p = target
    | TStructApp { path; args } | TEnumApp { path; args } ->
        path = target || List.exists (typ_mentions target) args
    | TPtr inner | TOwnPtr inner | TConstPtr inner ->
        typ_mentions target inner
    | TArray { elem; _ } -> typ_mentions target elem
    | TTuple ts -> List.exists (typ_mentions target) ts
    | TFnPtr { params; ret } ->
        List.exists (typ_mentions target) params
        || (match ret with Some t -> typ_mentions target t | None -> false)
    | _ -> false
  in
  let tfunc_mentions target tf =
    let body_mentions = ref false in
    let visit_expr (te : texpr) =
      if typ_mentions target te.ty then body_mentions := true
    in
    let visit_stmt s =
      List.iter (iter_texpr visit_expr) (tstmt_own_exprs s)
    in
    List.iter (iter_tstmt visit_stmt) tf.tf_body;
    !body_mentions
    || List.exists (typ_mentions target) tf.tf_param_tys
    || (match tf.tf_ret_ty with Some t -> typ_mentions target t | None -> false)
    || List.exists (fun (_, t) -> typ_mentions target t) tf.tf_lets
  in
  let is_from_prelude tf = tf.tf_func.Ast.pos.file = "<prelude>" in
  (* Prelude-origin methods (e.g. `StringBuilder::with_capacity`) all
     mention their own struct in self/ret — they'd vote themselves
     live.  A mono prelude type is "used" only when concrete USER
     code (or a USER-driven monomorphic instance) references it.
     Transitively: if `String` is used and its `alloc: Allocator`
     field pulls Allocator in, Allocator stays — otherwise the
     emitted C declares `struct ex_String { ... struct ex_Allocator
     alloc; }` without a definition for ex_Allocator. *)
  let user_direct =
    List.filter
      (fun n ->
        List.exists
          (fun tf ->
            tf.tf_func.Ast.tparams = []
            && not (is_from_prelude tf)
            && tfunc_mentions [n] tf)
          tp_funcs)
      prelude_mono_struct_names
  in
  let used_prelude =
    let keep = ref user_direct in
    let changed = ref true in
    let add_from_typ t =
      List.iter (fun candidate ->
        if List.mem candidate prelude_mono_struct_names
           && not (List.mem candidate !keep)
           && typ_mentions [candidate] t
        then (keep := candidate :: !keep; changed := true))
        prelude_mono_struct_names
    in
    let is_prelude_struct_name = function
      | [n] -> List.mem n prelude_mono_struct_names
      | _ -> false
    in
    while !changed do
      changed := false;
      (* Field-type reachability — DR-002 W1:
         (1) A KEPT prelude struct pulls in preludes its own fields
             reference (`String { ..., alloc: Allocator }` → keep
             Allocator).
         (2) A USER struct ALWAYS pulls in preludes its fields
             reference, because user struct decls are emitted
             unconditionally — without this `struct Token { name:
             String }` would emit `struct ex_Token { struct
             ex_String name; }` with no definition for `ex_String`
             and cc would reject `incomplete type`. *)
      let sweep_struct (s : struct_sig) =
        let is_prelude_decl = is_prelude_struct_name s.sname_path in
        let kept_prelude = match s.sname_path with
          | [n] when is_prelude_decl -> List.mem n !keep
          | _ -> false
        in
        (* Skip generic skeletons (`stparams <> []`): their fields
           may reference a concrete prelude type (`Vec<T>.alloc:
           Allocator`) but the skeleton itself never reaches codegen
           — only its mono instances do, and those carry the
           reference verbatim with `stparams = []`.  Without this
           filter Vec's `alloc` field would pull Allocator into
           every hello-world program. *)
        if s.stparams <> [] then ()
        else if (is_prelude_decl && kept_prelude) || not is_prelude_decl then
          List.iter (fun (_, t) -> add_from_typ t) s.sfields_ty
      in
      List.iter sweep_struct struct_index;
      (* Mono instances (`Vec_i32`, `HashMap_String_i32`, …) live
         separately from struct_index but reach codegen the same
         way.  A user struct `struct A { v: Vec<int> }` registers
         `Vec_i32` whose `alloc: Allocator` field pulls Allocator
         into the keep set. *)
      List.iter sweep_struct mono_structs;
      (* Same field-type reachability for enum variant payloads:
         `enum H { Has(Allocator) | Empty }` embeds Allocator by
         value in the union slot.  Skip generic skeletons
         (`etparams <> []`) for the same reason — only their mono
         instances reach codegen and carry the concrete reference. *)
      let sweep_enum (e : enum_sig) =
        if e.etparams = [] then
          List.iter (fun (v : variant_sig) ->
            List.iter (fun (_, t) -> add_from_typ t) v.vsfields)
            e.evariants
      in
      List.iter sweep_enum enum_index;
      List.iter sweep_enum mono_enums;
      (* Method signatures of a kept prelude struct also count
         (`String::build(sb: StringBuilder)` keeps StringBuilder
         alive even though String's fields don't reference it). *)
      List.iter (fun tf ->
        match tf.tf_path with
        | [n] when List.mem n !keep && is_from_prelude tf ->
            List.iter add_from_typ tf.tf_param_tys;
            Option.iter add_from_typ tf.tf_ret_ty
        | _ -> ())
        tp_funcs
    done;
    !keep
  in
  let struct_drop_set =
    List.filter
      (fun n -> not (List.mem n used_prelude))
      prelude_mono_struct_names
  in
  let struct_index =
    List.filter (fun s ->
      match s.sname_path with
      | [n] when List.mem n struct_drop_set -> false
      | _ -> true)
      struct_index
  in
  let tp_funcs =
    List.filter (fun tf ->
      not (is_from_prelude tf
           && match tf.tf_path with
              | [n] when List.mem n struct_drop_set -> true
              | _ -> false))
      tp_funcs
  in
  (* Reachability DCE for non-generic prelude fns (mod str, future
     mod sys, …): seed live set with every mangled name a non-prelude
     tfunc references, then transitively pull in prelude fns those
     references reach.  Anything still unreached and prelude-origin
     gets dropped, so a hello-world that never names `str::*` doesn't
     carry the byte-loop bodies. *)
  let referenced_mangled tf =
    let names = ref [] in
    let visit_expr (te : texpr) =
      match te.e with
      | TCall { mangled; _ } -> names := mangled :: !names
      | _ -> ()
    in
    let visit_stmt s =
      List.iter (iter_texpr visit_expr) (tstmt_own_exprs s)
    in
    List.iter (iter_tstmt visit_stmt) tf.tf_body;
    !names
  in
  let prelude_module_paths = [ ["str"] ] in
  let is_drop_candidate tf =
    is_from_prelude tf
    && tf.tf_func.Ast.tparams = []
    && List.mem tf.tf_path prelude_module_paths
  in
  (* Re-seed reachability so every always-kept tfunc (user + prelude
     that isn't a drop candidate) is in the BFS root set.  Without
     this, a prelude impl like `String::eq` that calls `str::eq`
     wouldn't pull the body in — `str::eq` would stay unreached
     and the C output ends with an unresolved external. *)
  let live_set =
    let live = Hashtbl.create 64 in
    let queue = Queue.create () in
    List.iter (fun tf ->
      if not (is_drop_candidate tf) then begin
        Hashtbl.replace live tf.tf_mangled ();
        Queue.add tf queue
      end)
      tp_funcs;
    while not (Queue.is_empty queue) do
      let tf = Queue.pop queue in
      List.iter (fun m ->
        if not (Hashtbl.mem live m) then begin
          Hashtbl.replace live m ();
          List.iter (fun caller ->
            if caller.tf_mangled = m then Queue.add caller queue)
            tp_funcs
        end)
        (referenced_mangled tf)
    done;
    live
  in
  let tp_funcs =
    List.filter (fun tf ->
      not (is_drop_candidate tf) || Hashtbl.mem live_set tf.tf_mangled)
      tp_funcs
  in
  (* After dropping prelude-mono methods, re-collect type tables and
     the include flags — the dropped bodies often referenced fn-ptr /
     array / tuple shapes (e.g. `Allocator`'s alloc_fn) and pulled in
     `<string.h>` (`cstr_len` for `push_str`) that nothing else uses.
     Leaving those in would emit dead typedefs and includes in every
     program that never touches the prelude type. *)
  let (tp_tuple_types, tp_fnptr_types, tp_array_types) =
    collect_tuple_types_of
      ~structs:(struct_index @ mono_structs)
      ~enums:(enum_index @ mono_enums)
      tp_funcs in
  let tp_uses_heap = uses_heap_of tp_funcs in
  let tp_uses_string_h = uses_string_h_of tp_funcs in
  let tp_uses_default_allocator = uses_default_allocator_of tp_funcs in
  (* Drop mono instances that no surviving tfunc still mentions.  When
     a prelude method like `StringBuilder::as_slice` is DCE'd, the
     `Slice<u8>` instance it registered would otherwise leak into the
     output as a dead struct decl.  Transitive closure: a mono struct
     stays alive if its name is mentioned anywhere in a surviving
     tfunc's signature/lets OR in another surviving mono struct's
     fields. *)
  let mentioned_paths =
    let paths = ref [] in
    let add p = if not (List.mem p !paths) then paths := p :: !paths in
    let rec walk = function
      | TStruct p | TEnum p -> add p
      | TStructApp { path; args } | TEnumApp { path; args } ->
          add path; List.iter walk args
      | TPtr t | TOwnPtr t | TConstPtr t -> walk t
      | TArray { elem; _ } -> walk elem
      | TTuple ts -> List.iter walk ts
      | TFnPtr { params; ret } ->
          List.iter walk params; Option.iter walk ret
      | _ -> ()
    in
    List.iter (fun tf ->
      List.iter walk tf.tf_param_tys;
      Option.iter walk tf.tf_ret_ty;
      List.iter (fun (_, t) -> walk t) tf.tf_lets)
      tp_funcs;
    (* Top-level user/prelude structs and enums are always emitted —
       seed them so anything they reference (e.g. a `Slice<int>` field)
       is kept too, even when no fn body mentions it. *)
    List.iter (fun (s : struct_sig) -> add s.sname_path) struct_index;
    List.iter (fun (e : enum_sig) -> add e.ename_path) enum_index;
    (* Transitive: keep walking through struct fields / enum variants
       of any reachable struct (mono OR top-level — a user struct
       `H { s: Slice<int> }` pulls Slice_i32 in via its field even
       though H itself lives in struct_index) until stable. *)
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter (fun (s : struct_sig) ->
        if List.mem s.sname_path !paths then
          List.iter (fun (_, t) ->
            let before = List.length !paths in
            walk t;
            if List.length !paths <> before then changed := true)
            s.sfields_ty)
        (struct_index @ mono_structs);
      List.iter (fun (e : enum_sig) ->
        if List.mem e.ename_path !paths then
          List.iter (fun (v : variant_sig) ->
            List.iter (fun (_, t) ->
              let before = List.length !paths in
              walk t;
              if List.length !paths <> before then changed := true)
              v.vsfields)
            e.evariants)
        (enum_index @ mono_enums)
    done;
    !paths
  in
  let mono_structs =
    List.filter (fun (s : struct_sig) ->
      List.mem s.sname_path mentioned_paths)
      mono_structs
  in
  let mono_enums =
    List.filter (fun (e : enum_sig) ->
      List.mem e.ename_path mentioned_paths)
      mono_enums
  in
  (* `@debug` field-type validation.  Every field of a `@debug` struct
     (or every payload of a `@debug` enum variant) must itself be
     printable — primitive int-like, bool, str, pointer (printed as
     address), or another `@debug` aggregate.  Catches the user trying
     to debug a type that references an opaque C handle or an
     un-debug-able aggregate. *)
  let all_structs = struct_index @ mono_structs in
  let all_enums = enum_index @ mono_enums in
  (* Hash the @debug paths up-front; field_ty_ok recurses per-tuple-element
     so the inner List.exists scan was N*M for programs with many @debug
     aggregates.  Now field validation is O(field_tree_size). *)
  let debug_structs = Hashtbl.create 16 in
  List.iter (fun (s : struct_sig) ->
    if s.sis_debug then Hashtbl.replace debug_structs s.sname_path ())
    all_structs;
  let debug_enums = Hashtbl.create 16 in
  List.iter (fun (e : enum_sig) ->
    if e.eis_debug then Hashtbl.replace debug_enums e.ename_path ())
    all_enums;
  let struct_is_debug path = Hashtbl.mem debug_structs path in
  let enum_is_debug path = Hashtbl.mem debug_enums path in
  let rec field_ty_ok = function
    | TInt _ | TCInt _ | TCShort _ | TCLong _
    | TCChar | TCSChar | TCUChar | TBool | TFloat _ | TString | TPtr _
    | TOwnPtr _ | TConstPtr _ -> true
    | TStruct p -> struct_is_debug p
    | TEnum p -> enum_is_debug p
    (* Tuple fields aren't rendered by the synthesized printer yet
       (codegen's emit_field_debug has no tuple case) — reject rather
       than crash; revisit if tuple-in-@debug becomes worth the syntax. *)
    | TTuple _ | TArray _
    | TCVoid | TExtStruct _ | TExtAlias _
    | TFnPtr _ | TNullPtr | TVar _
    | TStructApp _ | TEnumApp _ | TAssocProj _ -> false
  in
  (* A struct field / enum payload of opaque-`extern struct` type by value
     is rejected (exile doesn't know its layout — only `*Opaque` is usable;
     a by-value field would be C "incomplete type").  Validated over BOTH
     skeletons (`struct S { w: raw::Win }`) and mono instances (`Pair<raw::Win,
     int>`, where the opaque arrives as a by-value field of the instance).
     Exposed extern structs (`extern struct Foo { ... }`) carry a known
     layout and are fine. *)
  let exposed_extern = List.map fst ext_struct_fields in
  let struct_decl_pos path =
    match List.find_opt
            (fun (p, (d : Ast.struct_decl)) -> p @ [d.sname] = path) flat.structs with
    | Some (_, d) -> d.spos
    | None ->
        (match List.find_opt
                 (fun (p, (d : Ast.struct_decl)) ->
                    Mono.is_instance_of (p @ [d.sname]) path) flat.structs with
         | Some (_, d) -> d.spos
         | None -> Pos.zero)
  in
  let enum_decl_pos path =
    match List.find_opt
            (fun (p, (d : Ast.enum_decl)) -> p @ [d.ename] = path) flat.enums with
    | Some (_, d) -> d.epos
    | None ->
        (match List.find_opt
                 (fun (p, (d : Ast.enum_decl)) ->
                    Mono.is_instance_of (p @ [d.ename]) path) flat.enums with
         | Some (_, d) -> d.epos
         | None -> Pos.zero)
  in
  List.iter (fun (s : struct_sig) ->
    let pos = struct_decl_pos s.sname_path in
    List.iter (fun (_, ft) ->
      forbid_naked_opaque ~exposed:exposed_extern pos ft)
      s.sfields_ty)
    all_structs;
  List.iter (fun (e : enum_sig) ->
    let pos = enum_decl_pos e.ename_path in
    List.iter (fun (vs : variant_sig) ->
      List.iter (fun (_, ft) ->
        forbid_naked_opaque ~exposed:exposed_extern pos ft) vs.vsfields)
      e.evariants)
    all_enums;
  (* @debug-able field check.  Only concrete structs/enums are validated
     (and emitted): a generic SKELETON has free-TVar fields that aren't
     debug-able by themselves — its concrete instances are checked
     individually, where the field types are real. *)
  List.iter (fun (s : struct_sig) ->
    if s.sis_debug && s.stparams = [] then
      List.iter (fun (fname, fty) ->
        if not (field_ty_ok fty) then
          Error.failf (struct_decl_pos s.sname_path)
            "'@debug' struct '%s': field '%s' of type %s is not debug-able \
             (mark the type `@debug`, or remove `@debug` from the struct)"
            (render_typ_user_facing ~structs:all_structs ~enums:all_enums
               (TStruct s.sname_path))
            fname (typ_name fty))
        s.sfields_ty)
    all_structs;
  List.iter (fun (e : enum_sig) ->
    if e.eis_debug && e.etparams = [] then
      List.iter (fun (vs : variant_sig) ->
        List.iter (fun (fname, fty) ->
          if not (field_ty_ok fty) then
            Error.failf (enum_decl_pos e.ename_path)
              "'@debug' enum '%s': variant '%s' payload '%s' of type %s \
               is not debug-able"
              (render_typ_user_facing ~structs:all_structs ~enums:all_enums
                 (TEnum e.ename_path))
              vs.vsname fname (typ_name fty))
          vs.vsfields)
        e.evariants)
    all_enums;
  { tp_funcs;
    tp_struct_decls = flat.structs;
    tp_struct_index = all_structs;
    tp_enum_index = all_enums;
    tp_global = global;
    tp_modules = modules;
    tp_uses_heap;
    tp_uses_string_h;
    tp_uses_default_allocator;
    tp_tuple_types;
    tp_fnptr_types;
    tp_c_includes = flat.c_includes;
    tp_ext_consts = ext_consts;
    tp_ext_vars = ext_vars;
    tp_consts;
    tp_array_types }
