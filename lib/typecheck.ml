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
}

(* Skeleton ctx with every list field defaulted to [] and ret_ty None.
   [instances] is required because Mono.state has no zero value (it
   carries the mutable mono-job queue and visited set).  Use as
   `{ (empty_ctx ~instances) with structs = ...; modules; scope; ... }`
   — each callsite lists only its non-default fields, and adding a new
   field to fn_ctx requires editing this constant only. *)
let empty_ctx ~instances = {
  global = []; structs = []; enums = []; modules = [];
  scope = []; tparams = []; tvar_bindings = []; fn_asts = [];
  aliases = []; ext_vars = []; ext_struct_fields = [];
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
                    (match prefix with
                     | [] -> None
                     | _ -> walk (parent_path prefix)))
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
  | Ast.IntLit _ | Ast.BoolLit _ | Ast.StringLit _ | Ast.NullLit _
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
  | Ast.StructLit { tname; fields; base; pos } ->
      Ast.StructLit { tname;
                      fields = List.map (fun (n, e) -> (n, sub e)) fields;
                      base = subo base; pos }
  | Ast.FieldAccess (e', n, p) -> Ast.FieldAccess (sub e', n, p)
  | Ast.Ref (e', p) -> Ast.Ref (sub e', p)
  | Ast.Deref (e', p) -> Ast.Deref (sub e', p)
  | Ast.New { tname; fields; base; pos } ->
      Ast.New { tname;
                fields = List.map (fun (n, e) -> (n, sub e)) fields;
                base = subo base; pos }
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
                  head assoc assoc head assoc))
  | _ -> None

let rec resolve_type_ann_raw ~pos ctx ann =
  let recur = resolve_type_ann_raw ~pos ctx in
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
  | Ast.TyTuple ts -> TTuple (List.map recur ts)
  | Ast.TyPtr t -> TPtr (recur t)
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
  | Ast.TyStruct { path; args = [] } ->
      (* Non-generic case: tparam reference / extern type / extern
         struct / struct / enum.  ext_types / ext_structs are flat
         (single C symbol name); qualified paths like `raw::ULONG`
         accept the path as long as the last segment matches.  Tparam
         ref is single-segment only. *)
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
  | Ast.TyStruct { path; args } ->
      (* Generic application `Foo<T1, T2>`.  We do NOT instantiate here:
         args may still contain free `TVar`s (a generic `impl`/fn
         skeleton, e.g. `self: Pair<A, B>`).  Produce a TStructApp /
         TEnumApp carrying the skeleton's absolute path + resolved args;
         `resolve_type_ann` normalises it to a flat instance once every
         arg is concrete.  Arity is checked here (no concreteness
         needed). *)
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
                  (String.concat "::" path)))

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
         normalised once monomorphization substitutes a concrete head. *)
      let head = normalize_apps ctx head in
      if not (is_concrete head) then TAssocProj { head; assoc }
      else
        (match typ_head_path head with
         | None -> TAssocProj { head; assoc }
         | Some hp ->
             let matches =
               List.filter_map
                 (fun ((_trait, target), assocs) ->
                   if target = hp && List.mem_assoc assoc assocs
                   then Some (List.assoc assoc assocs) else None)
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
  | TConstPtr d, TConstPtr a -> expand_inst_pair ctx (d, a)
  (* `*const T` declaration paired with a `*U` actual: pointee-immutability
     coercion is implicit, so infer `T = U` from the pointees alone (the
     coercion itself runs after instance-resolution in the caller's
     type check). *)
  | TConstPtr d, TPtr a -> expand_inst_pair ctx (d, a)
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
    | [_] -> t_i32
    | tys ->
        Error.failf pos "%s() takes exactly one argument, got %d"
          name (List.length tys)

let builtin_print = { bname = "print"; bcheck = print_like_bcheck ~name:"print" }
let builtin_println =
  { bname = "println"; bcheck = print_like_bcheck ~name:"println" }

let builtin_free = {
  bname = "free";
  bcheck = (fun ~ctx:_ ~pos ~args ~allow_void ->
    match args with
    | [ { e = TRef _; _ } ] ->
        (* Syntactic guard: `free(&...)` is always wrong — `&` produces
           a stack-or-field address, never a heap pointer.  Calling free
           on it would corrupt the allocator's bookkeeping.  Real heap
           pointers come from `new T { ... }` (or are propagated through
           bindings from a `new`-let). *)
        Error.failf pos
          "'free' expects a heap-allocated pointer (from 'new'); got \
           '&...' which is a stack or field address — this would \
           corrupt the allocator"
    | [ { ty = TPtr _; _ } ] when allow_void -> t_i32
    | [ { ty = TPtr _; _ } ] ->
        Error.failf pos "'free' returns void, cannot use as a value"
    | [ { ty = other; _ } ] ->
        Error.failf pos "'free' expects a pointer, got %s" (typ_name other)
    | xs ->
        Error.failf pos "free() takes exactly one argument, got %d"
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

let builtins =
  [ builtin_print; builtin_println; builtin_free; builtin_type_name;
    builtin_cstr_len ]

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
let check_call_args ~pos ~kind ~name ?(variadic=false) ~param_tys
                    ~raw_args ~targs ~ret_ty ~allow_void () : typ =
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
  let fixed_arg_tys = take expected_n arg_tys in
  List.iteri (fun i (exp, act) ->
    if not (coercible_to ~from:act ~to_:exp)
       && not (int_lit_fits (List.nth raw_args i) exp)
    then
      Error.failf pos
        "argument %d of '%s': expected %s, got %s"
        (i + 1) name (typ_name exp) (typ_name act))
    (List.combine param_tys fixed_arg_tys);
  match ret_ty with
  | Some t -> t
  | None when allow_void -> t_i32
  | None ->
      Error.failf pos "'%s' returns void, cannot use as a value" name

(* Registry of `impl Trait for Type` pairs, as (trait-name, target
   abs-path).  Populated by [expand_impls] (which resolves both the trait
   and the target), read by [resolve_call_dispatch] to enforce a generic
   fn's `<T: Trait>` bounds at instantiation.  Trait names are matched by
   their last path segment — sufficient while traits are uniquely named
   (the conformance check already rejects ambiguity). *)
(* True when [ty] has an `impl <trait_name> for <ty>` registered. *)
let type_impls_trait ~trait_name ty =
  match typ_head_path ty with
  | None -> false
  | Some path -> List.mem (trait_name, path) !trait_impl_table

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
      | Some exp, Some r -> [(r, exp)]
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
    List.iter (fun (tparam, trait_written) ->
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
              (typ_name ty) trait_name tparam trait_name func.Ast.name)
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
let rec lower_pattern ?(allow_or = true) ctx (value_ty : typ)
    (pat : Ast.pattern) : tpattern * (string * typ) list =
  match pat with
  | Ast.PWildcard _ -> (TPWildcard, [])
  | Ast.PVar (n, _) -> (TPVar n, [ (n, value_ty) ])
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
          let (tp, bs) = lower_pattern ~allow_or:false ctx ft sp in
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
        | TPWildcard | TPVar _ -> ()
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
  | Ast.BoolLit (b, _) -> { e = TBoolLit b; ty = TBool; pos }
  | Ast.NullLit _ -> { e = TNullLit; ty = TNullPtr; pos }
  | Ast.StringLit (s, _) -> { e = TStringLit s; ty = TString; pos }
  | Ast.Neg (sub, neg_pos) ->
      let sub' = elab_expr ?expected ctx env sub in
      if not (is_int_like sub'.ty) then
        Error.failf neg_pos "negation '-' requires an integer, got %s"
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
      let result_t =
        match op with
        | (Ast.Div | Ast.Mod) when expr_int_lit r = Some 0 ->
            (* Constant division / modulo by zero is undefined in C (and
               would leak `-Wdiv-by-zero`); reject it at compile time. *)
            Error.failf pos "%s by zero"
              (match op with Ast.Mod -> "modulo" | _ -> "division")
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
            need_int_operands ();
            (if not (typ_eq l'.ty r'.ty) then
               ignore (promote_int_widen ()));
            TBool
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
      if is_int_like sub'.ty && is_int_like tgt then ()
      else if is_ptr sub'.ty && is_ptr tgt then ()
      else if is_int_like sub'.ty && is_ptr tgt then ()
      else
        Error.failf cast_pos
          "cannot cast %s to %s (supported: int↔int, ptr↔ptr, int→ptr)"
          (typ_name sub'.ty) (typ_name tgt);
      { e = TCast (sub', ann); ty = tgt; pos }
  | Ast.Ref (sub, _) ->
      let sub' = elab_expr ctx env sub in
      { e = TRef sub'; ty = TPtr sub'.ty; pos }
  | Ast.Deref (sub, deref_pos) ->
      let sub' = elab_expr ctx env sub in
      let ty =
        match sub'.ty with
        | TPtr t | TConstPtr t -> t
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
      if not (is_int_like tindex.ty) then
        Error.failf idx_pos
          "index must be an integer, got %s" (typ_name tindex.ty);
      { e = TIndex { base = tbase; index = tindex }; ty = elem_ty; pos }
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
      let tscrut = elab_expr ctx env scrutinee in
      let ename_path =
        match tscrut.ty with
        | TEnum p -> p
        | other ->
            Error.failf match_pos
              "'match' requires an enum value, got %s" (typ_name other)
      in
      let tarms =
        List.map (fun (a : Ast.match_arm) ->
          let (tpat, binds) = lower_pattern ctx tscrut.ty a.pat in
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
          let tbody = elab_expr ~allow_void ctx arm_env a.body in
          { tpat; tguard; tbody; tdiverges = false; tarm_pos = a.arm_pos })
          arms
      in
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
      List.iteri (fun i (a : Ast.match_arm) ->
        let before =
          List.concat
            (List.filteri (fun j _ -> j < i && is_unguarded j) rows_per_arm)
        in
        let any_useful =
          List.exists (fun row -> useful ctx [ tscrut.ty ] before row)
            (List.nth rows_per_arm i)
        in
        if not any_useful then
          Error.failf a.arm_pos
            "unreachable match arm: earlier arms already cover this case")
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
       | _ -> ());
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
  | (Ast.New { tname; fields; base; pos = lit_pos } as raw_lit) ->
      (* Enum struct-variant ctor: parser emits StructLit for
         `Foo::V { f: e }`; if the path resolves to an enum variant,
         re-elab via the EnumLit branch. *)
      let as_struct_lit = match raw_lit with
        | Ast.StructLit _ -> true | _ -> false
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
           let sname_path = s_concrete.sname_path in
           let result_node =
             if as_struct_lit
             then TStructLit { sname_path; fields = tfields; base = tbase }
             else TNew { sname_path; fields = tfields; base = tbase }
           in
           let result_ty =
             let st = TStruct sname_path in
             if as_struct_lit then st else TPtr st
           in
           { e = result_node; ty = result_ty; pos })
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
                 List.map2 (fun (n, _) te -> (n, te))
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
                 (* Emit args in variant's field-declaration order. *)
                 List.map (fun (n, _) ->
                   let (_, te, _) =
                     List.find (fun (m, _, _) -> m = n) fs
                   in
                   (n, te))
                   v_used.vsfields
           in
           { e = TEnumLit { ename_path = result_path; variant; tag;
                            args = targs };
             ty = TEnum result_path; pos })
  | Ast.Call { callee = path; args; pos = call_pos } ->
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
                     let result_ty =
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
                          let result_ty =
                            check_call_args ~pos:call_pos
                              ~kind:"function" ~name:display
                              ~variadic:fn_variadic ~param_tys
                              ~raw_args:args ~targs ~ret_ty ~allow_void ()
                          in
                          { e = TCall { mangled; args = targs };
                            ty = result_ty; pos }))))
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
        | TPtr _ | TConstPtr _ | TNullPtr -> true
        | _ -> false
      in
      (match is_primitive, name, args with
       | true, ("eq" | "ne"), [ arg ] ->
           let targ = elab_expr ctx env arg in
           if not (typ_eq trecv.ty targ.ty) then
             Error.failf mc_pos
               "'.%s' on %s expects a %s argument, got %s"
               name (typ_name trecv.ty) (typ_name trecv.ty) (typ_name targ.ty);
           let op = if name = "eq" then Ast.EqEq else Ast.NotEq in
           { e = TBinOp (op, trecv, targ); ty = TBool; pos }
       | true, "hash", [] ->
           (* Built-in `hash` on primitives → reinterpret as `u32` (a C
              cast).  Lets `@derive(Hash)` fold integer/bool fields.  No
              content hash for str / pointers yet. *)
           (match trecv.ty with
            | TInt _ | TCInt _ | TCShort _ | TCLong _ | TCChar | TCSChar
            | TCUChar | TBool | TExtAlias _ ->
                let u32_ann = Ast.TyInt { signed = false; width = Ast.W32 } in
                { e = TCast (trecv, u32_ann);
                  ty = TInt { signed = false; width = Ast.W32 }; pos }
            | _ ->
                Error.failf mc_pos
                  "`hash` is not built-in for %s (str / pointer content \
                   hashing is not supported yet)" (typ_name trecv.ty))
       | _ ->
      let struct_path =
        match trecv.ty with
        | TStruct p -> p
        | TPtr (TStruct p) -> p
        | TConstPtr (TStruct p) -> p   (* methods can mutate through self;
                                          MVP doesn't gate this — `&self`
                                          vs `&mut self` is future work *)
        | TEnum p | TPtr (TEnum p) | TConstPtr (TEnum p) -> p
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
                let result_ty =
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
                let result_ty =
                  check_call_args ~pos:mc_pos
                    ~kind:"method" ~name:display
                    ~param_tys:rest_params ~raw_args:args ~targs
                    ~ret_ty ~allow_void ()
                in
                (* Auto-ref / auto-deref: align receiver shape with the
                   method's self-param shape. *)
                (* Auto-ref / -deref by pointer-shape, independent of
                   whether the receiver is a struct or an enum. *)
                let recv_is_ptr =
                  match trecv.ty with TPtr _ | TConstPtr _ -> true | _ -> false in
                let self_is_ptr =
                  match self_ty with TPtr _ | TConstPtr _ -> true | _ -> false in
                let trecv_adj =
                  match self_is_ptr, recv_is_ptr with
                  | false, false | true, true -> trecv
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
        | TStruct p | TPtr (TStruct p) | TConstPtr (TStruct p) ->
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
        add_decl name t_actual pos;
        if is_mut then Hashtbl.replace mut_names name ();
        ((name, t_actual) :: env, TLet { name; value = tvalue; pos })
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
        let tvalue = elab_expr ctx env value in
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
          | TStruct p | TPtr (TStruct p) ->
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
        if not (typ_eq tvalue.ty fty) && not (int_lit_fits value fty) then
          Error.failf pos
            "field '%s': expected %s, got %s"
            field (typ_name fty) (typ_name tvalue.ty);
        (env, TAssignField { target = ttarget; field;
                                  value = tvalue; pos })
    | Ast.AssignIndex { base; index; value; pos } ->
        let tbase = elab_expr ctx env base in
        let through_ptr, elem_ty =
          match tbase.ty with
          | TArray { elem; _ } -> false, elem
          | TPtr elem ->
              (* Raw write-through-pointer `p[i] = v` (Delta B): the
                 element store routes to C `p[i]`.  Read-side index on
                 a bare `*T` stays rejected — reads go through `Slice`,
                 writes through this path. *)
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
        if not (typ_eq tvalue.ty elem_ty) && not (int_lit_fits value elem_ty) then
          Error.failf pos
            "array element: expected %s, got %s"
            (typ_name elem_ty) (typ_name tvalue.ty);
        (env, TAssignIndex { base = tbase; index = tindex;
                             value = tvalue; pos })
    | Ast.AssignDeref { target; value; pos } ->
        let ttarget = elab_expr ctx env target in
        let inner =
          match ttarget.ty with
          | TPtr t -> t
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
        if not (typ_eq tvalue.ty inner) && not (int_lit_fits value inner) then
          Error.failf pos
            "deref assignment: expected %s, got %s"
            (typ_name inner) (typ_name tvalue.ty);
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
               | Some p -> List.mem ("Iterator", p) !trait_impl_table
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
           Escape hatch: bind with `let _x = ...`. *)
        let has_effect =
          match tvalue.e with
          | TCall _ | TIndirectCall _ | TMatch _ -> true
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
    | TStructLit _ | TTupleLit _ | TNew _ | TEnumLit _ | TMatch _
    | TIfExpr _ | TArrayLit _ | TArrayRepeat _ -> true
    | _ -> false
  in
  let rec walk_expr ~allow_top (te : texpr) : texpr * tstmt list =
    let walked, prelude = walk_subs te in
    if is_block walked && not allow_top then
      let n = fresh_lift () in
      decls := (n, walked.ty) :: !decls;
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
    | TIntLit _ | TBoolLit _ | TNullLit | TStringLit _ | TVar _
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
        ({ te with e = TRef sub' }, p)
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
    | TNew { sname_path; fields; base } ->
        let (fields', pf) = map_fields fields in
        let (base', pb) = map_opt base in
        ({ te with e = TNew { sname_path; fields = fields';
                              base = base' } }, pf @ pb)
    | TEnumLit { ename_path; variant; tag; args } ->
        let (args', p) = map_fields args in
        ({ te with e = TEnumLit { ename_path; variant; tag;
                                  args = args' } }, p)
    | TMatch { scrutinee; ename_path; arms } ->
        let (scr', p) = walk_expr ~allow_top:false scrutinee in
        ({ te with e = TMatch { scrutinee = scr'; ename_path; arms } }, p)
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
    | TBlock _ ->
        (* Block lives only as a match arm body; arm-body texprs are
           handled directly by emit_arm_result and are NOT walked by
           lift_block_exprs (the lift pass operates on stmts/exprs in
           the surrounding context, never on arm payloads). *)
        (te, [])
  in
  let rec lift_stmts stmts = List.concat_map lift_stmt stmts
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
        (* If `cond` was block-shaped, the lift evaluates it once before
           the loop — subsequent iterations re-use the temp.  In
           practice cond is bool-typed, so block-shaped conds are
           rare; document this limitation if it bites. *)
        p @ [ TWhile { cond = c'; body = lift_stmts body;
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
  let c_includes = ref [] in
  (* Uniform "must be at top level" reject — `extern struct/type/const`
     and `@c_include` all share the same constraint with the same
     wording.  Path captured by walk and threaded in. *)
  let in_raw_module path = match List.rev path with
    | "raw" :: _ -> true
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
               (loader pass missing?)")
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
    aliases = List.rev !aliases }

(* Build the global function index: every function with its module path,
   exile-side name, and signature.  main() is excluded — it is not callable. *)
let build_global_index ~instances ~ext_structs ~ext_types ~ext_consts ~consts ~ext_struct_fields ~struct_index ~enum_index ~modules ~aliases flat =
  List.filter_map
    (fun (p, (f : Ast.func), mangled) ->
      if f.name = "main" then None
      else
        let ctx = { (empty_ctx ~instances) with
          structs = struct_index; enums = enum_index;
          modules; scope = p; tparams = f.tparams;
          aliases; ext_struct_fields;
          ext_structs; ext_types; ext_consts; consts;
        } in
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
let build_struct_index ~instances ~ext_structs ~ext_types ~ext_consts ~modules ~enums struct_flat =
  let skeleton =
    List.map
      (fun (p, (s : Ast.struct_decl)) ->
        { sname_path = p @ [s.sname];
          sfields_ty = [];
          sis_pub = s.sis_pub;
          stparams = s.stparams;
          sinstance_args = None;
          sis_debug = s.sis_debug })
      struct_flat
  in
  List.map2
    (fun (p, (s : Ast.struct_decl)) skel ->
      let ctx = { (empty_ctx ~instances) with
        structs = skeleton; enums;
        modules; scope = p; tparams = s.stparams;
        ext_structs; ext_types; ext_consts;
      } in
      { skel with
        sfields_ty =
          List.map (fun (n, t) ->
            (n, resolve_type_ann ~pos:s.spos ctx t)) s.sfields })
    struct_flat skeleton

(* Build the enum registry.  Like `build_struct_index` we go two-pass:
   first collect every enum's absolute path with empty variants (so
   payload types in any enum can refer to any other enum), then
   resolve each variant's `vfields` against the struct + enum
   skeleton.  Tuple variants synthesise `_0`/`_1`/... names so all
   three forms look the same to codegen; struct variants keep their
   user-given names.  `vsis_struct` lets the constructor type-check
   reject `Foo::V(args)` for a struct variant and vice versa. *)
let build_enum_index ~instances ~ext_structs ~ext_types ~ext_consts ~modules ~struct_index enum_flat =
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
        structs = struct_index; enums = skeleton;
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
let collect_tuple_types_of tfuncs =
  let tup_seen = ref [] in
  let fnptr_seen = ref [] in
  let arr_seen = ref [] in
  let add_tuple t =
    let name = mangle_typ t in
    if not (List.exists (fun (n, _) -> n = name) !tup_seen) then
      tup_seen := (name, t) :: !tup_seen
  in
  let add_fnptr t =
    let name = mangle_typ t in
    if not (List.exists (fun (n, _) -> n = name) !fnptr_seen) then
      fnptr_seen := (name, t) :: !fnptr_seen
  in
  let add_array t =
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
    | TPtr inner -> walk_typ inner
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
  (List.rev !tup_seen, List.rev !fnptr_seen, List.rev !arr_seen)

(* Detect heap usage by scanning the typed bodies for `TNew` expressions or
   builtin `free(p)` calls — both are emitted in C only when one of them is
   present, so codegen can conditionally include `<stdlib.h>`. *)
let uses_heap_of tfuncs =
  let expr_is_heap (te : texpr) = match te.e with
    | TNew _ -> true
    | TBuiltinCall { name = "free"; _ } -> true
    | _ -> false
  in
  let stmt_is_heap s =
    List.exists (exists_texpr expr_is_heap) (tstmt_own_exprs s)
  in
  List.exists (fun tf ->
    List.exists (exists_tstmt stmt_is_heap) tf.tf_body)
    tfuncs

(* `<string.h>` is pulled in only when something in the program needs
   it.  Today the only trigger is `cstr_len(s)` (lowers to `strlen()`).
   Same shape as [uses_heap_of] — checked once over the typed program
   and forwarded to codegen via [tp_uses_string_h]. *)
let uses_string_h_of tfuncs =
  let expr_uses (te : texpr) = match te.e with
    | TBuiltinCall { name = "cstr_len"; _ } -> true
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
  let cmp_ann tm_ann im_ann =
    let t1 = resolve_type_ann ~pos ctx
               (subst_assoc ~assoc:iassoc target_ann tm_ann) in
    let t2 = resolve_type_ann ~pos ctx im_ann in
    typ_eq t1 t2
  in
  let cmp_ret tm_ret im_ret =
    match tm_ret, im_ret with
    | None, None -> true
    | Some a, Some b -> cmp_ann a b
    | _ -> false
  in
  (* Substitute `Self` in a (default) method's signature so the synthesised
     impl method has the concrete target type for its receiver / params. *)
  let specialise_default (tm : Ast.func) : Ast.func =
    { tm with
      Ast.params = List.map (fun (p : Ast.param) ->
        { p with Ast.pty = subst_assoc ~assoc:iassoc target_ann p.pty })
        tm.params;
      Ast.ret_ty = Option.map (subst_assoc ~assoc:iassoc target_ann) tm.ret_ty }
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
          List.iter2 (fun (tp : Ast.param) (ip : Ast.param) ->
            if not (cmp_ann tp.pty ip.pty) then
              Error.failf im.pos
                "method '%s': parameter '%s' type does not match trait '%s'"
                tm.name ip.pname (String.concat "::" trait_path))
            tm.params im.params;
          if not (cmp_ret tm.ret_ty im.ret_ty) then
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
  (* Pre-pass: register every `impl Trait for Foo` as (trait-name, target)
     BEFORE any conformance check runs, so generic `<T: Trait>` bounds and
     supertrait requirements resolve order-independently (a supertrait's
     impl may be written after the impl that depends on it). *)
  List.iter
    (fun (parent_path, ib) ->
      match ib.Ast.itrait with
      | None -> ()
      | Some trait_written ->
          let ctx = { (empty_ctx ~instances) with
            structs = struct_index; enums = enum_index;
            modules; scope = parent_path; tparams = ib.Ast.itparams;
            ext_structs; ext_types; ext_consts } in
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
        let ctx = { (empty_ctx ~instances) with
          structs = struct_index; enums = enum_index;
          modules; scope = parent_path;
          (* The impl's type parameters are in scope while resolving the
             methods' self/param/ret types (`self: Pair<A, B>`). *)
          tparams = ib.Ast.itparams;
          ext_structs; ext_types; ext_consts;
        } in
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
                  | TStructApp { path; _ } when path = target_path -> ()
                  | TPtr (TStructApp { path; _ }) when path = target_path -> ()
                  | TEnum p when p = target_path -> ()
                  | TPtr (TEnum p) when p = target_path -> ()
                  | TEnumApp { path; _ } when path = target_path -> ()
                  | TPtr (TEnumApp { path; _ }) when path = target_path -> ()
                  | _ ->
                      Error.failf m.pos
                        "first parameter 'self' must have type '%s' or '*%s', \
                         got %s"
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
        (target_path, target_pub, ib.Ast.itparams,
         ib.Ast.iitems @ synthesised))
      flat.impls
  in
  (* Cross-block dup check: same method name on the same struct in two
     different impl blocks. *)
  let seen_methods = Hashtbl.create 16 in
  List.iter
    (fun (target_path, _, _, methods) ->
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
      (fun (target_path, sis_pub, _, _) ->
        if List.mem target_path !seen then None
        else (seen := target_path :: !seen;
              Some (target_path, sis_pub)))
      resolved
  in
  let impl_funcs =
    List.concat_map
      (fun (target_path, _, itparams, methods) ->
        List.map
          (fun (m : Ast.func) ->
            (* A generic impl's tparams become the method's own tparams,
               so the method is treated as a generic fn (inferred /
               instantiated per concrete receiver at the call site). *)
            let m = { m with Ast.tparams = itparams @ m.tparams } in
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
let prelude_mono_struct_names = ["Allocator"; "StringBuilder"]

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
  let cuint = Ast.TyCInt { signed = false } in
  (* `Range<T>` / `RangeInclusive<T>` — generic prelude structs carrying a
     pair of bounds.  `a..b` and `a..=b` desugar to a struct literal of the
     respective type; `for v in r` extracts `.lo` and `.hi` when `r` is a
     value of such a type (literal `..`/`..=` in for-head still take the
     fast path). *)
  let range_struct = {
    Ast.sname = "Range"; stparams = ["T"];
    sfields = [("lo", tvar "T"); ("hi", tvar "T")];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "core"; sis_debug = false; sderives = [];
  } in
  let range_inclusive_struct = {
    Ast.sname = "RangeInclusive"; stparams = ["T"];
    sfields = [("lo", tvar "T"); ("hi", tvar "T")];
    spos = prelude_pos; sis_pub = true;
    stier_hint = Some "core"; sis_debug = false; sderives = [];
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
    stier_hint = Some "core"; sis_debug = false; sderives = [];
  } in
  let alloc_struct = {
    Ast.sname = "Allocator";
    stparams = [];
    sfields = [
      ("state", cvoid_ptr);
      ("alloc_fn",
       Ast.TyFnPtr { params = [cvoid_ptr; cuint]; ret = Some cvoid_ptr });
      ("free_fn",
       Ast.TyFnPtr { params = [cvoid_ptr; cvoid_ptr]; ret = None });
    ];
    spos = prelude_pos;
    sis_pub = true;
    stier_hint = Some "core";
    sis_debug = false; sderives = [];
  } in
  (* Method bodies use the fn-ptr-field call syntax (`self.alloc_fn(...)`)
     directly — typecheck routes it through TIndirectCall when the
     receiver's matching field is a TFnPtr. *)
  let pos = prelude_pos in
  let var n = Ast.Var (n, pos) in
  let alloc_body = [
    (* return (self.alloc_fn(self.state, size_of(T))) as *T; *)
    Ast.Return (
      Some (Ast.Cast (
        Ast.MethodCall {
          receiver = var "self"; name = "alloc_fn";
          args = [ Ast.FieldAccess (var "self", "state", pos);
                   Ast.SizeOf (tvar "T", pos) ];
          pos;
        },
        Ast.TyPtr (tvar "T"), pos)),
      pos);
  ] in
  let free_body = [
    (* self.free_fn(self.state, p as *c_void); *)
    Ast.ExprStmt (Ast.MethodCall {
      receiver = var "self"; name = "free_fn";
      args = [ Ast.FieldAccess (var "self", "state", pos);
               Ast.Cast (var "p", cvoid_ptr, pos) ];
      pos;
    });
  ] in
  let mk_method name tparams params ret body = {
    Ast.name; c_name = name; tparams; tbounds = []; params; ret_ty = ret; body;
    is_pub = true; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false; pos;
  } in
  let self_param =
    { Ast.pname = "self";
      pty = Ast.TyStruct { path = ["Allocator"]; args = [] };
      preg = None; is_mut = false }
  in
  let alloc_method =
    mk_method "alloc" ["T"] [self_param]
      (Some (Ast.TyPtr (tvar "T"))) alloc_body
  in
  let free_method =
    mk_method "free" ["T"]
      [ self_param;
        { Ast.pname = "p"; pty = Ast.TyPtr (tvar "T");
          preg = None; is_mut = false } ]
      None free_body
  in
  let alloc_impl = {
    Ast.itparams = [];
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
  let u8_ptr = Ast.TyPtr u8_t in
  let u8_cptr = Ast.TyConstPtr u8_t in
  let sb_struct = {
    Ast.sname = "StringBuilder";
    stparams = [];
    sfields = [
      ("buf", u8_ptr);
      ("len", u32_t);
      ("cap", u32_t);
      ("alloc", Ast.TyStruct { path = ["Allocator"]; args = [] });
    ];
    spos = prelude_pos;
    sis_pub = true;
    stier_hint = Some "full";
    sis_debug = false; sderives = [];
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
  let mk_sb_method ?(is_pub = true) name params ret body = {
    Ast.name; c_name = name; tparams = []; tbounds = []; params; ret_ty = ret;
    body; is_pub; is_extern = false; is_variadic = false;
    tier_hint = Some "full"; amiga_lib = None; must_use = false; pos;
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
    Ast.Let { name = "buf"; is_mut = false; ty_ann = Some u8_ptr;
              value = Ast.Cast (
                methcall (Ast.Var ("a", pos)) "alloc_fn"
                  [ field (Ast.Var ("a", pos)) "state";
                    Ast.Cast (Ast.Var ("cap", pos), cuint, pos) ],
                u8_ptr, pos);
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
    mk_sb_method "length" [ sb_self_ptr_param ] (Some u32_t)
      [ Ast.Return (Some (field (Ast.Var ("self", pos)) "len"), pos) ]
  in
  (* as_slice( * self) -> Slice<u8>: read-only view backed by buf/len.
     Drops mutability via the `*u8 → *const u8` coercion. *)
  let slice_u8_ann =
    Ast.TyStruct { path = ["Slice"]; args = [ u8_t ] } in
  let as_slice_method =
    mk_sb_method "as_slice" [ sb_self_ptr_param ] (Some slice_u8_ann)
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
      Ast.Let { name = "new_buf"; is_mut = false; ty_ann = Some u8_ptr;
                value = Ast.Cast (
                  methcall self_alloc "alloc_fn"
                    [ field self_alloc "state";
                      Ast.Cast (Ast.Var ("new_cap", pos), cuint, pos) ],
                  u8_ptr, pos);
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
      (* free old buffer: alloc.free_fn(alloc.state, buf as *c_void) *)
      Ast.ExprStmt (methcall self_alloc "free_fn"
        [ field self_alloc "state";
          Ast.Cast (field self_v "buf", cvoid_ptr, pos) ]);
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
  let sb_impl = {
    Ast.itparams = []; itrait = None; iassoc = [];
    itarget = ["StringBuilder"];
    iitems = [
      with_capacity_method;
      grow_method;
      push_byte_method;
      length_method;
      as_slice_method;
    ];
    ipos = pos;
  } in
  let _ = int_lit in  (* kept for the upcoming push_int *)
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
      tier_hint = None; amiga_lib = None; must_use = false; pos }
  in
  let iterator_trait = {
    Ast.trname = "Iterator"; trassoc = ["Item"]; trsupers = [];
    trmethods = [ iter_next ]; trdefaults = [];
    trpos = pos; tris_pub = true;
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
    must_use = false; pos }
  in
  let eq_sig = trait_sig "eq" [self_v; other_v] (Some Ast.TyBool) [] in
  (* default `ne(self, other) = !self.eq(other)` *)
  let ne_default =
    trait_sig "ne" [self_v; other_v] (Some Ast.TyBool)
      [ Ast.Tail (Ast.Not (Ast.MethodCall {
          receiver = Ast.Var ("self", pos); name = "eq";
          args = [ Ast.Var ("other", pos) ]; pos }, pos)) ]
  in
  let eq_trait = {
    Ast.trname = "Eq"; trassoc = []; trsupers = [];
    trmethods = [ eq_sig; ne_default ]; trdefaults = [ "ne" ];
    trpos = pos; tris_pub = true;
  } in
  let clone_sig = trait_sig "clone" [self_v] (Some Ast.TySelf) [] in
  let clone_trait = {
    Ast.trname = "Clone"; trassoc = []; trsupers = [];
    trmethods = [ clone_sig ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  (* `Hash: Eq` — `fn hash(self) -> u32`.  Supertrait Eq signals the
     hash/eq contract (`a.eq(b)` ⟹ `a.hash() == b.hash()`), so a type
     deriving Hash must also derive/impl Eq. *)
  let u32_ann = Ast.TyInt { signed = false; width = Ast.W32 } in
  let hash_sig = trait_sig "hash" [self_v] (Some u32_ann) [] in
  let hash_trait = {
    Ast.trname = "Hash"; trassoc = []; trsupers = [ ["Eq"] ];
    trmethods = [ hash_sig ]; trdefaults = [];
    trpos = pos; tris_pub = true;
  } in
  [ Ast.Enum option_decl; Ast.Enum result_decl;
    Ast.Struct range_struct; Ast.Struct range_inclusive_struct;
    Ast.Struct slice_struct;
    Ast.Struct alloc_struct; Ast.Impl alloc_impl;
    Ast.Struct sb_struct; Ast.Impl sb_impl;
    Ast.Trait iterator_trait; Ast.Trait eq_trait; Ast.Trait clone_trait;
    Ast.Trait hash_trait ]

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
    must_use = false; pos }

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
    [ derive_field_param "self" target; derive_field_param "other" target ]
    (Some Ast.TyBool) body pos in
  Ast.Impl { itparams = []; itrait = Some ["Eq"]; iassoc = [];
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
        Ast.Match { scrutinee = Ast.Var ("other", pos); arms = inner_arms; pos } in
      Ast.{ pat = self_pat; guard = None; body = inner_match; arm_pos = pos })
      e.evariants
  in
  let outer_match =
    Ast.Match { scrutinee = Ast.Var ("self", pos); arms = outer_arms; pos } in
  let m = derive_mk_method "eq"
    [ derive_field_param "self" target; derive_field_param "other" target ]
    (Some Ast.TyBool) [ Ast.Tail outer_match ] pos in
  Ast.Impl { itparams = []; itrait = Some ["Eq"]; iassoc = [];
             itarget = [e.ename]; iitems = [m]; ipos = pos }

(* Clone is a trivial value copy — `fn clone(self) -> Self { self }` —
   for both structs and enums (value semantics; deep clone arrives with
   heap types). *)
let derive_clone ~name ~pos : Ast.item =
  let target = Ast.TyStruct { path = [name]; args = [] } in
  let m = derive_mk_method "clone" [ derive_field_param "self" target ]
    (Some target) [ Ast.Tail (Ast.Var ("self", pos)) ] pos in
  Ast.Impl { itparams = []; itrait = Some ["Clone"]; iassoc = [];
             itarget = [name]; iitems = [m]; ipos = pos }

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
  let m = derive_mk_method "hash" [ derive_field_param "self" target ]
    (Some derive_u32_ann) [ Ast.Tail body ] pos in
  Ast.Impl { itparams = []; itrait = Some ["Hash"]; iassoc = [];
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
  let match_e = Ast.Match { scrutinee = Ast.Var ("self", pos); arms; pos } in
  let m = derive_mk_method "hash" [ derive_field_param "self" target ]
    (Some derive_u32_ann) [ Ast.Tail match_e ] pos in
  Ast.Impl { itparams = []; itrait = Some ["Hash"]; iassoc = [];
             itarget = [e.ename]; iitems = [m]; ipos = pos }

let expand_derives (program : Ast.program) : Ast.program =
  let one ~kind ~name ~pos ~generic ~gen_eq ~gen_hash tr =
    let needs_mono trait =
      if generic then
        Error.failf pos
          "@derive(%s) on a generic %s '%s' is not supported yet"
          trait kind name
    in
    match tr with
    | "Eq" -> needs_mono "Eq"; gen_eq ()
    | "Clone" -> derive_clone ~name ~pos
    | "Hash" -> needs_mono "Hash"; gen_hash ()
    | "Debug" ->
        Error.failf pos
          "@derive(Debug) needs a StringBuilder (deferred); use `@debug` \
           for a direct-print debug helper for now"
    | other ->
        Error.failf pos
          "cannot derive '%s' (supported: Eq, Hash, Clone)" other
  in
  let derived =
    List.concat_map (fun item ->
      match item with
      | Ast.Struct s when s.sderives <> [] ->
          List.map (one ~kind:"struct" ~name:s.sname ~pos:s.spos
                      ~generic:(s.stparams <> [])
                      ~gen_eq:(fun () -> derive_eq_struct s)
                      ~gen_hash:(fun () -> derive_hash_struct s)) s.sderives
      | Ast.Enum e when e.ederives <> [] ->
          List.map (one ~kind:"enum" ~name:e.ename ~pos:e.epos
                      ~generic:(e.etparams <> [])
                      ~gen_eq:(fun () -> derive_eq_enum e)
                      ~gen_hash:(fun () -> derive_hash_enum e)) e.ederives
      | _ -> [])
      program
  in
  program @ derived

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
            (match ib.itarget with
             | [n] -> not (List.mem n user_top_struct_names)
             | _ -> true)
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
  | TLet _ | TLetTuple _ | TAssign _ | TAssignField _ | TAssignIndex _
  | TAssignDeref _ | TWhile _ | TFor _ | TForEach _ | TDefer _ | TExprStmt _
  | TBreak _ | TContinue _ -> false

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
  trait_impl_table := [];
  trait_assoc_table := [];
  let mono_state = Mono.new_state () in
  let program = prepend_prelude program in
  (* Expand `@derive(...)` into real `impl Trait for Foo` blocks (after the
     prelude so the Eq / Clone traits they target are in scope). *)
  let program = expand_derives program in
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
      ~modules:flat.modules ~enums:enum_skeleton flat.structs
  in
  let enum_index =
    build_enum_index ~instances:mono_state ~ext_structs ~ext_types ~ext_consts
      ~modules:flat.modules ~struct_index flat.enums
  in
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
      ~struct_index ~enum_index ~modules ~aliases:flat.aliases all_funcs
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
      aliases = flat.aliases;
      ext_vars; ext_struct_fields;
      ext_structs; ext_types; ext_consts;
    } in
    let resolves =
      lookup_fn probe_ctx target_path <> None
      || lookup_struct probe_ctx target_path <> None
      || lookup_enum probe_ctx target_path <> None
    in
    if not resolves then
      Error.failf decl_pos
        "'pub use %s' refers to unknown item — no fn, struct, or enum \
         with that path is visible from this scope"
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
    let ctx0 = { (empty_ctx ~instances:mono_state) with
      global; structs = struct_index; enums = enum_index;
      modules; scope = path; tparams = f.tparams;
      tvar_bindings; fn_asts; aliases = flat.aliases;
      ext_vars; ext_struct_fields;
      ext_structs; ext_types; ext_consts; consts = consts_index;
    } in
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
    | TPtr inner -> typ_mentions target inner
    | TArray { elem; _ } -> typ_mentions target elem
    | TTuple ts -> List.exists (typ_mentions target) ts
    | TFnPtr { params; ret } ->
        List.exists (typ_mentions target) params
        || (match ret with Some t -> typ_mentions target t | None -> false)
    | _ -> false
  in
  let tfunc_mentions target tf =
    List.exists (typ_mentions target) tf.tf_param_tys
    || (match tf.tf_ret_ty with Some t -> typ_mentions target t | None -> false)
    || List.exists (fun (_, t) -> typ_mentions target t) tf.tf_lets
  in
  let is_from_prelude tf = tf.tf_func.Ast.pos.file = "<prelude>" in
  let used_in_user_code path =
    (* Prelude-origin methods (e.g. `StringBuilder::with_capacity`) all
       mention their own struct in self/ret — they'd vote themselves
       live.  A mono prelude type is "used" only when concrete USER
       code (or a USER-driven monomorphic instance) references it. *)
    List.exists
      (fun tf ->
        tf.tf_func.Ast.tparams = []
        && not (is_from_prelude tf)
        && tfunc_mentions path tf)
      tp_funcs
  in
  let struct_drop_set =
    List.filter
      (fun n -> not (used_in_user_code [n]))
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
  (* After dropping prelude-mono methods, re-collect type tables and
     the include flags — the dropped bodies often referenced fn-ptr /
     array / tuple shapes (e.g. `Allocator`'s alloc_fn) and pulled in
     `<string.h>` (`cstr_len` for `push_str`) that nothing else uses.
     Leaving those in would emit dead typedefs and includes in every
     program that never touches the prelude type. *)
  let (tp_tuple_types, tp_fnptr_types, tp_array_types) =
    collect_tuple_types_of tp_funcs in
  let tp_uses_heap = uses_heap_of tp_funcs in
  let tp_uses_string_h = uses_string_h_of tp_funcs in
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
      | TPtr t | TConstPtr t -> walk t
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
    (* Transitive: keep walking through struct fields / enum variants
       of mono instances until the set stabilizes. *)
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
        mono_structs;
      List.iter (fun (e : enum_sig) ->
        if List.mem e.ename_path !paths then
          List.iter (fun (v : variant_sig) ->
            List.iter (fun (_, t) ->
              let before = List.length !paths in
              walk t;
              if List.length !paths <> before then changed := true)
              v.vsfields)
            e.evariants)
        mono_enums
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
    | TCChar | TCSChar | TCUChar | TBool | TString | TPtr _
    | TConstPtr _ -> true
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
    tp_tuple_types;
    tp_fnptr_types;
    tp_c_includes = flat.c_includes;
    tp_ext_consts = ext_consts;
    tp_ext_vars = ext_vars;
    tp_consts;
    tp_array_types }
