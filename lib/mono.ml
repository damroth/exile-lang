(* Monomorphization state + helpers.  Owns the mutable accumulator
   of instances generated while resolving a program, plus the pure
   helpers (path mangling, instance/skeleton matching, type-parameter
   unification) that drive instance construction.

   Typecheck calls into here whenever it encounters a generic application
   (`Foo<T1, T2>` in a type annotation, or a constructor that lets us
   infer args from payload).  Instances are deduped by path so repeated
   uses of the same `Foo<int>` share a single emitted struct in C.

   Lifting this out of typecheck keeps the typing rules readable and
   makes a future Ir → Ir mono pass easier to grow incrementally —
   the state and helpers are already independent. *)

open Ir

(* Generic-fn instance work item drained after the main elab loop:
   the AST body is re-elaborated under a context where every TVar
   resolves to its concrete binding, producing a fully monomorphic
   tfunc that codegen treats like any other. *)
type pending_fn_job = {
  pj_path : string list;             (* parent module path of the fn *)
  pj_func : Ast.func;                 (* original (still-generic) AST *)
  pj_bindings : (string * typ) list;  (* tparam name → concrete typ *)
  pj_mangled : string;                (* C-side instance name *)
  pj_param_tys : typ list;            (* substituted *)
  pj_ret_ty : typ option;             (* substituted *)
  pj_origin_pos : Pos.t;              (* call site that first triggered
                                         instantiation; subsequent calls of
                                         the same shape are deduped, so this
                                         is "first observed".  Used by the
                                         linter for tier warnings — better
                                         than the skeleton decl pos which
                                         can be `<prelude>:1:1` *)
}

type state = {
  mutable inst_structs : struct_sig list;
  mutable inst_enums : enum_sig list;
  mutable inst_funcs : fn_sig list;
  mutable pending_fn_jobs : pending_fn_job list;
}

let new_state () =
  { inst_structs = []; inst_enums = []; inst_funcs = []; pending_fn_jobs = [] }

(* Path of a monomorphic instance: take the decl's absolute path and
   tack a `_<arg-mangle>...` suffix onto the last segment.
     `Option<int>`           → `["Option_i32"]`
     `foo::Pair<int, str>`   → `["foo"; "Pair_i32_str"]`
   Codegen's `mangle_typ` then prefixes `ex_` / `mod__` as usual,
   yielding C names like `ex_Option_i32` / `foo__Pair_i32_str`. *)
let instance_path decl_path args =
  match List.rev decl_path with
  | [] -> failwith "Mono.instance_path: empty decl path"
  | last :: rev_init ->
      let arg_part = String.concat "_" (List.map mangle_typ args) in
      List.rev rev_init @ [ last ^ "_" ^ arg_part ]

(* True when [instance] is a monomorphic instance of [skeleton] produced
   by `instance_path skeleton args` — same prefix and the last segment
   is `<skeleton_last>_<arg-mangle>...`.  `skeleton = instance` (mono
   case) also matches. *)
let is_instance_of skeleton instance =
  skeleton = instance
  || (match List.rev skeleton, List.rev instance with
      | s_last :: s_init, i_last :: i_init ->
          s_init = i_init
          && String.length i_last > String.length s_last
          && String.sub i_last 0 (String.length s_last) = s_last
          && i_last.[String.length s_last] = '_'
      | _ -> false)

(* Infer values for [tparams] from [pairs] — a list of (declared
   type, actual type).  Each occurrence of `TVar T` inside a declared
   type binds T to the corresponding actual type; multiple bindings
   for the same T must agree.  Returns bindings in tparam order;
   raises if any tparam stays unbound or constraints conflict.
   `~seed` (typically extracted from an expected outer type) provides
   initial bindings — payload-driven constraints cross-check against
   them and tparams unreachable through payload (e.g. `E` of
   `Result<T, E>::Ok`) get their value from the seed. *)
let infer_tparams ~pos ?(seed = []) tparams pairs =
  let bindings = ref seed in
  let rec unify (decl_t : typ) (act_t : typ) =
    match decl_t, act_t with
    | TVar n, _ ->
        (match List.assoc_opt n !bindings with
         | Some prev when not (typ_eq prev act_t) ->
             Error.failf pos
               "type parameter '%s' inferred as both '%s' and '%s'"
               n (typ_name prev) (typ_name act_t)
         | Some _ -> ()
         | None -> bindings := (n, act_t) :: !bindings)
    | TPtr a, TPtr b -> unify a b
    | TTuple xs, TTuple ys when List.length xs = List.length ys ->
        List.iter2 unify xs ys
    | _ -> ()
  in
  List.iter (fun (decl_t, act_t) -> unify decl_t act_t) pairs;
  List.map (fun n ->
    match List.assoc_opt n !bindings with
    | Some t -> t
    | None ->
        Error.failf pos
          "could not infer type parameter '%s' from arguments \
           (add a type annotation on the surrounding let / return)" n)
    tparams

let find_struct state path =
  List.find_opt (fun is -> is.sname_path = path) state.inst_structs

let find_enum state path =
  List.find_opt (fun ie -> ie.ename_path = path) state.inst_enums

(* Build a fresh struct instance from skeleton + concrete args.
   Substitutes the tparam bindings into every field type. *)
let make_struct_instance (skel : struct_sig) (args : typ list) : struct_sig =
  let bindings = List.combine skel.stparams args in
  { sname_path = instance_path skel.sname_path args;
    sfields_ty =
      List.map (fun (n, t) -> (n, subst_typ bindings t)) skel.sfields_ty;
    sis_pub = skel.sis_pub;
    stparams = [];
    sinstance_args = Some args;
    sis_debug = skel.sis_debug }

let make_enum_instance (skel : enum_sig) (args : typ list) : enum_sig =
  let bindings = List.combine skel.etparams args in
  { ename_path = instance_path skel.ename_path args;
    evariants =
      List.map (fun vs ->
        { vsname = vs.vsname;
          vsfields =
            List.map (fun (n, t) -> (n, subst_typ bindings t)) vs.vsfields;
          vsis_struct = vs.vsis_struct })
        skel.evariants;
    eis_pub = skel.eis_pub;
    etparams = [];
    einstance_args = Some args;
    eis_must_use = skel.eis_must_use;
    eis_debug = skel.eis_debug }

(* Idempotent instantiation: returns the cached instance if one is
   already registered for the same (decl path, args), otherwise
   builds a fresh one and registers it. *)
let instantiate_struct state skel args =
  let path = instance_path skel.sname_path args in
  match find_struct state path with
  | Some inst -> inst
  | None ->
      let inst = make_struct_instance skel args in
      state.inst_structs <- inst :: state.inst_structs;
      inst

let instantiate_enum state skel args =
  let path = instance_path skel.ename_path args in
  match find_enum state path with
  | Some inst -> inst
  | None ->
      let inst = make_enum_instance skel args in
      state.inst_enums <- inst :: state.inst_enums;
      inst

(* Mangled name of a generic-fn instance: skeleton's mangled name plus
   `_<arg-mangle>...` in tparam declaration order.  The skeleton already
   carries the C-level prefix (`ex_alloc`, `Foo__push`, ...), so the
   arg suffix is all we need to keep instances unique. *)
let fn_instance_mangled skel_mangled args =
  skel_mangled ^ "_" ^ String.concat "_" (List.map mangle_typ args)

let find_fn state mangled =
  List.find_opt (fun s -> s.mangled = mangled) state.inst_funcs

(* Idempotent fn-instance creation.  Returns the cached instance if a
   prior call already built one for the same skeleton + args; otherwise
   substitutes the bindings into the skeleton's signature, registers
   the resulting `fn_sig`, and queues a job so the body gets re-elaborated
   under the substituted types after the main typecheck loop finishes. *)
let instantiate_fn state ~path ~func ~skel ~bindings ~origin_pos =
  let inst_args = List.map snd bindings in
  let inst_mangled = fn_instance_mangled skel.mangled inst_args in
  match find_fn state inst_mangled with
  | Some s -> s
  | None ->
      let inst_param_tys = List.map (subst_typ bindings) skel.param_tys in
      let inst_ret_ty = Option.map (subst_typ bindings) skel.ret_ty in
      let s = {
        param_tys = inst_param_tys;
        ret_ty = inst_ret_ty;
        mangled = inst_mangled;
        fn_pub = skel.fn_pub;
        fn_tparams = [];
        fn_variadic = skel.fn_variadic;
      } in
      state.inst_funcs <- s :: state.inst_funcs;
      state.pending_fn_jobs <- {
        pj_path = path; pj_func = func; pj_bindings = bindings;
        pj_mangled = inst_mangled;
        pj_param_tys = inst_param_tys;
        pj_ret_ty = inst_ret_ty;
        pj_origin_pos = origin_pos;
      } :: state.pending_fn_jobs;
      s

let take_pending_fn_jobs state =
  let jobs = List.rev state.pending_fn_jobs in
  state.pending_fn_jobs <- [];
  jobs
