(* Tier-aware diagnostics.  Runs after typecheck, before codegen, with
   the active compilation profile in hand.  Walks the typed program
   looking for items whose effective tier exceeds the profile, and
   emits warnings on stderr.  Warnings never block compilation —
   exit code stays 0; the user decides whether to act on them.

   Today the linter focuses on the highest-impact tier source:
   monomorphic instances of generic fns (each one adds a fresh
   function body to the emitted C, the dominant bloat vector under
   tight memory targets like Amiga 256K).  Generic struct/enum
   instances are cheap (one struct decl, no executable code) and
   skipped.  Future passes may add deep-nesting and threshold-based
   summaries. *)

open Ir

(* Effective tier of a fn: explicit `@tier(...)` hint wins; otherwise
   default to Full for generics and Core for mono fns. *)
let fn_effective_tier (f : Ast.func) : Tier.t =
  match f.tier_hint with
  | Some name ->
      (* Parser already validated the name; failing here would be a
         compiler bug, not user error. *)
      (match Tier.of_string name with
       | Some t -> t
       | None -> failwith ("internal: invalid tier_hint '" ^ name ^ "'"))
  | None ->
      if f.tparams <> [] then Tier.Full
      else Tier.Core

(* True when every typ in [tf]'s resolved signature is concrete — i.e.
   it's a monomorphic instance (or an originally-mono fn), not a
   generic skeleton waiting on instantiation. *)
let is_concrete_instance (tf : tfunc) : bool =
  List.for_all is_concrete tf.tf_param_tys
  && (match tf.tf_ret_ty with
      | Some t -> is_concrete t
      | None -> true)

type warning = { pos : Pos.t; msg : string }

(* Pure analysis: returns the warnings the linter would emit, without
   touching stderr.  Tests compare the list directly; CLI prints via
   [emit_warnings]. *)
let collect ~(profile : Profile.t) (tp : tprogram) : warning list =
  (* Dedupe by call-site (origin_pos when available, fallback to decl
     pos) so each unique site warns once.  Origin pos points at the
     actual `.alloc()` / `id(42)` rather than the prelude/decl, which
     is far more useful for finding what to refactor. *)
  let seen = Hashtbl.create 16 in
  let acc = ref [] in
  List.iter (fun tf ->
    let f = tf.tf_func in
    if f.tparams <> [] && is_concrete_instance tf then begin
      let item_tier = fn_effective_tier f in
      let warn_pos = match tf.tf_origin_pos with
        | Some p -> p
        | None -> f.pos
      in
      if Tier.exceeds ~profile ~item_tier
         && not (Hashtbl.mem seen warn_pos) then begin
        Hashtbl.add seen warn_pos ();
        let msg =
          Printf.sprintf
            "generic fn '%s' is tier=%s but compiled under \
             --profile=%s; each instantiation adds a body copy \
             (use --profile=%s to silence, or refactor to a mono \
             alternative)"
            f.name
            (Tier.to_string item_tier)
            (Profile.to_string profile)
            (Tier.to_string item_tier)
        in
        acc := { pos = warn_pos; msg } :: !acc
      end
    end)
    tp.tp_funcs;
  List.rev !acc

let emit_warnings (ws : warning list) : unit =
  List.iter (fun w ->
    Printf.eprintf "%s:%d:%d: warning: %s\n"
      w.pos.file w.pos.line w.pos.col w.msg)
    ws

let lint ~(profile : Profile.t) (tp : tprogram) : unit =
  emit_warnings (collect ~profile tp)
