(* Lint interface.  Walks a typechecked program and emits warnings for
   tier mismatches (generic-fn instantiation under a tight profile),
   unused let-bindings, unused parameters, unused private functions,
   and discarded `@must_use` values.  Warnings never block compilation
   — the compiler exits 0 even when warnings are present. *)

type warning = { pos : Pos.t; msg : string }

(* Pure analysis — returns the warning list without printing.  Used by
   the test harness to assert on specific warnings. *)
val collect : profile:Profile.t -> Ir.tprogram -> warning list

(* Run [collect] and print every warning to stderr in
   "<file>:<line>:<col>: warning: <msg>" form.  CLI entry point. *)
val lint : profile:Profile.t -> Ir.tprogram -> unit
