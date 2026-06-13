(* Codegen — turns a typed program ([Ir.tprogram]) into a C89 source
   string ready to write to disk.  Emits forward decls, struct / enum
   defs, function bodies, and a single `int main(void)` shim when the
   program defines `main`.

   [last_bloat] returns the per-function byte-count list collected
   during the most recent [gen_program] call (sorted by emission
   order); the `--bloat-report` CLI flag drives a sort + top-20 of
   these for inspection.

   Module-level state (annotate mode, enum index, bloat accumulator,
   active defer chain) is mutated through [gen_program] and read out
   via [last_bloat] — see REFACTOR.md item #2 for the planned move
   to an explicit gen_ctx. *)

val gen_program :
  ?annotate:bool -> ?freestanding:bool -> Ir.tprogram -> string

val last_bloat : unit -> (string * int) list
