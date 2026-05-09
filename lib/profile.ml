(* Comfort/budget tier the program is being compiled under.  Orthogonal
   to --target (which picks the toolchain).  Drives lint thresholds,
   default optimisation choices, and warnings on heavy features.

   - Core:     bare-metal, kernels, drivers, perf-critical paths,
               256K-class Amiga.  Generic instantiations and ADT-heavy
               constructs warned about; manual control expected.
   - Standard: typical AmigaOS userland app on a few-MB machine.
               Comfort features OK in moderation; warnings only on
               clearly extravagant patterns.
   - Full:     host development, Amiga with accelerator + plenty of
               RAM.  No bloat warnings; comfort first.

   Defaults derived from the chosen target — see [default_for_target].
   The user may override via --profile. *)

type t = Core | Standard | Full

let to_string = function
  | Core -> "core"
  | Standard -> "standard"
  | Full -> "full"

let of_string = function
  | "core" -> Some Core
  | "standard" -> Some Standard
  | "full" -> Some Full
  | _ -> None
