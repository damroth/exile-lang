(* Compilation profile: the comfort/budget tier the *program* is being
   compiled under.  Orthogonal to --target (which picks the toolchain).
   Drives lint thresholds and warnings on heavy features.

   - Core:     bare-metal, kernels, drivers, perf-critical paths,
               256K-class Amiga.  Generic instantiations and ADT-heavy
               constructs warned about; manual control expected.
   - Standard: typical AmigaOS userland app on a few-MB machine.
               Comfort features OK in moderation; warnings only on
               clearly extravagant patterns.
   - Full:     host development, Amiga with accelerator + plenty of
               RAM.  No bloat warnings; comfort first.

   Structurally identical to [Tier.t] (a per-item version of the same
   scale) — the manifest type below re-exports the constructors so
   `Profile.Core` and `Tier.Core` are the same value, and
   [Tier.exceeds] can compare them without a conversion. *)

type t = Tier.t = Core | Standard | Full

let to_string = Tier.to_string
let of_string = Tier.of_string
