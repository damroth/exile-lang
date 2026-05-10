(* Comfort/budget tier of an individual language item (fn, struct, enum,
   impl).  Mirrors `Profile.t` so the mapping between an item's tier
   and the program's compilation profile is direct: under
   `--profile=core`, anything above `Core` is reported by the linter;
   under `--profile=full`, nothing fires.

   Every item has a default tier inferred at type-check time (mono fns
   and structs land at [Core]; generic fns at [Full] because each
   instantiation copies the body into the binary).  Users can override
   with the `@tier(core|standard|full)` attribute on a decl when they
   know the cost is acceptable.

   `Lint` consumes the typed program plus the active profile and emits
   a warning per item whose effective tier exceeds the profile. *)

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

(* Ordinal: Core < Standard < Full.  An item with tier > profile is
   "out of budget" for that profile — the linter warns. *)
let to_int = function
  | Core -> 0
  | Standard -> 1
  | Full -> 2

let exceeds ~profile ~item_tier =
  to_int item_tier > to_int (
    match profile with
    | Profile.Core -> Core
    | Profile.Standard -> Standard
    | Profile.Full -> Full)
