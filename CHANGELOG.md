# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.4.0] - 2026-09-04

**The declarations reach hardware, and then the programs run.** Two constructs
finish the driver's side of the capability model: an `addr` kind, so the address
a program hands a device is a value the model can watch travel, and a `chip`
modifier on `extern var`, so memory the hardware touches is declared as such and
the loads that read it stop being folded away. Under them the compiler's own
output stops needing libc - `--freestanding` crosses into the self-hosted
compiler and a bare-metal seam answers it - and the release ends with a 68000
and a modelled custom chip in CI. Every previous statement about the chip was a
statement about emitted text; now a copper list is followed by something other
than the processor, and an interrupt nobody calls runs a handler written in
exile and returns through `rte`.

### Added

- An `addr` kind: the type of a value that is an address as far as the hardware
  is concerned. It is a ward field's type, and it is also a type an ordinary
  value carries, so an address can be prepared in one place and written in
  another:

  ```rust
  ward W {
      p:  addr at 0x00 write;
      sz: u16  at 0x08 write;
  }

  struct Job { dst: addr, words: u16 }

  fn plan(n: u16) -> Job {
      return Job { dst: &raw::BUF[0], words: n };
  }

  fn start(j: *const Job) {
      ward w: W at &raw::CHIP;
      seal { w.p.write(j.dst); w.sz.write(j.words); }
  }
  ```

  A pointer converts to an `addr` wherever a value is accepted - an argument, a
  struct field, an annotated `let`, an explicit cast - and so does a numeric
  literal, because the register that points at a program's buffer is the
  register that points at a fixed chip address. The reverse never opens: `addr
  as int` is refused, and so is `ptr as u32`. A `return` is still checked by
  equality, so a function declaring `addr` writes the cast, exactly as the
  language already requires between `*T` and `*const T`. Like the rest of the
  model the kind is free, and a gate holds the comparison rather than a comment
  claiming it: an `addr` field and a `u32` field writing the same literal emit
  byte-identical C.

- A `chip` modifier on `extern var`, naming memory a device reads or writes.
  The verbs take the chip as their subject, so the declaration says what the
  hardware does and not what the program may do:

  ```rust
  extern var BUF:  [u8; 16] chip writes;         // the device fills it
  extern var LIST: [u16; 8] chip reads;          // the device follows it
  extern var FIFO: [u8; 4]  chip reads writes;
  ```

  The mark rides in the type, so it travels with a pointer into that memory -
  spelled `*chip T` - and a helper that fills a buffer keeps the guarantee
  instead of losing it at the declaration. It is stripped at a dereference, and
  casting it away is refused, in both the shapes that reach for it: an explicit
  `*chip u8 as *u8` and a `*chip u8` passed to a `*u8` parameter. The
  measurement behind all of this is one program: read a buffer, start a
  transfer, poll, read again. The emitted C was always right - two reads, the
  volatile poll in the loop - and on m68k the second read was not in the output
  at all, because the compiler proved the two loads identical and folded the sum
  into a doubling.

  An `addr` register now accepts only an address into memory declared this way.
  A local's address was already refused because its frame dies; a parameter's
  was not, and the device keeps following the pointer after either is gone.

- `--freestanding` in the self-hosted compiler. The mode existed only in the
  frozen reference, and the capability model exists only in the port, so no
  single compiler could emit a program that reaches hardware through `rune`,
  `ward`, `sigil` and `seal` and still links without libc. The port now routes
  the print front, `strlen`, `mem_zero` and every `@debug` fragment to their
  libc-free counterparts and emits the helper prototypes inline, so the output
  needs no `-I` on any target. A float print is declined rather than
  approximated, and the refusal is reported by the driver before any file is
  written, so a rejected compile leaves nothing on disk. Measured across the
  whole example corpus: ninety-five emit byte-identically from both compilers,
  and the one that is refused is refused by both with the same first line.

- `runtime/sys_bare.c`, the silicon-side answer to the same five-symbol seam
  that `runtime/sys_amiga.c` answers over AmigaOS. Output goes to the serial
  port, allocation is a bump arena whose `free` rewinds only the most recent
  block, and a `seal` saves the status register and raises the interrupt level
  to seven - where the AmigaOS side must instead defer to exec's own nesting,
  because writing SR behind a scheduler's back breaks it. One guarantee, two
  things saved. A witness using a ward, a seal and an atomic group at once
  links `-nostdlib` into 2516 bytes with no libc at all.

- The NDK's first layer: the Amiga custom chip as exile declarations, a library
  rather than a binding. Four sigils naming who owns which byte range, three
  ward layouts with their canonical instances, and three atomic groups - the
  DMACONR/DMACON pair, the INTENAR/INTENA pair, and the blitter's thirteen
  registers. It binds to silicon and never to an OS, so one file serves a
  program under AmigaOS and one on bare metal. Compiled alone it emits a single
  `#include` and zero mentions of any name it declares, which a gate asserts,
  because "a declaration costs nothing" is the claim the whole design rests on.

- `make verify-emu`: a 68000 with a modelled custom chip, executing freestanding
  binaries and checking what they print. The CPU is Musashi, vendored under MIT
  at commit `313ebf1b` and byte-identical to upstream - every option the machine
  needs is set on the compiler line, and a gate arm fails if configuration ever
  moves into the vendored tree, because then a version bump stops being a copy
  and becomes a merge. Modelled beside it: DMACON and DMACONR as the SET/CLR
  pair they are, so clearing one bit leaves the others standing and the read
  port answers with state rather than with the last word written; the copper as
  a fetch-decode loop over MOVE, WAIT and the end marker; and the interrupt
  path as INTREQ, INTENA and the master bit, reaching the processor as an
  autovector. Three negative controls run before any witness is believed - an
  illegal instruction must stop the machine and name the opcode, a read outside
  the map must fault and name the address, and a handler that never
  acknowledges its source must die on the instruction budget rather than
  finish - and eight witnesses then run against pinned output.

- `make selfhost-warning-free`: the compiler compiles its own source with
  stderr open and the warning count asserted at zero, and that emission is then
  held to `-ansi -pedantic -Wall -Werror`. The open channel is the point, since
  a number read off a muted pipe is not a measurement.

### Changed

- A `seal` inside an interrupt handler is required and right, not redundant.
  The hardware masks only the level it dispatched; the region raises the mask to
  the top, so it is what keeps a higher level from preempting a sequence the
  chip must see whole, and it costs one restore before an `rte` that would have
  restored the status register anyway. The guide, the NDK header and the handler
  fixture had all been saying the opposite.

- An address made in a frame, handed down as a parameter and written to a
  register is refused, one frame below the local that made it. That program was
  pinned as an accepted blindness for three eras with a header naming what would
  close it; the `chip` rule is what closed it.

- A `chip` modifier written where the memory would be a local, a field or a
  parameter says where such memory has to live. It was already refused - only
  an `extern var` tail can parse it - but it surfaced as `expected '=', got
  identifier 'chip'`, which sends the reader to the assignment instead of to the
  storage.

### Fixed

- The escape analysis had never looked inside a `seal`. Its walker had an arm
  for every other body-bearing statement, so a seal body fell into the
  catch-all and was never descended into: the same store of a local's address
  was refused outside a region and accepted inside one. The finding is about the
  walker rather than about addresses, and it was demonstrated on a second,
  unrelated diagnostic family before being fixed.

- The same hole in two more passes, found by auditing every dispatcher that
  walks `defer` for whether it also walks `seal`. A double `free` inside a seal
  compiled clean; a sealed `mem_zero` emitted `memset` with no `<string.h>` and
  two implicit-declaration errors under the project's own standard.

- Assigning into an element of an `extern var` array. The rule that decides
  whether a target is mutable was asking on the elaborated node, where a
  module's extern var is an ordinary variable, so it looked the name up among
  mutable locals and printed advice that cannot be followed: an `extern var` has
  no `let mut` to declare. It now reads the root from the AST, where a qualified
  path is still a qualified path.

- A rune `.read()` accepted arguments and dropped them. `status.read(1, 2, 3)`
  compiled, and `status.read(bump())` compiled with `bump` emitted into the C
  and never called - so a side effect the user wrote disappeared. `.write` and
  `.strobe` had checked their arity since they landed, which is what made the
  gap read as covered. All three read paths are fixed: a plain rune, a
  register-file element, and a ward field.

- `runtime/freestanding.c` says a `-nostdlib` link of it plus a backend leaves
  only the sys seam undefined, and that was false on the target it exists for.
  The decimal conversion in `__ex_print_u32` used `/` and `%`; the 68000 has no
  32-bit divide, and on this toolchain the helpers ship only inside libc-shaped
  archives, never in `libgcc.a`. The claim therefore held on a 64-bit host and
  failed on an Amiga. Replaced with a width-agnostic shift-and-subtract divide
  by ten, verified over 1.5 million cases plus the 64-bit extremes before it was
  installed, since this file is shared with the hosted path.

- Thirty-three compiler warnings, to zero: seventeen unused parameters, fifteen
  capacity hints below the minimum, and a dead gating helper that was deleted
  rather than silenced. Two of the parameters were the `.read()` arity above.
  Beyond the exile side, two `assignment discards 'const' qualifier` warnings on
  the compiler's own emitted C are fixed in the source.

- The fuzzer's memory-budget class could never be reached. It attributes a death
  to the cap by re-running uncapped, and the condition underneath required that
  re-run to succeed - so an input the compiler legitimately rejects was never
  attributed to the cap. It now reads the variable the experiment actually names:
  the capped run died on a signal and the uncapped one did not, whatever its
  exit status.

## [1.3.0] - 2026-08-31

**The language grows in two places, and the shadowing class closes.** `seal`
answered *when* a sequence is indivisible and never *which registers* it is
about; an `atomic` clause answers that, and the last of the five limits the seal
era wrote down stops being a limit. Top-level `type` aliases resolve in the
compiler that ships. Behind both sits the round that made the compiler's own
prelude type-check as an ordinary program for the first time - which found that
the prelude had been written in a form the language rejects, and a chain of
type-checker defects behind it.

### Added

- An `atomic` clause inside a `ward` names the set of registers whose accesses
  must lie in one seal region:

  ```rust
  ward Custom {
      dmaconr: u16 at 0x002 readwrite;
      dmacon:  u16 at 0x096 readwrite;

      atomic { dmaconr, dmacon }
  }
  ```

  Two rules follow: every access to a member must be inside a `seal`, and every
  access to members of one group, within one function, must be inside the
  **same** region. A save read outside the region with only the restore inside -
  precisely the bug a seal exists to prevent - used to compile clean, run, and
  balance. The check is lexical and per-function; a helper called from inside a
  seal is deliberately not covered, because covering it means the flow analysis
  this design refuses, and ordering inside a group is a stronger property that
  would need a declaration of its own. Like every other construct in the
  capability model the clause costs nothing at run time: a program with it and
  the same program without it emit byte-identical C, which a gate asserts rather
  than a comment claiming it. `atomic` is not a keyword - a ward field, a struct
  field, a function or a local may still be named that.

- Top-level `type` aliases. A `type` declaration gives an existing type a second
  name by pure compile-time substitution - no new type, no conversion, and no
  trace in the emitted C:

  ```rust
  type Half   = u16;           // a plain alias
  type Word   = u32;
  type Buf<T> = [T; 4];        // one parameter
  type Line   = Buf<Word>;     // an alias whose body is another alias
  ```

  An alias is looked up before structs and enums, so it wins a shared name; its
  parameter count is checked; `pub type` exports one from a module; and a cycle
  is rejected rather than looped on, whether direct (`type A = B; type B = A;`)
  or through a nested position (`type A = Vec<A>;`). New example
  [`examples/type_alias.exl`](examples/type_alias.exl) and a tutorial section
  beside it.

- Type applications are checked against the declaration's arity - structs, enums
  and aliases alike. Too many arguments, too few, or any at all on a
  non-generic declaration is an error at the annotation instead of a silently
  accepted program.

- A user's top-level declaration shadows the prelude's of the same kind. A
  program that declares its own `struct Vec` gets its own, keeps the position it
  was written in, and the prelude items that name the shadowed type are dropped
  with it. Where a collision makes the surviving prelude itself illegal, the
  program is refused with a diagnostic about the prelude rather than compiled
  into C whose meaning depends on which declaration claimed the name first.

- The tutorial states the reserved-word set: `i64`, `u64`, `i128`, `u128`,
  `usize` and `isize` are rejected as identifiers so that adding those widths
  later cannot reinterpret source which used one as a name, and `shared` is held
  the same way for the capability model.

- Four gates, taking `selfhost-verify` from 28 to 32. `selfhost-atomic` pins the
  new clause - nine rejections asserted word for word from a table the gate
  reads rather than a string in the recipe, and five accepting programs built
  over a RAM stub and RUN, because with no oracle for this syntax "it compiles"
  is not evidence. `selfhost-own-tree` pins that the self-hosted compiler builds
  `src/main.exl` and agrees with the reference on it byte for byte.
  `selfhost-prelude-probe` and `selfhost-prelude-struct-lists` hold the
  shadowing rules.

### Changed

- The prelude's generic impls declare their parameter in the impl header -
  `impl<T> Vec<T>`, not `impl Vec<T>`. The old spelling is a form the language
  rejects, and it survived for the whole life of the compiler because
  instantiation binds the parameter from the call site: a declaration that
  carried no parameter was never asked to stand on its own.

- A ward fixture runs on m68k under an emulator now, not only on the host.
  Nothing in the ward era had ever been executed on the target. Asked, it
  faulted: the fixture's backing was a `c_ulong`, eight bytes on a 64-bit host
  and four on m68k, so the ward wrote and read past the end of its object. The
  backing is `[u8; 8]` - the same eight bytes on every target - and the gate now
  extracts the mangled type name out of the emission and asserts the C stub
  defines exactly it, so a rename fails loudly instead of linking to different
  storage.

- The documented limits of `seal` are four, not five. The fifth is replaced by
  the clause that closed it.

### Fixed

- A `match` arm binding keeps its payload's generic instantiation. A field read
  through such a binding reported `c_void` and a method call `unknown type 'T'`.
  This was the one file the self-hosted compiler could not build, and no single
  fixture could show it: constructing the value requires a type annotation, and
  that annotation registers the instance's fields before the match ever asks -
  the defect needs construction and consumption in different files.

- An `impl` whose declared parameters are not its target's arguments, in order,
  is rejected. `impl<A, B> Pair<B, A>` and `impl<T> Plain` used to compile, and
  a swapped binding compiles and means something else; three other malformed
  shapes sent the reader to a use of the parameter instead of to the header that
  failed to declare it.

- An associated type may be named in a trait default method's signature
  (`-> Vec<Self::Item>`), a method's signature resolves with its target pinned
  as `Self`, and a generic implementor inherits the trait's default methods. A
  projection under a tuple, an array or an `own *T` is refused with a diagnostic
  instead of aborting the compiler - an abort is the worst face a limit can wear.

- Emitted C that `cc` refused, from a program the compiler had accepted. A
  prelude type earns its declaration by being NAMED, not only by being built,
  and the used-set closes through the fields of reachable instances. Before,
  `struct Holder { b: *const Vec<int> }` emitted a `Vec` whose allocator field
  had no declaration: `field 'alloc' has incomplete type`.

- Signature collection resolves types with the whole skeleton in scope, so a
  container no longer reaches the aggregate list ahead of the type its own field
  points at; and an instance minted from a struct field names its nested types
  without registering them.

- Name resolution asks structs before enums, and a generic application binds
  structs before enums with its arity following the binding.

- A method call reports its arguments before its missing callee, so a call in
  which neither the method nor an argument exists names the argument.

- Every prelude struct declares how many type arguments it takes.

## [1.2.0] - 2026-08-03

**Consolidation.** 1.1.0 shipped the capability model; 1.2.0 spends the cycle on
what carries it. The prelude gains the two seam calls a program needs to resolve
a path from its environment, the self-hosted compiler's drop pass grows an error
channel and the rules that ride it, the fuzz era's stated limits become fixtures
with a red proof each, and every public artifact in the tree is swept of
pointers a reader cannot follow.

### Added

- `sys_getenv` and `sys_getcwd` in the `sys` seam. Together they let a program
  resolve a path from the environment with a compiled-in fallback.
  `sys_getenv` yields null for an unset name, which a caller must distinguish
  from set-and-empty. Real on the host; on the Amiga side both are stubs
  (null and an empty cwd), because there is no POSIX environment there and
  nothing on that target resolves a cross-toolchain - the amiga driver runs on
  the host.
- The tutorial's `rune` chapter now shows the volatile lowering checked by
  running it: a rune pointed at a RAM cell, written, read back and compared, at
  `-O2`, on m68k, under an emulator. The base is RAM deliberately - a chipset
  register cannot be read back inside an emulator, so an MMIO base would
  demonstrate the syntax and prove nothing.
- The tutorial states the static-auto-drop rule and quotes its diagnostic: an
  owner moved out on one branch but still owned on the other has no single
  answer the compiler can emit, because there are no runtime drop flags -
  consume it on every path or on none.
- Four gates, taking `selfhost-verify` from 24 to 28:
  `selfhost-port-drop-errors` and `selfhost-noentry-externs` pin the port's
  ownership diagnostics and its seam-extern emission; `docs-selfsufficient` and
  `docs-capability-golden` hold the documentation to what the compiler does.
- `fuzz-limits`, `fuzz-budget-witness` and `fuzz-gates` - the fuzz era's stated
  limits as executable fixtures instead of prose, each with a proof that it
  fails when it should.

### Changed

- The self-hosted compiler's drop pass reports instead of aborting, and carries
  the rules that error channel unblocked: an owning value discarded by an
  expression statement, an owned pointer dropped on the floor by a call, a root
  whose payload was moved out but still owns its storage, an owner auto-dropped
  before deferred code runs, and an owner consumed on one branch only. `defer`
  bodies now reach both the drop pass and the move pass.
- Seam externs are emitted for what the code names, with an entry point or
  without one (registered divergence #13 - given no entry point the frozen
  reference has no reachability root, falls back to declaring the whole seam
  and emits thirteen externs for a file that references none; the port is
  deliberately better, and mirroring would cost pay-for-use).
- No public artifact cites an internal design designator or points a reader
  outside the repository. Swept across every tracked file the project owns -
  documentation, fixture headers, gate messages, runtime sources, the CI
  workflow and the fuzz tooling - and the gate holding it names that boundary
  rather than a list of globs.
- CI no longer materialises the cross toolchain at a fixed prefix before the
  Amiga parity job; the port resolves `$EXILE_TOOLCHAIN` itself.

### Fixed

- An assignment's target type is its expected type, so a literal too wide for
  its slot is rejected at the assignment.
- An `own` field's allocator requirement is checked where the field is declared.
- A `defer` body's consume is visible to the move pass, and the defer-reads
  descent no longer runs two constructs deeper than the rule it serves.
- The planted-defect witness was passing on findings a clean tree already
  produces: the plant had drifted into the code path that emits `defer` bodies,
  which an ordinary loop never reaches, and the guard only checked that the
  planted string still existed. The plant is now anchored to the emitter, a
  canary asserts it reaches the emitted C before fuzzing starts, and the witness
  is a difference test whose two numbers are computed in the run that prints
  them.
- The tc-errors corpus grew 270 -> 273; every added fixture reproduces a
  machine-found defect.

## [1.1.0] - 2026-07-30

**The capability model.** 1.0.0 was the compiler's milestone — exile compiling
itself. 1.1.0 is the language growing its reason to exist: four constructs
that let a driver own hardware in the source and have the compiler prove it,
at zero cost in the output. All four shipped in five measured increments each,
with a ratified spec per construct and every claim below backed by a gate.

### Added

- **`rune`** — one register access, of declared width and direction:
  `rune bltsize: u16 at 0xDFF058 write;` with `.read()` / `.write()` /
  `.strobe()`, register files (`[u16; 32]`), and a rejection table (R1–R7)
  that keeps misuse a compile error. Direction lives in the type and is
  spelled in signatures — `fn kick(r: write rune<u16>)` — so a rune crosses
  a call boundary and `readwrite` attenuates to `read` or `write` on the way.
- **`ward`** — a typed overlay over MMIO or RAM: fields are runes at explicit
  offsets, overlaps are rejected with both fields and ranges named, instances
  occupy zero storage, and a top-level ward is a map every function reads.
- **`sigil`** — ownership of silicon: `sigil Blitter { 0xDFF040 .. 0xDFF05A }`
  declares the resource, `own Blitter;` claims it for a module, and no other
  module can materialise a handle into that range. The claim erases: with and
  without it, the emitted C is byte-identical.
- **`seal`** — an interrupt-atomic region: `seal { … }` masks interrupts and
  restores them on *every* exit path — fall-through, `return`, `break`,
  `continue`, `try` — nests safely by construction, and is true on both
  targets (SR mask bare-metal, `Disable()`/`Enable()` under AmigaOS).
- `tests/seal/blitter_setup.exl` — all four constructs in one NDK-shaped
  blitter program at the HRM's own addresses; composing them needed zero
  compiler changes.
- `defer` now fires on `break` and `continue` (closing a hole documented at
  their birth) and on `try`-propagation — every exit path, not most of them.
- `&raw::SCRATCH` — module-qualified globals as rune/ward bases.
- Six new examples with expected outputs (`exhaustiveness`, `host_only_argv`,
  `inferred_generics`, `let_else`, `module_const`, `sibling_shadowing`) and an
  `examples/README.md` index.
- A differential fuzzer (`tools/fuzz/`): the frozen OCaml reference and the
  self-hosted port disagree on an input, or either one crashes, and that is a
  finding — no expected outputs authored by anyone. Its gate plants a defect
  and must rediscover it, so a run that finds nothing is never mistaken for
  proof.
- Docs: a rewritten `README.md`, a new `docs/getting-started.md` (clone to a
  binary running under vamos), and a capability-model chapter in
  `docs/exile-by-example.md`.

### Changed

- FFI hygiene is enforced: `extern` declarations live inside `mod raw` (or
  `mod sys`) and are called through the module path — the quarantine the
  reference always required and the port silently didn't.
- Mixed bitwise nestings are parenthesised in the emitted C, which is now
  clean under `-Wparentheses` (registered divergence #7 — the frozen
  reference drops the parentheses; the port is deliberately better).
- `return` inside a match arm is legal (registered divergence #12 — the
  reference refuses the shape with a message about a construct that is not
  in the program; the seal era's fixtures are built on it).

### Fixed

- A function returning a value can no longer fall off its end — the
  exhaustive-return check covers every statement kind, mirrored from the
  reference down to its deliberate limits.
- Bindings die with their block: a `let` inside `if`/`while`/`for`/`defer`
  no longer leaks into the enclosing scope, and shadowing is rejected with
  the reference's three distinct messages.
- Generic calls check arity and argument types on all three generic paths,
  including prelude collection methods — `v.push(true)` on a `Vec<int>` and
  a borrowed pointer passed to `Allocator::free` are compile errors with the
  reference's exact wording, and arity answers before inference.
- A `defer` nested anywhere inside a defer body is rejected cleanly at any
  depth (previously an internal error), and `T`-inference reads through
  `own`, so the allocator opening shape gets the reference's repair hint.
- The tc-errors corpus grew 221 → 270; every added fixture reproduces a
  machine-found defect, none written by hand from theory.

## [1.0.0] - 2026-07-12

**exile-lang compiles itself.** `src/` is the exile compiler written in exile:
lexer, parser, typechecker, move pass, escape pass, drop pass, codegen. It is
self-hosted in the strict sense, and the claim is a gate rather than a story —
`make bootstrap-fixpoint` builds the port with the OCaml compiler, has it emit C
for its own source, compiles *that*, and has the result emit C for the same
source again. The two outputs are byte-identical (50 681 lines, zero diff). A
non-empty diff fails the build.

The second claim is just as checkable: across all 89 examples, the C emitted by
the exile compiler is byte-identical to the C emitted by the OCaml reference —
whole-file, not modulo whitespace. Every intermediate layer is diffed too
(tokens, AST, typed IR, post-drop IR), including the diagnostics of passes that
emit no code: `make selfhost-verify` runs the lot, and CI runs it on every push.

That is what 1.0 asserts. It does not assert the OCaml compiler is retired — it
is not. The port reproduces the *transformation* on valid input; it does not yet
carry the typechecker's diagnostics, so the reference implementation remains the
compiler you hand a broken program to. Closing that gap is the next arc, and
until it closes the two compilers ship together, with the reference as the
oracle every gate above measures against.

### Added
- **A self-hosted exile compiler** in `src/` (~10 000 lines of exile), covering
  the full pipeline: `lex -> parse -> typecheck -> move -> escape -> drop -> codegen`
- `make bootstrap-fixpoint` — the self-host proof as a build gate: the compiler
  built by the compiler must emit byte-identical C for its own source
- `make selfhost-verify` — every differential gate in one target (fixpoint,
  tokens, lexer errors, AST, parser errors, typed IR, post-drop IR, escape
  diagnostics), wired into CI
- Boolean literal patterns: `true` and `false` are pattern atoms, matchable
  against a `bool` scrutinee. `true | false` is exhaustive without a catch-all
  (see 0.11.6 below)
- `str::from_slice(arena, slice)` — the string-interning primitive: copy a
  `Slice<u8>` into an arena as a NUL-terminated `str`. A tokenizer holds
  identifiers as slices of its source buffer and materialises each one once
- `examples/bool_match.exl`, `examples/str_from_slice.exl`

### Changed
- The compiler source moved out of `examples/selfhost/` into `src/`. `examples/`
  is the language's example catalogue again; the compiler is not an example
- `docs/exile-by-example.md` no longer teaches a hand-written `free_tree` for
  owned recursive types — the drop glue is synthesized. A linear spine (a list)
  is torn down with a loop, so it costs constant stack however long it grows; a
  genuine tree keeps the recursive form and its depth lives on the C stack

### Fixed
- **Escape pass**: a param beyond the summary bitmask's width degraded to
  `Unknown`, which projects to "every param may carry the return" — so a
  `&local` passed at an unrelated position poisoned the result of a function that
  never returns it, and the pass rejected valid code. The high bit now saturates
  ("some param at or past 31") and the call site meets over the tail arguments
  only, keeping the imprecision where the unrepresentable index actually lives
- **Move / drop**: owned-transfer consume parity across `let` and reassignment;
  an owned RHS of a field / deref / index assignment is now consumed; a
  reassignment drops the old value *after* the RHS is evaluated, not before
- **Lift**: `if`-expression branches lift per-branch, so a block-shaped call
  argument hoists correctly instead of tripping an internal error

## [0.11.6] - 2026-06-15

Boolean literal patterns extend the literal-pattern family from
integers/chars to `true` / `false`, so `match b { true => ... | false => ... }`
matches a `bool` scrutinee directly. Because `bool` has a finite two-value
domain, a `true` / `false` pair is exhaustive on its own — no catch-all `_`
arm is required (and adding one is flagged unreachable), while a match missing
either value is a non-exhaustive compile error. Booleans lower to the same
scalar `switch` codegen path as integer literals (`case 1:` / `case 0:`).

### Added
- Boolean literal patterns: `true` and `false` are now pattern atoms, matchable
  against a `bool` scrutinee. `true | false` is exhaustive without a catch-all;
  a missing value is a non-exhaustive error and a duplicate value is flagged
  unreachable. Guards are supported (a guarded arm doesn't prove coverage)
- `examples/bool_match.exl` — boolean match demonstrating exhaustive
  `true` / `false` coverage and a guarded bool match

## [0.11.5] - 2026-06-13

Two threads land: the last own-pointer null soundness holes close, and a
`--freestanding` codegen mode gives the libc-free
output floor for exOS / bare-metal targets. An owned intrusive struct list
whose tail was a `null` terminator could segfault on teardown, and an `own *T`
binding initialized to `null` owned nothing yet escaped drop tracking, leaking
on the next reassignment — both are now closed, the second at type-check.
`--freestanding` drops `#include <stdio.h>` and routes print / strlen / memzero
to libc-free `__ex_*` helpers over `sys_write`; linked `-nostdlib` the object is
nm-clean (only `__ex_*` + the `sys_*` seam). 768 tests pass.

### Added
- `--freestanding` codegen mode: the emitted C drops `#include <stdio.h>` and
  routes print, strlen, and memzero to libc-free `__ex_*` helpers over the
  `sys_write` seam. Linked `-nostdlib` the object references only the `__ex_*`
  helpers and the `sys_*` seam (nm-clean) — the output floor for exOS /
  bare-metal targets. The same program still compiles the normal libc-printf
  way without the flag, byte-identically
- `runtime/freestanding.c` / `runtime/freestanding.h` — the `__ex_*` helper set
  backing freestanding output, implemented over `sys_write`
- `make verify-freestanding` — gates each `freestanding_*` example two ways:
  nm-clean (no libc symbol may leak into the object) and functional (output
  diff against `.expected`)
- `examples/freestanding_print.exl` — the libc-free output floor demo
- `examples/owned_struct_list.exl` — an owned intrusive struct list (`own *Self`
  next field) whose spine is auto-dropped iteratively

### Changed
- Auto-drop of an owned intrusive struct list — a struct with exactly one
  `own *Self` next-node field — now walks the spine with a loop instead of
  recursion, so teardown uses constant C stack however long the list grows.
  This ports the owned-enum-list change from 0.11.4 to the struct form

### Fixed
- segfault: dropping an owned struct or enum pointer whose `own *Self` field
  held a `null` terminator dereferenced null on teardown. The drop glue now
  null-guards each level, so the terminator that ends a list (or a null child of
  a tree) is safe (BUG-A)
- leak: an `own *T` binding initialized to `null` owned nothing and was not
  tracked by the drop pass, so a later `new(a) ...` reassignment leaked at scope
  exit. Such an initializer is now rejected at type-check, pointing at
  `new(a) ...` — a `null` terminator belongs in a field, not an owning binding
- `--freestanding` output no longer needs `-I runtime` to compile: the `__ex_*`
  prototypes are emitted inline rather than via an `#include`, so the C is
  self-contained

## [0.11.4] - 2026-06-13

The kernel-foundation freeze gate: the final hardening pass before
self-host bring-up, closing the three gating decisions from the 2026-06-11
freeze audit. A C cast of a binary operation lost its precedence; the
sanctioned rebind-after-consume list idiom miscompiled into a self-referential
cycle; dropping a long owned list overran the C stack one frame per node; and
the integer-width type names were still free identifiers. Each finding was
reproduced empirically — under ASan or a real m68k cross-build — before its
fix. 752 tests, verify-host 85/85, verify-amiga 84/84 (m68k Bebbo + vamos),
selfhost-diff 3/3 clean, ASan+UBSan sweep clean on all 84 host-buildable
examples.

### Added
- `examples/owned_list.exl` — a self-recursive owned enum list showing the
  rebind-after-consume idiom (`head = new(a) List::Cons(i, head)`) and the
  scope-exit auto-drop releasing all 50,000 nodes with no `free()` to write

### Changed
- Auto-drop of an owned linear list — a self-recursive enum where every variant
  owns at most one next-node of the same type — now walks the spine with a loop
  instead of recursion, so teardown uses constant C stack however long the list
  grows (the recursive form overran an 8 MB host stack around 200k nodes, and
  an Amiga-sized stack within hundreds). Genuine trees (a variant with two owned
  children) keep the recursive drop, where depth lives on the C stack by design
- Reserved `i64`, `u64`, `i128`, `u128`, `usize`, and `isize` as keywords.
  Only the names are frozen out of type positions ahead of self-host; the
  64-bit machinery itself stays additive future work

### Fixed
- miscompile: a C cast of a binary operation lost precedence — `(w >> 8) as u8`
  emitted `((unsigned char)w >> 8)`, truncating before the shift instead of
  after. A cast now parenthesizes its operand unless that operand is provably
  atomic
- miscompile / use-after-free: the rebind-after-consume idiom
  `head = new(a) List::Cons(i, head)` overwrote the destination before reading
  the consumed old value, so the new node pointed at itself — a cycle that
  auto-drop then walked into a use-after-free and double-free. Such assignments
  now build the node in a scratch and publish it last

## [0.11.3] - 2026-06-12

The freeze-hardening patch: the no-way-back freeze audit before self-host
bring-up rejected the freeze — despite a fully green suite it found ~18
root causes (38 findings) of silent use-after-free, double-free, leaks,
ICEs, and invalid C in the move/drop/escape ring and codegen. Two sprints
closed every one structurally — each finding reproduced
under ASan before the fix, then locked in with adversarial regressions.
740 tests, verify-host 84/84, selfhost-diff 3/3 clean, ASan+UBSan sweep
clean on all 85 examples (the shipped `hashmap.exl` had been leaking).

### Added
- `Slice` gains a `length()` method in the prelude, matching the
  Vec/String/HashMap/StringBuilder spelling; the `.len` field stays —
  a Slice is a transparent view and its explicit layout is its identity
- `new(a) Tree::Leaf` — bare unit-variant enum allocation (no parens)
  is now legal

### Changed
- `str::len` renamed to `str::length`, completing the 0.11.2 naming
  sweep (the audit showed module functions had been missed)
- `HashMap` lookups (`get` / `contains` / `remove`) now borrow their key
  argument instead of consuming it: the caller keeps ownership and its
  auto-drop frees the key, so probe keys no longer leak
- Host-vs-m68k arithmetic divergence documented as a decision: i32/u32
  on the host runs on 64-bit C `long` (no 32-bit wrap), div-by-zero is
  unchecked C semantics, negative div/mod is C89 implementation-defined
  — accepted until the port's differential testing

### Fixed
- ICE: nested `match` as the last statement of a match-arm body
- miscompile: a block-shaped `while` condition was hoisted once before
  the loop and never re-evaluated (infinite loop) — it now re-evaluates
  every iteration, with `continue` semantics preserved
- invalid C89: `new(a) Enum::V(...)` rvalue into a `*const T` slot wrote
  through a const-qualified temp; `(*p).field = v` emitted `*p.field`
  without parentheses; `Slice<str>` emitted a duplicated
  `const const char **`
- exhaustive `match`-as-expression emitted a switch without `default`,
  tripping may-be-uninitialized at `-O2` on host and m68k gcc — the last
  arm now carries a stacked `default:`
- write-after-consume UAF: `*q = 99` / `q.x = 99` after `free(a, q)`
  compiled silently — assignment targets now count as uses in the move
  pass
- shallow-drop leaks: auto-drop freed only the outermost allocation, so
  owning enum trees, `Vec<String>` elements, `HashMap` keys/values, and
  nested `own` fields leaked. The drop pass now synthesizes recursive
  drop functions (per-pointee, per-Vec, per-HashMap glue) so owned
  structures free depth-first by construction
- borrow-of-local-own escapes (UAF): returning or storing a borrow of a
  local `own` (or arena-local) compiled while the owner auto-dropped at
  scope exit, leaving a dangling alias. Loans are now restamped on every
  borrow slot (let, assign, struct/enum fields, call args, receivers),
  the escape pass treats a borrow of an own binding as local provenance,
  and `free(a, q)` invalidates q's outstanding borrows
- `defer` bodies reading an auto-dropped binding ran after the drop
  (guaranteed UAF) — rejected at registration; consuming defers stay
  legal
- steal-through-const-borrow double-free: match bindings through a
  borrowed scrutinee are demoted to borrows and can no longer consume
  own children
- partial-move tri-state for owned trees: consuming children via
  `match *t` marks the root partially moved — reading it errors, while
  `free(a, t)` legally releases just the root storage (the `free_tree`
  idiom); leaving a partially-moved root live at scope exit errors
- silent-leak rejects: a discarded own-returning call in statement
  position, an own rvalue in a borrow slot ("bind it first, then lend"),
  shallow `free(a, p)` on a root whose children still own memory, and a
  match arm consuming some but not all owning slots of its pattern are
  all compile errors now
- assignment right-hand sides were never typechecked (`b = 5` for a
  `b: *const int` produced invalid C) — assignments now coerce like
  `let`
- string literals with an embedded `\0` silently truncated the emitted
  C literal — now rejected (the `'\0'` char literal stays)
- unused match-payload bindings no longer emit C declarations (clean
  under `-Wall -Wextra -Werror`)
- derive-Debug output now closes with ` }` (space before the brace) for
  structs and struct variants

## [0.11.2] - 2026-06-10

The pre-port release: the entire gate list from the 2026-06-09 language
review is now closed (gates 3-5, after 0.11.1 closed 1-2), and the arena
allocator lands as the final port-prep block — the language is ready for
self-host bring-up to begin. 702 tests, verify-host 84/84, selfhost-diff
3/3 clean, ASan clean.

### Added
- literal patterns in `match` plus char literals: `'a'` is
  sugar for a byte int-literal (string-style escapes), usable in patterns,
  `==`, and arithmetic. An int-like scrutinee compiles to a C `switch`
  (a jump table on 68k — the lexer hot path), or-patterns become stacked
  `case` labels, guards a decision chain; scalar exhaustiveness requires
  an unguarded catch-all arm. New example: `literal_match.exl`
- `Arena` bump allocator in the prelude (`with_capacity` /
  `alloc_borrowed`): nodes are plain `*T` borrows owned by the arena, and
  the whole buffer releases in a single `free_fn` call on scope exit via
  the existing auto-drop machinery; align-8 bumps, `null` on exhaustion.
  New example: `arena.exl`
- `ptr_offset(p, n)` builtin — emits C `(p + n)` (one ADDA on 68k);
  u8-pointer base only, result is a plain borrow into the same storage
- captureless lambdas in method-argument position now route
  through the A2 closure machinery with an empty env, so they satisfy
  `Fn`-bounds: `.filter(|n| n > 1).map(|n| n * 10)` works end-to-end
- `rune` / `ward` / `sigil` / `seal` / `shared` reserved at
  the lexer level for future capability-model syntax

### Changed
- `free(alloc, p)` is now two-argument, releasing through the
  allocator seam (`free_fn`) in symmetry with `new(alloc)`; the one-arg form
  errors with guidance. One-arg `free` emitted libc `free()` while
  `new(alloc)` allocated through `alloc_fn` — correct on the host by
  coincidence, heap corruption with an arena or Amiga allocator
- pre-freeze naming sweep: `Vec.len` / `HashMap.len` (and
  friends) renamed to `length`; the `Debug` trait method renamed `fmt` →
  `fmt_debug` so `Display` and `Debug` can coexist on one type

### Fixed
- lift-pass matrix audit (one systematic pass instead of a
  patch per ICE): `for` in a match arm ICE'd — arm bodies now lift in
  place; shallow `texpr_children` blinded program scans, so dead-code
  elimination dropped functions called from loops inside arms (invalid
  C89); `new(a)` now counts as a use of `a` (fixes a false "unused
  variable" lint); method calls on rvalue receivers emitted invalid C
  (`&(ex_make())`) — the receiver is pinned to a temp; match bindings in
  the switch path were declared with the match-result type instead of the
  scrutinee type

## [0.11.1] - 2026-06-10

A hardening patch for the Owner-sigil memory model shipped in 0.11.0. A
design-session review found soundness gaps that green tests had missed, and
this release closes them: heap construction becomes fully allocator-explicit
(`new(alloc)`, bare `new` removed), `free` is gated to owning pointers, and
the drop pass is rewritten to share the move pass's consumption model —
completing the own-lifecycle so an owned value transfers, returns, or dies,
but never silently leaks or frees twice. 684 tests, verify-host 82/82,
selfhost-diff 3/3 clean, ASan/LeakSanitizer clean on the probe corpus.

### Added
- `new(alloc) T{}` and `new(alloc) Enum::V(...)` — allocator-explicit
  heap construction as the sanctioned origin of `own *T`, with `free()`
  accepting owning pointers
- first-class `own *T`: passing one to a `*T` / `*const T` parameter
  is a borrow (a loan — the value stays live), as opposed to an `own *T`
  parameter (a transfer); method dispatch and field assignment work through
  the owner pointer
- own-lifecycle completion: bare `own *T` bindings auto-drop at end
  of scope via static allocation-site provenance, reassigning a live `own`
  drops the old value first, and rebinding after consumption is legal
  (enables `s = next(s)` loops and `root = insert(a, root, v)` tree building)

### Changed
- option-1 clean break: bare `new T{}` (no allocator) is removed —
  every heap construction names its allocator
- unified drop pass: `drop.ml` rewritten to delegate consumption
  detection to the move pass (`Move.walk_expr`), so both passes share one
  liveness model instead of two drifting ones

### Fixed
- Auto-drop emitted `sizeof(unsigned char)` instead of the real buffer byte
  count
- `free()` accepted borrowed `*T` pointers — a latent double-free; it is now
  own-only
- ICE on nested enum boxing in argument position, e.g.
  `new(a) E::Add(new(a) E::Num(2), ...)`
- `Vec<T>` grow regression for aggregate element types ("cannot cast T to
  T"): identity casts on non-scalar types are now elided entirely, since C89
  has no aggregate casts
- Five drop-pass symptoms rooted in consumption-model drift — double frees
  and missed drops around callee-consumed and transitively-freed values
- Two silent leaks: reassigning a live `own` binding leaked the old value,
  and a bare `own` from `new(alloc)` was never dropped at all

## [0.11.0] - 2026-06-07

The closures-and-ownership release, and the last one before self-host bring-up
begins. Two arcs land together. First, the code-elevating feature wave finishes:
real closures with capture, the `Fn0`..`Fn4` callable-trait family, and a lazy
iterator-combinator stdlib (`map` / `filter` / `take` / `enumerate` / `fold` /
`collect`) built on bounded generic impls — the chained, zero-heap iteration
idiom the self-hosted compiler will lean on. Second, the memory model freezes:
the Owner-sigil `own *T` pointer retires `@move` in favour of static
end-of-scope auto-drop, extends to enum heap boxing for recursive trees (the
AST representation path), and covers the whole Vec/StringBuilder/HashMap stdlib.
Rounded out by the `sys_open`/`sys_close` file seam and transitive `pub use *`.
With this, the language surface and memory model needed to port the compiler
into Exile are complete.

### Added
- closures with capture (A2): a capturing closure lowers to an
  env-struct plus a synthesized `impl FnN`, with the body substituted over the
  captured fields — zero heap, monomorphized inline. Captureless (A1) lambdas
  still decay to a plain function pointer
- `Fn0`..`Fn4` prelude callable traits, with
  `f(x)` desugaring to `f.call(x)` on any `Fn`-bounded value and
  `self.f(v)` to `(self.f).call(v)`
- `|A| -> R` function-type source sugar in both bound and
  type-annotation position; `|| -> R` for the nullary `Fn0`
- `[&x]` explicit by-ref capture lists — opt-in borrow of a captured
  binding instead of by-value
- bounded generic impls (`impl<I: Iterator, F: Fn1> Iterator for Map`),
  the extension that unlocks lazy adapters
- iterator combinators: lazy, method-chained adapters `Map<I, F>` /
  `Filter<I, P>` / `Take<I>` / `Enumerate<I>` plus the consuming terminals
  `fold` and `collect`, exposed as `Iterator` default methods
- Owner-sigil memory model: `own *T`, a third owning pointer
  type that retires `@move` and auto-drops at end of scope via a new pass
  (`drop.ml`), LIFO-unified with user `defer`. `own *T` coerces to `*T` /
  `*const T` but never the reverse (a soundness tooth); drop is shallow,
  by-field. Full owner-sigil coverage across the Vec / StringBuilder / HashMap
  stdlib
- Enum heap boxing: `new Enum::Variant(args)` lowers
  to a malloc'd `own *Enum`, with drop synthesis over recursive enum trees —
  the representation path for the self-hosted AST
- `sys::sys_open` / `sys::sys_close` prelude file-handle seam: the host
  backend wraps libc `open`/`close`, the amiga backend stubs them — the seam
  future module-loading will read source files through
- transitive `pub use foo::*` re-export
- untyped-let mini-inferencer: `let x = expr` infers its type
  past bare literals
- transitive codegen dead-code elimination for prelude-emitted functions
- New examples: `closures_a2.exl`, `closure_byref.exl`, `fn_trait.exl`,
  `fnptr_sugar.exl`, `bounded_impls.exl`, `combinator_map.exl`,
  `combinator_filter.exl`, `combinator_take_enumerate.exl`,
  `combinator_fold_collect.exl`, `own_ptr.exl`, `enum_heap_box.exl`,
  `host_only_sys_open_demo.exl`

### Changed
- struct literals may appear inline as a call argument and inside
  parenthesized grouping without extra binding
- generic enum constructors in `match` arms get a bidirectional type
  seed, so the scrutinee's type parameters flow into the arm
- associated-type projection hardened for the
  combinator stack: bound-driven impl assoc-equality checking, a
  `try_resolve_assoc_proj` trait-decl shortcut, and bound-order /
  mono-instance fixes
- Host-only examples now follow a `host_only_*` naming convention and are
  filtered out of the amiga verify pipeline

### Fixed
- multi-hop `I::Item` associated-type projection over iterators with a
  generic `Item`, plus a closure-escape regression suite closing the
  outstanding escape hole

## [0.10.0] - 2026-06-04

The escape-analysis and self-host bring-up release. The keystone is the
a static escape/non-escape pass that closes the last four self-host soundness
blockers — it proves which values outlive their scope without being a borrow
checker. Around it lands the code-elevating feature wave queued ahead of
self-hosting (sub-slicing, scoped projection, active patterns, captureless
lambdas, generic trait methods, floats, type aliases, let-else, receiver
mutability), plus the bring-up machinery itself: a differential harness that
emits canonical token/AST/IR dumps, a golden corpus to lock them down, and the
first three ports of the compiler's own modules into Exile. Rounded out by
compile-time perf introspection (`--perf-report`) and two 68k perf wins.

### Added
- escape pass (`escape.ml`), in three phases: a floor that turns a returned
  borrow of a local into a hard error, a param-SET summary with an SCC fixpoint
  over recursion, and borrow invalidation. A static
  escape/non-escape analysis — not a borrow checker — closing the last four
  self-host soundness blockers
- sub-slicing: `a[lo..hi]` and `a[lo..=hi]` yield a `Slice<T>` view
  (`{ ptr, len }`); a `Range` is now usable inside `[]`
- scoped projection: `with <name> in <lvalue> { body }` binds a `*T`
  pointer to an lvalue for the block — read and write through `*name`
- active patterns: `view Name(p: T) -> A | B { body }`, total-only
  sugar over a synthesised `enum` plus a function, with full Maranget
  exhaustiveness on the synthesised variants
- A1 captureless lambdas: `|p: T| -> R body`; a lambda that captures
  nothing decays to a plain function pointer (zero heap)
- generic trait methods
- Floating point `f32` / `f64` with IEEE built-in operators; `Eq` / `Ord` /
  `Hash` are deliberately not implemented (distinct float identity)
- Type aliases (FP-1): `type Name<T...> = Type;`
- let-else (FP-2): `let <pat> = expr else { <divergent> };`
- Receiver mutability: `*self` / `*const self` method receivers
  (pointer-honest mutability)
- follow-up: builtin `str.hash()` / `str.eq()` dispatch, enabling
  `HashMap<str, _>`
- `mod sys` seam with per-target backends (`runtime/sys_host.c`,
  `runtime/sys_amiga.c`, the latter linking `-lm` for soft float)
- `default_allocator()` builtin; `println(x)` dispatches through `Display`
- Self-host bring-up harness: `--emit-tokens` / `--emit-ast` /
  `--emit-typed-ir` emit canonical dumps (`dump.ml`) — position-elided,
  collection-sorted, golden-stable — plus a golden corpus and
  `selfhost-corpus` / `selfhost-diff` Make targets
- First three ports of the compiler's own modules:
  `selfhost_pos.exl`, `selfhost_error.exl`, `selfhost_token.exl`
- perf-introspection: `--perf-report` (also `=json` / `=human`) — a
  compile-time budget-vs-actual cost report read from the typed IR
  (`perf_report.ml`)
- M1 `with_capacity` lint to size growable collections up front
- New examples: `escape_pass.exl`, `sub_slicing.exl`, `scoped_projection.exl`,
  `active_patterns.exl`, `lambdas.exl`, `generic_trait_methods.exl`,
  `floats.exl`, `selfhost_pos.exl`, `selfhost_error.exl`, `selfhost_token.exl`

### Changed
- M2: `HashMap` capacity is now rounded up to a power of two, so the probe
  index uses a bitmask instead of a 68k `DIVU`
- CI gates phase -1 corpus byte-stability and drops the now-redundant in-sync
  gate, keeping the `selfhost-diff` check
- Several existing examples refreshed to track the new features

## [0.9.0] - 2026-06-01

The stdlib-backbone release. Exile gains owned heap data structures —
`String`, `StringBuilder`, `Vec<T>`, and `HashMap<K, V>` — built on a new
affine move-checker (`@move`) that statically enforces use-at-most-once
for owned values, so the heap types free exactly once with no double-free or
use-after-free. Rounding it out: the `Display` / `Debug` writer-pattern traits
with `@derive(Debug)`, an Allocator size-on-free seam, and a
feature-interaction retrospective that closed eight self-host blockers,
restoring self-host-readiness.

### Added
- `@move` affine ownership: the move-checker (`move.ml`) enforces
  use-at-most-once dataflow over owned values, with a divergence oracle,
  `match` / `if`-expression fork-and-merge (may-consume union), and a LIFO
  `defer` end-of-scope check. Aggregate literals consume their `@move` fields;
  `[expr; N]` rejects a `@move` element
- Prelude `String`: owned NUL-terminated buffer with deep-copy (A2)
  semantics, content-based `Eq` / `Hash` / `Clone`, and `String::build`
- Prelude `StringBuilder`: `buf` / `len` / `cap` with `push_byte`, `push_str`,
  `push_int` (decimal render), `length`, and `as_slice`
- Prelude `Vec<T>`: the copy-out value-`T` workhorse collection
- Prelude `HashMap<K, V>`: linear-probe open-addressed symbol table,
  with `grow`, `remove`, and `iter`
- `Display` / `Debug` prelude traits (writer pattern) and `@derive(Debug)`
  auto-generated formatter
- Allocator size-on-free seam
- `*const self` borrow-only method receivers
- `cstr_len(s)` builtin — narrow `strlen` seam
- Raw write-through-pointer index assignment `p[i] = v`
- `I::Item` associated-type projection in generic signatures
- New examples: `string.exl`, `string_builder.exl`, `vec.exl`, `hashmap.exl`,
  `display.exl`, `derive_debug.exl`, `str_ops.exl`, `assoc_projection.exl`

### Changed
- `Clone` re-signatured to `fn clone(*const self) -> Self`; `Eq` / `Hash`
  re-signatured (move-checker prerequisites)
- `@derive(Eq)` on `*T` is address-equality by design (documented)
- Allocator `alloc_fn` byte-count width-pinned to `u32`

### Fixed
- S0: move-pass `merge_states` uses a may-consume union across branches
- S1/S2/S3: `@move` correctly tracked through aggregate literals, match-arm
  pattern-binds, and partial-move scrutinees (scrutinee marked `Consumed`
  post-arm); `String::free` takes `self` by value so the move-pass consumes it
- S4: `@derive(Clone)` recurses field-wise instead of a `*self` memcpy
- W1: a prelude struct used only as a field/payload now emits its definition
- W2: `==` / `!=` on aggregates dispatches through the `Eq` impl
- W3: cross-arm or-pattern duplicate constructor rejected at typecheck
- W4: slice-read cast to `T` silences a spurious warning
- C1/C2/C3: closed three ICE classes — `print`, generic tuple codegen, and
  `List.combine` on a wrong-arity generic call
- `(*p).field` lowers to `p->field` (precedence)
- empty enum instances refreshed after `build_enum_index`

## [0.8.0] - 2026-05-29

The traits release. `trait` / `impl Trait for Type` lands with static,
monomorphized dispatch, and brings the whole cascade it unlocks: generic
bounds `<T: Trait>`, default methods, supertraits, associated types,
`for`-in iteration over any `Iterator`, and `@derive(Eq, Hash, Clone)`.
Rounding it out: pointee-immutable `*const T`, bounded `Slice<T>` views,
`loop` / `break` / `continue`, and logical `!`.

### Added
- Traits: `trait T { fn sigs; }` + `impl T for Type`, with conformance
  checking (every required method present with a matching signature, no
  extras). Trait methods lower to ordinary `Type__method` functions —
  static dispatch, zero runtime cost
- Generic trait bounds `<T: Trait>` (and `<T: A + B>`): each instantiation
  monomorphizes to a direct call of the concrete type's method; a type
  that doesn't `impl` the bound is rejected at the call site
- Default trait methods: a trait method with a `{ ... }` body provides a
  default an `impl` may omit or override — synthesized per implementing
  type, and may call the type's other trait methods
- Supertraits: `trait B: A { ... }` requires implementors to also satisfy
  `A`; declaration-order independent
- Associated types: `type Item;` in a trait, bound by `type Item = T;` in
  each impl and referenced as the `Self::Item` projection in method
  signatures; erased before codegen
- `for x in <iterator>` over any type that `impl Iterator` — desugars to
  `loop { match it.next() { Some(x) => body | None => break } }`, reading
  the element type from `next()`'s `Option<…>`; `break` / `continue` work
  inside
- `@derive(Eq, Hash, Clone)` on structs and enums: synthesizes real `impl`
  blocks (field-wise `eq`, an `acc*31 + field.hash()` fold seeded by the
  enum variant index, value-copy `clone`) that flow through conformance,
  monomorphization, and codegen like any hand-written impl — so a derived
  type satisfies a `<T: Eq>` bound. `Hash: Eq`, so deriving Hash requires
  deriving Eq
- `*const T` pointee-immutable pointers (C `const T *`): a mutability axis
  orthogonal to `let` / `let mut`. `*T → *const T` coerces implicitly;
  `*const T → *T` needs an explicit `as` cast; writes through a `*const T`
  (`*p = v`, `p.f = v`) are rejected
- `Slice<T>` bounded view (MVP): a `*const T` + length, mapping to
  `struct { const T *ptr; unsigned long len; }`. Indexing `s[i]`, `.len` /
  `.ptr` access, and pass-by-value; sub-slicing and a mutable variant
  deferred
- `loop { ... }` infinite loop with `break` / `continue`; `continue` in a
  `for` still runs the counter step. `break` / `continue` outside a loop
  is a compile error
- Logical not `!` (bool → bool, maps to C `!`): postfix-tight and
  const-foldable
- New examples: `traits.exl`, `associated_types.exl`, `for_iterator.exl`,
  `derive.exl`, `const_pointers.exl`, `slice.exl`,
  `loop_break_continue.exl`, `logical_not.exl`

## [0.7.0] - 2026-05-27

A pattern-matching and self-host pre-work release: `match` arms now
support guards, or-patterns, and multi-statement bodies; doc-comment
syntax is recognised; and bare-metal addressing gets `int → *T` casts
alongside hex literals. Pipe operator `|>` lands as a postfix-tight
desugar for ergonomic function chaining.

### Added
- Multi-statement `match` arm bodies: `=> { stmts; trailing }`. Trailing
  expression is the arm's value (required in value position, optional in
  statement position)
- Pattern guards: `pat if <bool> => body`. Pattern binds are in scope
  for the guard; exhaustiveness is checked against unguarded arms only
- Or-patterns (MVP): `A | B | C => body` for wildcards, vars, and
  unit variants. Bindless top-level alternatives only — nested OR and
  equally-binding alternatives (`Some(x) | Other(x)`) are out of scope
- Doc-comment syntax: `///` line comments and `@doc("...")` attribute
  are recognised by the lexer/parser. Syntax-only MVP — no AST plumbing
  or C emission yet
- `int → *T` cast for MMIO-style addressing, e.g.
  `0xDFF000 as *CustomChip`. Pointer → int still rejected
- Pipe operator `|>`: `x |> f(a)` desugars to `f(x, a)` at parse time.
  Postfix-tight (binds like `.method()`); left-to-right chaining works
- Hex integer literals `0x...` / `0X...` in the lexer; empty `0x`
  reports a clear error
- New examples: `multi_stmt_match.exl`, `pattern_guards.exl`,
  `or_patterns.exl`, `doc_comments.exl`, `int_to_ptr_cast.exl`,
  `pipe.exl`, `hex_literals.exl`

### Changed
- Internal: `Ast.Call` refactored from tuple to record
  `{ callee; args; pos }`. Matches the shape of `MethodCall` and eases
  future field additions

## [0.6.0] - 2026-05-26

A substantial post-MVP release that pivots exile-lang toward an
immutable-by-default, expression-oriented core, and fills in the
C-style essentials needed for everyday work: fixed-size arrays,
ranges and `for` loops, compile-time constants, bitwise operators,
and short-circuit logical `&&` / `||`. Closes the post-MVP pack.

### Added
- Immutable-by-default bindings: `let` is read-only; `let mut` is the
  explicit opt-in (function parameters too)
- Expression-based function bodies — trailing expression is the return
  value; `if` is usable as an expression
- Compile-time `const NAME: T = expr;` folded to a C `#define`
- `size_of(T)` folds to a C `sizeof(...)` expression
- Fixed-size arrays `[T; N]` with array literals, indexing, and `len`
- `for v in lo..hi { ... }` loop with exclusive `..` and inclusive `..=`
  range syntax
- `Range<T>` and `RangeInclusive<T>` as first-class values; `for v in r`
  iterates a range bound to a variable
- Bitwise operators: `&`, `|`, `^`, `~`, `<<`, `>>`
- Short-circuit logical `&&` and `||` (bool-only, with proper
  short-circuit semantics)
- New examples: `arrays.exl`, `bitwise.exl`, `constants.exl`,
  `expressions.exl`, `for_loop.exl`

### Changed
- Enum and match arm syntax: `|` is now a separator only — the leading
  `|` before the first variant/arm is dropped (frees `|` for bitwise OR)
- Aggregate typedefs (struct/enum/tuple/array) are emitted in
  topological order, so dependent types appear after their dependencies
- Several existing examples updated to match the new syntax:
  `enums.exl`, `functions.exl`, `generics.exl`, `must_use.exl`,
  `structs.exl`, `tier.exl`, `debug_attr.exl`, `while_loop.exl`

### Fixed
- Typecheck propagates the expected `int` type through arithmetic
  sub-expressions, fixing cases where an integer literal in a nested
  binary op wasn't getting the surrounding context's type

## [0.5.0] - 2026-05-22

This release deepens generics and pattern matching: methods on generic
structs, nested match patterns with full exhaustiveness checking, and
`..base`/`@debug`/`pub use *` rounding out the generic and module surface.
It also splits `print`/`println` and adds a bare `return;` early-exit.

### Added
- Methods on generic structs (`impl<T> Pair<T>`) with bare `self`, and
  generic-struct types usable as function parameters
- Nested match patterns with full exhaustiveness and redundancy checking
  (Maranget's algorithm)
- `println` builtin that appends the trailing newline
- Bare `return;` early-exit that flushes pending `defer`s
- `..base` functional update on generic structs
- `@debug` attribute on generic structs
- `pub use foo::*` wildcard re-export
- New examples: `generic_impl.exl`, `reexport.exl`, and a multi-file
  `prelude/` demo

### Changed
- `print` no longer appends a trailing newline — use the new `println`
- `type_name()` now folds into the compile-time `++` operator
- Parser reports a clear error on a stray `;` in statement position

### Fixed
- Nested generic structs: `App` types normalized in instance fields, with
  struct- and enum-literal inference
- Generics review: detect value cycles, eliminate dead code through `App`,
  and reject opaque `extern` struct fields passed by value
- Eight robustness fixes found by fuzzing

## [0.4.2] - 2026-05-15

Small polish release on top of 0.4.1: two new compile-time builtins
(`++` for string concat, `type_name` for type introspection), broader
lint coverage, and stricter return-type enforcement.

### Added
- `++` compile-time string concat operator on `str` literals
- `type_name(expr)` compile-time builtin returning the expression's
  type as a `str`
- Linter warns on unused function parameters
- Linter rejects `free(&x)` — the `&` operator never produces a heap
  pointer
- New examples: `string_concat.exl`, `type_name.exl`

### Fixed
- Typecheck now enforces exhaustive return on value-returning
  functions
- Typecheck verifies the return-value type for `main` and for
  `void`-returning functions
- Top-level functions can no longer shadow a compiler builtin

## [0.4.1] - 2026-05-12

Polish release on top of the MVP: two new opt-in lints, a printable
`@debug` attribute, a stricter type checker, and a friendlier CLI
default.

### Added
- `@debug` attribute on structs and enums, generating a Rust-Debug-style
  printable form usable from `print`
- `@must_use` attribute and matching lint that warns when a non-`unit`
  return value is discarded (Rust-style, opt-in)
- Linter detects unused `let` bindings; the equivalent `cc` warning is
  silenced to avoid duplication

### Changed
- CLI now defaults `--target` to the host, and the "wrote" status line
  reports which target was used

### Fixed
- Type checker now enforces operand types in binary operators and
  matches function return values against the declared return type;
  previously these were under-checked

## [0.4.0] - 2026-05-11

The MVP-closing release. exile-lang can now drive a typical AmigaOS
application end-to-end: function pointer types, generic methods, an
allocator API on top of `size_of(T)`, a register-pinned FFI calling
convention, mutable extern globals, and a small `dos.library` demo
that runs natively under vamos.

### Added
- Function pointer types: `fn(T1, T2) -> R` as value, parameter, and
  return type, with per-shape `typedef` emission and bare-name
  autoconversion
- Generic methods on structs and generic free functions, monomorphized
  per concrete instance via body re-elaboration
- Allocator API in the prelude (`struct Allocator { state, alloc, free }`
  + `impl`), built on the new `size_of(T)` builtin and pointer↔pointer
  cast
- `--profile {core,standard,full}` compiler flag and `--bloat-report`
  output for measuring per-feature C-code footprint
- `@tier(core|standard|full)` attribute on declarations and a
  profile-aware linter that warns when a declaration above the active
  profile is used
- `pub use foo::bar;` single-name re-exports (wildcard re-exports
  rejected); aliases threaded through scope walk with cycle protection
- FFI extensions: `extern var NAME: T` for mutable C globals,
  `@reg(d0..d7/a0..a6)` per-parameter register pinning, and
  `@amiga_lib(Base)` metadata for AmigaOS library calls
- `extern struct Foo { fields }` with exposed fields readable and
  writable through `.field` and `->field`; the previous `extern struct
  Foo;` form remains for opaque types
- First AmigaOS example: `examples/amiga_hello.exl` calls
  `exec.library/OpenLibrary` and `dos.library/Output`/`Write`,
  running under vamos m68k; Makefile gains an `amiga_*.exl`
  convention for amiga-only examples
- New examples for the new features: `allocator_demo.exl`,
  `ctypes.exl`, `ffi_callback.exl`, `generic_methods.exl`,
  `tier.exl`; plus updates to `structs.exl`, `modules.exl`, and the
  FFI examples to use the now-required `mod raw { ... }` structure

### Changed
- FFI surface: `extern fn`/`extern struct`/`extern type`/`extern const`/
  `extern var` must live under a path whose last segment is `raw`
  (typically `pub mod raw { ... }`); call sites use `raw::name(...)`
- Method-call dispatch falls through to a function-pointer field when
  no method matches (`recv.field(args)`), emitted as a new
  `TIndirectCall` IR node
- Linter warnings now report the per-call-site origin (e.g. an
  `.alloc()` call site) instead of the prelude declaration position
- `defer` cleanups now flush correctly through diverging `match` arms
  produced by `try` / `?` desugaring

## [0.3.0] - 2026-05-08

A big release: adds methods, algebraic data types with pattern matching,
generics, an Option/Result prelude with `?T` propagation, and a first-cut
FFI to C. Plus snapshot tests for every example and a Makefile path for
running binaries under vamos.

### Added
- Methods on structs via `impl Foo { ... }` blocks, with `self` resolving
  relative struct paths
- Algebraic data types (`enum`) with three variant shapes: unit
  (`| None`), tuple (`| Some(T)`), and struct-like (`| Point { x: int }`)
- `match` as both a statement and an expression — usable on the right-hand
  side of `let` and inside `return`
- Generic ADTs and a built-in `Option` / `Result` prelude
- `try` / `orelse` / `?T` operators for error propagation through `Result`
- FFI MVP: `extern fn`, `extern struct`, `extern type`, `extern const`,
  `c_*` type aliases, variadic externs, and C-name mapping
- New examples: `methods.exl`, `enums.exl`, `generics.exl`, `ffi.exl`,
  `ffi_full.exl`, `ffi_libc.exl`, `ffi_opaque.exl`
- Snapshot tests: every example now ships a `.expected` file, with
  `verify-host` and `verify-amiga` Makefile targets to diff actual output
- Makefile targets for cross-compiling examples to Amiga m68k and running
  the resulting binaries under vamos; build outputs moved out-of-tree

### Changed
- Codegen lifts block-shaped sub-expressions into `__lift_N` temporaries,
  enabling `match`/`if` to appear inside larger expressions while staying
  C89-compatible
- Internal `Ir` module now owns the typed AST and shared type utilities;
  codegen consumes the typed form without re-deriving types
- CI no longer prebuilds a CI image in a separate workflow — the image is
  built locally as part of the regular pipeline

## [0.2.2] - 2026-05-04

Adds three small language conveniences and a debug-friendly codegen flag.

### Added
- `--annotate` flag emits C output with source-line comments, mapping each
  generated chunk back to its `.exl` origin
- `let x = some_tuple()` now binds the whole tuple to a single name (in
  addition to the existing `let (a, b) = ...` destructuring)
- `null` pointer literal, usable wherever a pointer is expected
- Functional update syntax for struct literals: `Point { x: 1, ..base }`
  copies remaining fields from `base` (Rust-style)

## [0.2.1] - 2026-05-03

Internal refactor: type checking moved into its own pass.

### Changed
- Type checking is now a dedicated `Typecheck` module that produces a
  `checked_program` consumed by codegen. Codegen no longer re-derives types
  while emitting C, shrinking it considerably and clearing the way for
  swappable backends.

## [0.2.0] - 2026-05-02

Adds aggregate data types and manual memory management: structs, pointers, and
heap allocation. All three land with example files in `examples/`.

### Added
- Struct types: declarations, struct literals (`Point { x: 3, y: 4 }`), field
  access (`p.x`), field assignment, nested structs, and pass-by-value to and
  from functions
- Pointer types (`*T`), address-of (`&x`), dereference (`*p`), and auto-deref
  on field access through pointers (`p.x` where `p: *Point`)
- Heap allocation via `new T { ... }` returning `*T`, paired with `free(p)` —
  typically used with `defer free(p)` for scoped cleanup
- New examples: `structs.exl`, `pointers.exl`, `heap.exl`

## [0.1.0] - 2026-05-01

First tagged preview. The compiler runs end-to-end: lexer, parser, type
checker, and a C89 code generator. Every feature listed below has a matching
file in [`examples/`](examples/) that compiles to C and builds cleanly under
`cc -ansi -pedantic -Wall`.

### Added
- Core types: `int`, `bool`, `str`
- Arithmetic, comparison, and boolean operators
- `let` bindings, `if` / `else`, `while` loops
- Functions with parameters, return values, and recursion
- Tuple return values (`-> (int, int)`) with `let (a, b) = ...` destructuring
- `defer` statements with LIFO ordering, block form, and early-exit semantics
- Modules (`mod` blocks) with `pub` visibility and `::` path access
- Multi-file projects via the loader
- Built-in `print` for `int`, `bool`, and `str`
- Single-line (`//`) and block (`/* */`) comments
- CI workflow building the compiler, running tests, and compiling every
  example with `-ansi -pedantic -Wall`

[1.4.0]: https://github.com/damroth/exile-lang/releases/tag/v1.4.0
