# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
- DR-046 `new(alloc) T{}` and `new(alloc) Enum::V(...)` — allocator-explicit
  heap construction as the sanctioned origin of `own *T`, with `free()`
  accepting owning pointers
- DR-047 first-class `own *T`: passing one to a `*T` / `*const T` parameter
  is a borrow (a loan — the value stays live), as opposed to an `own *T`
  parameter (a transfer); method dispatch and field assignment work through
  the owner pointer
- DR-048 own-lifecycle completion: bare `own *T` bindings auto-drop at end
  of scope via static allocation-site provenance, reassigning a live `own`
  drops the old value first, and rebinding after consumption is legal
  (enables `s = next(s)` loops and `root = insert(a, root, v)` tree building)

### Changed
- DR-047 option-1 clean break: bare `new T{}` (no allocator) is removed —
  every heap construction names its allocator
- DR-048 unified drop pass: `drop.ml` rewritten to delegate consumption
  detection to the move pass (`Move.walk_expr`), so both passes share one
  liveness model instead of two drifting ones

### Fixed
- Auto-drop emitted `sizeof(unsigned char)` instead of the real buffer byte
  count (DR-046)
- `free()` accepted borrowed `*T` pointers — a latent double-free; it is now
  own-only (DR-047)
- ICE on nested enum boxing in argument position, e.g.
  `new(a) E::Add(new(a) E::Num(2), ...)` (DR-047)
- `Vec<T>` grow regression for aggregate element types ("cannot cast T to
  T"): identity casts on non-scalar types are now elided entirely, since C89
  has no aggregate casts (DR-048 GATE-1)
- Five drop-pass symptoms rooted in consumption-model drift — double frees
  and missed drops around callee-consumed and transitively-freed values
  (DR-048 GATE-2)
- Two silent leaks: reassigning a live `own` binding leaked the old value,
  and a bare `own` from `new(alloc)` was never dropped at all (DR-048 GATE-2)

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
- DR-024 closures with capture (A2): a capturing closure lowers to an
  env-struct plus a synthesized `impl FnN`, with the body substituted over the
  captured fields — zero heap, monomorphized inline. Captureless (A1) lambdas
  still decay to a plain function pointer
- DR-017 / DR-023 / DR-029 `Fn0`..`Fn4` prelude callable traits, with
  `f(x)` desugaring to `f.call(x)` on any `Fn`-bounded value (DR-018) and
  `self.f(v)` to `(self.f).call(v)` (DR-019)
- DR-021 / DR-028 `|A| -> R` function-type source sugar in both bound and
  type-annotation position; DR-023 `|| -> R` for the nullary `Fn0`
- DR-033 `[&x]` explicit by-ref capture lists — opt-in borrow of a captured
  binding instead of by-value
- DR-016 bounded generic impls (`impl<I: Iterator, F: Fn1> Iterator for Map`),
  the extension that unlocks lazy adapters
- DR-026 iterator combinators: lazy, method-chained adapters `Map<I, F>` /
  `Filter<I, P>` / `Take<I>` / `Enumerate<I>` plus the consuming terminals
  `fold` and `collect`, exposed as `Iterator` default methods
- DR-030 Owner-sigil memory model (Faza-1a): `own *T`, a third owning pointer
  type that retires `@move` and auto-drops at end of scope via a new pass
  (`drop.ml`), LIFO-unified with user `defer`. `own *T` coerces to `*T` /
  `*const T` but never the reverse (a soundness tooth); drop is shallow,
  by-field. Full owner-sigil coverage across the Vec / StringBuilder / HashMap
  stdlib
- DR-031 / DR-030 Faza-1.1 enum heap boxing: `new Enum::Variant(args)` lowers
  to a malloc'd `own *Enum`, with drop synthesis over recursive enum trees —
  the representation path for the self-hosted AST
- DR-032 `sys::sys_open` / `sys::sys_close` prelude file-handle seam: the host
  backend wraps libc `open`/`close`, the amiga backend stubs them — the seam
  future module-loading will read source files through
- DR-040 transitive `pub use foo::*` re-export
- DR-036 / DR-038 untyped-let mini-inferencer: `let x = expr` infers its type
  past bare literals
- DR-035 transitive codegen dead-code elimination for prelude-emitted functions
- New examples: `closures_a2.exl`, `closure_byref.exl`, `fn_trait.exl`,
  `fnptr_sugar.exl`, `bounded_impls.exl`, `combinator_map.exl`,
  `combinator_filter.exl`, `combinator_take_enumerate.exl`,
  `combinator_fold_collect.exl`, `own_ptr.exl`, `enum_heap_box.exl`,
  `host_only_sys_open_demo.exl`

### Changed
- DR-034 struct literals may appear inline as a call argument and inside
  parenthesized grouping without extra binding
- DR-020 generic enum constructors in `match` arms get a bidirectional type
  seed, so the scrutinee's type parameters flow into the arm
- DR-022 / DR-025 / DR-027 associated-type projection hardened for the
  combinator stack: bound-driven impl assoc-equality checking, a
  `try_resolve_assoc_proj` trait-decl shortcut, and bound-order /
  mono-instance fixes
- Host-only examples now follow a `host_only_*` naming convention and are
  filtered out of the amiga verify pipeline

### Fixed
- DR-039 multi-hop `I::Item` associated-type projection over iterators with a
  generic `Item`, plus a closure-escape regression suite closing the
  outstanding escape hole

## [0.10.0] - 2026-06-04

The escape-analysis and self-host bring-up release. The keystone is DR-010,
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
- DR-010 escape pass (`escape.ml`), in three phases: Phase A Tier-1 floor
  (S5a folded to a hard error), Phase B param-SET-summary with an SCC fixpoint
  over recursion (S5b), Phase C borrow invalidation (S5c/S5d). A static
  escape/non-escape analysis — not a borrow checker — closing the last four
  self-host soundness blockers
- DR-011 sub-slicing: `a[lo..hi]` and `a[lo..=hi]` yield a `Slice<T>` view
  (`{ ptr, len }`); a `Range` is now usable inside `[]`
- DR-012 scoped projection: `with <name> in <lvalue> { body }` binds a `*T`
  pointer to an lvalue for the block — read and write through `*name`
- DR-009 active patterns: `view Name(p: T) -> A | B { body }`, total-only
  sugar over a synthesised `enum` plus a function, with full Maranget
  exhaustiveness on the synthesised variants
- DR-008 A1 captureless lambdas: `|p: T| -> R body`; a lambda that captures
  nothing decays to a plain function pointer (zero heap)
- DR-014 generic trait methods
- Floating point `f32` / `f64` with IEEE built-in operators; `Eq` / `Ord` /
  `Hash` are deliberately not implemented (distinct float identity)
- Type aliases (FP-1): `type Name<T...> = Type;`
- let-else (FP-2): `let <pat> = expr else { <divergent> };`
- Receiver mutability: `*self` / `*const self` method receivers
  (pointer-honest mutability)
- DR-007 follow-up: builtin `str.hash()` / `str.eq()` dispatch, enabling
  `HashMap<str, _>`
- DR-006 `mod sys` seam with per-target backends (`runtime/sys_host.c`,
  `runtime/sys_amiga.c`, the latter linking `-lm` for soft float)
- `default_allocator()` builtin; `println(x)` dispatches through `Display`
- Self-host bring-up harness: `--emit-tokens` / `--emit-ast` /
  `--emit-typed-ir` emit canonical dumps (`dump.ml`) — position-elided,
  collection-sorted, golden-stable — plus a golden corpus and
  `selfhost-corpus` / `selfhost-diff` Make targets
- First three Faza-0 ports of the compiler's own modules:
  `selfhost_pos.exl`, `selfhost_error.exl`, `selfhost_token.exl`
- DR-013 perf-introspection: `--perf-report` (also `=json` / `=human`) — a
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
affine move-checker (`@move`, DR-002) that statically enforces use-at-most-once
for owned values, so the heap types free exactly once with no double-free or
use-after-free. Rounding it out: the `Display` / `Debug` writer-pattern traits
with `@derive(Debug)`, an Allocator size-on-free seam (DR-004), and a
feature-interaction retrospective that closed eight self-host blockers,
restoring self-host-readiness.

### Added
- `@move` affine ownership (DR-002): the move-checker (`move.ml`) enforces
  use-at-most-once dataflow over owned values, with a divergence oracle,
  `match` / `if`-expression fork-and-merge (may-consume union), and a LIFO
  `defer` end-of-scope check. Aggregate literals consume their `@move` fields;
  `[expr; N]` rejects a `@move` element
- Prelude `String` (Faza 1): owned NUL-terminated buffer with deep-copy (A2)
  semantics, content-based `Eq` / `Hash` / `Clone`, and `String::build`
- Prelude `StringBuilder`: `buf` / `len` / `cap` with `push_byte`, `push_str`,
  `push_int` (decimal render), `length`, and `as_slice`
- Prelude `Vec<T>` (DR-003): the copy-out value-`T` workhorse collection
- Prelude `HashMap<K, V>` (DR-007): linear-probe open-addressed symbol table,
  with `grow`, `remove`, and `iter`
- `Display` / `Debug` prelude traits (writer pattern) and `@derive(Debug)`
  auto-generated formatter
- Allocator size-on-free seam (DR-004)
- `*const self` borrow-only method receivers
- `cstr_len(s)` builtin — narrow `strlen` seam (DR-001)
- Raw write-through-pointer index assignment `p[i] = v`
- `I::Item` associated-type projection in generic signatures
- New examples: `string.exl`, `string_builder.exl`, `vec.exl`, `hashmap.exl`,
  `display.exl`, `derive_debug.exl`, `str_ops.exl`, `assoc_projection.exl`

### Changed
- `Clone` re-signatured to `fn clone(*const self) -> Self`; `Eq` / `Hash`
  re-signatured (DR-002 prerequisites)
- `@derive(Eq)` on `*T` is address-equality by design (documented)
- Allocator `alloc_fn` byte-count width-pinned to `u32` (DR-001 §6(ii))

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

[0.11.1]: https://github.com/damroth/exile-lang/releases/tag/v0.11.1