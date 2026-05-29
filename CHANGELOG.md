# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

[0.8.0]: https://github.com/damroth/exile-lang/releases/tag/v0.8.0