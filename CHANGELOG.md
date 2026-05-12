# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

[0.4.1]: https://github.com/damroth/exile-lang/releases/tag/v0.4.1