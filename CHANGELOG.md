# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

[0.2.1]: https://github.com/damroth/exile-lang/releases/tag/v0.2.1