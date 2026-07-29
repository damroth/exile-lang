# examples

One file per feature. Every example that is a program has a `.expected` beside
it holding the exact stdout it must produce, so each one is something you can
build, run and diff rather than read and trust.

```sh
make compiler
./exilc --target host examples/hello_world.exl -o hello
./hello                                           # Hello, World!
./hello | diff - examples/hello_world.expected    # silent: it matches
```

The files under `multi_file/` and `prelude/` are the library halves of two of
the examples below, so they have no `.expected` of their own.

If you would rather read prose than source, [Exile by
Example](../docs/exile-by-example.md) walks the same ground with commentary,
and links back here at each step.

## First steps

| | |
|---|---|
| `hello_world` | printing, and the shape of a program |
| `arithmetic` | integer arithmetic and precedence |
| `comparisons` | comparison and equality operators |
| `logical_not` | `!` on `bool` |
| `bitwise` | bitwise, shift and modulo - the register-poking operators |
| `if_else` | `if`/`else` in statement and expression position |
| `while_loop` | `while` |
| `loop_break_continue` | `loop`, `break`, `continue` |
| `for_loop` | `for v in lo..hi` and `lo..=hi` |
| `functions` | declaring and calling functions |
| `expressions` | a block's trailing expression is its value |

## Types and data

| | |
|---|---|
| `integers` | the integer types and their literals |
| `floats` | `f32` / `f64` |
| `hex_literals` | `0x...` literals |
| `constants` | `const NAME: T = expr;`, folded at compile time |
| `tuples` | tuple types and destructuring |
| `structs` | structs, and functional update with `..base` |
| `arrays` | `[T; N]`, a by-value aggregate |
| `enums` | ADTs: unit, tuple and struct variants |
| `string_concat` | `++`, a compile-time string concatenation |
| `type_name` | `type_name(expr)` yields the type's name as a `str` |

## Memory

| | |
|---|---|
| `pointers` | `*T` and dereferencing |
| `const_pointers` | `*const T`, pointee-immutable |
| `int_to_ptr_cast` | `int -> *T`, MMIO-style addressing |
| `heap` | heap allocation |
| `own_ptr` | `own *T`: single ownership with auto-drop |
| `owned_list` | an owned linear list, consumed in a loop |
| `owned_struct_list` | intrusive list linked by an `own *Self` field |
| `enum_heap_box` | heap-boxed enum tuple-variants |
| `slice` | `Slice<T>`: a pointer plus a length |
| `sub_slicing` | `a[lo..hi]` and `a[lo..=hi]` |
| `scoped_projection` | `with name in lvalue { ... }` |
| `defer` | cleanup that runs on every exit path |
| `escape_pass` | the escape checker refusing a borrow that outlives its owner |
| `arena` | a bump allocator for tree-shaped workloads |
| `allocator_demo` | the pluggable allocator interface |

## Pattern matching

| | |
|---|---|
| `literal_match` | integer literal patterns |
| `bool_match` | `true` / `false` patterns |
| `multi_stmt_match` | `=> { stmts; trailing }` arm bodies |
| `or_patterns` | `pat1 \| pat2 => body` |
| `pattern_guards` | `pat if cond => body` |
| `let_else` | bind the payload, or leave |
| `exhaustiveness` | the compiler names the case you missed |
| `active_patterns` | `view Name(p: T) -> Case1 \| Case2 { ... }`, F#-flavoured |

## Abstractions

| | |
|---|---|
| `generics` | type parameters, monomorphised |
| `inferred_generics` | an instance is born where it is constructed |
| `generic_impl` | `impl<T> Foo<T> { ... }` |
| `generic_methods` | type parameters bound at the call site |
| `generic_trait_methods` | generic methods inside a trait |
| `bounded_impls` | bounded generic impls |
| `methods` | `impl` blocks and the receiver parameter |
| `traits` | `trait` plus `impl Trait for Type`, static dispatch |
| `associated_types` | `type Item;` in a trait |
| `assoc_projection` | `I::Item` in a generic function |
| `display` | the `Display` writer-pattern trait |
| `derive` | `@derive(...)` for prelude traits |
| `derive_debug` | `@derive(Debug)` and its generated formatter |
| `lambdas` | captureless lambdas |
| `closures_a2` | closures with capture |
| `closure_byref` | explicit by-ref capture lists |
| `fn_trait` | the `Fn` trait |
| `fnptr_sugar` | `\|A\| -> R` in type position |
| `for_iterator` | `for x in` anything that implements `Iterator` |
| `pipe` | `\|>`, first-argument style |

## Prelude collections

| | |
|---|---|
| `string` | `String`: an owned NUL-terminated buffer |
| `string_builder` | `StringBuilder`: a growable `u8` buffer |
| `str_ops` | `mod str`, pure-exile string operations |
| `str_from_slice` | materialising a `Slice<u8>` as a `str` |
| `vec` | `Vec<T>`, the growable workhorse |
| `hashmap` | `HashMap<K, V>`, open addressing with linear probing |
| `combinator_map` | `map` over an iterator |
| `combinator_filter` | `filter` |
| `combinator_take_enumerate` | `take` and `enumerate` |
| `combinator_fold_collect` | `fold` and `collect` |

## Modules

| | |
|---|---|
| `modules` | block modules and module-relative naming |
| `module_const` | a `const` inside a module |
| `reexport` | `pub use foo::*;` to build a prelude |
| `sibling_shadowing` | the same name in two disjoint blocks |
| `multi_file/` | one program across `main.exl` and `lib.exl` |
| `prelude/` | a directory module: `mod.exl` plus its submodules |

## The C boundary

| | |
|---|---|
| `ffi` | `extern fn` plus a linked C stub |
| `ffi_libc` | calling libc |
| `ffi_opaque` | `extern struct Foo;`, a type exile never looks inside |
| `ffi_full` | `@c_include`, `extern type`, `extern const` |
| `ffi_callback` | function pointer types as values |
| `ctypes` | the `c_*` aliases for C's primitive types |

## Amiga, and the floor beneath libc

| | |
|---|---|
| `amiga_hello` | an AmigaOS-native binding, end to end |
| `freestanding_print` | `--freestanding`: emitted C that links no libc at all |
| `host_only_argv` | the command line, through the `sys` seam |
| `host_only_sys_open_demo` | opening a file through the same seam |

## Attributes and tiers

| | |
|---|---|
| `doc_comments` | `///` and `@doc("...")` |
| `debug_attr` | `@debug` for printable structs and enums |
| `must_use` | `@must_use` on a fn or an enum |
| `tier` | `@tier(core\|standard\|full)`, the comfort-tier hint |

## The compiler's own source

| | |
|---|---|
| `selfhost_pos` | source positions, ported from the reference |
| `selfhost_error` | the diagnostic type |
| `selfhost_token` | the lexer's token type |

## Not here: the capability model

`rune`, `ward`, `sigil` and `seal` - the four constructs that let a driver own
a range of chip registers and have the compiler prove it - have no example in
this directory, and the contract at the top of this page is the reason. An
example here is a program you build, run, and diff against its `.expected`; a
program that stores to `$DFF058` runs on no host, and the chipset sits above
what vamos emulates. Pointing those constructs at a RAM buffer instead would
make them runnable and would teach the wrong thing, since what the model is for
is precisely the addresses that cannot be faked.

They are covered instead by [section 22 of Exile by
Example](../docs/exile-by-example.md#22-owning-the-hardware---rune-ward-sigil-seal),
which shows each construct, the C it emits, the diagnostics it produces, and
the five things it deliberately does not check. The fixtures behind that
chapter live in [`tests/rune/`](../tests/rune/), [`tests/ward/`](../tests/ward/),
[`tests/sigil/`](../tests/sigil/) and [`tests/seal/`](../tests/seal/), and are
checked by `make selfhost-rune`, `-ward`, `-sigil` and `-seal` against the C
they emit rather than against stdout.
