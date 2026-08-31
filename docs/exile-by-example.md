# Exile by Example

Exile, one feature at a time. Each section introduces a piece of the
language with a short annotated program, then points to the full
runnable version in [`examples/`](../examples/) — every example ships
with its `.expected` reference output, so you can build it, run it, and
diff. The path runs from "what is this" through setup, basic syntax, the
type system, ADTs, generics, traits, modules, all the way to FFI and
AmigaOS.

> Language status: the feature set as of **July 2026**. The capability
> model in section 22 landed after the `v1.0.0` tag, which marked the
> compiler self-hosting rather than a frozen language. Everything shown
> below is implemented and pinned by a checked-in fixture, and most of
> it also has a runnable example in [`examples/`](../examples/). Section
> 22 is the exception: a store to `$DFF000` runs on no host, and the
> chipset sits above what vamos emulates, so those fixtures live in
> [`tests/`](../tests/) and are checked by the C they emit. What landed
> when, and why, is recorded in [`CHANGELOG.md`](../CHANGELOG.md).

---

## 1. What is exile-lang

exile is a small programming language that compiles to **C89**, targeting
**Amiga m68k** (Kickstart 1.3 / A500), but perfectly runnable on
Linux/macOS for development and testing.

Flavour:

- **Rust-style** syntax (fn, let, braces, no parens around if/while).
- **Zero GC** — explicit allocators (`new(a)`), `own *T` owned pointers
  with compile-time auto-drop, cleanup via `defer`.
- **ADTs + pattern matching** in the OCaml flavour, but with braces.
- **Static traits** — `impl Trait for Type`, monomorphic dispatch, no `dyn`.
- **First-class modules** (block + per-file, à la Rust).
- **C FFI** designed for real AmigaOS ROM calls, but works just as well
  with libc.

What's **not** there (and won't be in the near horizon): i64/u64, async,
a lifetime/borrow checker (ownership is `own *T` plus move tracking —
[sec 10](#10-pointers-and-memory-stack-vs-heap) — not lifetimes), GC.

---

## 2. Setup

```sh
git clone --recurse-submodules <repo-url>
cd exile-lang
make compiler     # ~90 s — builds ./exilc using nothing but cc
make toolchain    # optional, ~30-60 min — Bebbo amiga-gcc for .adf-able binaries
```

exile is written in exile, and `seed/exilc.c` — the compiler's own C
output — is committed, so building it needs no package manager and no
language runtime: `cc` builds the seed, the seed compiles `src/*.exl`,
and `cc` builds that into `./exilc`. Every `./exilc` below is that
binary.

If you only want to write and run on the host (Linux/macOS), `make
compiler` is enough. `make toolchain` is needed only when you want to
produce m68k binaries.

The same ladder, run one stage further, is also the proof that the
compiler reproduces itself:

```sh
make bootstrap-from-seed   # ~3.5 min — and the last two stages must
                           # emit byte-identical C, or the build fails
```

---

## 3. Hello, World!

```rust
fn main() {
    println("Hello, World!");
}
```

[examples/hello_world.exl](../examples/hello_world.exl)

Build and run on the host:

```sh
./exilc --target host examples/hello_world.exl -o hello
./hello
```

For Amiga (with the toolchain):

```sh
./exilc --target amiga examples/hello_world.exl -o hello
# produces an m68k binary runnable under WinUAE / FS-UAE / vamos
```

Or stop at the C and go no further:

```sh
./exilc --target c examples/hello_world.exl
# writes examples/hello_world.c — C89, next to the source

./exilc --target c --c-out /tmp/hello.c examples/hello_world.exl
# same C, at a path you choose
```

A few more build-surface flags: `--c-out <path>` writes the generated C
alongside normal compilation, `--annotate` adds `file:line:col` markers
above each statement in the emitted C, and `--show-cc-warnings` passes
the C compiler's warnings through. `--emit-tokens` / `--emit-ast` /
`--emit-typed-ir` (plus `--user-only` to skip prelude items) dump
canonical compiler stages — the differential-testing harness for the
self-host port.

`--freestanding` emits C that does not use libc at all:

```sh
./exilc --target c --freestanding --c-out out.fs.c examples/freestanding_print.exl

# the floor is defined by what the object still needs:
cc -ffreestanding -fno-stack-protector -fno-pic -I runtime -c out.fs.c -o out.o
nm -u out.o        # only __ex_* helpers and the sys_* seam — no libc symbol

# link it against the freestanding runtime and run:
cc -I runtime out.fs.c runtime/freestanding.c runtime/sys_host.c -o out
./out
```

- `#include <stdio.h>` is gone. `print` / `strlen` / `mem_zero` route to
  libc-free `__ex_*` helpers written over the `sys_write` seam, so the
  object's undefined symbols are only those helpers plus `sys_*`. Any
  libc symbol showing up in `nm -u` is a hard failure — that nm-clean
  object *is* the definition of the freestanding floor.
- The same program still compiles the ordinary libc way without the flag
  and prints the same bytes: the flag changes the runtime floor, not the
  semantics. `make verify-freestanding-<name>` checks both — nm-clean and
  identical output.
- This is the floor for bare-metal / kernel output.

[examples/freestanding_print.exl](../examples/freestanding_print.exl)

---

## 4. Syntax in 5 minutes

```rust
fn add(a: int, b: int) -> int {
    return a + b;
}

fn greet(name: str) {            // no `-> X` = void
    println(name);
}

fn main() {
    let sum = add(3, 4);         // type inferred
    println(sum);                  // 7
    greet("hi");
}
```

Rules that come back everywhere:

- `fn`, `let`, `return` — keywords.
- **`let` is immutable by default** — write `let mut x = ...` if you'll
  reassign `x` later, or assign to its fields / elements. Function
  parameters take `mut` the same way: `fn f(mut buf: Buf)`.
- **A block's trailing expression** (no `;`) is the block's value. If
  the function body ends in one, it's the return — no `return` needed.
  Same for `if` and `match` as expressions (see [sec 6](#6-conditionals--if--else) / [sec 12](#12-enums-and-pattern-matching)).
- **Semicolons mandatory** after each statement (except the trailing
  expression above).
- **Braces mandatory** for `if`/`while`/`for`/`fn`.
- **No parens around the condition** in `if`/`while` (Rust-style).
- Comments `// single-line` and `/* block */`; `/// ...` is reserved
  for doc-comments (see [sec 21](#21-attributes)).
- **`print` vs `println`** — `print(x)` writes its argument with no
  trailing newline; `println(x)` appends `\n` (Rust/Go/Java convention).
  Build a line piecewise with `print` and end it with `println`.
- Identifiers: `snake_case` by convention for functions and variables,
  `PascalCase` for types (struct/enum), `SCREAMING_CASE` for consts.

[examples/functions.exl](../examples/functions.exl)

### Pipe operator `|>` — the dot for free functions

`x |> f(a, b)` is exactly `f(x, a, b)` — the piped value becomes the
**first argument** of the call on its right. Pure parser sugar with no
new AST node, chaining left to right so the reader follows data flow
top to bottom:

```rust
fn dbl(x: int) -> int { x + x }
fn inc(x: int) -> int { x + 1 }

fn main() {
    println(5 |> dbl);                      // 10 — bare ident is allowed
    let r = 3
        |> dbl()                            // 6
        |> inc()                            // 7
        |> dbl();                           // 14
    println(r);
}
```

- The right-hand side is a function call (or a bare function name) —
  not an arbitrary expression. `5 |> 1 + 2` is rejected.
- Precedence equals method-call (`.method()`), so binary operators
  outside the chain bind to the chain *result*: `2 + (3 |> dbl())` is
  `2 + 6`.
- Works with module-qualified callees too — `x |> math::clamp(0, 10)`.

[examples/pipe.exl](../examples/pipe.exl)

---

## 5. Primitive types

| type        | description                          |
|-------------|--------------------------------------|
| `i8`/`u8`   | 8-bit signed/unsigned                |
| `i16`/`u16` | 16-bit                               |
| `i32`/`u32` | 32-bit (`int` = `i32` as an alias)   |
| `f32`/`f64` | IEEE 754 single / double precision (see below) |
| `bool`      | `true`/`false`                       |
| `str`       | string literal (immutable, C `char*`) |

```rust
fn main() {
    let a: i8  = -42;
    let b: u16 = 50000;
    let c: i32 = -1000000;

    // implicit widening is lossless (i8 -> i32 OK):
    let widened: i32 = a as i32;

    // narrowing requires an explicit `as` cast:
    let big: i32 = 1000;
    let narrow: i8 = big as i8;
    println(narrow);
}
```

[examples/integers.exl](../examples/integers.exl)

Standard arithmetic (`+ - * / %`), comparisons (`== != < > <= >=`), unary
minus, logical `&&` / `||` (short-circuit, `bool` operands), bitwise /
shift / `~` (see subsection below). `bool` in `if`/`while` conditions.
No increment/decrement operators — write `i = i + 1;` (`++` is reserved
for compile-time string concat, see below).

> FFI note: `int`/`i32`/`u32` map to C `long` (cross-stability across
> SAS/C vs amiga-gcc vs vbcc). For libc, use `c_int` (section 16).

> Reserved: there is no 64-bit or pointer-sized integer yet, but the
> names are already taken. `i64`, `u64`, `i128`, `u128`, `usize` and
> `isize` are rejected as identifiers ("reserved word (future integer
> width)"), so adding those widths later cannot reinterpret source that
> used one as a name. Index and size with `i32`/`u32`. `shared` is
> reserved the same way for the capability model (section 22).

### Hex integer literals

Integer literals also take a hex form — `0x` or `0X` prefix,
case-insensitive digits. Pure source notation; the value is identical
to the decimal form and the emitted C still prints in decimal.

```rust
fn main() {
    let nl:          u8  = 0x0A;          // 10  — '\n'
    let upper_a:     u8  = 0x41;          // 65  — 'A'
    let mask:        u32 = 0xDEAD;        // 57005
    let custom_base: u32 = 0xDFF000;      // 14675968 — Amiga custom chip
    println(custom_base as int);
}
```

Use for masks, ASCII codes, and MMIO addresses — values that read more
naturally in base 16. Often paired with the `int → *T` cast in
[sec 10](#10-pointers-and-memory-stack-vs-heap).

[examples/hex_literals.exl](../examples/hex_literals.exl)

### Floats — `f32` / `f64`

Arithmetic (`+ - * /`) and comparisons on floats are built-in and IEEE —
they emit the raw C operator, so NaN ≠ NaN flows through untouched. A
bare literal (`3.14`) defaults to `f64`; a suffix pins the width
(`1.5f32`), and `6.022e23` is exponent notation.

```rust
fn main() {
    let pi: f64 = 3.14159;
    let two: f64 = 2.0;
    println(pi / two);                                 // 1.57079
    if pi > two { println(1); } else { println(0); }   // 1

    let a: f32 = 1.5f32;
    let b: f32 = 2.25f32;
    println(a + b);                                    // 3.75

    let i: int = 7;
    let f: f64 = i as f64;      // cross-width is always an explicit cast
    println(f as int);          // 7 — `f64 as int` truncates
}
```

- Floats deliberately implement **no** `Eq` / `Ord` / `Hash` traits
  ("operators yes, traits no" — distinct from Rust's `PartialEq` split):
  `HashMap<f64, _>` rejects at instantiation, and `@derive(Eq, Hash)` on
  a struct with a float field rejects too.
- No implicit widening — mixing `int` and `f64` needs `as` on one side.
- `%`, bitwise, and shifts stay integer-only (float `fmod` is deferred
  until the libm binding lands).

[examples/floats.exl](../examples/floats.exl)

### String concatenation — `++`

`++` is a **compile-time** operator: both sides must be compile-time
string constants — a string literal (or chain of them), or a
`type_name(expr)` call ([sec 13](#13-generics)) — and the compiler folds
them into a single constant in the emitted C with zero runtime cost.

```rust
fn main() {
    let greeting: str = "Hello, " ++ "World!";
    let banner:   str = "===" ++ " exile-lang " ++ "===";
    let version:  str = "v" ++ "0" ++ "." ++ "4" ++ "." ++ "0";
    println(greeting);
    println(banner);
    println(version);
}
```

Runtime string building is intentionally a separate facility going
through an explicit `Allocator` — `StringBuilder` / `String`,
[sec 19](#19-prelude-collections) — so any allocation remains a
deliberate user choice, not a hidden side effect of operator syntax.
(Bitwise XOR uses `^` — see the next subsection.)

[examples/string_concat.exl](../examples/string_concat.exl)

### Bitwise, shift and modulo

The standard C set is available, ordered Rust-style — bitwise binds
tighter than comparisons, so `a & b == c` parses as `(a & b) == c`
(the emitted C is fully parenthesised, so source ordering is preserved
regardless of C's own looser precedence).

| operator      | meaning                |
|---------------|------------------------|
| `&` `\|` `^`  | AND, OR, XOR           |
| `~`           | bitwise NOT (prefix)   |
| `<<` `>>`     | left / right shift     |
| `%`           | modulo                 |

```rust
fn main() {
    let read  = 1 << 0;
    let write = 1 << 1;
    let rw    = read | write;        // 3
    let cleared = rw & ~write;       // 1
    println(rw);
    println(cleared);
    println(12 ^ 10);                // 6
    println(30 % 8);                 // 6
}
```

`|` doubles as the separator in `enum` and `match` ([sec 12](#12-enums-and-pattern-matching)) —
the two roles are distinguishable by position, so there's no ambiguity.

[examples/bitwise.exl](../examples/bitwise.exl)

### Logical not — `!`

`!e` negates a `bool`, mapping straight to C's `!`. The operand must be
`bool` and the result is `bool` (it is not bitwise `~`, which is for
integers). It binds postfix-tight, so `!a.flag()` reads as `!(a.flag())`.

```rust
fn main() {
    let t: bool = true;
    if !t { println(1); } else { println(0); }      // 0
    if !(1 == 2) { println(1); } else { println(0); }  // 1
}
```

- `bool`-only — applying `!` to an integer is a type error.
- Const-foldable: `const DISABLED: bool = !true;` folds to a `#define`.

[examples/logical_not.exl](../examples/logical_not.exl)

### Compile-time constants — `const`

`const NAME: T = <expr>;` declares a value folded to a literal at compile
time and emitted as a C `#define`. Zero runtime cost; the `: T` annotation
is required. Initialisers may use the full operator set and reference
earlier consts; cycles and values that overflow the declared type are
compile errors.

```rust
const WIDTH:  int = 320;
const HEIGHT: int = 256;
const PIXELS: int = WIDTH * HEIGHT;       // derived from earlier consts

mod flags {
    pub const READ:  int = 1 << 0;
    pub const WRITE: int = 1 << 1;
    pub const RW:    int = READ | WRITE;
}

fn main() {
    println(PIXELS);                       // 81920
    println(flags::RW);                    // 3
}
```

`size_of(T)` is allowed inside a const initialiser and folds to a C
`sizeof(...)` expression rather than a literal — handy for sizing
buffers without hardcoding integers.

[examples/constants.exl](../examples/constants.exl)

---

## 6. Conditionals — `if` / `else`

`if`/`else` works in both statement and expression position. Braces
mandatory, no parens around the condition. Every value-returning fn
must return on every branch — there's no implicit fall-through to a
default value (see [sec 23](#23-where-to-next)).

```rust
fn classify(n: int) -> int {
    if n < 0 {
        return -1;
    } else if n == 0 {
        return 0;
    } else {
        return 1;
    }
}

fn main() {
    println(classify(-7));            // -1
    println(classify(0));             // 0
    println(classify(42));            // 1
}
```

[examples/if_else.exl](../examples/if_else.exl)

### `if` as an expression

In value position each arm is a single expression — no statements, no
trailing `;` — and `else` is required. The whole `if`-chain yields that
expression. Combined with trailing-expression function bodies, you can
drop `return` entirely:

```rust
fn label(n: int) -> str {
    if n > 0 {
        "positive"
    } else if n == 0 {
        "zero"
    } else {
        "negative"
    }
}

fn main() {
    let parity = if 7 % 2 == 0 { "even" } else { "odd" };
    println(parity);                  // odd
    println(label(-3));               // negative
}
```

- Both branches must produce a single expression of the **same type**.
- A branch that's a multi-statement block is rejected here — keep
  statement-form `if` for that.
- `match` is also an expression and follows the same rule
  (see [sec 12](#12-enums-and-pattern-matching)).

[examples/expressions.exl](../examples/expressions.exl)

---

## 7. Loops — `while` and `for`

Three loop forms — `while` for general conditions, `for` for integer
ranges, and the unconditional `loop`. `break` and `continue` work in
all three (see the subsection below).

```rust
fn countdown(n: int) {
    let mut i = n;
    while i > 0 {
        println(i);
        i = i - 1;
    }
}

fn main() {
    countdown(3);                   // 3, 2, 1
}
```

[examples/while_loop.exl](../examples/while_loop.exl)

### `for v in lo..hi { ... }` — ranges

`for` iterates over a half-open `..` or inclusive `..=` integer range;
the end bound is evaluated once. Reversed ranges (`5..3`) run zero
times — no special case needed.

```rust
fn main() {
    let mut s = 0;
    for i in 0..5 {
        s = s + i;                  // 0+1+2+3+4
    }
    println(s);                     // 10

    let mut t = 0;
    for i in 1..=5 {
        t = t + i;                  // 1+2+3+4+5
    }
    println(t);                     // 15
}
```

A range is itself a first-class value of type `Range<T>` /
`RangeInclusive<T>`: `let r = 0..5; for v in r { ... }` works.

[examples/for_loop.exl](../examples/for_loop.exl)

### `loop`, `break`, `continue`

`loop { ... }` repeats unconditionally — it desugars to `while true`, so
exit it with `break` (or `return`). `break` leaves the nearest enclosing
loop; `continue` skips to that loop's next iteration. Both are
statements, valid only inside a loop.

```rust
fn main() {
    // `loop` + `break`: count up to 3.
    let mut i = 0;
    loop {
        if i >= 3 { break; }
        println(i);                     // 0, 1, 2
        i = i + 1;
    }

    // `continue` in a `for` — skip one value, keep iterating.
    let mut sum = 0;
    for n in 0..6 {
        if n == 2 { continue; }
        sum = sum + n;
    }
    println(sum);                       // 0+1+3+4+5 = 13
}
```

- `break` / `continue` outside any loop is a compile error.
- In a `for` loop, `continue` still runs the range's step (it lives in
  the C `for(; ; step)` clause), so it advances rather than spins.
- `loop` has no condition — `break` or `return` is the only way out.

[examples/loop_break_continue.exl](../examples/loop_break_continue.exl)

### `for x in <iterator>` — iterating any `Iterator`

Besides integer ranges, `for` walks any type that `impl Iterator` — the
prelude trait `trait Iterator { type Item; fn next(*self) -> Option<Self::Item>; }`.
`next` takes `*self` so it can advance the cursor's state; the element
type is read from its `Option<…>` return.

```rust
struct Countdown { n: int }

impl Iterator for Countdown {
    type Item = int;
    fn next(*self) -> Option<int> {
        if self.n <= 0 { return Option::None; }
        let v = self.n;
        self.n = self.n - 1;
        Option::Some(v)
    }
}

fn main() {
    let cd = Countdown { n: 4 };
    for x in cd { println(x); }          // 4 3 2 1
}
```

- Desugars to `loop { match it.next() { Some(x) => body | None => break } }`.
- `break` / `continue` work inside, exactly as in a range `for`.
- Prelude collections expose `iter()` cursors (`Vec::iter`, `HashMap::iter`)
  that `impl Iterator` — see [sec 19](#19-prelude-collections).
- Any iterator also chains through lazy combinators — `.map()`,
  `.filter()`, `.take()`, `.enumerate()`, `fold`, `collect` — covered in
  [sec 19](#19-prelude-collections) too.

[examples/for_iterator.exl](../examples/for_iterator.exl)

---

## 8. Tuples

Tuples are a lightweight way to return multiple values without building
a struct.

```rust
fn min_max(a: int, b: int) -> (int, int) {
    if a < b { return (a, b); }
    return (b, a);
}

fn main() {
    let (lo, hi) = min_max(7, 3);     // destructuring in let
    println(lo);                        // 3
    println(hi);                        // 7

    let (x, y) = (10, 20);            // also from a literal
    println(x);
}
```

[examples/tuples.exl](../examples/tuples.exl)

Under the hood a tuple is emitted as an anonymous struct — full C89
compatibility, no magic.

---

## 9. Structs

```rust
struct Point { x: int, y: int }

struct Rect {
    origin: Point,
    width: int,
    height: int,
}

fn main() {
    let p = Point { x: 3, y: 4 };
    println(p.x);                       // 3

    // Field mutation goes through `.field =` and needs `let mut` on
    // the binding; structs are by value, so assigning `p` to `q` first
    // gives an independent copy.
    let mut q = p;
    q.x = 99;
    println(q.x);                       // 99
    println(p.x);                       // 3 — the original is untouched

    // Functional update: `..base` copies the fields you didn't name.
    let r = Rect { origin: p, width: 8, height: 5 };
    let taller = Rect { height: 99, ..r };
    println(taller.height);             // 99 — explicit override
    println(taller.width);              // 8  — copied from `r`
}
```

[examples/structs.exl](../examples/structs.exl)

Conventions:

- Fields in source order, natural alignment, **no bitfields**.
- Pass/return **by value** (the whole struct is copied).
- `..base` at the end — copies the fields you didn't name explicitly.
- `println(p)` on a struct doesn't work by default — `print`/`println`
  understand scalars only. If you want `println(p) // Point { x: 3, y: 4 }`,
  mark the type with `@debug` (see [sec 21](#21-attributes)).

### Fixed-size arrays — `[T; N]`

A by-value aggregate like a struct — copying on assignment, pass and
return; pass `&a` to avoid the copy. The size `N` is part of the type
and is a compile-time integer (a literal or a `const`). Indexing `a[i]`
is unchecked; `len(a)` folds to `N` at compile time.

```rust
const ROWS: int = 4;

fn sum(a: *[int; ROWS]) -> int {
    let mut total = 0;
    for i in 0..ROWS {
        total = total + (*a)[i];
    }
    total
}

fn main() {
    let mut data: [int; ROWS] = [10, 20, 30, 40];
    data[1] = 99;
    println(data[1]);                 // 99
    println(len(data));               // 4
    println(sum(&data));              // 10 + 99 + 30 + 40 = 179

    let zeros: [int; 3] = [0; 3];     // repeat-literal — N copies of v
    println(zeros[2]);                // 0
}
```

- Literals: `[a, b, c]` lists elements; `[v; N]` fills `N` copies of `v`.
- Element assignment (`data[i] = ...`) requires `let mut` on the
  binding, like field assignment on a struct.
- Arrays as a by-value field of another aggregate are not yet supported
  — store the array behind a pointer for now.

[examples/arrays.exl](../examples/arrays.exl)

---

## 10. Pointers and memory (stack vs heap)

Pointers are C-style — honest and simple.

```rust
struct Point { x: int, y: int }

fn shift(p: *Point, dx: int, dy: int) {
    p.x = p.x + dx;                 // auto-deref on .field
    p.y = p.y + dy;
}

fn swap(a: *int, b: *int) {
    let tmp = *a;
    *a = *b;
    *b = tmp;
}

fn main() {
    let p = Point { x: 1, y: 2 };
    shift(&p, 10, 20);              // & = address-of
    println(p.x);                     // 11
    println(p.y);                     // 22

    let x = 1;
    let y = 2;
    swap(&x, &y);
    println(x);                       // 2
    println(y);                       // 1
}
```

[examples/pointers.exl](../examples/pointers.exl)

**Heap** — `new(a) T { ... }` allocates through an explicit `Allocator`
([sec 18](#18-allocator--pluggable-memory)) and yields an **owned
pointer** `own *T`. When the compiler can see the `new(a)` site it also
knows the provenance allocator, so the value is **auto-dropped** at
scope exit — no manual `free`, no GC. (`default_allocator()` is a
prelude builtin returning the host's libc-backed allocator.)

```rust
struct Point { x: int, y: int }

fn make_point(a: Allocator, x: int, y: int) -> own *Point {
    return new(a) Point { x: x, y: y };
}

fn main() {
    let a = default_allocator();

    let p = new(a) Point { x: 1, y: 2 };   // auto-dropped at scope exit
    p.x = 10;
    println(p.x);                            // 10

    // From a call the provenance is unknown — consume explicitly;
    // leaving `q` unconsumed is a compile error, not a silent leak.
    let q = make_point(a, 100, 200);
    defer free(a, q);
    println(q.x);                            // 100
}
```

`free(a, p)` releases through the allocator seam, symmetric with
`new(a)`. The full ownership rules are in the next subsection.

[examples/heap.exl](../examples/heap.exl)

### `own *T` — owned pointers and auto-drop

`own *T` is the third pointer type, next to `*T` (mutable borrow) and
`*const T` (read-only borrow): it marks **unique ownership** of the
pointee. An `own *T` lends itself out as either kind of borrow
implicitly; the reverse never holds — a plain borrow can't be promoted
to ownership.

```rust
struct Buffer { p: own *int, alloc: Allocator }

fn alloc_buffer(a: Allocator, value: int) -> Buffer {
    let q: own *int = a.alloc();
    let buf = Buffer { p: q, alloc: a };
    *buf.p = value;
    buf
}

fn use_buffer() -> int {
    let a = default_allocator();
    // `buf` carries an `own *int`: the compiler synthesises
    // `Buffer__drop` and injects the call at scope exit.
    let mut buf = alloc_buffer(a, 42);
    *buf.p                              // 42
}
```

The lifecycle is checked at compile time: an owner is born (`new(a)`,
`a.alloc()`), lives, and must then **transfer** (move into a call or
field), **return**, or **die** (auto-drop, or an explicit `free(a, p)` /
`.drop()`) — it never silently leaks and never frees twice.

- Reassigning a live owner drops the old value first; rebinding a
  consumed one is legal again (`s = next(s)` loops, tree building).
- An owner returned **from a call** has unknown provenance, so it can't
  be auto-dropped — consume it explicitly (`defer free(a, q)`); leaving
  it unconsumed is a compile error.
- A struct with `own` fields (plus a sibling `alloc: Allocator` so the
  drop knows how to release) gets a synthesised drop; an explicit
  `.drop()` or `.free()` elides the auto-insert — no double-free.
- Copying a struct that carries `own` fields by value is rejected —
  move it or borrow it.

The net effect: manual `free` is the rare escape hatch — the good free
is the one you don't write.

[examples/own_ptr.exl](../examples/own_ptr.exl)

### Write through a raw pointer index — `p[i] = v`

A `*T` can be written by index: `p[i] = v` lowers to C `p[i] = v`. This
is the write-side counterpart to building up a buffer element by element
(the prelude's `StringBuilder` / `Vec` use it internally).

```rust
fn fill(p: *int, n: int) {
    let mut i = 0;
    while i < n {
        p[i] = i * i;                   // write-through-pointer index
        i = i + 1;
    }
}
```

- Write-only: a bare `*T` **read** by index (`let x = p[i];`) is rejected
  — reads go through an array or a `Slice<T>` ([see below](#slicet--a-bounded-view)),
  which carry a length.
- No bounds check — `i` must be in range, exactly like C.

### Cast from integer address — MMIO

`int as *T` pins a typed pointer to a known hardware address. The
canonical pairing: a hex literal for the base of a register block
(`0xDFF000` for the Amiga Custom chip) plus a struct describing the
layout, so call-sites can write `cust.bltddat` instead of raw offsets.

```rust
struct CustomChip {
    bltddat: u16,
    dmaconr: u16,
    vposr:   u16,
    vhposr:  u16,
}

fn main() {
    let cust: *CustomChip = 0xDFF000 as *CustomChip;
    if cust == null { println(1); } else { println(0); }   // 0
}
```

- Only `int → *T` is allowed; the reverse (`*T → int`) is not.
- No volatility, alignment, or bounds guarantees yet — `volatile` /
  `rune` for MMIO safety land in a later milestone.

[examples/int_to_ptr_cast.exl](../examples/int_to_ptr_cast.exl)

### `*const T` — pointee immutability

`*T` and `*const T` point at the same `T`; the difference is write
capability. Through a `*const T` you may read (`*p`, `p.field`) but not
write — it maps to C `const T *`. This is orthogonal to the binding's
own `mut`: `let mut p` controls rebinding the pointer, `*const` controls
writing through it.

```rust
struct Point { x: int, y: int }

fn dist_sq(p: *const Point) -> int {     // "I won't write through p"
    p.x * p.x + p.y * p.y
}

fn translate(p: *Point, dx: int, dy: int) {
    p.x = p.x + dx;                      // writes need the plain *T form
    p.y = p.y + dy;
}

fn main() {
    let mut q = Point { x: 3, y: 4 };
    let p: *Point = &q;
    println(dist_sq(p));                 // 25 — *T coerces to *const T
    translate(p, 1, 1);
    println(dist_sq(p));                 // 41
}
```

- `*T → *const T` is implicit (you can always drop write capability) at
  `let`-init and function-argument sites.
- The reverse, `*const T → *T`, needs an explicit `as` cast — a
  deliberate escape hatch, like a C `(T*)` on a `const T*`.
- Writing through a `*const T` (`*p = v` or `p.field = v`) is a compile
  error.

[examples/const_pointers.exl](../examples/const_pointers.exl)

### `Slice<T>` — a bounded view

A `Slice<T>` is a prelude struct — a `*const T` pointer plus a length:
`Slice<T> { ptr: *const T, len: u32 }`. It's the lightweight way to pass
"a pointer and how many" as one value, by value (a two-word copy).

```rust
fn sum(s: Slice<int>) -> int {
    let mut total = 0;
    let mut i: u32 = 0;
    while i < s.len {
        total = total + s[i];            // s[i] lowers to s.ptr[i]
        i = i + 1;
    }
    total
}

fn main() {
    let arr: [int; 5] = [3, 1, 4, 1, 5];
    let s: Slice<int> = Slice { ptr: &arr[0], len: 5 };
    println(sum(s));                     // 14
    println(s.len as int);               // 5
}
```

- Indexing `s[i]` lowers to `s.ptr[i]` — **no bounds check** in the
  current MVP.
- `.ptr` / `.len` are ordinary fields; `len` is `u32` (cast `as int` to
  print it). `s.length()` works too — the same method spelling as
  Vec/String/HashMap.
- Build one explicitly — `Slice { ptr: &arr[0], len: N }`; the
  `*T → *const T` coercion applies at the `ptr:` field — or carve one
  out of an array or another slice with `a[lo..hi]` (next subsection).
  A mutable variant (`SliceMut<T>`) is deferred.

[examples/slice.exl](../examples/slice.exl)

### Sub-slicing — `a[lo..hi]`

When the index expression is a `Range`, `[]` produces a fresh `Slice<T>`
view instead of an element copy: `a[lo..hi]` is
`Slice { ptr: &a[lo], len: (hi - lo) as u32 }`; the inclusive form
`a[lo..=hi]` adds one. Works on arrays and on existing slices.

```rust
fn main() {
    let arr: [int; 6] = [10, 20, 30, 40, 50, 60];

    let mid = arr[1..4];               // view of [20, 30, 40]
    println(mid.len as int);           // 3

    let s1 = arr[0..6];
    let s2 = s1[2..5];                 // sub-slicing a slice composes
    println(s2.len as int);            // 3

    let r = 2..4;                      // a Range is a value — build it
    let tail = arr[r];                 // first, index with it later
    println(tail.len as int);          // 2
}
```

- No bounds check, consistent with the rest of `[i]`.
- The views are read-only `Slice<T>` — mutable sub-slicing is deferred.
- The motivating use is cheap token spans: `src[start..end]` returns a
  `Slice<u8>` with no copy.

[examples/sub_slicing.exl](../examples/sub_slicing.exl)

### Scoped projection — `with x in place { ... }`

`with` binds a borrow to an lvalue for exactly one block — in-place
reads and writes with no way to leak the pointer:

```rust
fn main() {
    let mut arr: [int; 5] = [10, 20, 30, 40, 50];

    with x in arr[2] { *x = 999; }      // x: *int — borrow of the cell
    println(arr[2]);                    // 999

    with x in arr[0] {                  // read + write in one block
        let old = *x;
        *x = old * 2;
        println(old);                   // 10
    }
    println(arr[0]);                    // 20
}
```

- `x` is a `*T` to the lvalue; reached through a `Slice<T>` (whose
  `.ptr` is `*const T`) it becomes `*const T` automatically — writes
  through it are rejected.
- The target must be an lvalue (local, field, index, deref) — a call's
  return value is a fresh rvalue and is rejected (its address would
  dangle).
- `x` is out of scope after the `}` — the dangle is unutterable, no
  escape analysis needed. `with` does not consume the target.

[examples/scoped_projection.exl](../examples/scoped_projection.exl)

---

## 11. `defer` — resource cleanup

`defer` registers an action that fires on exit from the current function
— at the end of the body, on a `return`, or when `try` does an
early-return on an error.  Multiple `defer`s in the same scope fire
**LIFO** (last registered, first to run).

```rust
fn lifo() {
    defer println("third");
    defer println("second");
    defer println("first");
    println("body");
}

fn early_exit(n: int) -> int {
    defer println("outer cleanup");
    if n > 0 {
        defer println("inner cleanup");
        return n * 2;       // both cleanups fire here, inner first
    }
    return 0;
}

fn main() {
    lifo();                 // body, first, second, third
    println("---");
    println(early_exit(5));   // inner cleanup, outer cleanup, 10
}
```

[examples/defer.exl](../examples/defer.exl)

A bare `return;` (no value) early-exits a `void` function, flushing
pending `defer`s on the way out:

```rust
fn process(skip: bool) {
    defer cleanup();
    if skip { return; }     // cleanup() still runs
    work();
}
```

In `main` a bare `return;` means exit code 0; `return <int>;` sets a
specific code. A value-returning `fn` must always `return <expr>;`.

**Block variant** — bundle several cleanup statements:

```rust
defer { println("a"); println("b"); println("c"); }
```

Owners allocated locally with `new(a)` are auto-dropped
([sec 10](#10-pointers-and-memory-stack-vs-heap)), so the classic
defer-free pairing remains for owners the compiler can't trace — e.g.
one returned from a call — and for non-memory resources:

```rust
let q = make_point(a, 100, 200);   // own *Point of unknown provenance
defer free(a, q);                  // can't forget; fires on every exit path
```

---

## 12. Enums and pattern matching

The most enjoyable part of the language — full ADTs with exhaustive
match. An enum is a set of named variants; the simplest form is
all-unit:

```rust
enum Direction {
    North
    | South
    | East
    | West
}
```

`|` is the **separator** between variants — the first variant has no
leading `|`. The same shape applies to `match` arms below.

Variants can also carry data — three payload forms can coexist in one
enum. `match` is exhaustive (the compiler enforces coverage) and is an
**expression**, so it can be the body of a `return`:

```rust
enum Shape {
    Square                                  // unit
    | Circle(int)                           // tuple — positional
    | Rect(int, int)
    | Triangle { base: int, height: int }   // struct — named
}

fn area(s: Shape) -> int {
    return match s {
        Shape::Square => 1
        | Shape::Circle(r) => r * r * 3
        | Shape::Rect(w, h) => w * h
        | Shape::Triangle { base, height } => base * height / 2
    };
}

fn main() {
    println(area(Shape::Square));                          // 1
    println(area(Shape::Circle(3)));                       // 27
    println(area(Shape::Rect(4, 5)));                      // 20
    println(area(Shape::Triangle { base: 6, height: 4 })); // 12
}
```

Patterns:

- positional: `Shape::Circle(r)` — `r` is `int`
- named: `Shape::Triangle { base: b, height: h }`
- shorthand: `Shape::Triangle { base, height }` — same as `Shape::Triangle { base: base, height: height }`
- wildcard: `_` matches anything, no binding

Patterns **nest**: a field that is itself an enum can be matched in the
same arm, to any depth.

```rust
enum Tree {
    Leaf(int)
    | Node(Shape)
}

fn kind(t: Tree) -> int {
    return match t {
        Tree::Leaf(n)                  => n
        | Tree::Node(Shape::Circle(r))   => r
        | Tree::Node(Shape::Square)      => 0
        | Tree::Node(Shape::Rect(w, h))  => w + h
        | Tree::Node(Shape::Triangle { base, height }) => base + height
    };
}
```

Exhaustiveness is checked **through** the nesting — the compiler proves
every `(outer, inner)` combination is covered, so the example above
needs no `_`. Drop any inner arm and it names the exact missing case,
e.g. `non-exhaustive 'match': pattern 'Node(Square)' is not covered`.
A redundant arm (one an earlier arm already covers) is rejected too.

### Or-patterns

Several variants can share one arm body — list them separated by `|`
inside the arm head:

```rust
enum Token { Plus | Minus | Star | LParen | RParen | Ident }

fn category(t: Token) -> int {
    match t {
        Token::Plus | Token::Minus | Token::Star => 1
        | Token::LParen | Token::RParen          => 2
        | Token::Ident                           => 3
    }
}
```

Two roles for `|` in one match — position disambiguates: `|` between
patterns *before* `=>` is or-pattern; `|` *after* an arm body is the
arm separator.

MVP scope:

- Alternatives are unit variants, wildcards, or plain variable
  patterns — **no binds inside an alt**. To bind a payload, use
  separate arms.
- Top-level only — `|` nested inside a variant payload (e.g.
  `Foo(Bar | Baz)`) is rejected.

[examples/or_patterns.exl](../examples/or_patterns.exl)

### Pattern guards

A pattern can carry a boolean predicate that runs *after* the pattern
matches — `pat if cond => body`. The pattern's binds are in scope for
the guard:

```rust
enum Number { Whole(int) | Zero }

fn describe(n: Number) -> int {
    match n {
        Number::Whole(v) if v > 0 => 1
        | Number::Whole(v) if v < 0 => 2
        | Number::Whole(_)          => 3       // unguarded fallback
        | Number::Zero              => 0
    }
}
```

- A guarded arm **does not contribute to exhaustiveness** — the
  compiler can't prove `v > 0` covers every `int`. Pair every guarded
  pattern shape with an unguarded fallback (commonly `_`).
- Guards force the decision-chain codegen path; a flat-switch match
  upgrades transparently when a guard appears.

[examples/pattern_guards.exl](../examples/pattern_guards.exl)

### Multi-statement arm bodies

An arm body can be a brace-block of multiple statements — useful for a
few `let`s before the result, or for a side-effect arm in a
statement-position `match`:

```rust
fn area_v2(s: Shape) -> int {
    match s {
        Shape::Square => 1                  // single-expr arm still works
        | Shape::Circle(r) => {
            let r2 = r * r;
            (r2 * 314) / 100                // integer pi ≈ 3.14
        }
        | Shape::Rect(w, h) => w * h
        | Shape::Triangle { base, height } => {
            let a = base * height;
            a / 2
        }
    }
}
```

- **Value position** (the body of a value-returning fn, or
  `let x = match …`): the block needs a trailing expression with no
  `;` — its value is the arm's value.
- **Statement position**: no trailing expression needed — the block
  runs purely for side effects (e.g. several `println`s).
- `let`-bindings inside an arm block share the enclosing function's
  decl namespace; the same name reused in another arm is rejected (the
  exile-wide no-shadowing rule).

[examples/multi_stmt_match.exl](../examples/multi_stmt_match.exl)

[examples/enums.exl](../examples/enums.exl)

### Literal patterns and char literals

`match` also works directly on integer values — and a char literal
`'a'` is plain int-literal sugar for its byte value (exile strings are
bytes), so it works in `==`, arithmetic, and `let` initialisers too.
This is the lexer-shaped workload: classify a byte in one dispatch,
compiled to a C `switch` (dense `case` labels become a jump table on
m68k).

```rust
enum Kind {
    Space
    | Newline
    | Other(u8)
}

fn classify(b: u8) -> Kind {
    match b {
        ' ' | '\t' => Kind::Space
        | '\n' | '\r' => Kind::Newline
        | _ => Kind::Other(b)
    }
}
```

- The scrutinee must be an integer type (enum matches are unchanged);
  or-patterns compose into stacked `case` labels.
- The integer domain can't be enumerated, so the match must end with a
  catch-all arm (`_` or a binding).
- Duplicate literals, and literals that don't fit the scrutinee type
  (`300` against a `u8`), are compile errors.

[examples/literal_match.exl](../examples/literal_match.exl)

`true` and `false` are literal patterns too:

```rust
fn parity_word(n: int) -> str {
    return match n % 2 == 0 {
        true => "even"
        | false => "odd"
    };
}
```

- Unlike the integer case, `bool` has a finite two-value domain, so a
  `true` / `false` pair is exhaustive on its own — **no catch-all `_`**,
  and adding one is flagged unreachable.
- Dropping either arm is a compile error, the same way a missing enum
  variant is.
- A bool match can still carry a guard, but a guarded arm doesn't prove
  coverage: the unguarded arm is what closes the match.

[examples/bool_match.exl](../examples/bool_match.exl)

### Active patterns — `view`

A `view` names a total classification function and lets `match` arms use
its cases as if they were patterns over the input:

```rust
view Sign(n: int) -> Negative | Zero | Positive {
    if n < 0 { return Sign::Negative; }
    if n == 0 { return Sign::Zero; }
    return Sign::Positive;
}

fn classify(n: int) {
    match n {                                // scrutinee is an int...
        Sign::Negative => { println(-1); }   // ...arms name Sign cases
        | Sign::Zero => { println(0); }
        | Sign::Positive => { println(1); }
    }
}
```

- Expands at compile time to a nominal `enum Sign { ... }` plus an
  ordinary function `Sign(n: int) -> Sign`; the match-site rewrite wraps
  the scrutinee in a `Sign(n)` call. Exhaustiveness over the cases comes
  for free — the choice-enum is closed.
- Cases may carry a tuple payload (`Large(int)`) and destructure as
  usual; the explicit ctor form `let s = Sign(99); match s { ... }`
  works too.
- v1 limits: total views only (every input must classify), a single
  scrutinee parameter, one view per match.

[examples/active_patterns.exl](../examples/active_patterns.exl)

### Heap-boxed recursive enums — `new(a) Expr::Lit(3)`

`new(a) Enum::Variant(args)` heap-allocates an enum value and yields an
`own *Enum` — the enum mirror of `new(a) Struct { ... }`
([sec 10](#10-pointers-and-memory-stack-vs-heap)). With `own` payloads
this is the AST-building shape: a recursive enum whose constructors own
their subtrees.

```rust
enum Expr {
    Lit(int)
    | Add(own *Expr, own *Expr)
}

fn eval(e: *const Expr) -> int {     // reading only — borrow the tree
    match *e {
        Expr::Lit(n) => n
        | Expr::Add(a, b) => eval(a) + eval(b)
    }
}

fn main() {
    let a = default_allocator();
    let lhs = new(a) Expr::Lit(40);
    let rhs = new(a) Expr::Lit(2);
    let root = new(a) Expr::Add(lhs, rhs);
    println(eval(root));            // 42
}                                   // root's tree is released here
```

- An `own *Expr` argument lends itself to a `*const Expr` parameter
  without being consumed, so the caller keeps ownership while reading.
- **There is no `free_tree` to write.** `lhs` / `rhs` move into the
  `Add` node, `root` owns the whole tree, and the scope-exit auto-drop
  releases every node exactly once — depth-first, freeing each payload
  before its parent. The compiler synthesises that walk from the type.
- The shape of the walk follows the shape of the type. A **linear**
  spine — every variant owning at most ONE next node of the same enum,
  i.e. a list — is torn down with a loop: constant stack however long
  it grows ([`owned_list.exl`](../examples/owned_list.exl) drops 50 001
  nodes without touching the stack). A genuine **tree** (a variant with
  two owned children, like `Add` above) keeps the recursive form, so
  its depth lives on the C stack — an honest limit, and the reason a
  deep tree still wants an `Arena`
  ([sec 18](#18-allocator--pluggable-memory)).
- Struct-variant boxing (`new(a) Foo::Bar { f: 1 }`) is deferred; v1
  covers the tuple-variant case the AST port needs.

[examples/enum_heap_box.exl](../examples/enum_heap_box.exl)

---

## 13. Generics

Type parameters on structs, enums, and functions. Monomorphisation is
**eager** — every distinct set of type arguments generates its own code
path on the C side (Rust-style, no vtable).

```rust
struct Pair<A, B> {
    fst: A,
    snd: B,
}

fn id<T>(x: T) -> T {
    return x;
}

fn main() {
    let p = Pair { fst: 5, snd: "hi" };   // A=int, B=str inferred from payload
    println(p.fst);

    println(id(42));                        // T=int
    if id(true) { println(1); }             // T=bool — separate instance
}
```

**No turbofish** — inference is bottom-up (from arguments) and
bidirectional (from return type or `let` annotation):

```rust
fn make_null<T>() -> *T {
    return null;
}

fn main() {
    let p: *int = make_null();            // T pinned by the annotation
    if p == null { println(1); }            // 1
}
```

[examples/generics.exl](../examples/generics.exl) | [examples/generic_methods.exl](../examples/generic_methods.exl)

Generic structs nest and may recurse through a pointer — both
`struct Wrapper<T> { inner: Box<T> }` and `struct Node<T> { next: *Node<T> }`
work. A generic struct can also be a function parameter
(`fn f<T>(p: Pair<T, int>)`); `T` is recovered from the argument. The
`..base` functional update ([sec 9](#9-structs)) applies to a generic
instance too — the base pins the concrete instance, and the copied-over
fields keep its monomorphized types. Methods on generic structs are
covered in [sec 14](#14-methods).

### Type aliases - `type Name<T> = ...`

A `type` declaration gives an existing type a second name. It is pure
compile-time substitution - no new type, no conversion, and no trace in
the emitted C.

```rust
struct Item { id: int }

type Word = u32;                    // plain alias
type Id<T> = T;                     // one parameter, used as itself
type Twice<T> = Id<T>;              // an alias whose body is another alias
pub type Items = Vec<Item>;         // `pub` exports it from a module

fn takes(w: Word) -> u32 { return w; }
fn pick(x: Twice<int>) -> int { return x; }

fn main() { println((takes(7 as u32) as int) + pick(35)); }   // 42
```

- An alias is interchangeable with its target everywhere: `Word` *is*
  `u32`, so there is no cast, no wrapper, and no distinct type. For a
  type the checker keeps apart from its representation, declare a
  struct instead.
- Parameter count is checked. Passing arguments to a plain alias, or the
  wrong number to a generic one, is an error: `type alias 'Id' expects
  1 generic argument(s), got 2`.
- Cycles are rejected instead of looped on, whether direct
  (`type A = B; type B = A;`) or through another type (`type A = Vec<A>;`):
  `type alias cycle through 'A' — alias resolution would loop`.

Not to be confused with an associated type inside a `trait` or `impl`,
which uses the same keyword for a different job ([sec 15](#15-traits)).

### Option, Result, and error handling

`Option<T>` and `Result<T, E>` live in the prelude — no `use`, no `enum`
declaration.

```rust
// `?T` is an alias for `Option<T>`
fn first_or(d: int, opt: ?int) -> int {
    return opt orelse d;                  // None -> default, Some(x) -> x
}

fn incr(o: ?int) -> ?int {
    let v = try o;                        // None -> early-return None
    return Option::Some(v + 1);
}

fn main() {
    let some: ?int = Option::Some(7);
    let none: ?int = Option::None;

    println(first_or(99, some));            // 7
    println(first_or(99, none));            // 99

    let bumped = incr(some);
    println(bumped orelse 0);               // 8
    let nothing = incr(none);
    println(nothing orelse 0);              // 0 — try short-circuited
}
```

- `try expr` — unwraps `Some`/`Ok`; on `None`/`Err` it returns from the
  enclosing function (the return type must match).
- `orelse fallback` — alternative: `Some(x) -> x`, `None -> fallback`.
- `?T` is pure sugar over `Option<T>`.

### `type_name(expr)` — compile-time type introspection

Yields a `str` with the Rust-style name of the expression's
compile-time type. The expression is **never evaluated** — only its
`.ty` is consulted — and the result is baked into the emitted C as a
`.rodata` string literal. Zero runtime cost, no RTTI metadata in the
binary.

```rust
fn dbg<T>(label: str, x: T) -> T {
    println(label);
    println(type_name(x));
    return x;
}

fn main() {
    let a: i8 = 5;
    println(type_name(a));                    // i8
    println(type_name(42));                   // i32  (literal default)

    let r: Result<int, str> = Result::Ok(7);
    println(type_name(r));                    // Result<i32, str>

    // Generic fn: each instance bakes its own concrete name.
    let _v: int = dbg("v:", 42);            // v: \n i32
    let _s: str = dbg("v:", "text");        // v: \n str
}
```

Inside a generic fn, every monomorphic instance carries its own name
— `id(7)` prints `i32`, `id("hi")` prints `str`. `null` has no type
and is rejected with a typecheck error; everything else (primitives,
structs, enums, pointers, generic instances) works.

Because the result is a compile-time constant, it folds into a `++`
chain ([sec 5](#5-primitive-types)): `type_name(x) ++ "\n"` is a single
`.rodata` literal — handy for labels.

[examples/type_name.exl](../examples/type_name.exl)

---

## 14. Methods

An `impl` block forms a module around the type, so its methods are
private by default — `pub` makes one callable from outside the block.
(This is the first time `pub` shows up; full visibility rules are in
[sec 16](#16-modules).)

```rust
struct Point { x: int, y: int }

impl Point {
    // no self = static method, called as `Point::origin()`
    pub fn origin() -> Point {
        return Point { x: 0, y: 0 };
    }

    // by-value method — the receiver is a copy
    pub fn area(self: Point) -> int {
        return self.x * self.y;
    }

    // by-pointer method — mutates the caller's struct
    pub fn shift(self: *Point, dx: int, dy: int) {
        self.x = self.x + dx;
        self.y = self.y + dy;
    }
}

fn main() {
    let p = Point::origin();
    p.shift(3, 4);                  // auto-ref: compiler inserts `&p`
    println(p.area());                // 12

    let q: *Point = &p;
    println(q.area());                // auto-deref: works through a pointer too

    println(Point::area(p));          // UFCS — equivalent to `p.area()`
}
```

[examples/methods.exl](../examples/methods.exl)

Rules:

- `self` / `*self` — bare, no annotation; the compiler fills in the
  type. The explicit `self: Type` / `self: *Type` still works. Either
  way, no Rust-`&self` magic.
- Dot-call (`p.method()`) does auto-ref / auto-deref so it matches
  `self`'s shape.
- UFCS (`Type::method(p, args)`) always works.

### Methods on generic structs

Type parameters go after `impl`; the target carries them. Each method
is monomorphized per concrete receiver at the call site, and only the
methods actually called are emitted.

```rust
struct Pair<A, B> { fst: A, snd: B }

impl<A, B> Pair<A, B> {
    pub fn first(self) -> A { return self.fst; }
    pub fn second(self) -> B { return self.snd; }

    // the return type may permute the parameters
    pub fn swap(self) -> Pair<B, A> {
        return Pair { fst: self.snd, snd: self.fst };
    }
}

fn main() {
    let p = Pair { fst: 42, snd: true };     // Pair<int, bool>
    println(p.first());                       // 42
    let q = p.swap();                         // Pair<bool, int>
    println(q.second());                      // 42  (was fst)
}
```

- `impl<T> Foo<T>` — type parameters declared after `impl`; works with a
  pointer receiver (`*self`) too, which mutates in place.
- `self` / `*self` are bare — no annotation, the compiler infers the
  target type.

[examples/generic_impl.exl](../examples/generic_impl.exl)

---

## 15. Traits

A `trait` declares method signatures; `impl Trait for Type` supplies the
bodies. Dispatch is **static and monomorphic** — trait methods lower to
ordinary `Type__method` functions, exactly like the inherent methods of
[sec 14](#14-methods). No vtables, no `dyn`.

```rust
trait Shape {
    fn area(self) -> int;
    fn perimeter(self) -> int;
}

struct Rect { w: int, h: int }
struct Square { side: int }

impl Shape for Rect {
    fn area(self) -> int { self.w * self.h }
    fn perimeter(self) -> int { (self.w + self.h) * 2 }
}

impl Shape for Square {
    fn area(self) -> int { self.side * self.side }
    fn perimeter(self) -> int { self.side * 4 }
}

fn main() {
    let r = Rect { w: 3, h: 5 };
    println(r.area());                  // 15
    let sq = Square { side: 6 };
    println(sq.perimeter());            // 24
}
```

Conformance is checked: every method the trait requires must be present
with a matching signature, and an `impl` may not add methods the trait
didn't declare.

### Generic bounds — `<T: Trait>`

A type parameter can require a trait; inside the function the bound's
methods are callable on that type. Each instantiation monomorphizes to a
direct call of the concrete type's method, and a type that doesn't `impl`
the bound is rejected at the call site.

```rust
fn total_area<T: Shape>(a: T, b: T) -> int {
    a.area() + b.area()
}

fn main() {
    let r  = Rect { w: 3, h: 5 };
    let r2 = Rect { w: 2, h: 2 };
    println(total_area(r, r2));         // 15 + 4 = 19
}
```

Several bounds combine with `+`: `fn f<T: Eq + Hash>(x: T)` requires `T`
to implement both traits.

### Default methods

A trait method written with a `{ ... }` body is a **default** — an
`impl` may omit it to use the default, or supply its own to override.
The default is synthesised per implementing type and may call the type's
other trait methods.

```rust
trait Shape {
    fn area(self) -> int;
    fn perimeter(self) -> int;
    fn bbox(self) -> int { self.area() + self.perimeter() }   // default
}
```

Neither `impl Shape` above defines `bbox`, so both inherit the default:
`r.bbox()` is `15 + 16 = 31`.

### Supertraits

A trait can require another. `trait Hash: Eq { ... }` means any type with
`impl Hash for T` must also have `impl Eq for T` — the requirement is
checked across both `impl` blocks regardless of the order they appear.

[examples/traits.exl](../examples/traits.exl)

### Associated types

A trait can declare an output type with `type Item;`; each `impl` binds
it (`type Item = int;`), and signatures refer to it through the
projection `Self::Item`. Associated types are erased before codegen —
the C only ever sees concrete types.

```rust
trait Iterator {
    type Item;
    fn next(*self) -> Option<Self::Item>;
}
```

The projection also resolves inside generic functions bounded by the
trait: `fn first<I: Iterator>(it: *I) -> Option<I::Item>` returns
`Option<int>` for an iterator whose `Item = int` and `Option<str>` for
one whose `Item = str` — monomorphization picks the binding per instance.

[examples/associated_types.exl](../examples/associated_types.exl),
[examples/assoc_projection.exl](../examples/assoc_projection.exl)

### Closures — `|x: int| -> int body` and the `Fn` traits

A lambda literal is `|params| -> R body`, the body a single expression.
A captureless lambda lifts to an ordinary top-level fn and decays to a
C function pointer ([sec 17](#17-ffi--interop-with-c)). A lambda that
references enclosing locals becomes a **closure**: the compiler
synthesises a hidden env struct (captures copied in by value) plus an
`impl Fn1` — no heap, monomorphised and inlined like any other generic.

```rust
fn run_unary<F: |int|->int>(f: F, x: int) -> int { f(x) }

fn main() {
    let base = 10;
    let bump = |x: int| -> int x + base;    // captures `base` by value
    println(run_unary(bump, 5));            // 15
}
```

- `Fn1` / `Fn2` are prelude traits whose argument and result types are
  associated types (`Arg`, `Output`); `f(x)` on a Fn-bound value
  desugars to `f.call(x)`.
- The bound sugar `<F: |int|->int>` is `Fn1` with `Arg = int`,
  `Output = int` pinned right at the bound — the bound's shape mirrors
  the closure literal.
- The explicit form — a named struct with
  `impl Fn1 for T { type Arg = ...; type Output = ...; fn call(*const self, ...) }`
  — works anywhere a closure does; closure literals are sugar over it.
- A capture list `[&name] |x| ...` switches that capture to
  by-reference (a `*const T` env field) — the copy-vs-pointer cost
  stays visible at the construction site.
- The escape pass ([sec 21](#21-attributes)) keeps closures that borrow
  locals from escaping the frame.

[examples/lambdas.exl](../examples/lambdas.exl),
[examples/closures_a2.exl](../examples/closures_a2.exl),
[examples/closure_byref.exl](../examples/closure_byref.exl),
[examples/fn_trait.exl](../examples/fn_trait.exl)

### `Display` and `Debug` — the writer pattern

The prelude ships two formatting traits. Both render through a
**writer**: `fn fmt(*self, out: *StringBuilder)` borrows `self` and
appends into a `StringBuilder` ([sec 19](#19-prelude-collections)) the
caller owns. Threading the same `out` through nested `fmt` calls composes
with no intermediate allocation.

```rust
struct Point { x: int, y: int }

impl Display for Point {
    fn fmt(*self, out: *StringBuilder) {
        out.push_str("Point { x: ");
        out.push_int(self.x);
        out.push_str(", y: ");
        out.push_int(self.y);
        out.push_byte(125 as u8);             // '}'
    }
}
```

- `Display` is hand-written — you choose the output. `Debug` is the
  derivable sibling: `@derive(Debug)` ([sec 21](#21-attributes)) synthesises
  a Rust `{:?}`-style `fmt`. `@derive(Display)` is rejected on purpose
  (output is user-controlled).
- A nested `fmt` just calls `self.field.fmt(out)` with the same builder.

[examples/display.exl](../examples/display.exl),
[examples/derive_debug.exl](../examples/derive_debug.exl)

---

## 16. Modules

Two equally good ways:

**Inline in one file:**

```rust
mod math {
    pub fn double(n: int) -> int { return n * 2; }
    fn bump(n: int) -> int { return n + 1; }      // no pub = private
    pub fn double_then_bump(n: int) -> int {
        return bump(double(n));
    }
}

pub use math::double;                              // re-export

fn main() {
    println(math::double(21));
    println(double(15));                             // short name via `pub use`
}
```

[examples/modules.exl](../examples/modules.exl)

**Per file:**

```
my_project/
├── main.exl
└── lib.exl
```

```rust
// lib.exl
pub fn greet(name: str) { println(name); }
fn private() { println("invisible"); }

// main.exl
use lib::*;                                        // wildcard import

fn main() { greet("hello"); }
```

[examples/multi_file/](../examples/multi_file/)

Visibility rules:

- `pub` = visible from outside, absent = private (like Rust).
- `pub mod foo { ... }` = the module itself is public too.
- `use foo::bar` — a single symbol; `use foo::*` — wildcard.
- `pub use foo::bar` — re-export (shortens the path for downstream users).
- `pub use foo::*` — wildcard re-export: lifts every public item of
  `foo` into the enclosing scope at once. File-modules only (an inline
  `mod { ... }` still re-exports one name at a time); chain it to build a
  prelude — see [examples/reexport.exl](../examples/reexport.exl).

---

## 17. FFI — interop with C

The largest single surface of the language — exile was designed to
really call AmigaOS ROMs and libc. All `extern` declarations must live
in a module called `raw` (`mod raw {}` or `pub mod raw {}`) — namespace
hygiene.

### Simplest form

```rust
pub mod raw {
    extern fn add(a: int, b: int) -> int;     // declaration only
    extern fn shout();                        // void return
}

fn main() {
    println(raw::add(40, 2));                   // 42
    raw::shout();
}
```

The bodies of `add` and `shout` live on the C side — you link them via
`./exilc --link my_stub.c ...`.

[examples/ffi.exl](../examples/ffi.exl) | [examples/ffi_stub.c](../examples/ffi_stub.c)

### C types — the `c_*` aliases

| exile      | C                              |
|------------|--------------------------------|
| `c_char`   | `char` (impl-defined sign)     |
| `c_schar`  | `signed char`                  |
| `c_uchar`  | `unsigned char`                |
| `c_short`  | `short`                        |
| `c_ushort` | `unsigned short`               |
| `c_int`    | `int`                          |
| `c_uint`   | `unsigned int`                 |
| `c_long`   | `long`                         |
| `c_ulong`  | `unsigned long`                |
| `c_void`   | `void` (only legal as `*c_void`) |

Use these at the libc / system-header boundary — exile-side `int` ≠ C
`int` (on a 64-bit host C `int` is 32-bit, exile `int` is `long`).

```rust
pub mod raw {
    extern fn putchar(c: c_int) -> c_int;
}

fn main() {
    raw::putchar(72 as c_int);            // 'H'
    // ...
}
```

[examples/ffi_libc.exl](../examples/ffi_libc.exl) | [examples/ctypes.exl](../examples/ctypes.exl)

### The full FFI feature pack

Declaration shapes — not a runnable program on their own; each needs
the matching C header / stub. The linked examples wire them up end to
end.

```rust
@c_include("ffi_full_stub.h")                  // injects #include into the emitted C

pub mod raw {
    extern struct Library;                     // opaque type — only *Library is legal
    extern type ULONG;                         // alias for a C header type
    extern const PRETEND_VERSION: ULONG;       // a #define constant
    extern var DOSBase: *Library;              // mutable C-side global

    // rename: exile-side `open_lib`, in the emitted C `PretendOpenLibrary`
    extern fn open_lib as PretendOpenLibrary(name: str, version: ULONG) -> *Library;

    // variadic — caller is responsible for matching the format string
    extern fn print_f as printf(fmt: str, ...) -> c_int;
}
```

[examples/ffi_full.exl](../examples/ffi_full.exl) | [examples/ffi_opaque.exl](../examples/ffi_opaque.exl)

### Function pointers

```rust
pub mod raw {
    extern fn apply_int(x: c_int, f: fn(c_int) -> c_int) -> c_int;
}

fn double_it(x: c_int) -> c_int { return x + x; }

fn main() {
    println(raw::apply_int(21, double_it) as int);     // 42

    let f: fn(c_int) -> c_int = double_it;           // fn-ptr as a value
    println(f(50) as int);                             // 100
}
```

[examples/ffi_callback.exl](../examples/ffi_callback.exl)

### AmigaOS-specific

Writing for AmigaOS adds two attributes (both are **documentation** —
they don't change the emitted code; register loading is handled by
Bebbo's amiga.lib stubs). Declaration shape only — see the linked
example for the surrounding `mod raw` and `main`:

```rust
@amiga_lib(SysBase)
extern fn open_library as OpenLibrary(
    @reg(a1) name: *c_char,
    @reg(d0) version: ULONG
) -> *Library;
```

[examples/amiga_hello.exl](../examples/amiga_hello.exl) — a full
end-to-end example: OpenLibrary -> Output -> Write -> CloseLibrary,
running under vamos m68k.

---

## 18. Allocator — pluggable memory

The prelude ships an `Allocator` type (a struct with state plus two
function pointers). Generic methods `.alloc::<T>()` and `.free(p)` give
you a strongly-typed allocation surface; every concrete `T` is a
separate monomorphic instance — zero runtime polymorphism, the C
compiler can inline.

```rust
pub mod raw {
    extern fn make_c_allocator() -> Allocator;
}

fn main() {
    let a = raw::make_c_allocator();

    let p: own *int = a.alloc();             // T pinned by the annotation
    if p != null {
        *p = 42;
        println(*p);
    }
    a.free(p);                               // on EVERY path, not just the guarded one
}
```

The `free` sits outside the guard on purpose. Auto-drop is static — there are
no runtime drop flags — so an owner that is consumed on one branch and still
owned on the other has no single answer the compiler can emit, and that is
rejected:

    'p' is moved out on one branch but stays owned on the other — auto-drop is
    static (no runtime drop flags); consume it on every path or on none

Freeing a null pointer is a no-op, so moving the `free` out of the guard costs
nothing and keeps one owner with one lifetime.

[examples/allocator_demo.exl](../examples/allocator_demo.exl)

The backing implementation lives on the C side — on the host it's
malloc/free, on Amiga you can drop in `AllocMem`/`FreeMem` without
touching any exile call site.

The free seam carries the byte count back to the allocator: the compiler passes `size_of(T)` automatically, so the
`a.free(p)` call site is unchanged, but a custom allocator's `free`
hook receives `(state, ptr, size)`. libc ignores the size; Amiga
`FreeMem` and arena/pool allocators need it to reclaim.

On the host the prelude builtin `default_allocator()` returns the
libc-backed allocator with no `extern` ceremony — newer examples use it.
It needs the sys seam linked (`--link runtime/sys_host.c`; the Makefile
targets do this automatically).

### `Arena` — bump allocator for tree workloads

`Arena` is a prelude type over the `Allocator` seam: one upfront buffer,
`alloc_borrowed::<T>()` bumps a pointer per node, and the whole buffer
is released wholesale when the arena goes out of scope (its backing
field is an `own *u8`, so the release is the ordinary auto-drop of
[sec 10](#10-pointers-and-memory-stack-vs-heap)).

```rust
enum Expr {
    Lit(int)
    | Add(*Expr, *Expr)         // arena nodes borrow, they don't own
}

fn lit(ar: *Arena, v: int) -> *Expr {
    let n: *Expr = ar.alloc_borrowed();
    *n = Expr::Lit(v);
    return n;
}

fn main() {
    let a = default_allocator();
    let mut ar = Arena::with_capacity(a, 4096 as u32);
    let n = lit(&ar, 42);       // thousands of nodes, zero frees
    match *n { Expr::Lit(v) => { println(v); } | _ => { } }   // 42
}   // scope exit: ONE wholesale free of the whole buffer
```

- `alloc_borrowed()` returns a plain borrow `*T` — the node belongs to
  the **arena**, not the binding, so the own-lifecycle doesn't track
  it: no per-node free ceremony, which is the entire point.
- Exhaustion returns `null`, not a crash — pick the capacity
  generously.
- Borrows into the buffer must not outlive the arena binding (same rule
  of thumb as a `Slice` into a `Vec`).
- This is the self-host parser's idiom: build a whole AST through
  smart constructors, drop it all at once.

[examples/arena.exl](../examples/arena.exl)

---

## 19. Prelude collections

The prelude ships growable, allocator-backed collections built on top of
the `Allocator` seam ([sec 18](#18-allocator--pluggable-memory)). The
read side and the growth math are pure exile; only `alloc`/`free`
cross the FFI seam, so the data structures themselves compile to the
Amiga target unchanged.

`StringBuilder`, `String`, `Vec<T>`, and `HashMap<K, V>` own their
backing buffer through an `own *T` field
([sec 10](#10-pointers-and-memory-stack-vs-heap)). That makes them
affine — the move-pass forbids a silent by-value copy that would alias
the buffer — and droppable: the auto-drop pass frees each one at scope
exit. An explicit `x.free()` (or `defer x.free()`) still works and
elides the auto-drop.

### `StringBuilder` — mutable byte buffer

The keystone of the writer pattern ([sec 15](#15-traits)). A growable
`u8` buffer you append into.

```rust
pub mod raw { extern fn make_c_allocator() -> Allocator; }

fn main() {
    let a = raw::make_c_allocator();
    let mut sb = StringBuilder::with_capacity(a, 4 as u32);
    sb.push_str("Hi ");
    sb.push_int(2026);                       // decimal render
    sb.push_byte(33 as u8);                  // '!'
    println(sb.length() as int);             // 8
    let v = sb.as_slice();                   // read-only Slice<u8>
    println(v[0] as int);                    // 72 ('H')
}
```

- `with_capacity(a, hint)` clamps cap to `max(hint, 16)`; pushes grow it
  geometrically.
- Surface: `push_byte`, `push_str`, `push_int`, `length`, `as_slice`.
- `as_slice()` is read-only; bare-pointer reads stay rejected, so reads
  go through the `Slice<u8>`.

[examples/string_builder.exl](../examples/string_builder.exl)

### `String` — owned, NUL-terminated buffer

The frozen owner you hand around (`Vec<String>`, `HashMap<String, …>`).
Every constructor writes the trailing NUL, so `as_str()` is a
libc-`%s`-safe pointer by construction.

```rust
fn main() {
    let a = raw::make_c_allocator();
    let s = String::with_str(a, "Hello, exile!");
    defer s.free();
    println(s.length() as int);              // 13

    let mut sb = StringBuilder::with_capacity(a, 4 as u32);
    sb.push_str("built ");
    sb.push_int(2026);
    let b = String::build(sb);               // sb consumed — O(1) transfer
    defer b.free();
    println(b.length() as int);              // 10
}
```

- Surface: `with_str`, `empty`, `build`, `length`, `as_slice`, `as_str`,
  `free`.
- `build(sb)` projects a `StringBuilder`'s buffer into a `String` with no
  copy; the move-pass forbids reusing `sb` afterwards.
- Hand-written `eq` / `hash` / `clone` are content-based (delegating to
  `str::eq` / `str::hash`); `clone` deep-copies, so each owner frees its
  own allocation.

[examples/string.exl](../examples/string.exl)

### `Vec<T>` — growable array

The copy-out value-`T` workhorse.

```rust
fn main() {
    let a = raw::make_c_allocator();
    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);
    v.push(10); v.push(20); v.push(30);
    println(v.length() as int);              // 3
    match v.get(1 as u32) {                  // copy-out; OOB -> None
        Option::Some(x) => { println(x); }   // 20
        | Option::None  => { println(-1); }
    }
    let mut sum = 0;
    for x in v.iter() { sum = sum + x; }     // VecIter impl Iterator
    println(sum);                            // 60
}
```

- Surface: `with_capacity`, `length`, `push`, `get` (copy-out), `as_slice`,
  `iter`.
- `iter()` returns a cursor that `impl Iterator`, so `for x in v.iter()`
  works ([sec 7](#7-loops--while-and-for)).

[examples/vec.exl](../examples/vec.exl)

### Iterator combinators — `map` / `filter` / `take` / `enumerate` / `fold` / `collect`

Any `Iterator` ([sec 7](#7-loops--while-and-for)) chains through lazy
adapters via default methods. The whole chain monomorphises into a
single fused `next`-loop with the closure inlined — no temp `Vec`, no
heap.

```rust
fn main() {
    let a = default_allocator();
    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);
    v.push(10); v.push(20); v.push(30);

    let scale: int = 10;
    let m = v.iter().map(|x: int| -> int x * scale);
    let mut sum: int = 0;
    for x in m { sum = sum + x; }
    println(sum);                          // 600 = 100 + 200 + 300
}
```

- Lazy adapters: `map(f)`, `filter(p)` — `f` / `p` a closure or any
  `impl Fn1` ([sec 15](#15-traits)) — plus `take(n)` and `enumerate()`
  (yields `(u32, Item)` pairs).
- Consuming terminals: `fold(init, f)` with `f: impl Fn2`, and
  `collect(a)` — drains into a fresh `Vec<Item>` through the passed
  allocator.
- A full pipeline `v.iter().take(3 as u32).map(f).fold(0, g)` still
  lowers to one loop.
- The adapters are ordinary prelude structs (`Map<I, F>`, `Take<I>`, …);
  the manual constructor form `Map { inner: v.iter(), f: ... }` works
  when you want to name the type.

[examples/combinator_map.exl](../examples/combinator_map.exl),
[examples/combinator_filter.exl](../examples/combinator_filter.exl),
[examples/combinator_take_enumerate.exl](../examples/combinator_take_enumerate.exl),
[examples/combinator_fold_collect.exl](../examples/combinator_fold_collect.exl)

### `HashMap<K, V>` — open-addressing table

Linear-probing symbol table; `K` must `impl Hash + Eq`.

```rust
fn main() {
    let a = raw::make_c_allocator();
    let mut m: HashMap<int, int> = HashMap::with_capacity(a, 8 as u32);
    m.insert(1, 100);
    m.insert(2, 200);
    if m.contains(2) { println(1); } else { println(0); }   // 1
    match m.get(1) {
        Option::Some(v) => { println(v); }   // 100
        | Option::None => { println(-1); }
    }
    m.remove(1);
    println(m.length() as int);              // 1
}
```

- Surface: `with_capacity`, `length`, `contains`, `get` (copy-out),
  `insert` (grows past 0.75 load), `remove` (tombstone), `iter`.
- `iter()` yields `(K, V)` tuples for live slots: `for kv in m.iter() { let (k, v) = kv; ... }`.
- `HashMap<String, int>` is the self-host motivator — content `String`
  eq/hash make equal-text keys collide into the same slot.

[examples/hashmap.exl](../examples/hashmap.exl)

### `mod str` — string operations

Pure-exile ops over `cstr_len` + `Slice<u8>`. Only `cstr_len`, a narrow
`strlen` seam, crosses the FFI boundary.

```rust
fn main() {
    let a = "hello";
    if a == "hello" { println(1); } else { println(0); }   // 1  (content compare)
    println(str::length(a) as int);             // 5
    println(str::cmp(a, "world"));           // -15  (lexicographic)
    println(str::hash("a") as int);          // 97
    let b = str::as_bytes(a);                // Slice<u8>
    println(b[0] as int);                    // 104 ('h')
}
```

- `==` / `!=` between two `str` operands now dispatch to `str::eq`
  (content compare) — closing the old pointer-compare footgun.
- Surface: `str::length`, `str::as_bytes`, `str::eq`, `str::cmp`,
  `str::hash`, `str::from_slice`. `cstr_len(s)` is the underlying
  builtin (libc `strlen`).

[examples/str_ops.exl](../examples/str_ops.exl)

`str::as_bytes` goes one way; `str::from_slice` comes back:

```rust
fn main() {
    let a = default_allocator();
    let mut ar = Arena::with_capacity(a, 1024 as u32);

    let src = "fn greet";
    let bytes = str::as_bytes(src);
    let kw = str::from_slice(&ar, bytes[0..2]);     // "fn"
    let name = str::from_slice(&ar, bytes[3..8]);   // "greet"

    println(kw);
    println(name);
}
```

- `str::from_slice(arena, slice)` copies a `Slice<u8>` into the arena
  and NUL-terminates it, yielding a `str` the arena owns. It is the
  string-interning primitive: a tokenizer holds identifiers as slices
  of the source buffer and materialises each one once.
- The arena is the owner, so the `str` lives exactly as long as the
  arena does — no per-string `free`.
- The arena returns `null` when full and `from_slice` cannot fail
  gracefully, so size it generously
  ([sec 18](#18-allocator--pluggable-memory)).

[examples/str_from_slice.exl](../examples/str_from_slice.exl)

---

## 20. Profile and the tier system

exile has a "comfort tier" — three ergonomics levels, orthogonal to the
target:

```sh
./exilc --profile core     ...    # cheapest features only, warns on generics
./exilc --profile standard ...    # generics silent, some comfort still in lint
./exilc --profile full     ...    # everything, default
```

The `@tier(...)` annotation pins a level to a declaration:

```rust
@tier(core)     fn id<T>(x: T) -> T { return x; }
@tier(standard) struct Pair<A, B> { fst: A, snd: B }
@tier(full)     enum Maybe<T> { Just(T) | Nothing }

fn main() {
    println(id(42));                       // 42
    let p = Pair { fst: 1, snd: 2 };
    println(p.fst);                        // 1
    let m: Maybe<int> = Maybe::Just(7);
    println(match m { Maybe::Just(n) => n | Maybe::Nothing => 0 });    // 7
}
```

The emitted code is identical regardless — this is pure lint to keep
"bloat" in check on a 256K Amiga. Bonus: `--bloat-report` shows the
top-20 functions by emitted byte count, and `--perf-report` (or
`--perf-report=json` for tooling) reads cost sites off the typed IR —
the m68k costs that optimised C hides, like 32-bit mul/div lowering to
libgcc soft calls — flagging the ones that sit inside loops.

[examples/tier.exl](../examples/tier.exl)

---

## 21. Attributes (`@...`)

exile uses attributes for declaration metadata — they signal something
to the compiler or linter that doesn't fit the signature itself. The
syntax is uniform: `@name` or `@name(args)` **before** the decorated
declaration.

Full list (links go to the section that shows the attribute in action):

| attribute                     | placement                   | what it does                                                       |
|-------------------------------|-----------------------------|--------------------------------------------------------------------|
| `@must_use`                   | `fn` / `enum`               | warns when the call result (or a value of the type) is discarded   |
| `@derive(Trait, ...)`         | `struct` / `enum`           | synthesises real `impl` blocks for prelude traits (`Eq`, `Hash`, `Clone`, `Debug`) |
| `@move`                       | `struct`                    | marks an affine type — the move-pass forbids using a value after it is consumed |
| `@escapes`                    | `fn`                        | opts the function out of the escape-pass borrow checks (see below)  |
| `@debug`                      | `struct` / `enum` (mono)    | synthesises a Rust-Debug-style printer, unlocking `println(v)` for the type |
| `@tier(core\|standard\|full)` | `fn` / `struct` / `enum`    | hint for the `--profile`-aware linter ([sec 20](#20-profile-and-the-tier-system)) |
| `@c_include("path.h")`        | top-level (outside `fn`)    | injects `#include "path.h"` into the emitted C ([sec 17](#17-ffi--interop-with-c)) |
| `@reg(d0..d7\|a0..a6)`        | parameter in `extern fn`    | documents an AmigaOS register pin (doesn't change emission) ([sec 17](#17-ffi--interop-with-c)) |
| `@amiga_lib(BaseName)`        | `extern fn`                 | documents the AmigaOS lib base (doesn't change emission) ([sec 17](#17-ffi--interop-with-c)) |
| `@doc("text")`                | any decl                    | documentation string; recognised and validated, currently dropped (paired with `///` lines)  |

Most attributes are linter metadata; `@c_include` actually injects an
`#include`, and `@debug` actually generates code (a synthesised
printer).

### `@must_use` — don't let the result be ignored

Modelled on Rust `#[must_use]`. You decorate a function or an enum type
— if someone calls the function (or produces a value of the type) and
drops the result into a statement position (no binding), the compiler
emits a warning.

```rust
@must_use
fn classify(n: int) -> int {
    return n;
}

fn main() {
    classify(19);                        // warning: call result is `@must_use`;
                                         //          bind it or use `let _ = ...`

    let r: int = classify(19);           // OK — bound to a name
    println(r);

    let _ignored: int = classify(19);    // OK — `_`-prefix silences
}
```

The attribute also works at the *type* level — when you mark an `enum`,
every value of that type in drop position warns, even if the producing
function isn't marked `@must_use`. The prelude relies on this:
**`Option<T>` and `Result<T, E>` are marked `@must_use`**, so discarding
the result of a function returning `Result` warns out of the box, no
annotation needed:

```rust
fn try_div(n: int, d: int) -> Result<int, str> {
    if d == 0 { return Result::Err("div by zero"); }
    return Result::Ok(n / d);
}

fn main() {
    try_div(10, 0);                      // warning: unused 'Result<i32, str>' value
                                         //          (marked `@must_use`); bind it...

    let _ = try_div(10, 0);              // explicit opt-out
}
```

How to silence properly:

- `let r = call();` — bind to a name.
- `let _ = call();` / `let _name = call();` — explicit opt-out; names
  starting with `_` are also silenced by the unused-let pass.

Limitations: `@must_use` is legal **only** on `fn` and `enum`. Trying to
decorate `impl`, `struct`, or `mod` fails compilation.

### `@debug` — `println(v)` for structs and enums

Out of the box, `print`/`println` understand only scalars (`int`,
`bool`, `str`).
Trying `println(my_struct)` or `println(my_enum)` is an error — the
diagnostic suggests "print individual fields, or mark the type with
`@debug`". The latter is what `@debug` does.

You decorate the type, and the compiler synthesises a one-line printer
in the `{:?}` style from Rust; `println(v)` starts working for values of
that type:

```rust
@debug
struct Point { x: int, y: int }

@debug
enum Shape {
    Circle(int)
    | Rect { w: int, h: int }
}

fn main() {
    let p = Point { x: 3, y: 4 };
    println(p);                          // Point { x: 3, y: 4 }

    println(Shape::Circle(5));           // Shape::Circle(5)
    println(Shape::Rect { w: 8, h: 2 }); // Shape::Rect { w: 8, h: 2 }
}
```

Rules:

- Every **struct field** (and every **enum-variant payload**) must
  itself be debug-able: a primitive int-like type, `bool`, `str`, a
  pointer (printed as the address with `%p`), or another type marked
  `@debug` — otherwise the compiler rejects with the hint "mark the type
  `@debug`, or remove `@debug` from the struct". Tuple fields,
  fn-pointers, and opaque C types are not debug-able and are rejected.
- Nested aggregates call their own printer — you can have
  `@debug struct Outer { p: Point }` as long as `Point` is `@debug` too.
- **Generics work** — mark `@debug struct Pair<A, B> { ... }` and the
  printer is synthesised per monomorphic instance, so each value prints
  with its concrete type arguments (`Pair<i32, bool> { fst: 1, snd:
  true }`). No trait bounds; the per-field debug-able check runs on the
  concrete instance.

Output format: Rust-Debug-style, one line; `println(v)` appends `\n` at
the end (and `print(v)` omits it), just as for scalars.

[examples/debug_attr.exl](../examples/debug_attr.exl)

### `@derive(...)` — auto-implement prelude traits

`@derive(Trait, ...)` on a `struct` or `enum` synthesises **real** `impl`
blocks — the same code you could write by hand — so a derived type flows
through conformance, generic bounds, monomorphization, and codegen like
any other impl.

```rust
@derive(Eq, Hash, Clone)
struct Point { x: int, y: int }

fn all_equal<T: Eq>(a: T, b: T) -> bool { a.eq(b) }

fn main() {
    let p = Point { x: 3, y: 4 };
    let q = Point { x: 3, y: 4 };
    if all_equal(p, q.clone()) { println(1); } else { println(0); }  // 1
}
```

- `Eq` — field-wise `eq` (`ne` comes free from the trait default).
- `Hash` — `acc*31 + field.hash()` fold (enums seed with the variant
  index). `Hash: Eq`, so deriving `Hash` requires deriving `Eq`.
- `Clone` — a value copy.
- `Debug` — the `{:?}` writer-pattern `fmt` ([sec 15](#15-traits)).
  `@derive(Display)` is rejected — output is user-controlled.

[examples/derive.exl](../examples/derive.exl),
[examples/derive_debug.exl](../examples/derive_debug.exl)

### `@move` — affine, use-at-most-once types

`@move` on a `struct` marks it **affine**: a move-pass tracks each value
and rejects using it after it has been consumed (passed by value, or its
owning method called). It is how the prelude's owning collections
(`String`, `Vec`, `HashMap`) prevent a silent buffer alias that would
double-free.

```rust
@move
struct Buf { /* ... */ }

fn sink(b: Buf) { /* consumes b */ }

fn main() {
    let b = make_buf();
    sink(b);
    sink(b);          // error: `b` used after it was consumed
}
```

- A value is consumed by a by-value move (assignment, argument, or a
  by-value `self` method); reads through `*const self` borrow instead and
  don't consume.
- `defer x.free()` is the idiomatic single consumption at end of scope.
- The check forks and merges across `match` / `if` arms, and runs `defer`
  consumption LIFO at end of scope.
- Since the Owner sigil
  ([sec 10](#10-pointers-and-memory-stack-vs-heap)) a struct with an
  `own *T` field is affine *structurally* — no marker needed; the
  prelude collections work that way. `@move` remains the explicit
  marker for affine types that don't own heap memory.

### `@escapes` — escape-pass opt-out

The escape pass rejects returning or storing a borrow rooted in a local
— including one smuggled inside a struct, which gcc's
`-Wreturn-local-addr` misses:

```rust
fn make() -> Slice<int> {
    let arr: [int; 3] = [1, 2, 3];
    return Slice { ptr: &arr[0], len: 3 as u32 };   // error: escapes
}
```

Borrows rooted in pointer params pass (their storage is caller-owned),
and so does pushing `&x` into a container that lives in the same frame.
`@escapes` on a `fn` is the deliberate opt-out for borrows rooted in
storage the analyser can't yet model (arena / region returns).

[examples/escape_pass.exl](../examples/escape_pass.exl)

### `@doc("...")` and `///` — documentation strings

Two equivalent ways to attach a docstring to a declaration: `///` line
comments (Rust convention) or the explicit `@doc("...")` attribute.

```rust
/// Doubles its input.
/// Lines stack.
fn dbl(x: int) -> int { x + x }

@doc("Adds two integers — the explicit attribute form.")
fn add(a: int, b: int) -> int { a + b }

/// A point in 2D space.
struct Point { x: int, y: int }
```

Status (MVP): both forms are *recognised and validated* (the attribute
argument must be a string literal), then *dropped*. Nothing reaches
the AST or the emitted C yet. Shipping the syntax now lets source
files carry documentation that future tooling (formatter, doc
generator) can read back by re-parsing — without breaking the build
today.

[examples/doc_comments.exl](../examples/doc_comments.exl)

---

## 22. Owning the hardware - `rune`, `ward`, `sigil`, `seal`

On the Amiga the chipset is simply there, at a fixed address, and any code in
the program can reach it. C hands you a pointer and steps aside: nothing records
that `$DFF040` belongs to the blitter driver, nothing forces the `volatile` that
stops the compiler folding your stores away, and nothing notices when two
register writes that must happen together are pulled apart by an interrupt.

exile turns those into declarations a compiler can check. It does so without
lifetimes and without a borrow checker: a module *claims* a range of silicon,
and the claim is a compile-time fact that leaves nothing behind at run time.

Four constructs, smallest first.

### `rune` - one access, with its width and direction in the type

A `rune` names a single hardware register: the address, the width of an access,
and whether that access may read, write, or both.

```rust
rune cop1lc:  u32 at 0xDFF080 write;   // COP1LC - the copper list pointer
rune copjmp1: u16 at 0xDFF088 write;   // COPJMP1 - strobe to restart it

fn program_copper(list_addr: u32) { cop1lc.write(list_addr); }
fn start_copper()                 { copjmp1.strobe(); }

fn main() { program_copper(4096); start_copper(); }
```

At file scope a rune emits a `const` pointer the whole file shares:

```c
volatile unsigned long *const cop1lc = (volatile unsigned long *)14676096UL;
volatile unsigned short *const copjmp1 = (volatile unsigned short *)14676104UL;
```

`volatile` here is not an option you remember to pass - it is what a rune *is*.
`.write(v)` lowers to exactly one store of exactly that width, and `.strobe()`
covers the registers the hardware only cares about the timing of, lowering to a
store of zero (`*copjmp1 = 0;`).

Direction is part of the type, so it is checked:

```rust
fn main() {
    rune cop1lc: u32 at 0xDFF080 write;
    cop1lc.write(cop1lc.read());
}
```

```
error: cannot read a write-only rune
```

Runes are first class. `write rune<u16>` is a type, so a register can cross a
function boundary without shedding what it is:

```rust
fn burst(r: write rune<u16>) { r.write(64); }
```

Everything above is checked by compiling it. The volatile lowering is checked
one step further - by running it.
[tests/rune/ram_roundtrip.exl](../tests/rune/ram_roundtrip.exl) points a rune at
a static cell in RAM, writes, reads back and compares, at `-O2`, on the m68k,
under an emulator.

The RAM base is deliberate and worth saying out loud rather than hiding. On
hardware it would be a chipset register - `0xDFF180` is COLOR00, the background
colour - but a chipset register cannot be read back inside an emulator, so an
MMIO base would demonstrate the syntax and prove nothing. Over RAM the store is
observable, so the round-trip is a real check: the numbers that print are the
numbers that reached memory. What the address is never changes what a rune
guarantees - one access, at the declared width, that the compiler may not elide,
widen or reorder - and that guarantee is exactly what the emitted C above shows.

[tests/rune/](../tests/rune/)

### `ward` - a typed overlay on a register block

Registers arrive in blocks, and the NDK's answer is one large `struct Custom`
pointed at `$DFF000`. A `ward` is that struct with the parts C leaves to
discipline turned into rules.

```rust
ward Custom {
    bltcon0: u16 at 0x040 write;
    bltsize: u16 at 0x058 write;
}

fn main() {
    ward custom: Custom at 0xDFF000;
    custom.bltcon0.write(0x09F0);
    custom.bltsize.write(64);
}
```

The instance is not a variable. It occupies no storage whatsoever, and the
program above emits exactly two stores and nothing else:

```c
*((volatile unsigned short *)(14675968UL + 64UL)) = 2544;
*((volatile unsigned short *)(14675968UL + 88UL)) = 64;
```

Base and offset stay separate in the output instead of being folded into one
constant, so the register you meant is still legible in the generated C.

A field *is* a rune and inherits every rune rule - width, direction, one store
per write. What the ward adds is layout, and layout is checked: no two fields
may overlap.

```rust
ward Bad {
    a: u32 at 0x00 write;
    b: u16 at 0x02 write;
}
```

```
error: ward 'Bad' fields 'a' [0, 4) and 'b' [2, 4) overlap
```

[tests/ward/](../tests/ward/)

### `sigil` - the claim, and who may hold it

A rune or a ward says what a register is. A `sigil` says whose it is.

```rust
sigil Blitter { 0xDFF040 .. 0xDFF05A }

mod gfx {
    own Blitter;
    rune bltsize: u16 at 0xDFF058 write;
    pub fn go() { bltsize.write(64); }
}

mod sound {
    pub fn steal() { rune bltsize: u16 at 0xDFF058 write; }
}
```

```
error: address 0xDFF058 belongs to resource 'Blitter', claimed by 'gfx'
```

Read where that error lands. It fires on the *declaration*, not on the write:
a module that does not own the range cannot form a handle into it at all, so
there is never a window in which a stray pointer exists and merely happens not
to have been used yet.

The claim also costs nothing, and the repository keeps that as a diff rather
than a paragraph - the same program in two halves, one with the ownership
declared and one with it stripped out:

```sh
./exilc --target c --c-out /tmp/gated.c   tests/sigil/equality/gated.exl
./exilc --target c --c-out /tmp/ungated.c tests/sigil/equality/ungated.exl
cmp /tmp/gated.c /tmp/ungated.c    # silent: identical
```

That is the design in one line. Ownership is settled while you compile, and
none of it survives into the binary: no lifetime to annotate, no borrow to
prove, no descriptor to carry.

[tests/sigil/](../tests/sigil/)

### `seal` - a sequence an interrupt cannot tear apart

Programming a blit takes eight register writes. An interrupt landing between
any two of them leaves the hardware half-set-up. `seal` marks the region that
has to be indivisible.

The guarantee is that the region is left exactly once per entry, on *every*
exit path - including the ones that break hand-written enter/exit pairs:

```rust
fn seq(n: int) -> Option<int> {
    let mut i = 0;
    while i < 3 {
        seal {
            println(1);
            if n == 0 { return Option::None; }
            if n == 1 { break; }
            if n == 2 { i = i + 1; continue; }
            println(2);
        }
        i = i + 1;
    }
    return Option::Some(9);
}
```

`return`, `break` and `continue` all leave the region and all restore, as does
a propagating `try`. On the host you can watch that happen: the runtime seam
counts entries against exits and reports at process exit.

```sh
./exilc --target host --link runtime/sys_host.c -o seq tests/seal/exits.exl
./seq
```

```
1
2
...
seal-balance 0 misnest 0
```

The blitter sequence itself runs too:
[tests/seal/consumer_ram.exl](../tests/seal/consumer_ram.exl) is the composed
driver with its registers moved over RAM, cross-compiled and executed on the
m68k. The chipset is above the emulator, so what that run proves is the
SEQUENCE and the seam - the order of the stores and the balance of the region -
not the registers themselves. Said plainly because a witness that is taken for
more than it shows is worse than none.

What a seal lowers to depends on the target, deliberately. The construct emits
a call into a seam two functions wide:

```c
__seal0 = sys_seal_enter();
/* the region */
sys_seal_exit(__seal0);
```

On bare metal that seam masks interrupts in the status register, and the token
is the saved mask. Under AmigaOS it cannot be: exec keeps its own nesting count
inside `Disable()`/`Enable()`, and writing SR behind its back breaks it - so
there the seam *is* `Disable()`/`Enable()`, and the token carries exec's depth
instead. The token is opaque exactly so that two targets can save two different
things behind one guarantee.

[tests/seal/](../tests/seal/)

### All four at once

[tests/seal/blitter_setup.exl](../tests/seal/blitter_setup.exl) programs a blit
at the addresses the Hardware Reference Manual gives. A `sigil` owns the chip
range, a `ward` lays the NDK's layout over it, the fields are runes and one
crosses a call boundary as `write rune<u16>`, and a `seal` makes the sequence
indivisible.

```rust
mod gfx {
    own Blitter;
    own DmaControl;

    fn start(size: write rune<u16>, v: u16) { size.write(v); }

    pub fn blit(src: u32, dst: u32, size: u16) {
        seal {
            let old = custom.dmaconr.read();
            custom.bltcon0.write(2544);      // 0x09F0 - minterm D = A
            custom.bltcon1.write(0);
            custom.bltafwm.write(65535);
            custom.bltalwm.write(65535);
            custom.bltapt.write(src);
            custom.bltdpt.write(dst);
            start(custom.bltsize, size);
            custom.dmacon.write(ndk::DMAF_SETCLR | (old & ndk::DMAF_BLITTER));
        }
    }
}
```

The C it emits, checked line for line against a golden file by
`make selfhost-seal`:

```c
__seal0 = sys_seal_enter();
old = *((volatile unsigned short *)(14675968UL + 2UL));
*((volatile unsigned short *)(14675968UL + 64UL)) = 2544;
*((volatile unsigned short *)(14675968UL + 66UL)) = 0;
*((volatile unsigned short *)(14675968UL + 68UL)) = 65535;
*((volatile unsigned short *)(14675968UL + 70UL)) = 65535;
*((volatile unsigned long *)(14675968UL + 80UL)) = src;
*((volatile unsigned long *)(14675968UL + 84UL)) = dst;
gfx__start((volatile unsigned short *)(14675968UL + 88UL), size);
*((volatile unsigned short *)(14675968UL + 150UL)) = ndk__DMAF_SETCLR | (old & ndk__DMAF_BLITTER);
sys_seal_exit(__seal0);
```

Ten stores and one seam pair. No descriptor, no ownership token, no residual
check: all four constructs are gone, and what is left is the driver you would
have written by hand.

Notice what the `dmaconr` read is for. `DMACON` at `$DFF096` is write-only and
its bit 15 is SET/CLR, so the live state must be read from the separate
`DMACONR` port at `$DFF002`, never back from `DMACON`. That same SET/CLR bit is
why *enabling* a channel needs no seal at all - one write touches the bits you
name and leaves every other bit alone. What needs the seal is the sequence.

### What it will not do

A capability that quietly protects the empty set is worse than none, so the
limits are written down, and each is pinned by a fixture that must keep
compiling. Four of them:

1. **A forgotten seal is not diagnosed.** The compiler masks where you say it
   should; it does not know what ought to have been masked. The blitter
   sequence written with no seal at all compiles fine.
2. **Masking is all interrupts, not one level.** There is no syntax for "mask
   only level 3" - correct, but blunt. Per-level masking waits for a consumer
   that needs it.
3. **A seal does not disprove a race.** It makes *its own* sequence
   indivisible and says nothing about another party reaching the same register
   without one. What the language cannot see, it does not claim.
4. **Region length is neither measured nor limited.** Entering is cheap on the
   68000; *holding* costs interrupt latency - audio glitches, missed disk DMA.
   A seal wrapped around a loop compiles in silence.

### `atomic` - naming the registers a region is about

There was a fifth limit, and it was the sharpest: **nothing verified that the
sealed region was the right region.** A `DMACON` save read outside the region
with only the restore inside - precisely the bug a seal exists to prevent -
compiled clean, ran, and balanced, because exactly-once and nesting both
genuinely held. The host witness was blind to it too.

Seal answers *when* a sequence is indivisible. It never answered *which
registers* the sequence is about, and that is the whole of the gap. An `atomic`
clause answers it, declared where the addresses and widths already live:

```rust
ward Custom {
    dmaconr: u16 at 0x002 readwrite;
    dmacon:  u16 at 0x096 readwrite;

    atomic { dmaconr, dmacon }
}
```

Two rules follow from the declaration. Every access to a member must be inside a
`seal`; and every access to members of one group, within one function, must be
inside the **same** region. Two seals each holding half of a pair is the shape
that makes the first rule alone insufficient, and it is why the group rather
than the field is the unit.

The declaration is a fact about the ward, fixed where it is written - there is no
inference and no widening at a use site. The check is lexical and per-function,
and that boundary is deliberate rather than incidental: a helper called from
inside a seal is not covered, because covering it means the flow analysis this
design refuses. Ordering inside a group is not covered either. The blitter cares
that `BLTSIZE` is written **last**, not only that it is written together, and
that is a stronger property needing a declaration of its own; conflating the two
would make the group mean two things.

Like every other construct in this chapter, the clause costs nothing at run time:
a program with it and the same program without it emit byte-identical C.

---

## 23. Where to next

Every feature covered here has a working example in
[`examples/`](../examples/) (one file = one feature). A recommended
path:

1. Walk through in order: `hello_world` -> `arithmetic` -> `if_else` ->
   `while_loop` -> `functions`.
2. Then data types: `integers` -> `floats` -> `tuples` -> `structs` ->
   `pointers` -> `heap` -> `own_ptr` -> `slice` -> `sub_slicing` ->
   `scoped_projection` -> `defer`.
3. Abstractions: `enums` -> `literal_match` -> `active_patterns` ->
   `generics` -> `methods` -> `generic_methods` -> `generic_impl` ->
   `traits` -> `associated_types` -> `lambdas` -> `closures_a2` ->
   `fn_trait` -> `for_iterator` -> `derive` -> `display` -> `modules` ->
   `multi_file`.
4. Prelude collections: `string_builder` -> `string` -> `vec` ->
   `hashmap` -> `str_ops` -> `combinator_map` -> `combinator_filter` ->
   `combinator_take_enumerate` -> `combinator_fold_collect` ->
   `enum_heap_box` -> `arena`.
5. Out into the world: `ffi` -> `ffi_libc` -> `ffi_opaque` -> `ffi_full` ->
   `ffi_callback` -> `ctypes` -> `allocator_demo` -> `amiga_hello`.

For what has landed and why — release by release — see
[`CHANGELOG.md`](../CHANGELOG.md); for what is deliberately absent,
[sec 1](#1-what-is-exile-lang).

Good luck. Write for the 68k.