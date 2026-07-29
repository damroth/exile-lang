<p align="center">
  <img src="https://github.com/damroth/exile-lang/actions/workflows/ci.yml/badge.svg?branch=master" />
  <img src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  <img src="https://img.shields.io/badge/status-experimental-blueviolet" />
  <a href="https://github.com/damroth/exile-lang/releases"><img src="https://img.shields.io/github/v/release/damroth/exile-lang?include_prereleases&display_name=tag&sort=semver" /></a>

</p>

# exile-lang

A systems language for the Amiga - Rust-inspired syntax, compiled to C89.

Inspired, not copied. If you have written Rust you can read exile on sight -
`fn`, `let`, braces, traits, ADTs, exhaustive `match`. But variants and match
arms are separated by `|`, ML-style; active patterns come from F#; and
underneath there are no lifetimes and no borrow checker.

Ownership is `own *T` with compile-time auto-drop, moves and escapes are
tracked, and the hardware itself is owned by capability - claimed in the
source, proved by the compiler, erased from the output.

The output is C89, so it runs wherever `cc` runs: an A500 on Kickstart 1.3, or
the laptop you develop on. The compiler is written in exile, and builds from a
committed C seed with nothing but `cc`.

## Why exile

### A driver can own the silicon, and the compiler proves it

Four constructs describe hardware. A `sigil` claims a range of addresses for one
module; a `ward` lays a typed overlay over it; a `rune` is one access of a
declared width and direction; a `seal` makes a multi-register sequence
indivisible against interrupts.

```rust
sigil Blitter { 0xDFF040 .. 0xDFF05A }

mod gfx {
    own Blitter;                          // this silicon is ours
    rune bltsize: u16 at 0xDFF058 write;
    pub fn go() { bltsize.write(64); }
}

mod sound {
    pub fn steal() { rune bltsize: u16 at 0xDFF058 write; }
}
```

No other module can so much as form a handle into that range:

```
error: address 0xDFF058 belongs to resource 'Blitter', claimed by 'gfx'
```

And the guarantee costs nothing. The same program with the ownership declared
and with it stripped out emits the same C - byte for byte:

```sh
./exilc --target c --c-out /tmp/gated.c   tests/sigil/equality/gated.exl
./exilc --target c --c-out /tmp/ungated.c tests/sigil/equality/ungated.exl
cmp /tmp/gated.c /tmp/ungated.c    # silent: identical
```

Because what `gfx::go()` compiles to is the store you would have written by
hand anyway:

```c
volatile unsigned short *const bltsize = (volatile unsigned short *)14676056UL;

void gfx__go(void) {
    *bltsize = 64;
}
```

Ownership without lifetimes and without a borrow checker: capabilities are
claimed where the handle is created, checked at compile time, and erased.

### It builds with nothing but a C compiler

`seed/exilc.c` is the compiler's own C output, committed to the repository. So
`make compiler` needs no package manager and no language runtime: `cc` builds
the seed, the seed compiles `src/*.exl`, and `cc` builds that into `./exilc`.

### It compiles itself, and the build checks that it does

```sh
make bootstrap-from-seed   # ~3.5 min
```

The seed builds a compiler, that compiler compiles the current source, the
result compiles the same source again - and the last two outputs must be
byte-identical or the build fails. CI runs it on a plain runner.

### Ergonomics that cost no runtime

`defer` runs on every exit path - `return`, `break`, `continue`, `try` - not
only the one you happened to think about. Traits dispatch statically, generics
are monomorphised, `match` is checked for exhaustiveness.

```rust
view Size(n: int) -> Small | Medium | Large {   // an active pattern: classify
    if n < 16   { return Size::Small; }         // a plain int, then match on
    if n < 1024 { return Size::Medium; }        // the answer
    return Size::Large;
}

fn describe(n: int) {
    defer println("(checked)");

    match n {
        Size::Small  => println("small")
      | Size::Medium => println("medium")
      | Size::Large  => println("large")
    }
}

fn main() { describe(3); describe(4096); }
```

```
small
(checked)
large
(checked)
```

A `view` is exile's answer to F#'s active patterns, which it spells as a
keyword rather than F#'s banana clips: it expands to an ordinary enum plus a
function, so exhaustiveness is checked against a closed nominal type and costs
nothing at run time.

All of it is erased into C89: there is no garbage collector, and no runtime to
ship alongside the binary.

### C89 out, so it runs where `cc` runs

The Amiga target is m68k for Kickstart 1.3 and up - an A500 is enough. The same
source builds and runs on Linux and macOS while you develop.

## Five minutes

```sh
git clone --recurse-submodules <repo-url>
cd exile-lang
make compiler                                            # ~90 s, needs only cc
./exilc --target host examples/hello_world.exl -o hello
./hello                                                  # Hello, World!
```

## Amiga

```sh
make toolchain   # one-time, ~30-60 min: builds the bundled m68k-amigaos
                 # cross-compiler into _build/toolchain/
./exilc --target amiga examples/amiga_hello.exl -o hello
```

The bundled cross-compiler is [Bebbo's amiga-gcc](https://codeberg.org/bebbo/amiga-gcc),
producing 68000 binaries by default - compatible with Amiga 1000 through 4000,
and runnable under WinUAE, FS-UAE or vamos.

If you cloned without `--recurse-submodules`:

```sh
git submodule update --init
```

## Where to go next

- **[docs/getting-started.md](docs/getting-started.md)** - from a clone to a
  program of your own running on an Amiga, cross-compiler and vamos included.
- **[docs/exile-by-example.md](docs/exile-by-example.md)** - the language, one
  feature at a time, from hello world to FFI and AmigaOS.
- **[examples/](examples/)** - one file per feature, each with its `.expected`
  output, so you can build it, run it and diff.
- **[CHANGELOG.md](CHANGELOG.md)** - what landed when, and why.

## Status

`v1.0.0` marks a compiler milestone - exile compiling itself, byte for byte -
not a frozen language. The language is still moving, and the version number
tracks it, so expect the surface to change. Tagged releases live on the
[Releases page](https://github.com/damroth/exile-lang/releases).
