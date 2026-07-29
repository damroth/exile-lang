# Getting started

From a fresh clone to a program of your own running on an Amiga. Nothing here
needs a package manager or a language runtime - the host half needs only a C
compiler, and the Amiga half needs the cross-compiler this repository bundles.

If you want the language itself rather than the setup, read [Exile by
Example](exile-by-example.md) instead; this page is about getting a working
binary in your hands.

## What you need

For the host half: `git`, `make`, and a C compiler. That is the whole list.

For the Amiga half you also build a cross-compiler once, which needs the usual
GCC build dependencies. The authoritative list is the one CI builds from, in
[`Dockerfile.ci`](../Dockerfile.ci) - on Debian or Ubuntu:

```sh
sudo apt-get install make wget git xz-utils gcc g++ patch \
     libgmp-dev libmpfr-dev libmpc-dev \
     flex bison gettext texinfo libncurses-dev autoconf rsync lhasa
```

## 1. Build the compiler

```sh
git clone --recurse-submodules <repo-url>
cd exile-lang
make compiler
```

That takes about a minute and a half, and finishes with something like:

```
compiler: ./exilc ready (79407 lines of C, built by cc; no OCaml involved)
```

What just happened is worth a sentence, because it is unusual. exile is written
in exile, and `seed/exilc.c` - the compiler's own C output - is committed to the
repository. So `cc` builds the seed, the seed compiles `src/*.exl`, and `cc`
builds *that* into `./exilc`. There is no bootstrap problem to solve and nothing
to install first.

If you would like the build to prove it reproduces itself rather than just
asserting it, `make bootstrap-from-seed` walks one stage further and fails
unless the last two stages emit byte-identical C. It takes about three and a
half minutes.

## 2. Hello on the host

```sh
./exilc --target host examples/hello_world.exl -o hello
./hello
```

```
Hello, World!
```

Every example under [`examples/`](../examples/) has a `.expected` file beside it
holding the exact output it should produce, so you can always check rather than
squint:

```sh
./hello | diff - examples/hello_world.expected
```

## 3. Your first program

Put this in `factorial.exl` in the repository root:

```rust
fn main() {
    let mut n = 1;
    for i in 1..=5 {
        n = n * i;
        println(n);
    }
}
```

```sh
./exilc --target host factorial.exl -o factorial
./factorial
```

```
1
2
6
24
120
```

To see the C rather than run it, ask for the C target - it writes
`factorial.c` beside the source, or wherever `--c-out` points:

```sh
./exilc --target c factorial.exl
./exilc --target c --c-out /tmp/factorial.c factorial.exl
```

The output is C89 with no exile runtime underneath it. Reading it is a
reasonable way to find out what a construct costs.

## 4. The cross-compiler

The Amiga half needs a m68k compiler, and the repository bundles one as a
submodule: [Bebbo's amiga-gcc](https://codeberg.org/bebbo/amiga-gcc). Building
it is a one-time job measured in tens of minutes:

```sh
make toolchain
```

It installs into `_build/toolchain/`, which is exactly where `./exilc` looks for
it. If the submodule is missing, the target says so and tells you the fix:

```sh
git submodule update --init
```

## 5. The same program, on m68k

The same source file, one flag different:

```sh
./exilc --target amiga factorial.exl -o factorial_am
file factorial_am
```

```
factorial_am: AmigaOS loadseg()ble executable/binary
```

That is a real AmigaOS executable, built for the 68000, so it will run on
anything from an A500 up - under an emulator, or off a floppy on the real
machine.

## 6. Running it under vamos

You do not need a full emulator to run a plain command-line binary. `vamos`,
from [amitools](https://github.com/cnvogelg/amitools), runs AmigaOS executables
directly on your machine by emulating the CPU and implementing `dos.library`,
`exec.library` and a few others in Python.

The PyPI releases of `amitools` and `machine68k` drift apart, so the working
install pulls both from git. This is the recipe CI's image is built from - see
[`Dockerfile.ci`](../Dockerfile.ci):

```sh
pipx install amitools
pipx runpip amitools install --force-reinstall --no-deps \
     git+https://github.com/cnvogelg/machine68k.git
pipx runpip amitools install --force-reinstall --no-deps \
     git+https://github.com/cnvogelg/amitools.git
pipx runpip amitools install greenlet lhafile
```

It needs `python3-dev` and a C compiler, since `machine68k` builds a C
extension.

Then:

```sh
vamos factorial_am
```

```
1
2
6
24
120
```

Byte for byte what the host binary printed. That is the point of the exercise:
one source, two targets, the same answer.

## Where things can go wrong

**`command failed: m68k-amigaos-gcc`** - the path to the cross-compiler is
relative, so `--target amiga` only works with the repository root as your
working directory. Building a file that lives elsewhere is fine; being
elsewhere yourself is not.

**vamos prints nothing useful for a program that touches the hardware.** It
emulates AmigaOS libraries, not the Amiga. A program that stores to `$DFF000`
runs, and moves no blitter: the chipset is above what vamos implements. For
that you want FS-UAE or WinUAE, or the real machine.

**`./exilc` is stale after a `git pull`.** It is a build artifact, not a
checked-in binary. Re-run `make compiler` when the compiler's own sources
change.

## Where to go next

- **[Exile by Example](exile-by-example.md)** - the language, one feature at a
  time, from syntax through generics and traits to FFI and AmigaOS bindings.
- **[Section 22](exile-by-example.md#22-owning-the-hardware---rune-ward-sigil-seal)**
  - `rune`, `ward`, `sigil` and `seal`: how a driver claims a range of chip
  registers and has the compiler prove the claim, for zero bytes of output.
- **[examples/](../examples/)** - one program per feature, each with the output
  it is supposed to produce.
