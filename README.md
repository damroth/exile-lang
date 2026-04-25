# exile-lang

A toy programming language that compiles to C, targeting Amiga m68k.

Work in progress. Transpiler written in OCaml.

## Setup

```sh
git clone --recurse-submodules <repo-url>
cd exile-lang
make build       # build the exile transpiler
make toolchain   # one-time: build the bundled m68k-amigaos cross-compiler
                 # (~30-60 minutes; output goes to _build/toolchain/)
```

If you cloned without `--recurse-submodules`:
```sh
git submodule update --init
```

The bundled cross-compiler is [Bebbo's amiga-gcc](https://codeberg.org/bebbo/amiga-gcc),
producing 68000 binaries by default — compatible with Amiga 1000 through 4000.
