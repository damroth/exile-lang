<p align="center">
  <img src="https://github.com/damroth/exile-lang/actions/workflows/ci.yml/badge.svg?branch=master" />
  <img src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  <img src="https://img.shields.io/badge/status-WIP-orange" />
  <a href="https://github.com/damroth/exile-lang/releases"><img src="https://img.shields.io/github/v/release/damroth/exile-lang?include_prereleases&display_name=tag&sort=semver" /></a>

</p>

# exile-lang

A toy programming language that compiles to C, targeting Amiga m68k.

Work in progress. The reference compiler is written in OCaml; exile-lang is also
**self-hosting** — `src/*.exl` is the compiler written in Exile, and it emits C
byte-identical to the reference for its own source. So you can build it either
way: through OCaml, or from the committed seed with nothing but a C compiler.

## Setup

```sh
git clone --recurse-submodules <repo-url>
cd exile-lang
make build       # build the exile compiler
make toolchain   # one-time: build the bundled m68k-amigaos cross-compiler
                 # (~30-60 minutes; output goes to _build/toolchain/)
```

### Build without OCaml (from the seed)

`seed/exilc.c` is the self-hosted compiler's own C output, committed so a fresh
clone can bootstrap with no OCaml, no opam, and no dune:

```sh
make bootstrap-from-seed   # cc seed/exilc.c -> exilc -> rebuilds itself
```

This walks the ladder and checks it lands: the seed builds a compiler, that
compiler compiles the current `src/*.exl`, the result compiles the source again,
and the last two outputs must be byte-identical. CI runs it on a plain runner
with no OCaml on `PATH`.

The seed is a deliberate snapshot, not a mirror of `HEAD` — it is allowed to lag,
and is refreshed (`make seed`) only when `bootstrap-from-seed` reports it can no
longer build the current source.

If you cloned without `--recurse-submodules`:
```sh
git submodule update --init
```

The bundled cross-compiler is [Bebbo's amiga-gcc](https://codeberg.org/bebbo/amiga-gcc),
producing 68000 binaries by default — compatible with Amiga 1000 through 4000.

## Releases

See [CHANGELOG.md](CHANGELOG.md) for the version history. Tagged releases live on the
[Releases page](https://github.com/damroth/exile-lang/releases).
