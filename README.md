<p align="center">
  <img src="https://github.com/damroth/exile-lang/actions/workflows/ci.yml/badge.svg?branch=master" />
  <img src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  <img src="https://img.shields.io/badge/status-WIP-orange" />
  <a href="https://github.com/damroth/exile-lang/releases"><img src="https://img.shields.io/github/v/release/damroth/exile-lang?include_prereleases&display_name=tag&sort=semver" /></a>

</p>

# exile-lang

A toy programming language that compiles to C, targeting Amiga m68k.

Work in progress. Compiler written in OCaml.

## Setup

```sh
git clone --recurse-submodules <repo-url>
cd exile-lang
make build       # build the exile compiler
make toolchain   # one-time: build the bundled m68k-amigaos cross-compiler
                 # (~30-60 minutes; output goes to _build/toolchain/)
```

If you cloned without `--recurse-submodules`:
```sh
git submodule update --init
```

The bundled cross-compiler is [Bebbo's amiga-gcc](https://codeberg.org/bebbo/amiga-gcc),
producing 68000 binaries by default — compatible with Amiga 1000 through 4000.

## Releases

See [CHANGELOG.md](CHANGELOG.md) for the version history. Tagged releases live on the
[Releases page](https://github.com/damroth/exile-lang/releases).
