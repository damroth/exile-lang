# The vendored 68000 core

`musashi/` is a copy of Karl Stenerud's Musashi, upstream commit
`313ebf1bd9f4d0d93341eb5ce21fd8a119e9dbdd`, with the upstream `test/`,
`example/` and version-control metadata removed and nothing else changed. The
generated opcode tables are NOT vendored: the build runs the upstream generator,
which also keeps the generator honest.

## Licences

- **Musashi** - the MIT licence, stated in the header of every source file
  ("LICENSING & COPYRIGHT"), Copyright Karl Stenerud. There is no separate
  `LICENSE` file upstream, so the notices live where upstream put them and must
  not be stripped from these files.
- **SoftFloat release 2b** (`musashi/softfloat/`) - by John R. Hauser, under the
  notice reproduced in `musashi/softfloat/README.txt`: free to use and
  redistribute with the notice, and offered with no warranty. It arrives because
  the core includes it unconditionally for the FPU; it is not cherry-picked out,
  because trimming a working core to look smaller is how a verification tool
  acquires a defect of its own.

## Why this core, measured rather than assumed

The emulator exists to tell the truth about the compiler, so a core whose own
correctness is unknown is the worst possible dependency: its bugs would arrive
disguised as language bugs. "Has users" was therefore treated as a claim to
measure, not a reason to state.

| | Musashi | Moira |
|---|---|---|
| licence | MIT, in every file header | **NOASSERTION** - not identifiable without legal work |
| users (stars / forks) | 633 / 125 | 145 / 17 |
| language | C, as the rest of this repo's tooling | C++ |
| builds here | clean at `-O2`, **zero warnings** | not attempted - the licence settles it first |
| last upstream push | 2026-03-08 | 2026-09-01 |

Moira is the newer and more actively pushed of the two and would have been a
reasonable choice on every axis but the first. A licence that cannot be named is
not a licence a repository can ship, and that decided it before the other columns
mattered.

Writing our own core was measured too and declined: the whole bare-metal witness
uses 47 distinct mnemonics with no multiply and no divide, which makes an
interpreter look tractable - but tractable is not the same as trustworthy, and the
half of this problem we actually know something about is the CHIPSET, not the CPU.
