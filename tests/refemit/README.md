# Fixtures where the reference COMPILES and its own output is the problem

`tests/refdefect` next door is for inputs the reference aborts on. These are not
those: both compilers accept every file here, and the divergence is in what comes
out. Two classes, by filename prefix, because the gate walks them differently.

## `named_*` - declarations the reference's own artefact never uses

Naming a prelude type whose methods touch strings makes the reference emit
`#include <string.h>` and struct declarations for that type's machinery. Its own
C then uses none of it: no `strlen`, no `memset`, no `memcpy`, and each extra
struct is declared once and referenced nowhere. The port materialises prelude
methods on demand, so it emits what it references and nothing else.

The gate does not compare against a fixed list of lines. It reads the diff and
requires that every line unique to the reference be an include or a struct
declaration, that the port add nothing of its own, and that each extra struct be
mentioned exactly once in the reference's output. The day one of them is used,
the difference stops being dead weight and the gate goes red - which is the only
thing that keeps a registered divergence from outliving its reason.

## `dropped_*` - a call the reference emits and a callee it removed

An uncalled method whose body calls a prelude method: the reference keeps the
body and drops the callee, so its C contains a call to a function it never
defines and the reference's own build fails to link. Compiling the file alone
succeeds, so the assertion is on the LINK and on the words `undefined reference`
rather than on an exit status - an ordinary failure exits non-zero too.

## The classes are ORDERED, not parallel

A shape can have both faces at once: an uncalled method whose body calls
`push_int` makes the reference emit the dead include AND drop the callee. The
rule is that the LINK decides. A fixture whose reference output does not link
belongs under `dropped_`, whatever else is also true of it.

This is not a filing preference. Class A asserts that the port adds nothing, and
for such a shape that is false - the port defines the whole chain the reference
dropped, some sixty lines of it. Filed as `named_` the fixture is red, and the
gate says which way to move it rather than leaving the reader to work it out.

That red is also the only proof available for class A's one-way assertion: every
other shape either diverges by declarations alone or uses a string, and a program
that uses a string makes the divergence vanish before the arm can fire.

## What both classes assert about the port

That its output builds, runs, and prints exactly the `.expected` file beside the
fixture. A registered divergence is only defensible while the side that diverges
is the side that works, so the gate makes that executable rather than claimed.

Every fixture needs its `.expected`; the gate refuses an empty one, refuses a
name without a class prefix, and refuses to run with fewer than two `named_`
fixtures - one shape is a lower bound, and the set of types that trigger this was
measured rather than assumed.
