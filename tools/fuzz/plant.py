#!/usr/bin/env python3
"""Plant / unplant the witness defect (FUZZ-SPEC 7a).

The plant lives OUTSIDE the capability model on purpose: constructs the frozen
oracle cannot parse are filtered out of F1, so a plant inside them would be
invisible to the class the witness measures.

It is ANCHORED to a function, not to a string. The first version replaced the
first occurrence of `while (` in the file, and a later round added a `TWhile`
arm to `emit_simple_stmt` ABOVE `gen_while` - so the plant moved, silently, into
the emitter for `defer` BODIES, which an ordinary loop never reaches. The old
guard checked that the string still existed and was therefore happy: shape, not
position. The witness then went green on a real stream finding while the planted
defect was never in the artifact at all.
"""
import sys

PATH = "src/codegen.exl"
ANCHOR = "fn gen_while("
GOOD = 'out.push_str("while (");'
BAD = 'out.push_str("while ( ");'


def site(s):
    """The offset of the plant site INSIDE the anchored function."""
    a = s.find(ANCHOR)
    if a < 0:
        return -1, "the anchor `%s` is gone - re-choose the plant site, do not skip the witness" % ANCHOR
    i = s.find(GOOD, a)
    if i < 0:
        return -1, "the anchored emitter no longer contains the plant string - re-choose it"
    # The site must be the one inside gen_while, not an earlier namesake.
    if s.count(GOOD, 0, a) and i < a:
        return -1, "the plant resolved above its anchor"
    return i, None


def main():
    mode = sys.argv[1]
    s = open(PATH).read()
    if mode == "plant":
        i, err = site(s)
        if i < 0:
            print("plant: " + err, file=sys.stderr)
            return 2
        open(PATH, "w").write(s[:i] + BAD + s[i + len(GOOD):])
    elif mode == "restore":
        a = s.find(ANCHOR)
        if a >= 0:
            j = s.find(BAD, a)
            if j >= 0:
                open(PATH, "w").write(s[:j] + GOOD + s[j + len(BAD):])
    else:
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
