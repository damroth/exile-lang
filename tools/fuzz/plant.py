#!/usr/bin/env python3
"""Plant / unplant the witness defect (FUZZ-SPEC §7a).

The plant lives OUTSIDE the capability model on purpose: constructs the frozen
oracle cannot parse are filtered out of F1, so a plant inside them would be
invisible to the class the witness measures.
"""
import sys

PATH = "src/codegen.exl"
GOOD = 'out.push_str("while (");'
BAD = 'out.push_str("while ( ");'

def main():
    mode = sys.argv[1]
    s = open(PATH).read()
    if mode == "plant":
        if GOOD not in s:
            print("plant: the plant site moved — re-choose it, do not skip the witness",
                  file=sys.stderr)
            return 2
        open(PATH, "w").write(s.replace(GOOD, BAD, 1))
    elif mode == "restore":
        if BAD in s:
            open(PATH, "w").write(s.replace(BAD, GOOD, 1))
    else:
        return 2
    return 0

if __name__ == "__main__":
    sys.exit(main())
