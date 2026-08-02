#!/usr/bin/env python3
"""The differential fuzzer's STATED limits, as ACCEPT contracts.

A limit is a boundary the tool STATES, not one it enforces, and the way this
project keeps the two apart is to make each limit a fixture that must keep
behaving as claimed. A limit nobody measures drifts into folklore; one measured
here goes red the day a future round closes it, forcing that to be a decision
rather than a side effect.

One limit is deliberately NOT pinned, and saying so beats padding the table:
a new IR node dropped by a catch-all walker cannot be fixtured, because the
fixture would have to contain a node the grammar does not yet have. That is the
limit describing itself.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fuzz


class A:
    budget = 10.0
    rss = 2048
    cc = True


def observe(path):
    tmp = os.path.join(fuzz.ROOT, "_build/out/fuzz")
    os.makedirs(tmp, exist_ok=True)
    src = fuzz.read(path)
    o = fuzz.write_tmp(tmp, "lim_o.exl", src)
    p = fuzz.write_tmp(tmp, "lim_p.exl", src)
    return fuzz.observe(src, o, p, A())


def main():
    fails = []
    base = os.path.join(fuzz.ROOT, "tests/fuzzlimits")
    need = ["capability_outside_f1.exl", "not_yet_ported.exl", "agreement_is_silence.exl"]
    for n in need:
        if not os.path.exists(os.path.join(base, n)):
            print(f"fuzz-limits: MISSING tests/fuzzlimits/{n}", file=sys.stderr)
            return 2

    # L2 - capability: F1 blocked, port-side classes still live on the SAME input.
    ev = observe(os.path.join(base, "capability_outside_f1.exl"))
    if fuzz.registered_divergence(ev) != "kernel-era-superset":
        fails.append("capability input is not recognised as register #9 - F1 would judge what the oracle cannot parse")
    if ev["port_status"] != 0 or ev["port_c"] is None:
        fails.append("capability input produced no port emission - F2/F3/F4 have nothing to hunt, and the limit would become a blanket exclusion")

    # L3 - the announced boundary, recognised from the DIAGNOSTIC.
    ev = observe(os.path.join(base, "not_yet_ported.exl"))
    if fuzz.registered_divergence(ev) != "not-yet-ported":
        fails.append("the port's announced boundary is not recognised - register #10 would be rediscovered every run")

    # L4 / L6 - agreement is silence, whatever the agreement is worth.
    ev = observe(os.path.join(base, "agreement_is_silence.exl"))
    kind, _sig = fuzz.classify(ev, A())
    if kind is not None:
        fails.append(f"a program both sides agree on produced a {kind} finding - the comparator is judging, not comparing")

    # L5 - absence proves nothing, so a zero-finding run may never be quotable
    # without the budget that produced it.
    import subprocess
    out = subprocess.run([sys.executable, os.path.join(fuzz.ROOT, "tools/fuzz/fuzz.py"),
                          "--seed", "3", "-n", "5", "--quiet"],
                         capture_output=True, text=True, cwd=fuzz.ROOT).stdout
    line = next((l for l in out.splitlines() if l.startswith("fuzz: seed=")), "")
    if not ("inputs=" in line and "budget=" in line):
        fails.append("a run reports findings without stating seed / inputs / budget - 'findings=0' would be quotable on its own")

    for f in fails:
        print(f"fuzz-limits: {f}", file=sys.stderr)
    if fails:
        return 1
    print("fuzz-limits: clean (4 limits pinned as contracts: capability outside F1 with the "
          "port-side classes still live, the announced not-yet-ported boundary, agreement "
          "producing silence, and no zero-finding run quotable without its budget; the "
          "new-IR-node limit is NOT pinnable and says so)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
