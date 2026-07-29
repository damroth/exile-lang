#!/usr/bin/env python3
"""Differential fuzzer over the oracle/port pair — FUZZ-SPEC Increment 1.

The whole value of this tool is that it authors no expectations (I-F1): a
finding is a disagreement between two independent implementations of one
semantics, or a crash, or an emission that will not compile. Everything below
serves that and nothing else.

Increment 1 is deliberately the SPINE at small N: one generator strategy, the
three observables, a triage stub, and a signature-preserving shrinker — end to
end, so that finding -> shrink -> fixture works before volume arrives.
"""

import argparse
import os
import random
import re
import resource
import subprocess
import sys
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
ORACLE = os.path.join(ROOT, "_build/default/bin/main.exe")
PORT = os.path.join(ROOT, "_build/out/host/exilc")

# FUZZ-SPEC 3.3 / Z1 — shapes where the two sides differ BY DESIGN. Recombination
# manufactures these in bulk (a defer from one file, a loop from another), so
# without an explicit filter real findings drown in deliberate divergence. The
# filter is COUNTED, never silent (I-F4).
# FUZZ-SPEC 3.3 / Z1 — shapes where the two sides differ BY DESIGN.
#
# Increment 1 matched these with regexes over the SOURCE, and both of its
# patterns were wrong in opposite directions: the capability pattern anchored at
# line start, so `sys::sys_seal_*` calls slipped past it (false negative → noise),
# and the parenthesis pattern matched any `|`, including enum-variant separators
# and closure heads (false positive → eaten budget). A pattern guesses; Z4 asks
# for a mechanism.
#
# So a registered divergence is now recognised from what was OBSERVED — the two
# diagnostics and the two emissions — never from the shape of the input.

DIAG_POS = re.compile(r"INPUT:(\d+):(\d+):")


def diag_pos(diag):
    m = DIAG_POS.search(diag)
    return (m.group(1), m.group(2)) if m else None


CAP_RESERVED = re.compile(r"'(seal|ward|rune|sigil|own)' is a reserved word")
CAP_SEAM = re.compile(r"unknown function 'sys::sys_seal_(enter|exit)'")
NOT_PORTED = re.compile(r"not yet ported")
PARENS = re.compile(r"[()]")


def registered_divergence(ev):
    """Name the registered divergence that explains this evidence, or None.

    Each test is a MECHANISM on observed behaviour:
      #9  the oracle refuses a word the port implements — its own diagnostic says so
      #10 the port announces its own border in its own diagnostic
      #7  both compiled, and the emissions differ ONLY in parentheses
      #5  reserved for defer x loop-jump, which is a semantic difference rather
          than a lexical one and is still recognised from the source
    """
    if CAP_RESERVED.search(ev["oracle_diag"]) or CAP_SEAM.search(ev["oracle_diag"]):
        return "kernel-era-superset"          # register #9
    if NOT_PORTED.search(ev["port_diag"]):
        return "not-yet-ported"               # register #10
    # #11 — both reject, at DIFFERENT positions: each side found a different
    # error first, because their passes run in a different order. Same position
    # with different text is a real message divergence and stays B1.
    if (ev["oracle_status"] not in (0, None) and ev["port_status"] not in (0, None)
            and ev["oracle_diag"] != ev["port_diag"]
            and diag_pos(ev["oracle_diag"]) != diag_pos(ev["port_diag"])):
        return "multi-error-pass-order"       # register #11
    co, cp = ev["oracle_c"], ev["port_c"]
    if co is not None and cp is not None and co != cp:
        if PARENS.sub("", co) == PARENS.sub("", cp):
            return "R7-mixed-bitwise-parens"  # register #7 — parens ALONE differ
    if re.search(r"\bdefer\b", ev["src"]) and re.search(r"\b(break|continue)\b", ev["src"]):
        return "R5-defer-loopjump"            # register #5
    return None


# FUZZ-SPEC 3.1 — Increment 1's single strategy: CONSTRUCT-WRAPPING. A run of
# statements in a recipient body is wrapped in one construct, which composes that
# code with a construct it was never written next to. That is the class this
# era's defects actually lived in (seal x defer, seal x match-arm x return);
# donor-block splicing across files is Increment 3's job.
# `seal` IS in the vocabulary, on the corrected reading of 3.3: a capability
# construct blocks F1 for that input and nothing else, so seal-wraps are hunted
# with F2/F3/F4 — which is where this era's own defects would have surfaced.
WRAPPERS = [
    ("seal", "seal {", "}"),
    ("defer", "defer {", "}"),
    ("block", "{", "}"),
    ("if", "if true {", "}"),
    ("while", "while false {", "}"),
]


def read(path):
    with open(path, "r", encoding="utf-8", errors="replace") as fh:
        return fh.read()


def stmt_boundaries(src):
    """Offsets that plausibly start a statement inside a function body.

    Line-based and deliberately crude: a wrap that lands badly produces a parse
    error, which is a legitimate input whose death-stage is recorded (3.2). What
    it must never do is silently produce the ORIGINAL program, so callers check
    that the mutation changed the text.
    """
    out = []
    depth = 0
    for m in re.finditer(r"[^\n]*\n?", src):
        line = m.group(0)
        stripped = line.strip()
        if depth > 0 and stripped and not stripped.startswith("//"):
            out.append((m.start(), m.end(), depth))
        depth += line.count("{") - line.count("}")
    return out


def wrap(src, rng):
    """Apply one construct-wrap. Returns (text, note) or None."""
    bounds = [b for b in stmt_boundaries(src) if b[2] >= 1]
    if not bounds:
        return None
    i = rng.randrange(len(bounds))
    span = min(rng.randint(1, 3), len(bounds) - i)
    start = bounds[i][0]
    end = bounds[i + span - 1][1]
    name, open_tok, close_tok = WRAPPERS[rng.randrange(len(WRAPPERS))]
    body = src[start:end]
    indent = re.match(r"[ \t]*", body).group(0)
    new = f"{indent}{open_tok}\n{body}{indent}{close_tok}\n"
    out = src[:start] + new + src[end:]
    return (out, f"{name}@{start}+{span}") if out != src else None



KEYWORDS = {
    "let", "mut", "if", "else", "while", "for", "return", "break", "continue",
    "match", "fn", "struct", "enum", "impl", "trait", "mod", "use", "pub",
    "extern", "const", "static", "defer", "seal", "ward", "rune", "sigil", "own",
    "as", "true", "false", "type", "view", "new", "in", "where", "try", "self",
}

IDENT = re.compile(r"\b([a-z_][A-Za-z0-9_]*)\b")


def harvest(seeds, limit_per_file=12):
    """Whole, well-formed simple statements, for grafting.

    FUZZ-SPEC 3.1 named recombination; Increment 1 shipped only construct-wraps,
    whose inputs died in the parser 40% of the time. A grafted statement is
    parsable BY CONSTRUCTION — it already parsed where it came from — so the
    distribution moves toward the stages that have not been exercised.
    """
    pool = []
    for path in seeds:
        src = read(path)
        depth = 0
        taken = 0
        for line in src.split("\n"):
            body = line.strip()
            opens, closes = line.count("{"), line.count("}")
            if (depth > 0 and body.endswith(";") and not body.startswith("//")
                    and opens == closes and taken < limit_per_file
                    and not body.startswith(("return", "break", "continue", "use ", "pub "))
                    and len(body) < 90):
                pool.append(body)
                taken += 1
            depth += opens - closes
    return pool


def bindings_before(src, offset):
    """Names bound above `offset` — params of the enclosing fn, and `let`s."""
    head = src[:offset]
    names = set()
    for m in re.finditer(r"\blet\s+(?:mut\s+)?([a-z_][A-Za-z0-9_]*)", head):
        names.add(m.group(1))
    fn = None
    for m in re.finditer(r"\bfn\s+[A-Za-z0-9_]*\s*\(([^)]*)\)", head):
        fn = m.group(1)
    if fn:
        for part in fn.split(","):
            m = re.match(r"\s*(?:mut\s+)?([a-z_][A-Za-z0-9_]*)\s*:", part)
            if m:
                names.add(m.group(1))
    return sorted(names)


def fixup_bindings(stmt, avail, rng):
    """Rebind the graft's free names to names that exist HERE.

    Single biggest lever from parse-survival to typecheck-survival: a grafted
    statement parses anywhere, but `undefined variable` kills it immediately
    unless its names mean something at the destination.
    """
    if not avail:
        return stmt
    local = set(m.group(1) for m in re.finditer(
        r"\blet\s+(?:mut\s+)?([a-z_][A-Za-z0-9_]*)", stmt))

    def sub(m):
        name = m.group(1)
        if name in KEYWORDS or name in local:
            return name
        start, end = m.span()
        after = stmt[end:end + 2]
        if after.startswith("(") or after.startswith("::"):
            return name          # a call or a path — leave it alone
        if stmt[max(0, start - 1):start] == ".":
            return name          # a field or method name
        return avail[rng.randrange(len(avail))]

    return IDENT.sub(sub, stmt)


def self_pool(src):
    """Statements harvested from the RECIPIENT itself.

    Measured lever: a cross-file graft parses but dies in typecheck, because the
    fixup can rebind names and cannot reconcile TYPES. A statement lifted from
    the same program already has both — so the composition is new while the
    types stay sound, which is the combination the witness needs.
    """
    out = []
    depth = 0
    for line in src.split("\n"):
        body = line.strip()
        opens, closes = line.count("{"), line.count("}")
        if (depth > 0 and body.endswith(";") and not body.startswith("//")
                and opens == closes and len(body) < 90
                and not body.startswith(("return", "break", "continue", "use ", "pub "))):
            out.append(body)
        depth += opens - closes
    return out


def graft(src, rng, pool):
    """Splice one donor statement into a statement position (FUZZ-SPEC 3.1)."""
    mine = self_pool(src)
    if mine and rng.random() < 0.8:
        pool = mine
    if not pool:
        return None
    bounds = [b for b in stmt_boundaries(src) if b[2] >= 1]
    if not bounds:
        return None
    i = rng.randrange(len(bounds))
    start = bounds[i][0]
    donor = pool[rng.randrange(len(pool))]
    fixed = fixup_bindings(donor, bindings_before(src, start), rng)
    indent = re.match(r"[ \t]*", src[start:]).group(0)
    out = src[:start] + f"{indent}{fixed}\n" + src[start:]
    return (out, f"graft@{start}") if out != src else None


def generate(seeds, rng, wraps, pool=None, rate=0.25):
    base = seeds[rng.randrange(len(seeds))]
    src = read(base)
    notes = []
    for _ in range(wraps):
        # Grafts dominate on purpose: they preserve parsability by construction,
        # while wraps are what produce the CROSSINGS this era's defects lived in.
        got = graft(src, rng, pool) if (pool and rng.random() < rate) else wrap(src, rng)
        if got is None:
            got = wrap(src, rng)
        if got is None:
            break
        src, note = got
        notes.append(note)
    if not notes:
        return None
    return src, os.path.relpath(base, ROOT), notes


def run(binary, path, cout, budget_s, rss_mb):
    """Run one compiler. Returns (status, first_stderr_line, c_text, kind).

    kind is '' normally, 'ice' for FUZZ-SPEC F2, 'timeout'/'rss' for F4 — each
    detected by the MECHANISM 5.1 names, not by a label.
    """
    def limit():
        resource.setrlimit(resource.RLIMIT_AS, (rss_mb * 1024 * 1024,) * 2)

    try:
        p = subprocess.run(
            [binary, "--target", "c", "--c-out", cout, path],
            capture_output=True, text=True, timeout=budget_s, preexec_fn=limit,
        )
    except subprocess.TimeoutExpired:
        return (None, "", None, "timeout")
    except MemoryError:
        return (None, "", None, "rss")

    err = p.stderr or ""
    first = next((ln for ln in err.splitlines() if ln.strip()), "")
    kind = ""
    if p.returncode == 134 or "internal:" in err:
        kind = "ice"
    elif p.returncode < 0:
        kind = "ice"
    c = None
    if os.path.exists(cout):
        c = read(cout)
        os.unlink(cout)
    return (p.returncode, first, c, kind)


def strip_pos(line, path):
    """Diagnostics carry the input path; the two runs use different temp files."""
    return line.replace(path, "INPUT")


def cc_check(c):
    """FUZZ-SPEC F3: does this emission survive the project's own standard?"""
    r = subprocess.run(
        ["cc", "-O2", "-ansi", "-pedantic", "-Wall", "-Werror", "-I", "src",
         "-c", "-x", "c", "-", "-o", "/dev/null"],
        input=c, capture_output=True, text=True, cwd=ROOT)
    if r.returncode == 0:
        return None
    first = next((ln for ln in r.stderr.splitlines() if ": error:" in ln), "")
    return first.split(": error:")[-1].strip()[:90]


def observe(src, tmp_o, tmp_p, args):
    """Run both sides and record WHAT HAPPENED. No judgement here on purpose —
    triage is a separate step so its verdicts rest on evidence rather than on
    whatever the classifier happened to notice first (Z4)."""
    so, eo, co, ko = run(ORACLE, tmp_o, tmp_o + ".c", args.budget, args.rss)
    sp, ep, cp, kp = run(PORT, tmp_p, tmp_p + ".c", args.budget, args.rss)
    return {
        "src": src,
        "oracle_status": so, "port_status": sp,
        "oracle_diag": strip_pos(eo, tmp_o), "port_diag": strip_pos(ep, tmp_p),
        "oracle_c": co, "port_c": cp,
        "oracle_kind": ko, "port_kind": kp,
    }


def classify(ev, args):
    """(kind, signature) or (None, None). Signature drives the shrinker (Z5).

    A registered divergence disqualifies F1 and F1 ONLY (3.3): F2/F3/F4 are
    properties of one side, so they stay live.
    """
    ko, kp = ev["oracle_kind"], ev["port_kind"]
    if ko in ("timeout", "rss") or kp in ("timeout", "rss"):
        side = "oracle" if ko else "port"
        return "F4", f"F4:{side}:{ko or kp}"
    if ko == "ice" or kp == "ice":
        side = "oracle" if ko == "ice" else "port"
        msg = ev["oracle_diag"] if ko == "ice" else ev["port_diag"]
        return "F2", f"F2:{side}:{msg[:90]}"

    blocked = registered_divergence(ev)
    so, sp = ev["oracle_status"], ev["port_status"]
    if not blocked:
        if so != sp:
            return "F1", f"F1:status:{so}vs{sp}"
        if ev["oracle_diag"] != ev["port_diag"]:
            return "F1", f"F1:diag:oracle[{ev['oracle_diag'][:60]}]port[{ev['port_diag'][:60]}]"
        if so == 0 and ev["oracle_c"] is not None and ev["port_c"] is not None \
                and ev["oracle_c"] != ev["port_c"]:
            return "F1", "F1:emitted-c"
    if sp == 0 and ev["port_c"] is not None and args.cc:
        key = cc_check(ev["port_c"])
        if key:
            return "F3", f"F3:{key}"
    return None, None


def triage(kind, sig, ev):
    """FUZZ-SPEC §6 — three buckets, assigned by MECHANISM (Z4).

    "The port differs" is an observation. Each verdict below is a narrowing
    measurement that ends in one variable: WHICH SIDE misbehaved, or WHICH
    registered mechanism explains the difference. Where no measurement decides,
    the answer is B? — an honest absence, never a guess wearing a bucket's name.
    """
    reg = registered_divergence(ev)

    # F2/F4 are properties of ONE side, so the side IS the one variable.
    if kind in ("F2", "F4"):
        side = "oracle" if sig.split(":")[1] == "oracle" else "port"
        if side == "oracle":
            return "B3", "the FROZEN reference crashed or hung; it cannot be fixed, only registered"
        return "B1", "the port crashed or hung where the reference completed"

    # F3 needs no divergence, so the one variable is whether the two agree.
    if kind == "F3":
        if ev["oracle_c"] is not None and ev["oracle_c"] == ev["port_c"]:
            return "B3", "both emit the same C and it fails -Werror — the defect is the reference's, and it is frozen"
        if reg:
            return "B2", f"emissions differ only by {reg}; the failing side is the port's improvement"
        return "B1", "the port's emission alone fails -Werror"

    # F1: the one variable is whether a registered mechanism explains it.
    if reg:
        return "B2", f"explained by {reg}, recognised from observed behaviour"
    return "B1", "the reference defines the behaviour and the port does not reproduce it"


def write_tmp(d, name, src):
    path = os.path.join(d, name)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(src)
    return path


def shrink(src, sig, tmpdir, args, rounds=200):
    """Line-wise minimisation that preserves the SIGNATURE, not just the predicate.

    Z5: a reduction that still fails but fails DIFFERENTLY is rejected, because a
    shrinker without a signature hands back a fixture for someone else's defect.
    """
    best = src
    changed = True
    guard = 0
    while changed and guard < rounds:
        changed = False
        lines = best.split("\n")
        i = 0
        while i < len(lines):
            guard += 1
            if guard >= rounds:
                break
            cand = "\n".join(lines[:i] + lines[i + 1:])
            if not cand.strip():
                i += 1
                continue
            o = write_tmp(tmpdir, "shrink_o.exl", cand)
            p = write_tmp(tmpdir, "shrink_p.exl", cand)
            k, s = classify(observe(cand, o, p, args), args)
            if s == sig:
                best = cand
                lines = cand.split("\n")
                changed = True
            else:
                i += 1
    return best



def selftest(args):
    """Recogniser floors, both directions (FUZZ-SPEC 3.3).

    A filter's false POSITIVE eats budget in silence; its false NEGATIVE drowns
    real findings in noise. Increment 1 shipped one of each, so the cases below
    assert what MUST be recognised AND what must NOT be.
    """
    cases_path = os.path.join(ROOT, "tests/fuzzfilter/CASES")
    if not os.path.exists(cases_path):
        print("fuzz-filters: MISSING tests/fuzzfilter/CASES", file=sys.stderr)
        return 2
    tmp = os.path.join(ROOT, "_build/out/fuzz")
    os.makedirs(tmp, exist_ok=True)
    rows, bad = 0, 0
    for line in read(cases_path).split("\n"):
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        name, want = [x.strip() for x in line.split("|")]
        fx = os.path.join(ROOT, "tests/fuzzfilter", name + ".exl")
        if not os.path.exists(fx):
            print(f"fuzz-filters: case '{name}' has no fixture {fx}")
            return 1
        rows += 1
        src = read(fx)
        o = write_tmp(tmp, "sel_o.exl", src)
        pp = write_tmp(tmp, "sel_p.exl", src)
        got = registered_divergence(observe(src, o, pp, args)) or "none"
        if got != want:
            bad += 1
            direction = ("FALSE POSITIVE — filtered something that must stay live"
                         if want == "none" else
                         "FALSE NEGATIVE — did not recognise a registered divergence"
                         if got == "none" else "WRONG recogniser")
            print(f"fuzz-filters: {name}: wanted {want}, got {got}  ({direction})")
    for fx in sorted(os.listdir(os.path.join(ROOT, "tests/fuzzfilter"))):
        if fx.endswith(".exl") and f"\n{fx[:-4]}|" not in "\n" + read(cases_path):
            print(f"fuzz-filters: tests/fuzzfilter/{fx} has NO row in CASES")
            return 1
    if bad:
        return 1
    print(f"fuzz-filters: clean ({rows} recogniser cases, both directions: "
          f"every registered shape recognised, every non-shape left live)")
    return 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("-n", type=int, default=200, help="inputs to generate")
    ap.add_argument("--wraps", type=int, default=2)
    ap.add_argument("--graft-rate", type=float, default=0.25,
                    help="share of mutations that are statement-grafts (Z2 steers this)")
    ap.add_argument("--budget", type=float, default=10.0, help="seconds per compile (F4)")
    ap.add_argument("--rss", type=int, default=2048, help="MB per compile (F4)")
    ap.add_argument("--cc", action="store_true", help="enable the F3 class")
    ap.add_argument("--out", default=None, help="directory for findings")
    ap.add_argument("--shrink", action="store_true")
    ap.add_argument("--quiet", action="store_true")
    ap.add_argument("--selftest", action="store_true",
                    help="run the recogniser floors instead of fuzzing")
    args = ap.parse_args()

    if args.selftest:
        for b in (ORACLE, PORT):
            if not os.path.exists(b):
                print(f"fuzz: MISSING {b}", file=sys.stderr)
                return 2
        return selftest(args)

    for b in (ORACLE, PORT):
        if not os.path.exists(b):
            print(f"fuzz: MISSING {b} — build both sides first", file=sys.stderr)
            return 2

    seeds = []
    for sub in ("examples", "tests"):
        for dirpath, _dirs, files in os.walk(os.path.join(ROOT, sub)):
            for f in files:
                if f.endswith(".exl"):
                    seeds.append(os.path.join(dirpath, f))
    seeds.sort()
    if not seeds:
        print("fuzz: no corpus seeds found", file=sys.stderr)
        return 2

    pool = harvest(seeds)
    rng = random.Random(args.seed)
    tmpdir = args.out or os.path.join(ROOT, "_build/out/fuzz")
    os.makedirs(tmpdir, exist_ok=True)

    stages = {"gen-fail": 0, "parse": 0, "typecheck": 0, "ok": 0}
    filtered = {}
    findings = []
    t0 = time.time()

    for i in range(args.n):
        got = generate(seeds, rng, args.wraps, pool, args.graft_rate)
        if got is None:
            stages["gen-fail"] += 1
            continue
        src, base, notes = got

        o = write_tmp(tmpdir, "cand_o.exl", src)
        p = write_tmp(tmpdir, "cand_p.exl", src)
        ev = observe(src, o, p, args)
        # Per-CLASS, never per-input: a registered divergence blocks F1 and
        # leaves F2/F3/F4 live. Counted, never silent (I-F4).
        skip = registered_divergence(ev)
        if skip:
            filtered[skip] = filtered.get(skip, 0) + 1
        kind, sig = classify(ev, args)

        # death-per-stage (Z2), read off the PORT's own behaviour
        st, first, _c, _k = run(PORT, p, p + ".c", args.budget, args.rss)
        if st == 0:
            stages["ok"] += 1
        elif "expected" in first or "unexpected" in first:
            stages["parse"] += 1
        else:
            stages["typecheck"] += 1

        if kind:
            body = shrink(src, sig, tmpdir, args) if args.shrink else src
            bucket, why = triage(kind, sig, ev)
            findings.append((kind, bucket, sig, base, notes, body, why))
            if not args.quiet:
                print(f"fuzz: {kind} [{bucket}] {sig}  (from {base}, {'+'.join(notes)})")
                print(f"fuzz:      mechanism: {why}")

    dt = time.time() - t0
    total = sum(stages.values())
    print(f"fuzz: graft pool={len(pool)} statements, graft-rate={args.graft_rate}")
    print(f"fuzz: seed={args.seed} inputs={args.n} compiled={total} "
          f"time={dt:.1f}s budget={args.budget}s/{args.rss}MB")
    print("fuzz: death-per-stage " + " ".join(f"{k}={v}" for k, v in stages.items()))
    if filtered:
        print("fuzz: filtered (registered divergences) " +
              " ".join(f"{k}={v}" for k, v in sorted(filtered.items())))
    else:
        print("fuzz: filtered (registered divergences) none")
    print(f"fuzz: findings={len(findings)}")

    buckets = {}
    for f in findings:
        buckets[f[1]] = buckets.get(f[1], 0) + 1
    if findings:
        print("fuzz: buckets " + " ".join(f"{k}={v}" for k, v in sorted(buckets.items())))
    for idx, (kind, bucket, sig, base, notes, body, why) in enumerate(findings):
        path = os.path.join(tmpdir, f"finding_{args.seed}_{idx}_{kind}.exl")
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(f"// fuzz finding: {kind} [{bucket}] {sig}\n"
                     f"// bucket by mechanism: {why}\n"
                     f"// seed {args.seed}, from {base}, wraps {'+'.join(notes)}\n"
                     f"// FUZZ-SPEC I-F3: the expected side is CAPTURED from whichever\n"
                     f"// implementation triage judges correct — never authored here.\n"
                     f"{body}")
        print(f"fuzz: wrote {os.path.relpath(path, ROOT)}")

    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
