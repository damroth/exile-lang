#!/usr/bin/env python3
"""Differential fuzzer over the oracle/port pair — FUZZ-SPEC Increment 1.

The whole value of this tool is that it authors no expectations (I-F1): a
finding is a disagreement between two independent implementations of one
semantics, or a crash, or an emission that will not compile. Everything below
serves that and nothing else.

Increment 1 was the SPINE at small N: one generator strategy, the three
observables, a triage stub, and a signature-preserving shrinker. Increment 2 gave
the comparator its filter and its buckets. Increment 3 is the GENERATOR:
cross-file construct implantation (FUZZ-SPEC 3.1's actual sentence), statement
ranges that a wrap cannot cut in half, and an operator mix steered by the
measured death-per-stage distribution rather than by a constant on the command
line (3.2 — "the distribution must MEASURE and STEER, not be discovered later as
an excuse").
"""

import argparse
import os
import random
import re
import resource
import subprocess
import sys
import textwrap
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


def msg_only(diag):
    """The message with its position normalised away — what a signature should
    key on, so shrinking a line above the defect does not look like a different
    defect."""
    return DIAG_POS.sub("INPUT:", diag)


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


STRLIT = re.compile(r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'')
LINE_COMMENT = re.compile(r"//.*$")
CONTAINER = re.compile(
    r"^\s*(?:pub\s+)?(?:extern\s+)?(fn|impl|struct|enum|trait|mod|match|union|view)\b")


def _code(line):
    """The line with string literals and comments blanked, so delimiter counting
    is not fooled by a `{` inside a message."""
    return LINE_COMMENT.sub("", STRLIT.sub('""', line))


def stmt_boundaries(src):
    """Statement RANGES inside function bodies: (start, end, depth).

    Increment 1 offered every line at brace depth > 0, so a wrap could cut a
    multi-line expression in half. That was the single largest cause of death on
    the shipped stream — `expected expression`, 154 of 600 inputs at seed 1,
    ahead of every other message. A range now begins only where a statement can
    begin (paren/bracket depth zero, previous line closed) and ends where that
    statement closes with its delimiters back where they started.
    """
    rows = []
    depth = inner = 0
    stack = []
    stacks = []
    for m in re.finditer(r"[^\n]*\n?", src):
        line = m.group(0)
        if not line:
            break
        code = _code(line)
        db = code.count("{") - code.count("}")
        di = (code.count("(") - code.count(")")
              + code.count("[") - code.count("]"))
        rows.append((m.start(), m.end(), depth, inner, code.strip(), db, di))
        stacks.append(list(stack))
        kind_m = CONTAINER.match(code)
        kind = kind_m.group(1) if kind_m else "block"
        for _ in range(max(0, code.count("{") - code.count("}"))):
            stack.append(kind)
        for _ in range(max(0, code.count("}") - code.count("{"))):
            if stack:
                stack.pop()
        depth += db
        inner += di

    starts = []
    prev_closed = True
    for r, st in zip(rows, stacks):
        text = r[4]
        # A statement position is inside a FUNCTION BODY. Brace depth alone also
        # matches an `enum` variant list, a `struct` field list, an `impl` body
        # and a `match` arm list — and wrapping one of those is what produced the
        # stream's loudest divergences: `expected '|' (next variant)`, `expected
        # 'fn' or 'type' inside 'impl' block`, and every `parser: this item kind
        # not yet ported` (which no seed produces unmutated — measured: 0 of 250).
        inside_fn = "fn" in st and st[-1:] in (["fn"], ["block"])
        starts.append(bool(inside_fn and r[3] == 0 and prev_closed and text
                           and not text.startswith(
                               ("|", "=>", "else", "}", ")", "]", "@", "#"))))
        if text:
            prev_closed = text.endswith((";", "{", "}"))

    out = []
    for i, r in enumerate(rows):
        if not starts[i]:
            continue
        d, inn = r[2], r[3]
        cd, ci = d, inn
        for j in range(i, len(rows)):
            cd += rows[j][5]
            ci += rows[j][6]
            if cd == d and ci == inn and rows[j][4].endswith((";", "}")):
                out.append((r[0], rows[j][1], d))
                break
    return out


def _span(bounds, i, want):
    """Extend a start index over ADJACENT ranges at the same depth.

    Ranges nest, so consecutive entries are not necessarily siblings; taking
    `bounds[i+span-1][1]` blindly is how a span comes to end inside itself.
    """
    d, start, end, n = bounds[i][2], bounds[i][0], bounds[i][1], 1
    j = i + 1
    while n < want and j < len(bounds):
        if bounds[j][2] == d and bounds[j][0] == end:
            end = bounds[j][1]
            n += 1
            j += 1
        else:
            break
    return start, end, n


BOUND = re.compile(r"\blet\s+(?:mut\s+)?([a-z_][A-Za-z0-9_]*)")


def _rest_of_block(src, end, depth):
    """Text from `end` to the close of the block the span sits in."""
    out = []
    d = depth
    for line in src[end:].split("\n"):
        code = _code(line)
        out.append(code)
        d += code.count("{") - code.count("}")
        if d < depth:
            break
    return "\n".join(out)


def _escapes(src, start, end, depth):
    """Does the span bind a name that is USED after it in the same block?

    Wrapping such a span moves the binding into a nested scope, and since the
    block-scope fix the port correctly answers `undefined variable` — 53 of 600
    inputs at seed 1, the largest non-parse killer. The wrap is legal; it just
    stops testing anything past name resolution.
    """
    names = set(m.group(1) for m in BOUND.finditer(src[start:end]))
    if not names:
        return False
    rest = _rest_of_block(src, end, depth)
    return any(re.search(r"\b%s\b" % re.escape(n), rest) for n in names)


def wrap(src, rng, avoid_escape=True):
    """Apply one construct-wrap. Returns (text, note) or None."""
    bounds = stmt_boundaries(src)
    if not bounds:
        return None
    order = list(range(len(bounds)))
    rng.shuffle(order)
    fallback = None
    for i in order:
        start, end, n = _span(bounds, i, rng.randint(1, 3))
        name, open_tok, close_tok = WRAPPERS[rng.randrange(len(WRAPPERS))]
        body = src[start:end]
        indent = re.match(r"[ \t]*", body).group(0)
        new = f"{indent}{open_tok}\n{body}{indent}{close_tok}\n"
        out = src[:start] + new + src[end:]
        if out == src:
            continue
        got = (out, f"{name}@{start}+{n}")
        if avoid_escape and _escapes(src, start, end, bounds[i][2]):
            fallback = fallback or got
            continue
        return got
    return fallback



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


CONSTRUCT_LOCAL = re.compile(
    r"\blet\s+(?:mut\s+)?([a-z_][A-Za-z0-9_]*)"
    r"|\bfor\s+([a-z_][A-Za-z0-9_]*)\s+in\b"
    r"|\bwith\s+([a-z_][A-Za-z0-9_]*)\s+in\b"
    r"|::[A-Za-z0-9_]+\s*[({]\s*([a-z_][A-Za-z0-9_]*)")


def construct_locals(text):
    """Names a whole construct binds for itself — `let`, loop and projection
    counters, and the first payload name of a pattern. Rewriting one of these
    breaks the construct, which a statement-level fixup never had to consider."""
    out = set()
    for m in CONSTRUCT_LOCAL.finditer(text):
        for g in m.groups():
            if g:
                out.add(g)
    return out


def fixup_bindings(stmt, avail, rng, local=None):
    """Rebind the graft's free names to names that exist HERE.

    Single biggest lever from parse-survival to typecheck-survival: a grafted
    statement parses anywhere, but `undefined variable` kills it immediately
    unless its names mean something at the destination.
    """
    if not avail:
        return stmt
    if local is None:
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
    # The graft's OWN `let` names are minted fresh. A donor that binds `v` lands
    # next to the recipient's `v` and is rejected by the shadowing rule this
    # session landed — measured as this operator's dominant death (82 of 349
    # single grafts opened with `variable ...`), and a rejection that says
    # nothing about the composition the graft was made to test.
    donor = re.sub(r"\blet(\s+(?:mut\s+)?)([a-z_][A-Za-z0-9_]*)",
                   lambda m: f"let{m.group(1)}g{rng.randrange(1 << 20):x}_{m.group(2)}",
                   donor)
    fixed = fixup_bindings(donor, bindings_before(src, start), rng)
    indent = re.match(r"[ \t]*", src[start:]).group(0)
    out = src[:start] + f"{indent}{fixed}\n" + src[start:]
    return (out, f"graft@{start}") if out != src else None


CONSTRUCT_HEAD = re.compile(r"^(match|while|for|if|defer|seal|loop|with)\b")


def free_names(text):
    """Lowercase identifiers a construct uses without binding — every one of them
    is something the destination has to supply."""
    local = construct_locals(text)
    out = set()
    for m in IDENT.finditer(text):
        name = m.group(1)
        end = m.end()
        if (name in KEYWORDS or name in local
                or text[end:end + 1] == "(" or text[end:end + 2] == "::"
                or text[max(0, m.start() - 1):m.start()] == "."):
            continue
        out.add(name)
    return out


def harvest_constructs(seeds, limit_per_file=8, max_lines=16):
    """Whole CONSTRUCTS lifted from corpus programs, for CROSS-FILE implantation.

    FUZZ-SPEC 3.1's sentence is specific: "it takes a construct from one corpus
    program and implants it inside another (a `seal` block from here, a `match`
    from there, nested)". Increment 1 wrapped a recipient's OWN statements and
    Increment 2 grafted single statements — neither crosses a construct between
    two programs, which is the composition class this era's defects lived in.
    """
    out = []
    for path in seeds:
        src = read(path)
        lines = src.split("\n")
        depth = 0
        taken = 0
        for i, line in enumerate(lines):
            code = _code(line)
            body = code.strip()
            if (depth > 0 and taken < limit_per_file
                    and CONSTRUCT_HEAD.match(body) and body.endswith("{")):
                d = 0
                for j in range(i, min(i + max_lines, len(lines))):
                    c = _code(lines[j])
                    d += c.count("{") - c.count("}")
                    if d == 0:
                        text = "\n".join(lines[i:j + 1])
                        if len(text) < 600:
                            out.append((textwrap.dedent(text), path))
                            taken += 1
                        break
            depth += code.count("{") - code.count("}")
    return out


def implant(src, rng, constructs, base):
    """Splice a construct from a DIFFERENT corpus program into this one.

    The donor keeps the names it binds for itself; only its free names are
    rebound to what is visible at the destination. The result is a composition
    neither program contained — which is the point (3.1).
    """
    donors = [c for c in constructs if c[1] != base]
    if not donors:
        return None
    bounds = stmt_boundaries(src)
    if not bounds:
        return None
    # Prefer a SELF-CONTAINED donor: the fixup can rebind a free name but cannot
    # give it the right type, so every free name is a chance to die in typecheck
    # before the composition has been tested. Best of a few draws, which keeps
    # the choice random and still biased the way the measurement points.
    text, donor_path = min(
        (donors[rng.randrange(len(donors))] for _ in range(3)),
        key=lambda c: len(free_names(c[0])))
    # The destination must have something to rebind TO. `fixup_bindings` returns
    # the donor untouched when nothing is visible, and the donor's names then
    # mean nothing here — measured as this operator's dominant death by a wide
    # margin (103 of 351 single implants died on `undefined variable`). Take the
    # richest of a few candidate positions rather than the first one drawn.
    picks = [bounds[rng.randrange(len(bounds))][0] for _ in range(4)]
    start = max(picks, key=lambda s: len(bindings_before(src, s)))
    avail = bindings_before(src, start)
    if not avail:
        return None
    fixed = fixup_bindings(text, avail, rng, local=construct_locals(text))
    indent = re.match(r"[ \t]*", src[start:]).group(0)
    block = "\n".join(indent + ln if ln.strip() else ln
                      for ln in fixed.split("\n"))
    out = src[:start] + block + "\n" + src[start:]
    if out == src:
        return None
    return out, f"implant<{os.path.basename(donor_path)[:-4]}>@{start}"


class Mixer:
    """Operator weights steered by the measured death-per-stage split (3.2).

    "The distribution must be MEASURED and must steer the generator, not be
    discovered later as an excuse." Until now the mix was `--graft-rate`: a
    constant chosen by hand, which is a setting, not steering. Each operator now
    earns its share from the stage its inputs actually reach, and the weights are
    REPORTED at the end — a steering nobody can read is a silent cap (I-F4).
    """

    # The objective is REACHING CODEGEN, so the reward has to say that. A first
    # cut scored `typecheck` at 0.5 and the mixer converged on the two operators
    # with the LOWEST codegen survival (graft 4.0%, implant 2.0%) over the one
    # with the highest (wrap 9.4%) — because reliably dying one stage later beat
    # occasionally arriving. A reward that is not the objective steers away from
    # it, confidently.
    REWARD = {"parse": 0.0, "typecheck": 0.1, "codegen": 0.9, "ok": 1.0}

    def __init__(self, ops, alpha=0.12, floor=0.03, sharpen=2):
        self.w = {o: 0.5 for o in ops}
        self.used = {o: 0 for o in ops}
        self.stage = {o: {} for o in ops}
        self.novel = {o: 0 for o in ops}
        self.alpha, self.floor, self.sharpen = alpha, floor, sharpen

    def pick(self, rng):
        # Sharpened, because an input uses several operators and they all take
        # the same credit: the averages separate by a factor, not by a margin,
        # and proportional selection would keep the mix nearly uniform. The floor
        # keeps every operator alive — an operator selected out of the stream is
        # a silent cap (I-F4), and its share is reported either way.
        share = {o: max(self.floor, v) ** self.sharpen for o, v in self.w.items()}
        total = sum(share.values())
        r = rng.random() * total
        for o, v in share.items():
            r -= v
            if r <= 0:
                return o
        return next(iter(self.w))

    def credit(self, ops, stage, novel=False):
        # Stage depth is the DENSE signal — it moves on every input and makes
        # learning possible at all. A new signature is the SPARSE one, and it is
        # what the fuzzer is actually for. Depth alone measured badly: the
        # deepest-reaching mix found 19 distinct signatures where the shallower
        # shipped one found 35, because most findings live in the rejection tail
        # that survival trades away. An objective that is only half the purpose
        # optimises away the other half.
        r = self.REWARD.get(stage, 0.0) + (1.0 if novel else 0.0)
        for o in set(ops):
            self.w[o] = (1 - self.alpha) * self.w[o] + self.alpha * r
            self.used[o] += 1
            self.stage[o][stage] = self.stage[o].get(stage, 0) + 1
            if novel:
                self.novel[o] = self.novel.get(o, 0) + 1

    def report(self):
        rows = []
        for o in sorted(self.w, key=lambda k: -self.w[k]):
            split = " ".join(f"{k}={v}" for k, v in sorted(self.stage[o].items()))
            rows.append(f"  {o:<8} weight={self.w[o]:.3f} used={self.used[o]:<5} "
                        f"new-sigs={self.novel[o]:<3} {split}")
        return "\n".join(rows)


def pick_base(seeds, weights, rng):
    if not weights:
        return seeds[rng.randrange(len(seeds))]
    total = sum(weights)
    r = rng.random() * total
    for s, w in zip(seeds, weights):
        r -= w
        if r <= 0:
            return s
    return seeds[-1]


def generate(seeds, rng, wraps, pool=None, rate=0.25,
             constructs=None, mixer=None, weights=None):
    """Build one input. Returns (text, base, notes, ops) or None.

    `ops` is what the mixer credits: an operator is judged by the stage its
    inputs reach, so the generator has to say which ones it used.
    """
    base = pick_base(seeds, weights, rng)
    src = read(base)
    rel = os.path.relpath(base, ROOT)
    notes, ops = [], []
    for _ in range(wraps):
        if mixer is not None:
            op = mixer.pick(rng)
        elif pool and rng.random() < rate:
            op = "graft"
        else:
            op = "wrap"
        got = None
        if op == "graft" and pool:
            got = graft(src, rng, pool)
        elif op == "implant" and constructs:
            got = implant(src, rng, constructs, base)
        if got is None:
            op, got = "wrap", wrap(src, rng)
        if got is None:
            break
        src, note = got
        notes.append(note)
        ops.append(op)
    if not notes:
        return None
    return src, rel, notes, ops


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
    lines = [ln for ln in err.splitlines() if ln.strip()]
    # The verdict is the first ERROR, never the first LINE. Increment 2 compared
    # the first non-blank line and so compared a `warning: unused parameter`
    # against the other side's silence — a finding the port's own probe canon
    # already names as a trap. Warnings are counted, never compared.
    nwarn = sum(1 for ln in lines if "warning:" in ln)
    first = next((ln for ln in lines if "error:" in ln), "")
    if not first and p.returncode != 0:
        first = next((ln for ln in lines if "warning:" not in ln), "")
    kind = ""
    if p.returncode == 134 or "internal:" in err:
        kind = "ice"
    elif p.returncode < 0:
        kind = "ice"
    c = None
    if os.path.exists(cout):
        c = read(cout)
        os.unlink(cout)
    return (p.returncode, first, c, kind, nwarn)


def phase_ok(flag, path, budget_s):
    """Does the port complete this PHASE on this input?"""
    try:
        p = subprocess.run([PORT, flag, "-o", os.devnull, path],
                           capture_output=True, text=True, timeout=budget_s)
    except subprocess.TimeoutExpired:
        return False
    return p.returncode == 0


def death_stage(ev, path, budget_s):
    """Which stage this input died in — asked of the compiler, not of its wording.

    Increment 2 split parse from typecheck by looking for `expected` in the
    diagnostic, and measured against the two corpora that is not a mechanism: a
    type mismatch reads `return: expected i32, got bool` (same word, other
    stage), while `parser: this item kind not yet ported` contains neither — so
    88 of 600 inputs at seed 1 were filed as typecheck deaths that never reached
    the typechecker. The phase flags answer the question the phase itself
    answers, and they add the stage 3.2 names but nothing yet counted: CODEGEN,
    where an input parses and typechecks and still emits nothing.
    """
    if ev["port_status"] == 0:
        return "ok"
    if ev["port_kind"] in ("timeout", "rss"):
        return "budget"
    if not phase_ok("--emit-ast", path, budget_s):
        return "parse"
    if not phase_ok("--emit-typed-ir", path, budget_s):
        return "typecheck"
    return "codegen"


def seed_weights(seeds, budget_s):
    """Down-weight bases the port cannot parse UNMUTATED.

    Steering applies to the base too: a seed whose item kinds the port announces
    it has not ported (register #10) spends the stream's budget re-announcing a
    known border. Demoted, never dropped — the class stays in the stream and the
    count is reported, because a silent exclusion is a silent cap (I-F4).
    """
    weights, demoted = [], 0
    for s in seeds:
        if phase_ok("--emit-ast", s, budget_s):
            weights.append(1.0)
        else:
            weights.append(0.1)
            demoted += 1
    return weights, demoted


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
    so, eo, co, ko, wo = run(ORACLE, tmp_o, tmp_o + ".c", args.budget, args.rss)
    sp, ep, cp, kp, wp = run(PORT, tmp_p, tmp_p + ".c", args.budget, args.rss)
    return {
        "src": src,
        "oracle_status": so, "port_status": sp,
        "oracle_diag": strip_pos(eo, tmp_o), "port_diag": strip_pos(ep, tmp_p),
        "oracle_c": co, "port_c": cp,
        "oracle_kind": ko, "port_kind": kp,
        "oracle_warn": wo, "port_warn": wp,
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
            # Positions are NORMALISED out of the signature and kept as a
            # relation. They were in it verbatim, and the shrinker rejects any
            # reduction whose signature changed (Z5) — so deleting a line above
            # the error moved the line number, changed the signature, and was
            # refused. A shrinker that cannot delete anything before the defect
            # is not a shrinker; measured, one finding came back 34 -> 32 lines.
            rel = "samepos" if diag_pos(ev["oracle_diag"]) == diag_pos(ev["port_diag"]) else "diffpos"
            return "F1", (f"F1:diag:{rel}:oracle[{msg_only(ev['oracle_diag'])[:60]}]"
                          f"port[{msg_only(ev['port_diag'])[:60]}]")
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
                    help="fixed graft share, used only with --no-steer")
    ap.add_argument("--no-steer", action="store_true",
                    help="disable Z2 steering and use the fixed --graft-rate mix "
                         "(reproduces the Increment 2 stream)")
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
    constructs = harvest_constructs(seeds)
    rng = random.Random(args.seed)
    tmpdir = args.out or os.path.join(ROOT, "_build/out/fuzz")
    os.makedirs(tmpdir, exist_ok=True)

    steer = not args.no_steer
    mixer = Mixer(["wrap", "graft", "implant"]) if steer else None
    weights, demoted = (seed_weights(seeds, args.budget) if steer else (None, 0))

    stages = {"gen-fail": 0, "parse": 0, "typecheck": 0,
              "codegen": 0, "ok": 0, "budget": 0}
    filtered = {}
    findings = []
    seen_sigs = set()
    t0 = time.time()

    for i in range(args.n):
        got = generate(seeds, rng, args.wraps, pool, args.graft_rate,
                       constructs, mixer, weights)
        if got is None:
            stages["gen-fail"] += 1
            continue
        src, base, notes, ops = got

        o = write_tmp(tmpdir, "cand_o.exl", src)
        p = write_tmp(tmpdir, "cand_p.exl", src)
        ev = observe(src, o, p, args)
        # Per-CLASS, never per-input: a registered divergence blocks F1 and
        # leaves F2/F3/F4 live. Counted, never silent (I-F4).
        skip = registered_divergence(ev)
        if skip:
            filtered[skip] = filtered.get(skip, 0) + 1
        kind, sig = classify(ev, args)

        # death-per-stage (Z2), asked of the compiler's own phases
        stage = death_stage(ev, p, args.budget)
        stages[stage] += 1
        novel = bool(sig) and sig not in seen_sigs
        if sig:
            seen_sigs.add(sig)
        if mixer is not None:
            mixer.credit(ops, stage, novel)

        if kind:
            body = shrink(src, sig, tmpdir, args) if args.shrink else src
            bucket, why = triage(kind, sig, ev)
            findings.append((kind, bucket, sig, base, notes, body, why))
            if not args.quiet:
                print(f"fuzz: {kind} [{bucket}] {sig}  (from {base}, {'+'.join(notes)})")
                print(f"fuzz:      mechanism: {why}")

    dt = time.time() - t0
    total = sum(stages.values())
    reached = total - stages["gen-fail"]
    print(f"fuzz: pools graft={len(pool)} statements, implant={len(constructs)} "
          f"constructs from {len(set(c[1] for c in constructs))} files")
    if steer:
        print(f"fuzz: steering ON (Z2); bases demoted (port cannot parse them "
              f"unmutated) {demoted}/{len(seeds)}")
    else:
        print(f"fuzz: steering OFF; fixed graft-rate={args.graft_rate}")
    print(f"fuzz: seed={args.seed} inputs={args.n} compiled={total} "
          f"time={dt:.1f}s budget={args.budget}s/{args.rss}MB")
    print("fuzz: death-per-stage " + " ".join(f"{k}={v}" for k, v in stages.items()))
    if reached:
        print(f"fuzz: survival-to-codegen {stages['ok']}/{reached} "
              f"({100.0 * stages['ok'] / reached:.1f}% of compiled inputs)")
    if mixer is not None:
        print("fuzz: operator mix (weight earned from the stage its inputs reached)")
        print(mixer.report())
    if filtered:
        print("fuzz: filtered (registered divergences) " +
              " ".join(f"{k}={v}" for k, v in sorted(filtered.items())))
    else:
        print("fuzz: filtered (registered divergences) none")
    # Distinct SIGNATURES, not raw findings: a stream that reports one divergence
    # forty times has found one thing, and counting the reports would rank it
    # above a stream that found four. The shrinker already keys on the signature
    # (Z5); this is the same key, used as the coverage measure.
    sigs = sorted(set(f[2] for f in findings))
    print(f"fuzz: findings={len(findings)} distinct-signatures={len(sigs)}")

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
