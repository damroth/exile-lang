#!/bin/sh
# Differential probe runner - the rule that stops "no artifact" from being read
# as "a verdict".
#
# Three times in one round a probe reported a divergence that was really a
# missing file: the reference had REJECTED the input, nothing was written, and
# `diff` failed for a reason that had nothing to do with what the probe asked.
# The discipline to check by hand is cheaper to state than to keep, so it lives
# here instead: every comparison passes through this script, and the script
# cannot forget.
#
#   tools/probe.sh <file.exl> [more.exl ...]
#
# For each input it runs BOTH compilers, then classifies before comparing:
#
#   CRASH         died, aborted, or refused mute - a compiler killed by a signal,
#                                         one whose message carries an INTERNAL
#                                         marker, or one exiting non-zero without
#                                         saying anything a reader can act on.
#                                         Kept OUT of the rejection classes on
#                                         purpose: an ICE counted as a rejection
#                                         reads as parity with a real diagnostic -
#                                         and the reference's abort says `internal
#                                         compiler error:`, which contains the very
#                                         substring this script greps for, so the
#                                         marker is tested explicitly.
#   REJECT-BOTH   both refused          - a rejection-class input; the messages
#                                         are compared, the emissions are not
#   REJECT-ONE    exactly one refused   - a verdict divergence, printed with the
#                                         side that refused and its first line
#   EMPTY         accepted but no file  - the compiler lied about succeeding;
#                                         never silently treated as a diff
#   AGREE         both emitted, equal
#   DIVERGE       both emitted, differ  - the only case where a diff is shown
#
# Exit status is the number of inputs that are not AGREE, so a caller can gate
# on it.

ORACLE_RUN="dune exec --no-print-directory exilc --"
PORT_BIN="./_build/out/host/exilc"
TMP="${TMPDIR:-/tmp}/exile-probe.$$"
mkdir -p "$TMP" || exit 2
trap 'rm -rf "$TMP"' EXIT

if [ ! -x "$PORT_BIN" ]; then
    echo "probe: MISSING $PORT_BIN - build it before probing, or the run measures nothing" >&2
    exit 2
fi

bad=0
for f in "$@"; do
    if [ ! -f "$f" ]; then
        echo "MISSING   $f - the probe input itself is absent"
        bad=$((bad + 1))
        continue
    fi
    name=$(basename "$f" .exl)
    oc="$TMP/$name.o.c"
    pc="$TMP/$name.p.c"
    oe="$TMP/$name.o.err"
    pe="$TMP/$name.p.err"

    $ORACLE_RUN --target c --c-out "$oc" "$f" >/dev/null 2>"$oe"; orc=$?
    "$PORT_BIN"  --target c --c-out "$pc" "$f" >/dev/null 2>"$pe"; prc=$?

    # The FULL first error line, position included. Stripping the `file:line:col`
    # prefix would call two diagnostics equal that point a reader at different
    # places, and position parity is part of the contract - a message that agrees
    # about the words and disagrees about where is still a divergence.
    oline=$(grep -m1 'error:' "$oe" 2>/dev/null)
    pline=$(grep -m1 'error:' "$pe" 2>/dev/null)

    # An ABORT announces itself in words, and its words contain the substring
    # this script greps for: the reference says `internal compiler error: File
    # ... Assertion failed`, which matches `error:` and so arrived here looking
    # like an ordinary refusal. Two rounds quoted an assertion failure as
    # "oracle refused" before anyone noticed. The port's own aborts are spelled
    # `internal: ...`, which matches nothing and would have been caught by the
    # rc test below - the asymmetry is the point: a marker is a contract and a
    # status is not.
    oice=$(grep -m1 -E 'internal compiler error|(^|: )internal: ' "$oe" 2>/dev/null)
    pice=$(grep -m1 -E 'internal compiler error|(^|: )internal: ' "$pe" 2>/dev/null)

    # A death is not a verdict. A signal (rc >= 128), an abort MARKER, or a
    # non-zero exit with no line a reader can act on is a CRASH, and it is
    # reported as one before any message comparison: otherwise the rejection
    # classes below call it "refused" and a series of them looks like a quiet
    # class of rejections.
    ocrash=no; pcrash=no
    if [ "$orc" -ge 128 ] || [ -n "$oice" ] || { [ "$orc" -ne 0 ] && [ -z "$oline" ]; }; then ocrash=yes; fi
    if [ "$prc" -ge 128 ] || [ -n "$pice" ] || { [ "$prc" -ne 0 ] && [ -z "$pline" ]; }; then pcrash=yes; fi
    if [ "$ocrash" = yes ] || [ "$pcrash" = yes ]; then
        who=""
        [ "$ocrash" = yes ] && who="oracle"
        [ "$pcrash" = yes ] && who="${who:+$who and }port"
        printf 'CRASH        %-34s %s died (rc oracle=%s port=%s)\n' "$name" "$who" "$orc" "$prc"
        [ "$ocrash" = yes ] && printf '    oracle: %s\n' "$(head -1 "$oe" 2>/dev/null)"
        [ "$pcrash" = yes ] && printf '    port:   %s\n' "$(head -1 "$pe" 2>/dev/null)"
        bad=$((bad + 1))
        continue
    fi

    # Classify on the STATUS first. A non-zero rc means there is no emission to
    # compare, and comparing anyway is what produced the false divergences.
    if [ "$orc" -ne 0 ] && [ "$prc" -ne 0 ]; then
        if [ "$oline" = "$pline" ]; then
            # Print the shared line. Two sides agreeing on a diagnostic is only
            # interesting when the diagnostic is about the LANGUAGE - a syntax
            # error in the probe itself also agrees, perfectly, and reads as
            # parity until someone looks at the words.
            printf 'REJECT-BOTH  %-34s same: %s\n' "$name" "${oline#*error: }"
        else
            # Say which half differs, because the two are fixed differently: the
            # words come from the check, the position from where it fires.
            owords=${oline#*error: }; pwords=${pline#*error: }
            if [ "$owords" = "$pwords" ]; then kind="SAME WORDS, DIFFERENT POSITION"
            else kind="MESSAGES DIFFER"; fi
            printf 'REJECT-BOTH  %-34s %s\n' "$name" "$kind"
            printf '    oracle: %s\n    port:   %s\n' "$oline" "$pline"
            bad=$((bad + 1))
        fi
        continue
    fi
    if [ "$orc" -ne 0 ] || [ "$prc" -ne 0 ]; then
        if [ "$orc" -ne 0 ]; then who="oracle"; msg="$oline"; else who="port"; msg="$pline"; fi
        printf 'REJECT-ONE   %-34s %s refused: %s\n' "$name" "$who" "$msg"
        bad=$((bad + 1))
        continue
    fi
    # Both claimed success - so both files must exist and be non-empty before a
    # diff means anything.
    if [ ! -s "$oc" ] || [ ! -s "$pc" ]; then
        printf 'EMPTY        %-34s a compiler reported success and wrote nothing\n' "$name"
        bad=$((bad + 1))
        continue
    fi
    if diff -q "$oc" "$pc" >/dev/null; then
        printf 'AGREE        %-34s %s lines\n' "$name" "$(wc -l < "$pc")"
    else
        printf 'DIVERGE      %-34s\n' "$name"
        diff "$oc" "$pc" | head -12 | sed 's/^/    /'
        bad=$((bad + 1))
    fi
done

exit $bad
