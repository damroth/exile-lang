#!/bin/sh
# Corpus sweep - what one compiler binary does with every tracked `.exl`, one
# line per file, sorted so two sweeps compare line for line:
#
#   path|ACCEPT|rc|<md5 of the emitted C>
#   path|REFUSE|rc|<the first error line>
#   path|CRASH|rc|                  no C and no error line: a signal, an abort,
#                                   or the memory cap - never a verdict
#
#   tools/sweep.sh <exilc> <out>        sweep with that binary into <out>
#   tools/sweep.sh --diff <old> <new>   the files whose line changed; exit 1 if any
#
# The compiler's own top-level modules (`src/*.exl`) are left out: one compile of
# the compiler's source takes about 12 GB, and four of them side by side took the
# machine down twice. The fixpoint gates compile them, one at a time. Each compile
# here runs under a memory cap, four at a time.
#
# SWEEP_LIST=<file> replaces the corpus with the paths in that file, and
# SWEEP_ULIMIT=<kB> the cap - both exist for the instrument's control.
set -u

# One file, one line - what each of the four parallel workers runs.
if [ "${1:-}" = "--one" ]; then
  bin=$2; cap=$3; f=$4
  c=$(mktemp); rm -f "$c"; e=$(mktemp)
  ( ulimit -v "$cap"; "$bin" --target c --c-out "$c" "$f" >/dev/null 2>"$e" ); rc=$?
  if [ -s "$c" ]; then
    echo "$f|ACCEPT|$rc|$(md5sum < "$c" | cut -c1-32)"
  elif grep -q 'error:' "$e"; then
    echo "$f|REFUSE|$rc|$(grep -m1 'error:' "$e")"
  else
    echo "$f|CRASH|$rc|"
  fi
  rm -f "$c" "$e"
  exit 0
fi

if [ "${1:-}" = "--diff" ]; then
  [ $# -eq 3 ] || { echo "usage: tools/sweep.sh --diff <old> <new>" >&2; exit 2; }
  for f in "$2" "$3"; do
    [ -s "$f" ] || { echo "sweep: MISSING or EMPTY sweep $f" >&2; exit 2; }
  done
  n2=$(wc -l < "$2"); n3=$(wc -l < "$3")
  [ "$n2" -eq "$n3" ] || { echo "sweep: the two sweeps cover $n2 and $n3 files - not the same corpus" >&2; exit 2; }
  d=$(diff "$2" "$3" | grep -c '^>' || true)
  diff "$2" "$3" | grep '^[<>]'
  echo "sweep: $d of $n3 files changed"
  [ "$d" -eq 0 ]
  exit $?
fi

[ $# -eq 2 ] || { echo "usage: tools/sweep.sh <exilc> <out> | --diff <old> <new>" >&2; exit 2; }
bin=$1; out=$2
[ -x "$bin" ] || { echo "sweep: MISSING binary $bin" >&2; exit 2; }
cap=${SWEEP_ULIMIT:-2097152}
list=$(mktemp)
if [ -n "${SWEEP_LIST:-}" ]; then
  cp "$SWEEP_LIST" "$list"
else
  git ls-files '*.exl' | grep -vE '^src/[^/]+\.exl$' > "$list"
fi
n=$(wc -l < "$list")
[ "$n" -gt 0 ] || { echo "sweep: the corpus is empty - a sweep over nothing reads as clean" >&2; rm -f "$list"; exit 2; }

tmp=$(mktemp)
xargs -P 4 -n 1 sh "$0" --one "$bin" "$cap" < "$list" > "$tmp"
sort "$tmp" > "$out"
rm -f "$tmp" "$list"
got=$(wc -l < "$out")
[ "$got" -eq "$n" ] || { echo "sweep: $got lines for $n files - a compile left no line" >&2; exit 2; }
echo "sweep: $n files - $(grep -c '|ACCEPT|' "$out") accept, $(grep -c '|REFUSE|' "$out") refuse, $(grep -c '|CRASH|' "$out") crash -> $out"
