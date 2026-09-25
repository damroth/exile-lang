#!/bin/sh
# Rebuild BOTH port binaries, exclusively.
#
# The gates and the probe runner drive different ones: `selfhost-port-tc-errors`
# runs `selfhost_tc`, while `tools/probe.sh`, `selfhost-xprod` and every emission
# check run `exilc`.  Rebuilding one and measuring the other reads a stale
# binary and reports the change as a no-op - it has cost this session two
# measurements, so the pairing lives here instead of in anyone's memory.
#
# A FAILED build used to read as silence: `make ... >/dev/null 2>&1` under `set -e`
# aborted before the ok line, and a caller that did not notice the missing line
# went on to probe the PREVIOUS binary and call the result a measurement. The
# build's diagnostics now reach the caller, and the failure says so in the same
# words the success does - so "no ok line" is never the only signal.
set -e
n=$(ps -eo comm | grep -cE '^(make|dune)$' || true)
if [ "$n" -ne 0 ]; then
  echo "rebuild: $n make/dune already running - refusing, a measurement now would not be exclusive" >&2
  exit 1
fi
log=$(mktemp)
if make host-selfhost-tc _build/out/host/exilc >"$log" 2>&1; then
  rm -f "$log"
  echo "rebuild: selfhost_tc + exilc ok"
else
  rc=$?
  echo "rebuild: FAILED - the binaries are STALE, any measurement now reads the previous build" >&2
  grep -E "error|Error" "$log" | head -5 >&2 || true
  rm -f "$log"
  exit $rc
fi
