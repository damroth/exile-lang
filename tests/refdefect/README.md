# Fixtures the REFERENCE cannot compile

Every other differential gate in this repo runs both compilers and compares them.
These cannot: the reference aborts on them, so there is nothing to compare against
and `dev/probe.sh` would classify the run as a crash rather than a measurement.

A fixture belongs here only when the reference's failure is a MEASURED defect with
a register entry naming its mechanism. The gate then pins two things:

- the reference still fails, so the fixture keeps describing the defect it was
  written for - if the reference is ever fixed, the gate goes red and the entry is
  re-measured instead of quietly outliving its subject;
- the port builds it, runs it, and produces exactly its `.expected` output.

The second half is the port's registered divergence made executable. The port does
not copy an abort: an abort replaces the reader's program with a crash, and the
zone rule lets the port do better here provided the difference is registered and
gated behaviourally - which is what this directory is.
