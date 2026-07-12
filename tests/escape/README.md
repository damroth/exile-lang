Negative + positive probes for the DR-010 escape pass, lifted from the OCaml
suite (test_exile_lang.ml, the DR-010 Phase A/B/C blocks).  `make selfhost-port-escape`
runs both compilers over each one and byte-compares the diagnostic:

  p1  returning `Slice { ptr: &local[..] }`          -> reject
  p2  returning `new(al) Box { p: &local }`          -> reject
  p3  `Slice { ptr: <ptr param> }`                   -> accept (param, not local)
  p4  laundering `&local` through a `let`            -> reject
  p5  bare `return &local`                           -> reject
  p6  `v.as_slice()` read after `v.push(...)`        -> reject (Phase C, S5c)
  p7  `s.as_str()` read after `s.free()`             -> reject (Phase C, S5d)
  p8  the same as p5 but `@escapes`                  -> accept (the hatch)
  p9  `v.push(&local)` into a non-local container    -> reject
  p10 borrow REBUILT after the mutation              -> accept (no dangling)
  p11 33 params, return carrier = p32, `&local` at p0    -> accept (precision)
  p12 33 params, return carrier = p32, `&local` AS p32   -> reject (soundness)

p11/p12 pin the summary bitmask's saturating high bit (escape.exl PARAM_HIGH).
The first cut degraded any param >= 32 to `Unknown`, which projects to "every param
may carry the return" — so p11 was REJECTED by the port and accepted by the oracle
(a false positive: precision lost on the wrong side of the lattice).  Bit 31 now
saturates to "some param >= 31", and the call site meets over the tail args only.
p12 is the mirror: confining the imprecision must not lose the real leak.
