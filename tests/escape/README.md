Negative + positive probes for the escape pass, lifted from the OCaml suite
(test/test_exile_lang.ml).  `make selfhost-port-escape`
runs both compilers over each one and byte-compares the diagnostic:

  p1  returning `Slice { ptr: &local[..] }`          -> reject
  p2  returning `new(al) Box { p: &local }`          -> reject
  p3  `Slice { ptr: <ptr param> }`                   -> accept (param, not local)
  p4  laundering `&local` through a `let`            -> reject
  p5  bare `return &local`                           -> reject
  p6  `v.as_slice()` read after `v.push(...)`        -> reject (invalidation)
  p7  `s.as_str()` read after `s.free()`             -> reject (invalidation)
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

  p13 diagnostic on a RENAMED binding (`s` -> `s__1`)  -> reject, quoting `s`

p13 pins the srcnames contract.  p1..p12 only ever name bindings the compiler
left alone, so they pass whether or not the renaming map reaches the pass — the
port printed the minted `s__1` for two years and every one of them stayed green.
The IR carries `tf_srcnames` (C name -> the user's name, only the entries that
differ) and the pass renders through `ir::src_name`; p13 is the shape that fails
if either half goes missing.
