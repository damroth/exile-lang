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
