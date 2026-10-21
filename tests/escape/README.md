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

  p14 returning a local `StringBuilder`'s `as_slice()`   -> reject
  p15 returning a local `String`'s `as_slice()`          -> reject
  p16 pushing a local `StringBuilder`'s `as_slice()` into a non-local container -> reject
  p17 returning `str::as_bytes` of a str that points into a local array       -> reject

p14..p16 pin the prelude's non-generic methods in the summary table.  The port
holds them apart from the user's functions (`tp_pmeth` / `tp_pfree`) and the pass
used to summarise only the user's list, so a borrow those functions return came
back as caller-owned and all four compiled to a dangling slice.  p14..p16 go
through the methods, p17 through the `str` free functions - one per list.  The gate compares the
whole line, position included.

  p18 A str borrowed from a String the caller owns, returned. -> accept
  p19 A str made in an Arena the caller owns, returned. -> accept
  p20 A string literal, returned through a binding. -> accept
  p21 A str from a parameter, carried through a slice of a local Vec and pushed into a non-local one. -> accept
  p22 The same read by index instead of through the slice. -> accept
  p23 An arena str pushed straight into a non-local Vec. -> accept
  p24 An arena str read back from a local Vec by index. -> accept
  p25 An arena str read back from a local Vec by a for-each. -> accept
  p26 An arena str read back from a local array. -> accept
  p27 An arena str read back from a local Vec by get. -> accept
  p28 The names a local table holds, copied out through a borrow of one row. -> accept
  p29 A position read through a local cursor that holds the file's name. -> accept
  p30 A context struct lends a local table to a callee, which returns the lent table itself. -> reject
  p31 A struct wider than the field mask: the local sits in field 66 and the callee reads field 68; past the mask both share one slot, so the read is refused. -> reject
  p32 A local's address read back through a slice of a local Vec. -> reject
  p33 A local's address pushed straight into a non-local Vec. -> reject
  p34 The address of an element read through a slice of a local Vec, returned. -> reject
  p35 The address of a field read through a pointer to a local struct, returned. -> reject

p18..p35 are the cases where the escape pass that looks further - past a
parameter's address to what lies under it, into what a container holds, across
the fields of a struct, and at a `str` as a borrow - still says what the
reference says: the accepts pin that the extra precision refuses nothing the
reference takes, the rejects that it lets nothing through the reference
catches.  p31 is the struct wider than the field mask: fields past it share one
slot, so the lent local in one of them keeps every other one from leaving.
Where the two passes do NOT agree, the programs live in tests/escape-divergence.

  p36 A callee pushes a pointer the caller received into the caller's Vec. -> accept
  p37 An int field of an element read out of a local Vec whose elements hold a local address. -> accept
  p38 An int read through an element of a local Vec of pointers to a local. -> accept
  p39 A scope lends a local table; the callee returns an element read through the scope's pointer field. -> accept

  p40 A callee fills a Vec declared inside a match arm with the address of a local from outside the arm. -> accept

p36 pins what a callee's stores do not refuse: a pointer the caller itself
received, pushed through a callee into the caller's container.  p37 and p38 are
values that cannot carry an address read out of a container that holds a
local's: every site a value leaves by asks the same question the return does.
p39 is an element of a table a scope lends, read through the scope's pointer
field: what lies under that pointer is the table's elements, not the pointer.
p40 is a callee's store into a Vec that a match arm declares: the arm's
expressions are also visited from outside it, before its own lets are in scope,
and a target rooted in one of the fn's own locals is not "not local" there.


  p41 An unknown callee, a function pointer, given a pointer the function received. -> accept
  p42 An unknown callee given a local's address, its result read and not kept. -> accept
  p43 An unknown callee given a value with no address in it. -> accept
  p44 An unknown callee given a received pointer while a local is in scope. -> accept
  p45 A named callee that returns its argument, given a local's address. -> reject
  p46 A named callee whose return is never its argument, given a local's address. -> accept
  p47 A callee marked `@escapes`, given a local's address. -> accept
  p48 An extern callee, given a local's address. -> accept

p41-p44 pin what the cautious answer for a callee the pass cannot see does not
refuse: a call through a function pointer, a field or a closure counts every
argument as what its result may carry, so only an argument that carries a
local's address makes the result a local's. p45 and p46 are the named callee
next to them, answered by its summary as before. The cautious answer is for a
callee that is a value - a binding, a field, a call's result, a closure - and
not for every callee without a summary: p47's `@escapes` is the hatch a function
takes so its callers trust what it returns, and p48's extern is trusted as the
reference trusts it, its C declaration saying nothing of its result. The calls that do carry a local's
address out are refused by the port and accepted by the reference; they live in
tests/escape-divergence as refuse_149_*.
