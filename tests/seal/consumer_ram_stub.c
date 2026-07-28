/* Storage for the sealed-sequence witness (tests/seal/consumer_ram.exl).

   Sized deliberately, and this is the interesting part: exile's `c_ulong` is 8
   bytes on a 64-bit host but 4 on m68k, so a single `unsigned long` backs the
   ward's four u16 fields on the host and leaves the last two PAST THE END of
   the object on the target. Two longs cover both word sizes. The existing
   ward-over-RAM witness has the smaller backing and does not survive m68k —
   register #8. */
unsigned long SCRATCH[2];
