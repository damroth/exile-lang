/* Storage the overlay sits on. Same contract as the other ward-over-RAM stubs:
 * the emission renders an INCOMPLETE `struct ex_arr8_u8` because it only takes
 * the address, and this file supplies the definition, so the two agree by NAME. */
struct ex_arr8_u8 { unsigned char b[8]; };
struct ex_arr8_u8 SCRATCH;
