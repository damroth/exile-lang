/* The chip registers the overlay sits on, and the STATIC list the copper would
 * follow. Static is the point: the hardware reads this memory for as long as its
 * DMA is on, which is longer than any function that built it. */
struct ex_arr256_u8 { unsigned char b[256]; };
struct ex_arr16_u16 { unsigned short b[16]; };
struct ex_arr256_u8 CHIPREGS;
struct ex_arr16_u16 COPLIST;
