/* The overlay's RAM and the buffer the register is pointed at. BUF[0] is seeded so
 * the witness prints a value a reader can check rather than a zero. */
struct ex_arr64_u8 { unsigned char b[64]; };
struct ex_arr4_u16 { unsigned short b[4]; };
struct ex_arr64_u8 CHIP;
struct ex_arr4_u16 BUF = { { 1, 2, 3, 4 } };
