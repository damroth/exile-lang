/* The 256 bytes the test overlay sits on. A real program overlays the chip at
 * 0xDFF000; this one overlays RAM so the sequence can be executed and checked. */
struct ex_arr256_u8 { unsigned char b[256]; };
struct ex_arr256_u8 CHIPREGS;
