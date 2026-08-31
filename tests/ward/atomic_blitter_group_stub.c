/* Storage for the blitter group witness (tests/ward/atomic_blitter_group.exl).
 *
 * Same contract as the other ward-over-RAM stubs: the exile side declares
 * `[u8; 32]`, the emission renders an INCOMPLETE `struct ex_arr32_u8` because it
 * only ever takes the address, and this file supplies the definition. The two
 * agree by NAME, so a codegen rename fails at the linker instead of quietly
 * binding to different storage.
 *
 * Thirty-two bytes exactly, on every target - the eight members reach 0x16. */

struct ex_arr32_u8 { unsigned char b[32]; };

struct ex_arr32_u8 BLTREGS;
