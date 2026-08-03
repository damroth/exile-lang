/* Storage for the ward-over-RAM witness (tests/ward/ward_roundtrip.exl).
 *
 * The exile side declares `[u8; 8]`, which the emission renders as an
 * INCOMPLETE `struct ex_arr8_u8` - it only ever takes the address, so it never
 * needs the definition. This file supplies it, which means the two agree by
 * NAME: if codegen ever renames its array structs, the object here stops being
 * the object there. `selfhost-ward` asserts the emitted name against this file
 * so that rename fails loudly instead of linking to the wrong storage.
 *
 * Eight bytes exactly, on every target - the reason the fixture stopped using a
 * `c_ulong`, which is 8 bytes on a 64-bit host and 4 on m68k. */

struct ex_arr8_u8 { unsigned char b[8]; };

struct ex_arr8_u8 SCRATCH;
