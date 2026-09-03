/* Storage for the extern the fixture writes into. The emission renders `[u16; 4]`
 * as an INCOMPLETE `struct ex_arr4_u16` - it only ever takes the address - and
 * this file supplies the definition, so the two agree by name. */
struct ex_arr4_u16 { unsigned short b[4]; };
struct ex_arr4_u16 CELLS;
