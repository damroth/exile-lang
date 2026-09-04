/* The list's storage. It is `chip reads` on the exile side - memory the device
 * follows - so the definition here is volatile too: the emitted declaration says
 * volatile, and a definition that disagreed would be the one place the guarantee
 * could quietly stop applying. */
struct ex_arr8_u16 { unsigned short data[8]; };
volatile struct ex_arr8_u16 COPLIST;
