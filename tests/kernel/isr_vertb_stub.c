/* The vector half of the stub-style handler, and the RAM both halves overlay.
 *
 * On the target this is the real ISR: the m68k dispatches to it with the level
 * masked, it calls the exile body, and `rte` restores the status register the
 * exception frame saved. That prologue and epilogue - the frame, the mask, the
 * `rte` - are the silicon truths the scope verdict left on this side of the seam.
 *
 * The two halves agree by NAME: a `pub fn on_vertb()` in exile emits
 * `void ex_on_vertb(void)` with external linkage. Nothing checks that agreement,
 * which is the first thing a notation would take off the table. */

struct ex_arr256_u8 { unsigned char b[256]; };
struct ex_arr8_u8   { unsigned char b[8]; };

struct ex_arr256_u8 CHIPREGS;
struct ex_arr8_u8   ISRSTATE;

extern void ex_on_vertb(void);

#ifdef __mc68000__
/* The real vector entry. `interrupt` gives the m68k prologue/epilogue and the
 * `rte`; without it the compiler would emit an ordinary `rts` and the exception
 * frame would be left on the stack. */
__attribute__((interrupt)) void ex_vertb_vector(void) {
    ex_on_vertb();
}
#endif
