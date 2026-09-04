/* The vector half, and the RAM the handler and the main loop overlay.
 *
 * Installing the vector lives here for the same reason the prologue does: the
 * address of an exception entry and the status register's mask are silicon truths,
 * not language ones, and the scope verdict put that boundary on this side.
 *
 * 0x6C is the level-3 autovector. The Amiga's VBLANK arrives on the 68000 as
 * level 3, and the CPU takes autovectored interrupts without asking the chip for a
 * vector number - so installing a handler is one longword write. */
struct ex_arr8_u8 { unsigned char b[8]; };
struct ex_arr8_u8 ISRSTATE;

extern void ex_on_vertb(void);

#ifdef __mc68000__
__attribute__((interrupt)) void ex_vertb_vector(void) {
    ex_on_vertb();
}

void isr_install(void) {
    *(volatile unsigned long *)0x6CUL = (unsigned long)&ex_vertb_vector;
    /* Reset leaves the mask at 7, so nothing would ever be delivered. Drop it to
     * zero and level 3 gets through. */
    __asm__ __volatile__("and.w #0xf8ff,%%sr" : : : "cc");
}
#else
void isr_install(void) { }
#endif
