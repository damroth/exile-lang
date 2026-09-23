/* The poison's own positive control.  A block is written, released, and read back
   PAST its first sixteen bytes, which glibc's free lists overwrite for their own
   use; beyond them the witness's pattern has to be what comes back.  Reading
   released memory is exactly what this file exists to do, so it goes through a
   volatile pointer the optimiser may not remove.

   The gate builds it at -O2, the level the fixtures are built at: the poisoning
   loop first ran at -O2 as dead stores and was removed, and a control built at
   -O0 passed while every fixture it vouched for had lost the poison. */
extern void *sys_alloc(void *state, unsigned long n);
extern void sys_free(void *state, void *p, unsigned long n);

int main(void) {
    volatile unsigned char *b = (volatile unsigned char *)sys_alloc((void *)0, 64UL);
    b[40] = 7;
    sys_free((void *)0, (void *)b, 64UL);
    return b[40] == 0xA5 ? 0 : 1;
}
