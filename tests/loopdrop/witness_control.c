/* The allocation witness's own positive control, and the proof that it is off
   by default.  One block is handed out and never taken back: with
   EXILE_ALLOC_WITNESS set the seam must report it, and without the variable it
   must print nothing at all.  Without the first half, a witness that always said
   zero would turn every fixture beside this file green whether the drops ran or
   not; without the second, turning it on for this gate could have changed the
   stdout of every program that allocates. */
extern void *sys_alloc(void *state, unsigned long n);

int main(void) {
    (void)sys_alloc((void *)0, 16UL);
    return 0;
}
