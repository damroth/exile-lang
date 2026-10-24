/* Companion stub for array_element_of_string_borrowed.exl: the default allocator's two seam functions
 * over libc malloc, counting blocks so the run ends with the balance - a leak
 * or a second release of the array's elements shows in the output. */

#include <stdio.h>
#include <stdlib.h>

static long live_blocks;
static int reported;

static void report_balance(void) {
    printf("alloc-balance %ld\n", live_blocks);
}

void *sys_alloc(void *_state, unsigned long n) {
    (void)_state;
    if (!reported) { reported = 1; atexit(report_balance); }
    live_blocks++;
    return malloc((size_t)n);
}

void sys_free(void *_state, void *p, unsigned long _n) {
    (void)_state;
    (void)_n;
    live_blocks--;
    free(p);
}
