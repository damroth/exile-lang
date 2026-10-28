/* Companion stub for prelude_name_module_struct_vec_beside_prelude_vec.exl: the default allocator's two
 * seam functions over libc malloc, counting blocks so the run ends with the balance - the vector is released once. */


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
