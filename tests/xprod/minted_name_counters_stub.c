/* Companion stub for minted_name_counters.exl: the default allocator's two seam
 * functions over libc malloc, which a `Display` print reaches for its builder. */

#include <stdlib.h>

void *sys_alloc(void *_state, unsigned long n) {
    (void)_state;
    return malloc((size_t)n);
}

void sys_free(void *_state, void *p, unsigned long _n) {
    (void)_state;
    (void)_n;
    free(p);
}
