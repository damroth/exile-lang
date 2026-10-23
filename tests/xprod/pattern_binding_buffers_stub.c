/* Companion stub for pattern_binding_buffers.exl: the default allocator's two
 * seam functions over libc malloc, for the Vecs and Strings it builds. */

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
