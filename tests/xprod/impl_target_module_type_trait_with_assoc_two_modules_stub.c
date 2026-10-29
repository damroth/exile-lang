/* Companion stub for impl_target_module_type_trait_with_assoc_two_modules.exl: the default allocator's two seam functions
 * over libc malloc. */

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
