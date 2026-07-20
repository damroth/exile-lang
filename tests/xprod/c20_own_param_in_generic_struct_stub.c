/* Companion stub for own_ptr.exl.  Same libc-malloc bridge as
 * string_stub.c — repeated per the one-feature-one-file rule. */

#include <stdlib.h>

typedef void *(*alloc_fn_t)(void *, unsigned long);
typedef void (*free_fn_t)(void *, void *, unsigned long);

struct ex_Allocator {
    void *state;
    alloc_fn_t alloc_fn;
    free_fn_t free_fn;
};

static void *c_alloc_thunk(void *_state, unsigned long n) {
    (void)_state;
    return malloc((size_t)n);
}

static void c_free_thunk(void *_state, void *p, unsigned long _n) {
    (void)_state;
    (void)_n;
    free(p);
}

struct ex_Allocator make_c_allocator(void) {
    struct ex_Allocator a;
    a.state = 0;
    a.alloc_fn = c_alloc_thunk;
    a.free_fn = c_free_thunk;
    return a;
}
