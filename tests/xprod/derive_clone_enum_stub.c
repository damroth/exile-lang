/* Companion stub: the libc-malloc bridge this directory uses whenever a
 * fixture needs a real allocator, repeated per the one-feature-one-file rule.
 * The struct laid out here has to match the allocator the compiler emits -
 * which is the whole point of the fixture beside it. */

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
