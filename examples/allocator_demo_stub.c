/* Companion stub for allocator_demo.exl.  Wires the prelude
 * Allocator struct to libc malloc/free.  Defining the thunks +
 * factory C-side keeps the size_t skew on the C side: exile
 * passes a u32 byte-count through the seam, the thunk casts to
 * size_t on its way to malloc.  The free thunk receives the
 * matching byte-count (DR-004 size-on-free) but libc free
 * ignores it — the size is what an Amiga FreeMem / arena / pool
 * / kernel ward-region backend actually needs. */

#include <stdlib.h>

/* Layout must match `struct ex_Allocator` emitted by exile codegen
 * for the prelude `Allocator`.  Fields declared in source order:
 *   state : *c_void
 *   alloc_fn : fn(*c_void, u32) -> *c_void
 *   free_fn  : fn(*c_void, *c_void, u32) -> void
 * Keep this synchronized with prelude_items() in lib/typecheck.ml. */
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
