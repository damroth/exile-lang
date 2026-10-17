/* The C side of an opaque type: only this file knows the layout. */
#include <stdlib.h>

struct Opaque { long v; };

struct Opaque *opq_make(void) { struct Opaque *o = malloc(sizeof *o); o->v = 42; return o; }
long opq_get(struct Opaque *o) { return o->v; }
