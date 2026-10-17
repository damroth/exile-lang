/* The C side of the opaque type, sharing the program's header. */
#include <stdlib.h>
#include "opaque_with_header.h"

struct Opaque *opq_make(void) { struct Opaque *o = malloc(sizeof *o); o->v = 42; return o; }
long opq_get(struct Opaque *o) { return o->v; }
