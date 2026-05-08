/* C-side definitions for examples/ffi.exl.  Linked by `make host-ffi` /
 * `make amiga-ffi` automatically thanks to the `examples/NAME_stub.c`
 * convention in the Makefile. */

#include <stdio.h>

long add(long a, long b) {
    return a + b;
}

long square(long x) {
    return x * x;
}

void shout(void) {
    puts("HEY");
}
