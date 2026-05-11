/* C-side definitions for examples/ctypes.exl.  Each fn echoes back
 * (or lightly transforms) a value of the matching C primitive type.
 * Linked by `make host-ctypes` automatically via the
 * `examples/NAME_stub.c` convention. */

short          s_neg(short x)                       { return -x; }
unsigned short us_double(unsigned short x)          { return (unsigned short)(x * 2u); }
long           l_add(long a, long b)                { return a + b; }
unsigned long  ul_inc(unsigned long x)              { return x + 1ul; }
unsigned int   ui_double(unsigned int x)            { return x * 2u; }
signed char    sc_neg(signed char x)                { return (signed char)-x; }
unsigned char  uc_id(unsigned char x)               { return x; }

/* c_void only legal under a pointer.  Round-trip identity. */
void *vp_id(void *p) { return p; }
