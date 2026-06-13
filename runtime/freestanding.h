/* freestanding.h — libc-free print / strlen / memzero helpers used by
 * exilc's `--freestanding` output.  Emitted C `#include`s this instead of
 * <stdio.h>/<string.h>; every helper is layered over the same `sys_write`
 * seam the kernel already implements (runtime/freestanding.c).  Link the
 * freestanding C with `-nostdlib` against freestanding.c + a sys_*.c
 * backend and `nm -u` shows only { sys_write, sys_alloc, sys_free }.
 */
/* NOTE: exilc's `--freestanding` output does NOT `#include` this header —
 * it emits the same prototypes inline so the generated C is self-contained
 * (no -I path needed on any target).  This header is the canonical
 * declaration freestanding.c compiles against; keep the two in sync (the
 * inline copy lives in codegen.ml's gen_program). */
#ifndef EXILE_FREESTANDING_H
#define EXILE_FREESTANDING_H

/* The one syscall seam these helpers stand on (fd 1 = stdout). */
extern long sys_write(int fd, const unsigned char *buf, unsigned long n);

unsigned long __ex_strlen(const char *s);
void __ex_memzero(void *p, unsigned long n);

/* `__ex_print_*` write with no trailing newline; `__ex_println_*` append
 * one '\n'.  The integer helpers take the widest signed / unsigned C type
 * (every exile int width funnels through a `(long)` / `(unsigned long)`
 * cast at the call site). */
void __ex_print_i32(long v);
void __ex_println_i32(long v);
void __ex_print_u32(unsigned long v);
void __ex_println_u32(unsigned long v);
void __ex_print_str(const char *s);
void __ex_println_str(const char *s);
void __ex_print_str_quoted(const char *s);   /* `"` + raw + `"` (Debug) */
void __ex_print_ptr(const void *p);           /* `0x` + lowercase hex   */

#endif
