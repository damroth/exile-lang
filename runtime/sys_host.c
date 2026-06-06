/* runtime/sys_host.c — host backend for the `sys::*` seam (DR-006).
 *
 * Implements the prelude-declared `extern fn sys_*` thunks against
 * libc.  Auto-linked by `exilc --target host` whenever the program
 * uses the `sys` module (or any feature that lowers to it — e.g.
 * `default_allocator()`).
 *
 * Each `sys_*` here is the host fork of the layer-1 service seam
 * (per DR-001).  Other targets ship their own backend (e.g.
 * `runtime/sys_amiga.c` will wrap dos.library / exec.library).
 * Layer-0 stdlib code (Vec / HashMap / StringBuilder / String) is
 * target-agnostic and stays in exile, threading through `sys::*`
 * without knowing the implementation. */

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

/* Width-pin signatures match what the prelude declares:
 *   sys_alloc(state: *c_void, n: c_ulong) -> *c_void
 *   sys_free (state: *c_void, p: *c_void, n: c_ulong)
 *   sys_write(fd: c_int, buf: *const c_uchar, n: c_ulong) -> c_long
 *   sys_read (fd: c_int, buf: *c_uchar, n: c_ulong) -> c_long
 *
 * `state` is for stateful allocators (arena base ptr, etc.); libc
 * malloc is stateless so the parameter is ignored.  `n` on free
 * also goes unused under libc but is part of the contract because
 * other backends (AllocMem, fixed-cap arenas) need it.  Keeping
 * the same arity across backends means exile callers never have
 * to branch on target. */

void *sys_alloc(void *_state, unsigned long n) {
    (void)_state;
    return malloc((size_t)n);
}

void sys_free(void *_state, void *p, unsigned long _n) {
    (void)_state;
    (void)_n;
    free(p);
}

long sys_write(int fd, const unsigned char *buf, unsigned long n) {
    return (long)write(fd, buf, (size_t)n);
}

long sys_read(int fd, unsigned char *buf, unsigned long n) {
    return (long)read(fd, buf, (size_t)n);
}

/* DR-032 sys_open / sys_close.  `flags` follows the POSIX numeric
 * convention (O_RDONLY=0, O_WRONLY=1, O_RDWR=2 + the create-mode
 * bits); the exile caller may hand-roll the constants until a
 * proper `sys::O_*` constant block lands.  Mode is 0644 for the
 * create-path callers that need it. */
int sys_open(const char *path, int flags) {
    return open(path, flags, 0644);
}

int sys_close(int fd) {
    return close(fd);
}
