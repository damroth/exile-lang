/* runtime/sys_amiga.c — AmigaOS backend for the `sys::*` seam (DR-006).
 *
 * Mirror of `runtime/sys_host.c` for `exilc --target amiga`.  Wraps
 * `exec.library` AllocVec / FreeVec for memory and `dos.library`
 * Write / Read for IO.  Auto-linked by the Makefile `amiga-%` rule
 * whenever the program uses the sys module (which any caller of
 * `default_allocator()` does transitively).
 *
 * Per DR-001 / DR-006: layer-0 stdlib (Vec / HashMap / StringBuilder
 * / String) calls the seam verbatim; the compiler swaps the backend
 * by linking the matching runtime file per `--target`. */

#include <proto/exec.h>
#include <proto/dos.h>
#include <exec/memory.h>

/* AllocVec records the request size internally so FreeVec doesn't
 * need it — the seam's `n` parameter is honoured by the host /
 * kernel backends but ignored here.  MEMF_PUBLIC keeps the buffer
 * accessible after a task switch (matches what layer-0 collections
 * expect); MEMF_CLEAR mirrors `mem_zero` for slot tables. */
void *sys_alloc(void *_state, unsigned long n) {
    (void)_state;
    return AllocVec((ULONG)n, MEMF_PUBLIC | MEMF_CLEAR);
}

void sys_free(void *_state, void *p, unsigned long _n) {
    (void)_state;
    (void)_n;
    if (p) FreeVec(p);
}

/* AmigaOS has no POSIX file descriptors — DOS uses BPTR handles
 * (`BCPL pointer`).  We map the conventional POSIX fds the prelude
 * uses (0=stdin, 1=stdout, 2=stderr) onto `Input()` / `Output()`;
 * other fds aren't supported.  The cast through (APTR) silences
 * Bebbo's strict prototype warnings for the BPTR-vs-void * mix. */
long sys_write(int fd, const unsigned char *buf, unsigned long n) {
    BPTR fh = (fd == 0) ? Input() : Output();
    return (long)Write(fh, (APTR)buf, (LONG)n);
}

long sys_read(int fd, unsigned char *buf, unsigned long n) {
    BPTR fh = (fd == 0) ? Input() : Output();
    return (long)Read(fh, (APTR)buf, (LONG)n);
}

/* DR-032 sys_open / sys_close stubs.  Amiga DOS uses BPTR file
 * handles (BCPL pointers), not the small-int fds the seam exposes
 * — wiring them up cleanly needs a BPTR->fd mapping table on the
 * runtime side and a stable convention for which fd values point
 * at Input/Output vs. opened files.  Single-file bootstrap on
 * stdin/stdout (the path the future self-host port takes for
 * its initial run) doesn't open new files, so the stubs return
 * -1 (failure) for now.  Real implementation lands when the port
 * starts loading multi-file modules. */
int sys_open(const char *_path, int _flags) {
    (void)_path;
    (void)_flags;
    return -1;
}

int sys_close(int _fd) {
    (void)_fd;
    return -1;
}
