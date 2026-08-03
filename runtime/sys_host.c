/* runtime/sys_host.c — host backend for the `sys::*` seam.
 *
 * Implements the prelude-declared `extern fn sys_*` thunks against
 * libc.  Auto-linked by `exilc --target host` whenever the program
 * uses the `sys` module (or any feature that lowers to it — e.g.
 * `default_allocator()`).
 *
 * Each `sys_*` here is the host fork of the layer-1 service seam
 * in exile.  Other targets ship their own backend (e.g.
 * `runtime/sys_amiga.c` will wrap dos.library / exec.library).
 * Layer-0 stdlib code (Vec / HashMap / StringBuilder / String) is
 * target-agnostic and stays in exile, threading through `sys::*`
 * without knowing the implementation. */

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>

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

/* sys_open / sys_close.  `flags` follows the POSIX numeric
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

/* Format an IEEE double to a round-trip decimal, the byte-exact string the
 * codegen needs for a C float literal.  Mirrors the oracle's OCaml
 * `Printf.sprintf "%.17g"` / `"%.9g"` (OCaml's float formatting is libc's), so
 * the self-hosted codegen emits the same literal.  Writes into `buf` (caller
 * sizes it >= 40) and returns the length; the `.0` / `f` decoration is applied
 * by the caller, matching where the oracle applies it. */
unsigned long sys_fmt_f64(double f, int is32, unsigned char *buf) {
    /* sprintf, not snprintf: the runtime compiles under -ansi (C89), where
     * snprintf is not declared.  `%.17g` of a double is at most ~24 bytes
     * (sign + 17 digits + '.' + 'e' + sign + 3-digit exponent), well inside
     * the caller's 40-byte buffer. */
    int n = sprintf((char *)buf, is32 ? "%.9g" : "%.17g", f);
    return (unsigned long)n;
}

/* The argv seam — the host backend reads the command line from
 * /proc/self/cmdline (Linux) once, on first use, and caches the parsed
 * vector.  This keeps the generated `int main(void)` untouched: argv never
 * has to be threaded through the entry point, so every existing C dump and
 * the bootstrap fixpoint stay byte-identical.  A program that never calls
 * sys_argc/sys_argv pays nothing (the cmdline file is opened lazily).
 *
 * The amiga backend will implement these against its own argument source;
 * the exile caller is target-agnostic. */
#define SYS_ARGV_MAX 256
#define SYS_ARGV_BUF 8192
static int   sys_argv_ready = 0;
static int   sys_argv_count = 0;
static char  sys_argv_buf[SYS_ARGV_BUF];
static char *sys_argv_ptr[SYS_ARGV_MAX];

static void sys_argv_init(void) {
    FILE *f;
    unsigned long n, i;
    int started;
    if (sys_argv_ready) return;
    sys_argv_ready = 1;
    f = fopen("/proc/self/cmdline", "rb");
    if (!f) return;
    n = fread(sys_argv_buf, 1, (size_t)(SYS_ARGV_BUF - 1), f);
    fclose(f);
    sys_argv_buf[n] = '\0';
    /* cmdline is NUL-separated; each run of non-NUL bytes is one argument. */
    started = 0;
    for (i = 0; i < n && sys_argv_count < SYS_ARGV_MAX; i++) {
        if (!started && sys_argv_buf[i] != '\0') {
            sys_argv_ptr[sys_argv_count++] = &sys_argv_buf[i];
            started = 1;
        } else if (sys_argv_buf[i] == '\0') {
            started = 0;
        }
    }
}

int sys_argc(void) {
    sys_argv_init();
    return sys_argv_count;
}

const char *sys_argv(int i) {
    sys_argv_init();
    if (i < 0 || i >= sys_argv_count) return (const char *)0;
    return sys_argv_ptr[i];
}

/* cc-invoke seam — spawn argv[0] with argv[0..n-1], searching PATH, and
 * wait.  No shell: the caller (exilc --target host) hands an argv array
 * built as a Vec<str>, so there is nothing to quote.  This is the port's
 * fork of the oracle's `Sys.command` (= libc system()) — a driver-zone
 * divergence: better plumbing than the oracle's shell, gated behaviorally
 * (both compilers build or both fail), not by byte-comparing a command
 * string.
 *
 * Returns the child's exit status: 0 on success, the program's exit code
 * otherwise, 127 if exec itself failed (the shell convention, so an
 * exec-not-found is distinguishable from a clean build), or -1 on a fork /
 * wait / arity-limit error. */
#define SYS_SPAWN_MAX 256
int sys_spawn(const char *const *argv, int n) {
    char *local[SYS_SPAWN_MAX + 1];
    pid_t pid;
    int status, i;
    if (n <= 0 || n > SYS_SPAWN_MAX) return -1;
    for (i = 0; i < n; i++) local[i] = (char *)argv[i];
    local[n] = (char *)0;
    pid = fork();
    if (pid < 0) return -1;
    if (pid == 0) {
        execvp(local[0], local);
        _exit(127);
    }
    if (waitpid(pid, &status, 0) < 0) return -1;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    return -1;
}

/* Terminate the process with `code` — the port fork of the oracle's `exit 1`,
 * so a self-hosted exilc fails a build with a non-zero status the way the
 * reference does (a Makefile calling exilc can tell a failed build from a
 * clean one). */
void sys_exit(int code) {
    exit(code);
}

/* ---- seal seam -------------------------------------------------------------
   The host has no interrupts to mask, so the stub exists to WITNESS the
   guarantee rather than to provide it.  It keeps a counter AND a stack of
   handed-out tokens: balance alone proves exactly-once (every enter has an exit)
   but cannot see MIS-NESTING, which nesting forbids — so exit asserts it was
   handed exactly the top of the stack.                                        */
#define EX_SEAL_MAX 64
static unsigned long ex_seal_stack[EX_SEAL_MAX];
static int ex_seal_depth = 0;
static unsigned long ex_seal_next = 1;
static int ex_seal_enters = 0;
static int ex_seal_exits = 0;
static int ex_seal_misnest = 0;

/* Reported at exit, but only by a program that actually entered a seal: the
   report registers itself on first enter, so the seam stays two functions wide
   and non-seal programs keep their exact stdout.                               */
static int ex_seal_armed = 0;
static void ex_seal_report(void) {
    printf("seal-balance %d misnest %d\n", ex_seal_enters - ex_seal_exits, ex_seal_misnest);
}

unsigned long sys_seal_enter(void) {
    unsigned long tok = ex_seal_next++;
    if (!ex_seal_armed) { ex_seal_armed = 1; atexit(ex_seal_report); }
    if (ex_seal_depth < EX_SEAL_MAX) { ex_seal_stack[ex_seal_depth] = tok; }
    ex_seal_depth++;
    ex_seal_enters++;
    return tok;
}

void sys_seal_exit(unsigned long tok) {
    if (!ex_seal_armed) { ex_seal_armed = 1; atexit(ex_seal_report); }
    ex_seal_exits++;
    if (ex_seal_depth <= 0) { ex_seal_misnest++; return; }
    ex_seal_depth--;
    if (ex_seal_depth < EX_SEAL_MAX && ex_seal_stack[ex_seal_depth] != tok) { ex_seal_misnest++; }
}

/* Environment + cwd for the amiga driver's toolchain resolution.  `getenv`
 * returns NULL when the name is unset; the caller distinguishes that from a
 * set-but-empty value, because the reference does.  `getcwd` writes into a
 * static buffer: the driver reads it once, at startup, to build one path. */
const char *sys_getenv(const char *name) {
    return getenv(name);
}

static char sys_cwd_buf[4096];

const char *sys_getcwd(void) {
    if (getcwd(sys_cwd_buf, sizeof sys_cwd_buf) == 0) {
        sys_cwd_buf[0] = '\0';
    }
    return sys_cwd_buf;
}
