/* sys_bare.c — the seam on BARE METAL: no AmigaOS, no libc, no exec.
 *
 * `runtime/sys_amiga.c` is the same seam over AmigaOS; this file is its twin
 * over the silicon.  Both answer the identical five symbols an emitted
 * `--freestanding` program can reference, measured with `nm -u` rather than
 * assumed:
 *
 *     sys_write                 the print front, via runtime/freestanding.c
 *     sys_alloc / sys_free      any growable collection
 *     sys_seal_enter / _exit    any `seal` region
 *
 * Nothing else. A program that neither allocates nor seals needs `sys_write`
 * alone, which is why the contract is small enough to hand-implement per target.
 *
 * The seal half is the reason this file could be written at all. The AmigaOS
 * twin says, in its own comment, that under exec the language must NOT write SR
 * — the scheduler keeps its own Disable nesting and touching SR behind its back
 * breaks it — so there the token is exec's DEPTH. Here there is no scheduler and
 * the token is a SAVED SR. One guarantee, two things saved: that is what the
 * seam's opaque token was for, written before this side of it existed.
 */

/* ---- custom-chip registers used here ------------------------------------- */
#define CUSTOM_BASE 0xDFF000UL
#define SERDAT   (*(volatile unsigned short *)(CUSTOM_BASE + 0x030))
#define SERDATR  (*(volatile unsigned short *)(CUSTOM_BASE + 0x018))
#define SERPER   (*(volatile unsigned short *)(CUSTOM_BASE + 0x032))
#define SERDATR_TBE 0x2000  /* transmit buffer empty */

/* PAL colour clock; SERPER = clock/baud - 1. 9600 8N1 is the rate a bare-metal
 * Amiga console is conventionally read at. A caller that has already programmed
 * SERPER can define EX_BARE_NO_SERPER to leave it alone. */
#define EX_PAL_CLOCK 3546895UL
#define EX_BAUD      9600UL

/* ---- write: the serial line, not a filesystem ----------------------------- */
/* There is no dos.library here and no fd table: everything goes to the serial
 * port, which is the console a bare-metal Amiga actually has. `fd` is accepted
 * and ignored rather than dropped from the signature, because the seam is shared
 * with the hosted backends and a target-specific signature would not be one seam.
 */
static int ex_serial_ready = 0;

static void ex_serial_init(void) {
#ifndef EX_BARE_NO_SERPER
    SERPER = (unsigned short)((EX_PAL_CLOCK / EX_BAUD) - 1UL);
#endif
    ex_serial_ready = 1;
}

static void ex_serial_putc(unsigned char c) {
    while ((SERDATR & SERDATR_TBE) == 0) { }
    /* 8N1: the 0x100 bit is the stop bit the hardware shifts out after the byte. */
    SERDAT = (unsigned short)(0x100u | (unsigned int)c);
}

long sys_write(int fd, const unsigned char *buf, unsigned long n) {
    unsigned long i;
    (void)fd;
    if (!ex_serial_ready) ex_serial_init();
    for (i = 0; i < n; i++) ex_serial_putc(buf[i]);
    return (long)n;
}

/* ---- allocation: a bump arena, and it says so ----------------------------- */
/* Bare metal has no allocator, so this is one: a static arena handed out in
 * order. `free` rewinds ONLY the most recent block — the case `Vec::grow` hits
 * when it doubles and releases the old buffer immediately — and is otherwise a
 * NO-OP. That is a leak, stated rather than hidden: a kernel that needs real
 * reclamation supplies its own sys_alloc/sys_free, which is the whole point of
 * these being a seam and not a runtime.
 *
 * EX_BARE_HEAP_BYTES sizes the arena at compile time; exhaustion returns NULL,
 * which every caller in the prelude already treats as failure. */
#ifndef EX_BARE_HEAP_BYTES
#define EX_BARE_HEAP_BYTES 65536
#endif

static unsigned char ex_heap[EX_BARE_HEAP_BYTES];
static unsigned long ex_brk = 0;

void *sys_alloc(void *_state, unsigned long n) {
    unsigned long start;
    (void)_state;
    /* Align to 4: the 68000 faults on a misaligned word access, so an unaligned
     * block would turn a correct program into an address error. */
    n = (n + 3UL) & ~3UL;
    if (n > (unsigned long)EX_BARE_HEAP_BYTES - ex_brk) return 0;
    start = ex_brk;
    ex_brk += n;
    return (void *)&ex_heap[start];
}

void sys_free(void *_state, void *p, unsigned long n) {
    unsigned long off;
    (void)_state;
    if (p == 0) return;
    n = (n + 3UL) & ~3UL;
    off = (unsigned long)((unsigned char *)p - ex_heap);
    if (off + n == ex_brk) ex_brk = off;   /* the last block, and only that one */
}

/* ---- seal: the saved SR ---------------------------------------------------- */
/* `move.w sr,d` and `move.w d,sr` are the whole implementation. On the 68000
 * both are user-mode legal; on 68010+ reading SR is privileged, and bare-metal
 * code runs in supervisor mode anyway, which is the state this backend assumes.
 *
 * Masking sets IPL to 7 (bits 10-8) and leaves every other SR bit alone, so the
 * condition codes a caller had are still there when the region ends. Restoring
 * writes the WHOLE saved word back, which is what makes nesting hold: an inner
 * region restores the outer region's mask rather than unmasking outright. */
unsigned long sys_seal_enter(void) {
    unsigned short sr;
    __asm__ __volatile__("move.w %%sr,%0" : "=d"(sr));
    __asm__ __volatile__("or.w #0x0700,%%sr" : : : "cc");
    return (unsigned long)sr;
}

void sys_seal_exit(unsigned long tok) {
    unsigned short sr = (unsigned short)tok;
    __asm__ __volatile__("move.w %0,%%sr" : : "d"(sr) : "cc");
}
