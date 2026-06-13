/* freestanding.c — libc-free implementations of the __ex_* helpers that
 * exilc's `--freestanding` output calls instead of printf/strlen/memset.
 * Everything goes through the `sys_write` seam; no libc, no headers.
 * Build with `-ffreestanding -nostdlib` and link against a sys_*.c backend.
 */
#include "freestanding.h"

/* One byte to stdout — the shared primitive for newlines and digits. */
static void ex_putc(unsigned char c) {
    sys_write(1, &c, 1UL);
}

unsigned long __ex_strlen(const char *s) {
    unsigned long n = 0UL;
    while (s[n] != '\0') n++;
    return n;
}

void __ex_memzero(void *p, unsigned long n) {
    unsigned char *b = (unsigned char *)p;
    unsigned long i;
    for (i = 0UL; i < n; i++) b[i] = 0;
}

void __ex_print_str(const char *s) {
    sys_write(1, (const unsigned char *)s, __ex_strlen(s));
}

void __ex_println_str(const char *s) {
    __ex_print_str(s);
    ex_putc((unsigned char)'\n');
}

void __ex_print_str_quoted(const char *s) {
    /* `"` + raw bytes + `"` — matches the hosted printf("\"%s\"") Debug
     * rendering exactly (no escaping; lossy for embedded quotes, as on
     * the hosted side). */
    ex_putc((unsigned char)'"');
    __ex_print_str(s);
    ex_putc((unsigned char)'"');
}

void __ex_print_u32(unsigned long u) {
    unsigned char buf[24];
    int i = (int)sizeof(buf);
    if (u == 0UL) {
        buf[--i] = (unsigned char)'0';
    } else {
        while (u > 0UL) {
            buf[--i] = (unsigned char)('0' + (int)(u % 10UL));
            u /= 10UL;
        }
    }
    sys_write(1, &buf[i], (unsigned long)((int)sizeof(buf) - i));
}

void __ex_print_i32(long v) {
    unsigned long u;
    int neg = 0;
    if (v < 0) {
        neg = 1;
        /* negate via unsigned to stay defined at LONG_MIN */
        u = (unsigned long)(-(v + 1)) + 1UL;
    } else {
        u = (unsigned long)v;
    }
    if (neg) ex_putc((unsigned char)'-');
    __ex_print_u32(u);
}

void __ex_println_i32(long v) {
    __ex_print_i32(v);
    ex_putc((unsigned char)'\n');
}

void __ex_println_u32(unsigned long v) {
    __ex_print_u32(v);
    ex_putc((unsigned char)'\n');
}

void __ex_print_ptr(const void *p) {
    static const char hex[] = "0123456789abcdef";
    unsigned long v = (unsigned long)p;
    unsigned char buf[2 + 2 * sizeof(void *)];
    int i = (int)sizeof(buf);
    if (v == 0UL) {
        buf[--i] = (unsigned char)'0';
    } else {
        while (v > 0UL) {
            buf[--i] = (unsigned char)hex[(int)(v & 0xFUL)];
            v >>= 4;
        }
    }
    buf[--i] = (unsigned char)'x';
    buf[--i] = (unsigned char)'0';
    sys_write(1, &buf[i], (unsigned long)((int)sizeof(buf) - i));
}
