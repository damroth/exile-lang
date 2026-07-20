#include <stdio.h>

struct ex___closure_0 { long base; };

long __closure_0__call(const struct ex___closure_0 *self, long x);
static long ex_apply2_ex___closure_0(struct ex___closure_0 f, long n);

int main(void) {
    long base;
    struct ex___closure_0 g;
    base = 10;
    g.base = base;
    printf("%ld\n", (long)(ex_apply2_ex___closure_0(g, 5)));
    return 0;
    return 0;
}

long __closure_0__call(const struct ex___closure_0 *self, long x) {
    return x + self->base;
}

static long ex_apply2_ex___closure_0(struct ex___closure_0 f, long n) {
    return __closure_0__call(&f, n);
}
