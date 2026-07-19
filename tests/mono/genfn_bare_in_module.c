#include <stdio.h>

long inner__call_local(void);
long inner__label_i32(long x);
static long ex_label_i32(long x);

long inner__call_local(void) {
    return inner__label_i32(((long)0));
}

int main(void) {
    printf("%ld\n", (long)(ex_label_i32(((long)0))));
    printf("%ld\n", (long)(inner__call_local()));
    printf("%ld\n", (long)(inner__label_i32(((long)0))));
    return 0;
    return 0;
}

long inner__label_i32(long x) {
    return ((long)2);
}

static long ex_label_i32(long x) {
    return ((long)1);
}
