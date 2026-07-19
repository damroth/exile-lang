#include <stdio.h>

long util__pick_i32(long a, long b, int first);

int main(void) {
    printf("%ld\n", (long)(util__pick_i32(((long)3), ((long)9), 0)));
    printf("%ld\n", (long)(util__pick_i32(((long)1), ((long)2), 1)));
    return 0;
    return 0;
}

long util__pick_i32(long a, long b, int first) {
    if (first) {
        return a;
    }
    return b;
}
