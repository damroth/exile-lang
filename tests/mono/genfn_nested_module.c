#include <stdio.h>

long outer__deep__twice_i32(long x, int f);

int main(void) {
    printf("%ld\n", (long)(outer__deep__twice_i32(((long)0), 0)));
    return 0;
    return 0;
}

long outer__deep__twice_i32(long x, int f) {
    if (f) {
        return ((long)2);
    }
    return ((long)4);
}
