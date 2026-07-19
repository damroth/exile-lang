#include <stdio.h>

long alpha__tag_i32(long x);
long beta__tag_i32(long x);

int main(void) {
    printf("%ld\n", (long)(alpha__tag_i32(((long)1))));
    printf("%ld\n", (long)(beta__tag_i32(((long)1))));
    return 0;
    return 0;
}

long alpha__tag_i32(long x) {
    return ((long)10);
}

long beta__tag_i32(long x) {
    return ((long)20);
}
