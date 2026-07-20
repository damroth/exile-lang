#include <stdio.h>

struct m__Box_i32 { long it; };

struct m__Box_i32 m__make(long x);
long m__Box__show_i32(struct m__Box_i32 self);

struct m__Box_i32 m__make(long x) {
    {
        struct m__Box_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

int main(void) {
    printf("%ld\n", (long)(m__Box__show_i32(m__make(1))));
    return 0;
    return 0;
}

long m__Box__show_i32(struct m__Box_i32 self) {
    return 2;
}
