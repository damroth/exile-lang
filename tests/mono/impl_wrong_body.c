#include <stdio.h>

struct alpha__Holder_i32 { long it; };

long alpha__decoy0(long _x);
long alpha__decoy1(long _x);
struct alpha__Holder_i32 alpha__make(long x);
long alpha__Holder__second_i32(const struct alpha__Holder_i32 *self);

long alpha__decoy0(long _x) {
    return ((long)1000);
}

long alpha__decoy1(long _x) {
    return ((long)2000);
}

struct alpha__Holder_i32 alpha__make(long x) {
    {
        struct alpha__Holder_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

int main(void) {
    struct alpha__Holder_i32 h;
    h = alpha__make(((long)5));
    printf("%ld\n", (long)(alpha__Holder__second_i32(&h)));
    printf("%ld\n", (long)(alpha__decoy1(((long)0))));
    return 0;
    return 0;
}

long alpha__Holder__second_i32(const struct alpha__Holder_i32 *self) {
    return self->it;
}
