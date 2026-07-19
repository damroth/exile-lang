#include <stdio.h>

struct alpha__Holder_i32 { long it; };

struct alpha__Holder_i32 alpha__make(long x);
long alpha__Holder__get_i32(const struct alpha__Holder_i32 *self);

struct alpha__Holder_i32 alpha__make(long x) {
    {
        struct alpha__Holder_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

int main(void) {
    struct alpha__Holder_i32 h;
    h = alpha__make(((long)3));
    printf("%ld\n", (long)(alpha__Holder__get_i32(&h)));
    return 0;
    return 0;
}

long alpha__Holder__get_i32(const struct alpha__Holder_i32 *self) {
    return self->it;
}
