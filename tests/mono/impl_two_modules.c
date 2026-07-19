#include <stdio.h>

struct alpha__Holder_i32 { long it; };
struct beta__Holder_i32 { long it; };

struct alpha__Holder_i32 alpha__make(long x);
struct beta__Holder_i32 beta__make(long x);
long alpha__Holder__get_i32(const struct alpha__Holder_i32 *self);
long beta__Holder__get_i32(const struct beta__Holder_i32 *self);

struct alpha__Holder_i32 alpha__make(long x) {
    {
        struct alpha__Holder_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

struct beta__Holder_i32 beta__make(long x) {
    {
        struct beta__Holder_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

int main(void) {
    struct alpha__Holder_i32 __lift_0;
    struct beta__Holder_i32 __lift_1;
    __lift_0 = alpha__make(((long)1));
    printf("%ld\n", (long)(alpha__Holder__get_i32(&__lift_0)));
    __lift_1 = beta__make(((long)2));
    printf("%ld\n", (long)(beta__Holder__get_i32(&__lift_1)));
    return 0;
    return 0;
}

long alpha__Holder__get_i32(const struct alpha__Holder_i32 *self) {
    return self->it;
}

long beta__Holder__get_i32(const struct beta__Holder_i32 *self) {
    return self->it + ((long)100);
}
