#include <stdio.h>

struct store__Box_i32 { long it; };

struct store__Box_i32 store__make(long x);
long store__Box__get_i32(const struct store__Box_i32 *self);

struct store__Box_i32 store__make(long x) {
    {
        struct store__Box_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

int main(void) {
    struct store__Box_i32 __lift_0;
    __lift_0 = store__make(9);
    printf("%ld\n", (long)(store__Box__get_i32(&__lift_0)));
    return 0;
    return 0;
}

long store__Box__get_i32(const struct store__Box_i32 *self) {
    return self->it;
}
