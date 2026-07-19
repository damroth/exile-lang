#include <stdio.h>

enum inner__Box_i32_tag { inner__Box_i32_Empty, inner__Box_i32_Full };
struct inner__Box_i32 { enum inner__Box_i32_tag tag; union { struct { long _0; } Full; } data; };

struct inner__Box_i32 inner__wrap(long x);
long inner__open(struct inner__Box_i32 b);
static long ex_take(struct inner__Box_i32 b);

struct inner__Box_i32 inner__wrap(long x) {
    {
        struct inner__Box_i32 __exile_ret;
        __exile_ret.tag = inner__Box_i32_Full;
        __exile_ret.data.Full._0 = x;
        return __exile_ret;
    }
}

long inner__open(struct inner__Box_i32 b) {
    {
        long __exile_ret;
        {
            struct inner__Box_i32 __m;
            __m = b;
            switch (__m.tag) {
            case inner__Box_i32_Full:
                {
                    long v = __m.data.Full._0;
                    __exile_ret = v;
                    break;
                }
            case inner__Box_i32_Empty:
            default:
                {
                    __exile_ret = ((long)0);
                    break;
                }
            }
        }
        return __exile_ret;
    }
}

static long ex_take(struct inner__Box_i32 b) {
    return inner__open(b);
}

int main(void) {
    struct inner__Box_i32 b;
    b = inner__wrap(((long)11));
    printf("%ld\n", (long)(ex_take(b)));
    return 0;
    return 0;
}
