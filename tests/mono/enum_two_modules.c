#include <stdio.h>

enum inner__Box_i32_tag { inner__Box_i32_Empty, inner__Box_i32_Full };
struct inner__Box_i32 { enum inner__Box_i32_tag tag; union { struct { long _0; } Full; } data; };
enum other__Box_str_tag { other__Box_str_Empty, other__Box_str_Full };
struct other__Box_str { enum other__Box_str_tag tag; union { struct { const char *_0; } Full; } data; };

struct inner__Box_i32 inner__wrap(long x);
long inner__open(struct inner__Box_i32 b);
struct other__Box_str other__wrap(const char *s);
const char *other__open(struct other__Box_str b);

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

struct other__Box_str other__wrap(const char *s) {
    {
        struct other__Box_str __exile_ret;
        __exile_ret.tag = other__Box_str_Full;
        __exile_ret.data.Full._0 = s;
        return __exile_ret;
    }
}

const char *other__open(struct other__Box_str b) {
    {
        const char * __exile_ret;
        {
            struct other__Box_str __m;
            __m = b;
            switch (__m.tag) {
            case other__Box_str_Full:
                {
                    const char *v = __m.data.Full._0;
                    __exile_ret = v;
                    break;
                }
            case other__Box_str_Empty:
            default:
                {
                    __exile_ret = "empty";
                    break;
                }
            }
        }
        return __exile_ret;
    }
}

int main(void) {
    printf("%ld\n", (long)(inner__open(inner__wrap(((long)42)))));
    printf("%s\n", other__open(other__wrap("hello")));
    return 0;
    return 0;
}
