#include <stdio.h>

enum opt__Maybe_i32_tag { opt__Maybe_i32_Nothing, opt__Maybe_i32_Just };
struct opt__Maybe_i32 { enum opt__Maybe_i32_tag tag; union { struct { long _0; } Just; } data; };

struct opt__Maybe_i32 opt__some(long x);

struct opt__Maybe_i32 opt__some(long x) {
    {
        struct opt__Maybe_i32 __exile_ret;
        __exile_ret.tag = opt__Maybe_i32_Just;
        __exile_ret.data.Just._0 = x;
        return __exile_ret;
    }
}

int main(void) {
    struct opt__Maybe_i32 m;
    m = opt__some(8);
    {
        struct opt__Maybe_i32 __m;
        __m = m;
        switch (__m.tag) {
        case opt__Maybe_i32_Just:
            {
                long v = __m.data.Just._0;
                printf("%ld\n", (long)(v));
                break;
            }
        case opt__Maybe_i32_Nothing:
        default:
            {
                printf("%ld\n", (long)(0));
                break;
            }
        }
    }
    return 0;
    return 0;
}
