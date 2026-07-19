#include <stdio.h>

enum inner__Box_i32_tag { inner__Box_i32_Empty, inner__Box_i32_Full };
struct inner__Box_i32 { enum inner__Box_i32_tag tag; union { struct { long _0; } Full; } data; };
enum other__Box_i32_tag { other__Box_i32_Full, other__Box_i32_Pair };
struct other__Box_i32 { enum other__Box_i32_tag tag; union { struct { long _0; } Full; struct { long _0; long _1; } Pair; } data; };

struct inner__Box_i32 inner__wrap(long x);
struct other__Box_i32 other__wrap(long x);

struct inner__Box_i32 inner__wrap(long x) {
    {
        struct inner__Box_i32 __exile_ret;
        __exile_ret.tag = inner__Box_i32_Full;
        __exile_ret.data.Full._0 = x;
        return __exile_ret;
    }
}

struct other__Box_i32 other__wrap(long x) {
    {
        struct other__Box_i32 __exile_ret;
        __exile_ret.tag = other__Box_i32_Pair;
        __exile_ret.data.Pair._0 = x;
        __exile_ret.data.Pair._1 = x;
        return __exile_ret;
    }
}

int main(void) {
    inner__wrap(((long)7));
    other__wrap(((long)8));
    printf("%s\n", "built");
    return 0;
    return 0;
}
