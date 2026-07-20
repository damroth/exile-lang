#include <stdio.h>

struct ex_Pair_i32 { long a; long b; };
struct ex_Box_ex_Pair_i32 { struct ex_Pair_i32 it; };

int main(void) {
    struct ex_Box_ex_Pair_i32 x;
    struct ex_Pair_i32 __lift_0;
    __lift_0.a = 1;
    __lift_0.b = 2;
    x.it = __lift_0;
    printf("%ld\n", (long)(x.it.b));
    return 0;
    return 0;
}
