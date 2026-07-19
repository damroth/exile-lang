#include <stdio.h>

struct outer__deep__Cell_i32 { long it; };

struct outer__deep__Cell_i32 outer__deep__make(long x);

struct outer__deep__Cell_i32 outer__deep__make(long x) {
    {
        struct outer__deep__Cell_i32 __exile_ret;
        __exile_ret.it = x;
        return __exile_ret;
    }
}

int main(void) {
    struct outer__deep__Cell_i32 c;
    c = outer__deep__make(((long)5));
    printf("%ld\n", (long)(c.it));
    return 0;
    return 0;
}
