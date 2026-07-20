#include <stdio.h>

struct ex_Box_i32 { long it; };

long Box__show_i32(struct ex_Box_i32 self);

int main(void) {
    struct ex_Box_i32 __lift_0;
    __lift_0.it = 5;
    printf("%ld\n", (long)(Box__show_i32(__lift_0)));
    return 0;
    return 0;
}

long Box__show_i32(struct ex_Box_i32 self) {
    return 1;
}
