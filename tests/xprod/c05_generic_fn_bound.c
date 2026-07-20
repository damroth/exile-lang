#include <stdio.h>

struct ex_Rect { long w; };

long Rect__show(struct ex_Rect self);
static long ex_twice_ex_Rect(struct ex_Rect x);

int main(void) {
    struct ex_Rect __lift_0;
    __lift_0.w = 6;
    printf("%ld\n", (long)(ex_twice_ex_Rect(__lift_0)));
    return 0;
    return 0;
}

long Rect__show(struct ex_Rect self) {
    return self.w;
}

static long ex_twice_ex_Rect(struct ex_Rect x) {
    return Rect__show(x) * 2;
}
