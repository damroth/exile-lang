#include <stdio.h>

struct shapes__Rect { long w; long h; };

struct shapes__Rect shapes__make(long w, long h);
long shapes__Rect__area(struct shapes__Rect self);

struct shapes__Rect shapes__make(long w, long h) {
    {
        struct shapes__Rect __exile_ret;
        __exile_ret.w = w;
        __exile_ret.h = h;
        return __exile_ret;
    }
}

int main(void) {
    printf("%ld\n", (long)(shapes__Rect__area(shapes__make(3, 5))));
    return 0;
    return 0;
}

long shapes__Rect__area(struct shapes__Rect self) {
    return self.w * self.h;
}
