#include <stdio.h>

struct pair__P_i32 { long first; long second; };
struct duo__P_str { const char *first; const char *second; };

struct pair__P_i32 pair__make(long x, long y);
long pair__sum(struct pair__P_i32 p);
struct duo__P_str duo__make(const char *x, const char *y);
const char *duo__head(struct duo__P_str p);

struct pair__P_i32 pair__make(long x, long y) {
    {
        struct pair__P_i32 __exile_ret;
        __exile_ret.first = x;
        __exile_ret.second = y;
        return __exile_ret;
    }
}

long pair__sum(struct pair__P_i32 p) {
    return p.first + p.second;
}

struct duo__P_str duo__make(const char *x, const char *y) {
    {
        struct duo__P_str __exile_ret;
        __exile_ret.first = x;
        __exile_ret.second = y;
        return __exile_ret;
    }
}

const char *duo__head(struct duo__P_str p) {
    return p.first;
}

int main(void) {
    printf("%ld\n", (long)(pair__sum(pair__make(((long)20), ((long)22)))));
    printf("%s\n", duo__head(duo__make("first", "second")));
    return 0;
    return 0;
}
