/* Opaque library handle — the struct's actual fields are an
 * implementation detail kept entirely on the C side; exile never
 * sees them.  In a real AmigaOS scenario this would be the system's
 * `struct Library` from <exec/libraries.h>. */

struct Library {
    long counter;
};

static struct Library the_library = { 100 };

struct Library *lib_open(void) {
    return &the_library;
}

long lib_use(struct Library *lib) {
    return lib->counter;
}

void lib_close(struct Library *lib) {
    (void)lib;
}
