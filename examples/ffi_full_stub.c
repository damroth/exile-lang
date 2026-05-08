#include "ffi_full_stub.h"

/* Concrete definitions for the constants the header declares
 * `extern`. */
const ULONG PRETEND_VERSION = 39;
const ULONG PRETEND_FLAGS   = 3;

/* Mock library with a single opaque instance. */
struct Library {
    ULONG id;
};

static struct Library the_lib = { 0xCAFE };

struct Library *PretendOpenLibrary(const char *name, ULONG version) {
    (void)name; (void)version;
    return &the_lib;
}

void PretendCloseLibrary(struct Library *lib) {
    (void)lib;
}

ULONG PretendLibraryID(struct Library *lib) {
    return lib->id;
}
