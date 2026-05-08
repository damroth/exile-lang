/* Header-side definitions: aliasy typu, opaque struct, stałe,
 * prototypy.  Exile importuje wszystko przez `extern type/struct/
 * const/fn` razem z `@c_include("ffi_full_stub.h")`. */

#ifndef FFI_FULL_STUB_H
#define FFI_FULL_STUB_H

typedef unsigned long ULONG;

struct Library;

/* Header declares constants as `extern` symbols (defined in the .c
 * stub).  In real AmigaOS bindings these would be `#define` macros
 * re-exported by a small bridging stub; the pattern is the same. */
extern const ULONG PRETEND_VERSION;
extern const ULONG PRETEND_FLAGS;

struct Library *PretendOpenLibrary(const char *name, ULONG version);
void PretendCloseLibrary(struct Library *lib);
ULONG PretendLibraryID(struct Library *lib);

#endif
