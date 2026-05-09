/* Minimal callback bridge: takes an int and a unary int->int
 * function pointer, returns f(x).  Demonstrates that exile fns can
 * be passed across the FFI boundary and invoked from C. */

int ffi_apply_int(int x, int (*f)(int)) {
    return f(x);
}
