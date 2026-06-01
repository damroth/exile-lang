let cc_check label c_code =
  let tmp = Filename.temp_file "exile_test_" ".c" in
  Out_channel.with_open_text tmp (fun oc -> Out_channel.output_string oc c_code);
  let quiet = Printf.sprintf
    "cc -ansi -pedantic -Wall -Werror -c -o /dev/null %s 2>/dev/null"
    (Filename.quote tmp)
  in
  if Sys.command quiet <> 0 then begin
    Printf.eprintf "FAIL: %s (cc rejected)\n--- C output ---\n%s--- cc diagnostics ---\n"
      label c_code;
    let verbose = Printf.sprintf
      "cc -ansi -pedantic -Wall -Werror -c -o /dev/null %s"
      (Filename.quote tmp)
    in
    let _ = Sys.command verbose in
    Sys.remove tmp;
    exit 1
  end;
  Sys.remove tmp

let check label src expected =
  let actual = Exile_lang.Compiler.compile src in
  if actual <> expected then begin
    Printf.eprintf "FAIL: %s\n--- expected ---\n%s--- got ---\n%s" label expected actual;
    exit 1
  end;
  cc_check label actual;
  Printf.printf "ok: %s\n" label

(* Like `check`, but skip the cc compile step.  Use when the generated
   C deliberately references an unresolved external (extern type alias
   without its defining header, etc) — cc's job here is the user's,
   not ours. *)
let check_no_cc label src expected =
  let actual = Exile_lang.Compiler.compile src in
  if actual <> expected then begin
    Printf.eprintf "FAIL: %s\n--- expected ---\n%s--- got ---\n%s" label expected actual;
    exit 1
  end;
  Printf.printf "ok: %s\n" label

let check_multi label files entry_relpath expected =
  let dir = Filename.temp_file "exile_multi_" "" in
  Sys.remove dir;
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)) in
  List.iter (fun (relpath, content) ->
    let full = Filename.concat dir relpath in
    let parent = Filename.dirname full in
    let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote parent)) in
    Out_channel.with_open_text full (fun oc ->
      Out_channel.output_string oc content)) files;
  let entry = Filename.concat dir entry_relpath in
  let actual = Exile_lang.Compiler.compile_file entry in
  let cleanup () =
    ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))
  in
  if actual <> expected then begin
    Printf.eprintf "FAIL: %s\n--- expected ---\n%s--- got ---\n%s"
      label expected actual;
    cleanup ();
    exit 1
  end;
  cc_check label actual;
  cleanup ();
  Printf.printf "ok: %s\n" label

let check_error label src expected_msg =
  match Exile_lang.Compiler.compile src with
  | exception Exile_lang.Error.Compile_error { msg; _ } when msg = expected_msg ->
      Printf.printf "ok: %s\n" label
  | exception Exile_lang.Error.Compile_error { msg; _ } ->
      Printf.eprintf "FAIL: %s\n--- expected error ---\n%s\n--- got error ---\n%s\n"
        label expected_msg msg;
      exit 1
  | _ ->
      Printf.eprintf "FAIL: %s\n--- expected error ---\n%s\n--- got: success\n"
        label expected_msg;
      exit 1

let check_assert label cond =
  if cond then Printf.printf "ok: %s\n" label
  else begin
    Printf.eprintf "FAIL: %s\n" label;
    exit 1
  end

(* Run the front end up to typecheck and return the linter's warnings
   for the given profile.  Pure — does not touch stderr. *)
let lint_warnings ~profile src =
  src
  |> Exile_lang.Lexer.tokenize ~file:"<input>"
  |> Exile_lang.Parser.parse_program
  |> Exile_lang.Typecheck.check_program
  |> Exile_lang.Lint.collect ~profile

let check_lint label src ~profile expected_msg_substrs =
  let ws = lint_warnings ~profile src in
  let msgs = List.map (fun w -> w.Exile_lang.Lint.msg) ws in
  let n_expected = List.length expected_msg_substrs in
  let n_got = List.length msgs in
  if n_got <> n_expected then begin
    Printf.eprintf
      "FAIL: %s\n--- expected %d warning(s) ---\n%s\n--- got %d ---\n%s\n"
      label n_expected
      (String.concat "\n" expected_msg_substrs)
      n_got (String.concat "\n" msgs);
    exit 1
  end;
  List.iter2 (fun expected actual ->
    let contains s sub =
      let ls = String.length s and lsub = String.length sub in
      let rec loop i =
        if i + lsub > ls then false
        else if String.sub s i lsub = sub then true
        else loop (i + 1)
      in
      loop 0
    in
    if not (contains actual expected) then begin
      Printf.eprintf
        "FAIL: %s\n--- expected substring ---\n%s\n--- actual ---\n%s\n"
        label expected actual;
      exit 1
    end)
    expected_msg_substrs msgs;
  Printf.printf "ok: %s\n" label

let () =
  check "hello world"
    "fn main() {\n    println(\"Hello, World!\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"Hello, World!\");\n    return 0;\n}\n";

  check "print emits no trailing newline (println adds it)"
    "fn main() {\n    print(\"a\");\n    print(\"b\");\n    println(\"c\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\", \"a\");\n    printf(\"%s\", \"b\");\n    printf(\"%s\\n\", \"c\");\n    return 0;\n}\n";

  check "let int + print"
    "fn main() {\n    let x = 6 * 7;\n    println(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 6 * 7;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "let string + print"
    "fn main() {\n    let msg = \"hi\";\n    println(msg);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    const char *msg;\n    msg = \"hi\";\n    printf(\"%s\\n\", msg);\n    return 0;\n}\n";

  check "arithmetic precedence"
    "fn main() {\n    let x = 1 + 2 * 3;\n    println(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 1 + 2 * 3;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "if without else"
    "fn main() {\n    let x = 10;\n    if x < 5 {\n        println(x);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 10;\n    if (x < 5) {\n        printf(\"%ld\\n\", (long)(x));\n    }\n    return 0;\n}\n";

  check "if with else"
    "fn main() {\n    let x = 10;\n    if x < 5 {\n        println(x);\n    } else {\n        println(0);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 10;\n    if (x < 5) {\n        printf(\"%ld\\n\", (long)(x));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check "multi-function call"
    "fn add(a: int, b: int) -> int {\n    return a + b;\n}\n\nfn main() {\n    let x = add(3, 4);\n    println(x);\n}\n"
    "#include <stdio.h>\n\nstatic long ex_add(long a, long b);\n\nstatic long ex_add(long a, long b) {\n    return a + b;\n}\n\nint main(void) {\n    long x;\n    x = ex_add(3, 4);\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "assignment"
    "fn main() {\n    let mut x = 1;\n    x = x + 41;\n    println(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 1;\n    x = x + 41;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "while loop"
    "fn main() {\n    let mut i = 0;\n    while i < 3 {\n        println(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    i = 0;\n    while (i < 3) {\n        printf(\"%ld\\n\", (long)(i));\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "while with hoisted inner let"
    "fn main() {\n    let mut i = 0;\n    while i < 2 {\n        let doubled = i * 2;\n        println(doubled);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    long doubled;\n    i = 0;\n    while (i < 2) {\n        doubled = i * 2;\n        printf(\"%ld\\n\", (long)(doubled));\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "bool literals"
    "fn main() {\n    let x = true;\n    let y = false;\n    println(x);\n    println(y);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    int y;\n    x = 1;\n    y = 0;\n    printf(\"%d\\n\", x);\n    printf(\"%d\\n\", y);\n    return 0;\n}\n";

  check "else if chain"
    "fn main() {\n    let x = 2;\n    if x < 1 {\n        println(1);\n    } else if x < 3 {\n        println(2);\n    } else {\n        println(3);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 2;\n    if (x < 1) {\n        printf(\"%ld\\n\", (long)(1));\n    } else if (x < 3) {\n        printf(\"%ld\\n\", (long)(2));\n    } else {\n        printf(\"%ld\\n\", (long)(3));\n    }\n    return 0;\n}\n";

  (* Expression-based bodies: trailing expression (no `;`) is a block's
     value; `if` works as an expression in value position. *)
  check "if-expression in let RHS"
    "fn main() {\n    let x = if 3 < 5 { 10 } else { 20 };\n    println(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    if (3 < 5) {\n        x = 10;\n    } else {\n        x = 20;\n    }\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "function-body trailing expression is the return value"
    "fn inc(a: int) -> int {\n    a + 1\n}\nfn main() {\n    println(inc(4));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_inc(long a);\n\nstatic long ex_inc(long a) {\n    return a + 1;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_inc(4)));\n    return 0;\n}\n";

  check "if-expression as a function's trailing value"
    "fn sign(x: int) -> int {\n    if x > 0 { 1 } else { 0 }\n}\nfn main() {\n    println(sign(5));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_sign(long x);\n\nstatic long ex_sign(long x) {\n    {\n        long __exile_ret;\n        if (x > 0) {\n            __exile_ret = 1;\n        } else {\n            __exile_ret = 0;\n        }\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_sign(5)));\n    return 0;\n}\n";

  check_error "if-expression requires else in value position"
    "fn main() {\n    let x = if 1 < 2 { 10 };\n    println(x);\n}\n"
    "`if` used as a value needs an `else` branch (a value must exist on every path)";

  check_error "if-expression branches must agree in type"
    "fn main() {\n    let x = if 1 < 2 { 1 } else { \"hi\" };\n    println(x);\n}\n"
    "`if` branches have inconsistent types: i32 vs str";

  check_error "if-expression branch with statements is deferred"
    "fn main() {\n    let x = if 1 < 2 { let y = 3; y } else { 0 };\n    println(x);\n}\n"
    "`if` then branch must be a single expression to yield a value (block expressions in branches are not yet supported)";

  check_error "dropped trailing value (`;` footgun) suggests dropping the `;`"
    "fn g() -> int { 1 }\nfn f() -> int { g(); }\nfn main() { println(f()); }\n"
    "function 'f' returns i32 but its last statement is a discarded expression — drop the trailing `;` to return its value, or add an explicit `return`";

  check "line and block comments"
    "// top comment\nfn main() {\n    /* block\n       comment */\n    let x = 1; // trailing\n    println(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 1;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "all comparison operators"
    "fn main() {\n    let a = 5;\n    if a == 5 {\n        println(1);\n    }\n    if a != 0 {\n        println(2);\n    }\n    if a <= 5 {\n        println(3);\n    }\n    if a >= 5 {\n        println(4);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long a;\n    a = 5;\n    if (a == 5) {\n        printf(\"%ld\\n\", (long)(1));\n    }\n    if (a != 0) {\n        printf(\"%ld\\n\", (long)(2));\n    }\n    if (a <= 5) {\n        printf(\"%ld\\n\", (long)(3));\n    }\n    if (a >= 5) {\n        printf(\"%ld\\n\", (long)(4));\n    }\n    return 0;\n}\n";

  (* Bitwise / shift / modulo operators (Rust-order precedence; C is
     emitted with explicit parens so its looser bitwise precedence never
     leaks). *)
  check "bitwise and/or/xor"
    "fn main() {\n    let a = 12 & 10;\n    let b = 12 | 10;\n    let c = 12 ^ 10;\n    println(a + b + c);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long a;\n    long b;\n    long c;\n    a = 12 & 10;\n    b = 12 | 10;\n    c = 12 ^ 10;\n    printf(\"%ld\\n\", (long)(a + b + c));\n    return 0;\n}\n";

  check "shift, modulo, bitwise-not"
    "fn main() {\n    let s = 1 << 4;\n    let m = 17 % 5;\n    let n = ~s;\n    println(s + m + n);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long s;\n    long m;\n    long n;\n    s = 1 << 4;\n    m = 17 % 5;\n    n = ~s;\n    printf(\"%ld\\n\", (long)(s + m + n));\n    return 0;\n}\n";

  check "bitwise binds tighter than comparison (parens emitted)"
    "fn main() {\n    let r = 12 & 10 == 8;\n    println(r);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int r;\n    r = (12 & 10) == 8;\n    printf(\"%d\\n\", r);\n    return 0;\n}\n";

  check "nested generic closes with `>>` (token split)"
    "struct Box<T> { v: T }\nfn inner(b: Box<Box<int>>) -> int {\n    b.v.v\n}\nfn main() {\n    let b: Box<Box<int>> = Box { v: Box { v: 7 } };\n    println(inner(b));\n}\n"
    "#include <stdio.h>\n\nstruct ex_Box_i32 { long v; };\nstruct ex_Box_ex_Box_i32 { struct ex_Box_i32 v; };\n\nstatic long ex_inner(struct ex_Box_ex_Box_i32 b);\n\nstatic long ex_inner(struct ex_Box_ex_Box_i32 b) {\n    return b.v.v;\n}\n\nint main(void) {\n    struct ex_Box_ex_Box_i32 b;\n    struct ex_Box_i32 __lift_0;\n    __lift_0.v = 7;\n    b.v = __lift_0;\n    printf(\"%ld\\n\", (long)(ex_inner(b)));\n    return 0;\n}\n";

  check_error "constant modulo by zero rejected"
    "fn main() {\n    let x = 5 % 0;\n    println(x);\n}\n"
    "modulo by zero";

  check_error "constant shift out of range rejected"
    "fn main() {\n    let x: i32 = 1;\n    println(x << 32);\n}\n"
    "shift amount 32 is out of range for i32 (32 bits)";

  check_error "bare bitor in match arm body is the separator, not an operator"
    "enum E { A | B }\n\
     fn f(e: E) -> int { match e { E::A => 1 | 2 } }\n\
     fn main() { println(f(E::A)); }\n"
    "expected pattern, got integer 2";

  (* Compile-time `const`: folds to a literal, emitted as `#define`; can
     reference earlier consts and use the full operator set. *)
  check "const folds to a #define and use sites read the macro"
    "const MAX: int = 100;\nconst HALF: int = MAX / 2;\nfn main() {\n    println(MAX);\n    println(HALF);\n}\n"
    "#include <stdio.h>\n\n#define ex_MAX 100\n#define ex_HALF 50\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_MAX));\n    printf(\"%ld\\n\", (long)(ex_HALF));\n    return 0;\n}\n";

  check "module const with shift fold, qualified reference"
    "mod cfg { pub const DEPTH: int = 1 << 3; }\nfn main() {\n    println(cfg::DEPTH);\n}\n"
    "#include <stdio.h>\n\n#define cfg__DEPTH 8\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(cfg__DEPTH));\n    return 0;\n}\n";

  check_error "cyclic const definition rejected"
    "const A: int = B;\nconst B: int = A;\nfn main() { println(A); }\n"
    "cyclic constant definition involving 'A'";

  check_error "const value overflowing its type rejected"
    "const X: u8 = 300;\nfn main() { let x: u8 = X; println(x); }\n"
    "constant 'X' = 300 does not fit in u8";

  check_error "non-constant const initializer rejected"
    "fn f() -> int { 5 }\nconst X: int = f();\nfn main() { println(X); }\n"
    "not a constant expression";

  check_error "assignment to a const rejected"
    "const X: int = 5;\nfn main() { X = 6; println(X); }\n"
    "cannot assign to 'X' — it's a `const` (compile-time constant)";

  (* `size_of(T)` is allowed in const initialisers: folds to a C
     `sizeof(...)` expression in the `#define` rather than a literal.
     Such a const has no compile-time-known integer for exile, so it
     can't be used as an array size — that has its own error path. *)
  check "size_of(T) in a const folds to a C sizeof expression"
    "struct Foo { x: int, y: int }\nconst HSZ: c_uint = size_of(Foo);\nfn main() { let n: c_uint = HSZ; println(n as int); }\n"
    "#include <stdio.h>\n\n#define ex_HSZ (sizeof(struct ex_Foo))\n\nstruct ex_Foo { long x; long y; };\n\nint main(void) {\n    unsigned int n;\n    n = ex_HSZ;\n    printf(\"%ld\\n\", (long)(((long)n)));\n    return 0;\n}\n";

  check "size_of composes with other const operators"
    "struct P { x: int }\nconst T: c_uint = size_of(P) * 2 as c_uint;\nfn main() { println(T as int); }\n"
    "#include <stdio.h>\n\n#define ex_T ((sizeof(struct ex_P) * 2))\n\nstruct ex_P { long x; };\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(((long)ex_T)));\n    return 0;\n}\n";

  check_error "a sizeof-based const can't be an array size"
    "struct P { x: int }\nconst SZ: c_uint = size_of(P);\nfn main() { let a: [int; SZ] = [0; SZ]; println(len(a)); }\n"
    "array size 'SZ' is not a known integer at exile time (a bool const, or a `sizeof`/`as`-based value that folds to a C expression)";

  (* Fixed-size arrays `[T; N]`: by-value aggregates (wrapper struct),
     indexing `a[i]`, `len(a)` folds to N. *)
  check "array literal, index and len"
    "fn main() {\n    let a: [int; 3] = [10, 20, 30];\n    println(a[0]);\n    println(len(a));\n}\n"
    "#include <stdio.h>\n\nstruct ex_arr3_i32 { long data[3]; };\n\nint main(void) {\n    struct ex_arr3_i32 a;\n    a.data[0] = 10;\n    a.data[1] = 20;\n    a.data[2] = 30;\n    printf(\"%ld\\n\", (long)(a.data[0]));\n    printf(\"%ld\\n\", (long)(3));\n    return 0;\n}\n";

  check "array repeat literal fills with a loop, element assign"
    "fn main() {\n    let mut a: [int; 4] = [0; 4];\n    a[2] = 7;\n    println(a[2]);\n}\n"
    "#include <stdio.h>\n\nstruct ex_arr4_i32 { long data[4]; };\n\nint main(void) {\n    struct ex_arr4_i32 a;\n    {\n        int __af0;\n        for (__af0 = 0; __af0 < 4; __af0 = __af0 + 1) {\n            a.data[__af0] = 0;\n        }\n    }\n    a.data[2] = 7;\n    printf(\"%ld\\n\", (long)(a.data[2]));\n    return 0;\n}\n";

  check_error "empty array literal rejected"
    "fn main() {\n    let a = [];\n    println(1);\n}\n"
    "empty array literal `[]` is not allowed";

  check_error "array elements must share a type"
    "fn main() {\n    let a: [int; 2] = [1, true];\n    println(1);\n}\n"
    "array elements must share one type: i32 vs bool";

  check_error "array literal size must match annotation"
    "fn main() {\n    let a: [int; 3] = [1, 2];\n    println(1);\n}\n"
    "array literal has 2 element(s) but type expects 3";

  check_error "indexing a non-array rejected"
    "fn main() {\n    let x = 5;\n    println(x[0]);\n}\n"
    "indexing `[...]` requires an array or Slice, got i32";

  check_error "mutating an array element needs `let mut`"
    "fn main() {\n    let a: [int; 2] = [1, 2];\n    a[0] = 9;\n    println(a[0]);\n}\n"
    "cannot assign into immutable 'a' — declare it with `let mut`";

  (* Delta B — write-through-pointer index store `*T[i] = v` (raw write,
     no read-side counterpart on bare `*T`; reads keep flowing through
     `Slice`).  Lowers to plain C `p[i] = v` — no `.data` wrapper.
     Ungated by mut-root, same axis as `AssignDeref`. *)
  check "`*T[i] = v` lowers to raw C `p[i] = v`"
    "mod raw {\n\
    \    extern fn malloc(n: c_ulong) -> *c_void;\n\
    \    extern fn free(p: *c_void);\n\
     }\n\
     fn main() {\n\
    \    let p = raw::malloc(2 as c_ulong) as *u8;\n\
    \    p[0] = 7 as u8;\n\
    \    p[1] = 11 as u8;\n\
    \    println(*p as int);\n\
    \    raw::free(p as *c_void);\n\
     }\n"
    "#include <stdio.h>\n\nextern void *malloc(unsigned long n);\nextern void free(void *p);\n\nint main(void) {\n    unsigned char *p;\n    p = ((unsigned char *)malloc(((unsigned long)2)));\n    p[0] = ((unsigned char)7);\n    p[1] = ((unsigned char)11);\n    printf(\"%ld\\n\", (long)(((long)*p)));\n    free(((void *)p));\n    return 0;\n}\n";

  check_error "`*const T[i] = v` rejected (pointee is read-only)"
    "mod raw {\n\
    \    extern fn malloc(n: c_ulong) -> *c_void;\n\
     }\n\
     fn main() {\n\
    \    let p = raw::malloc(1 as c_ulong) as *const u8;\n\
    \    p[0] = 5 as u8;\n\
     }\n"
    "cannot assign through '*const' pointer *const u8 (pointee is read-only)";

  check_error "indexed assignment on a non-array, non-pointer value rejected"
    "fn main() {\n    let x = 5;\n    x[0] = 7;\n}\n"
    "indexed assignment `a[i] = ...` requires an array or '*T' pointer, got i32";

  check "array as a by-value struct field (wrapper struct emitted first)"
    "struct G { cells: [int; 3] }\n\
     fn main() { let g: G = G { cells: [10, 20, 30] }; println(g.cells[1]); }\n"
    "#include <stdio.h>\n\nstruct ex_arr3_i32 { long data[3]; };\nstruct ex_G { struct ex_arr3_i32 cells; };\n\nint main(void) {\n    struct ex_G g;\n    struct ex_arr3_i32 __lift_0;\n    __lift_0.data[0] = 10;\n    __lift_0.data[1] = 20;\n    __lift_0.data[2] = 30;\n    g.cells = __lift_0;\n    printf(\"%ld\\n\", (long)(g.cells.data[1]));\n    return 0;\n}\n";

  check "nested aggregate ordering: array-of-array, inner shape first"
    "fn main() {\n    let a: [[int; 2]; 3] = [[1, 2], [3, 4], [5, 6]];\n    println(a[1][0]);\n}\n"
    "#include <stdio.h>\n\nstruct ex_arr2_i32 { long data[2]; };\nstruct ex_arr3_arr2_i32 { struct ex_arr2_i32 data[3]; };\n\nint main(void) {\n    struct ex_arr3_arr2_i32 a;\n    struct ex_arr2_i32 __lift_0;\n    struct ex_arr2_i32 __lift_1;\n    struct ex_arr2_i32 __lift_2;\n    __lift_0.data[0] = 1;\n    __lift_0.data[1] = 2;\n    __lift_1.data[0] = 3;\n    __lift_1.data[1] = 4;\n    __lift_2.data[0] = 5;\n    __lift_2.data[1] = 6;\n    a.data[0] = __lift_0;\n    a.data[1] = __lift_1;\n    a.data[2] = __lift_2;\n    printf(\"%ld\\n\", (long)(a.data[1].data[0]));\n    return 0;\n}\n";

  (* `for v in lo..hi { body }` (and `..=`) — desugars to a while-loop with
     gensym counter/end, so multiple sequential `for i in ...` blocks in one
     function don't collide on let-hoisting.  Bounds are evaluated once
     (end pinned). *)
  check "exclusive `for` desugars to a counter+end while-loop"
    "fn main() {\n    let mut s = 0;\n    for i in 0..3 { s = s + i; }\n    println(s);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long s;\n    long __fv0;\n    long __fe0;\n    s = 0;\n    __fe0 = 3;\n    __fv0 = 0;\n    for (; __fv0 < __fe0; __fv0 = __fv0 + 1) {\n        s = s + __fv0;\n    }\n    printf(\"%ld\\n\", (long)(s));\n    return 0;\n}\n";

  check "inclusive `for ..=` emits `<=` in the while condition"
    "fn main() {\n    let mut s = 0;\n    for i in 0..=4 { s = s + i; }\n    println(s);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long s;\n    long __fv0;\n    long __fe0;\n    s = 0;\n    __fe0 = 4;\n    __fv0 = 0;\n    for (; __fv0 <= __fe0; __fv0 = __fv0 + 1) {\n        s = s + __fv0;\n    }\n    printf(\"%ld\\n\", (long)(s));\n    return 0;\n}\n";

  check_error "`for` bound must be an integer"
    "fn main() {\n    for i in \"a\"..\"b\" { println(1); }\n}\n"
    "'for' loop bound must be an integer, got str";

  check_error "`for ... ..=MAX` on bounded type rejected at compile time"
    "fn main() {\n    for i in 0 as u8 ..= 255 as u8 { println(i); }\n}\n"
    "inclusive `for ... ..=255` reaches the maximum of u8 — `i + 1` wraps and the loop never ends; widen the counter type";

  check "loop + break: `loop { ... break; }` emits `while (1)`"
    "fn main() {\n\
    \    let mut i: int = 0;\n\
    \    loop {\n\
    \        if i >= 3 { break; }\n\
    \        println(i);\n\
    \        i = i + 1;\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    i = 0;\n    while (1) {\n        if (i >= 3) {\n            break;\n        }\n        printf(\"%ld\\n\", (long)(i));\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "continue in `for` runs the counter step (C for third clause)"
    "fn main() {\n\
    \    let mut s: int = 0;\n\
    \    for n in 0..6 {\n\
    \        if n == 2 { continue; }\n\
    \        s = s + n;\n\
    \    }\n\
    \    println(s);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    long s;\n    long __fv0;\n    long __fe0;\n    s = 0;\n    __fe0 = 6;\n    __fv0 = 0;\n    for (; __fv0 < __fe0; __fv0 = __fv0 + 1) {\n        if (__fv0 == 2) {\n            continue;\n        }\n        s = s + __fv0;\n    }\n    printf(\"%ld\\n\", (long)(s));\n    return 0;\n}\n";

  check_error "`break` outside a loop rejected"
    "fn main() {\n    break;\n}\n"
    "'break' outside a loop";

  check_error "`continue` outside a loop rejected"
    "fn main() {\n    continue;\n}\n"
    "'continue' outside a loop";

  check_error "`while` condition must be bool"
    "fn main() {\n    while 1 { println(1); }\n}\n"
    "'while' condition must be of type bool, got i32";

  check "logical not: `!e` negates a bool, maps to C `!`"
    "fn main() {\n\
    \    let t: bool = true;\n\
    \    if !t { println(1); } else { println(0); }\n\
    \    if !(1 == 2) { println(1); } else { println(0); }\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    int t;\n    t = 1;\n    if (!t) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    if (!(1 == 2)) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check_error "logical not: non-bool operand rejected"
    "fn main() { let x = !5; println(x); }\n"
    "logical negation '!' requires a bool, got i32";

  check "logical not: folds in a bool const"
    "const FLAG: bool = !true;\n\
     fn main() { if FLAG { println(1); } else { println(0); } }\n"
    "#include <stdio.h>\n\n#define ex_FLAG 0\n\nint main(void) {\n    if (ex_FLAG) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  (* Traits (krok 1): `trait T { fn sigs; }` + `impl T for Foo` + monomorphic
     dispatch.  Trait methods lower to ordinary `Foo__method` fns; the trait
     impl is checked for signature conformance.  Generic bounds `<T: Trait>`
     are a later step. *)
  check "trait + impl: monomorphic method dispatch"
    "trait Area { fn area(self) -> int; }\n\
     struct Square { side: int }\n\
     impl Area for Square {\n\
    \    fn area(self) -> int { self.side * self.side }\n\
     }\n\
     fn main() {\n\
    \    let sq = Square { side: 5 };\n\
    \    println(sq.area());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Square { long side; };\n\nlong Square__area(struct ex_Square self);\n\nint main(void) {\n    struct ex_Square sq;\n    sq.side = 5;\n    printf(\"%ld\\n\", (long)(Square__area(sq)));\n    return 0;\n}\n\nlong Square__area(struct ex_Square self) {\n    return self.side * self.side;\n}\n";

  check_error "trait impl: missing required method rejected"
    "trait Area { fn area(self) -> int; }\n\
     struct Sq { side: int }\n\
     impl Area for Sq { }\n\
     fn main() { let s = Sq { side: 2 }; println(s.side); }\n"
    "missing method 'area' required by trait 'Area'";

  check_error "trait impl: unknown trait rejected"
    "struct Sq { side: int }\n\
     impl Area for Sq { fn area(self) -> int { 1 } }\n\
     fn main() { let s = Sq { side: 2 }; println(s.area()); }\n"
    "unknown trait 'Area'";

  check_error "trait impl: signature mismatch (return type) rejected"
    "trait Area { fn area(self) -> int; }\n\
     struct Sq { side: int }\n\
     impl Area for Sq { fn area(self) -> bool { true } }\n\
     fn main() { let s = Sq { side: 2 }; println(s.side); }\n"
    "method 'area' return type does not match trait 'Area'";

  check_error "trait impl: extra method not in trait rejected"
    "trait Area { fn area(self) -> int; }\n\
     struct Sq { side: int }\n\
     impl Area for Sq { fn area(self) -> int { 1 } fn bogus(self) -> int { 2 } }\n\
     fn main() { let s = Sq { side: 2 }; println(s.area()); }\n"
    "method 'bogus' is not a member of trait 'Area'";

  check "trait bound: `<T: Area>` generic dispatch monomorphizes per type"
    "trait Area { fn area(self) -> int; }\n\
     struct Square { side: int }\n\
     impl Area for Square { fn area(self) -> int { self.side * self.side } }\n\
     fn total<T: Area>(a: T, b: T) -> int { a.area() + b.area() }\n\
     fn main() {\n\
    \    let s1 = Square { side: 3 };\n\
    \    let s2 = Square { side: 4 };\n\
    \    println(total(s1, s2));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Square { long side; };\n\nlong Square__area(struct ex_Square self);\nstatic long ex_total_ex_Square(struct ex_Square a, struct ex_Square b);\n\nint main(void) {\n    struct ex_Square s1;\n    struct ex_Square s2;\n    s1.side = 3;\n    s2.side = 4;\n    printf(\"%ld\\n\", (long)(ex_total_ex_Square(s1, s2)));\n    return 0;\n}\n\nlong Square__area(struct ex_Square self) {\n    return self.side * self.side;\n}\n\nstatic long ex_total_ex_Square(struct ex_Square a, struct ex_Square b) {\n    return Square__area(a) + Square__area(b);\n}\n";

  check_error "trait bound: type not implementing the trait rejected"
    "trait Area { fn area(self) -> int; }\n\
     struct Square { side: int }\n\
     impl Area for Square { fn area(self) -> int { self.side * self.side } }\n\
     struct Plain { x: int }\n\
     fn total<T: Area>(a: T, b: T) -> int { a.area() + b.area() }\n\
     fn main() {\n\
    \    let p1 = Plain { x: 3 };\n\
    \    let p2 = Plain { x: 4 };\n\
    \    println(total(p1, p2));\n\
     }\n"
    "type 'Plain' does not implement trait 'Area' (required by bound 'T: Area' on 'total')";

  check_error "trait bound: multiple bounds `<T: A + B>` — missing one rejected"
    "trait Area { fn area(self) -> int; }\n\
     trait Name { fn tag(self) -> int; }\n\
     struct Half { side: int }\n\
     impl Area for Half { fn area(self) -> int { self.side } }\n\
     fn describe<T: Area + Name>(x: T) -> int { x.area() + x.tag() }\n\
     fn main() {\n\
    \    let h = Half { side: 3 };\n\
    \    println(describe(h));\n\
     }\n"
    "type 'Half' does not implement trait 'Name' (required by bound 'T: Name' on 'describe')";

  check "trait default method: synthesized when impl omits it"
    "trait Greet {\n\
    \    fn hi(self) -> int;\n\
    \    fn greet(self) -> int { self.hi() }\n\
     }\n\
     struct A { v: int }\n\
     impl Greet for A {\n\
    \    fn hi(self) -> int { self.v }\n\
     }\n\
     fn main() {\n\
    \    let a = A { v: 5 };\n\
    \    println(a.greet());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_A { long v; };\n\nlong A__hi(struct ex_A self);\nlong A__greet(struct ex_A self);\n\nint main(void) {\n    struct ex_A a;\n    a.v = 5;\n    printf(\"%ld\\n\", (long)(A__greet(a)));\n    return 0;\n}\n\nlong A__hi(struct ex_A self) {\n    return self.v;\n}\n\nlong A__greet(struct ex_A self) {\n    return A__hi(self);\n}\n";

  check "trait default method: impl may override it"
    "trait Greet {\n\
    \    fn hi(self) -> int;\n\
    \    fn greet(self) -> int { self.hi() }\n\
     }\n\
     struct A { v: int }\n\
     impl Greet for A {\n\
    \    fn hi(self) -> int { self.v }\n\
    \    fn greet(self) -> int { self.v + 100 }\n\
     }\n\
     fn main() {\n\
    \    let a = A { v: 5 };\n\
    \    println(a.greet());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_A { long v; };\n\nlong A__hi(struct ex_A self);\nlong A__greet(struct ex_A self);\n\nint main(void) {\n    struct ex_A a;\n    a.v = 5;\n    printf(\"%ld\\n\", (long)(A__greet(a)));\n    return 0;\n}\n\nlong A__hi(struct ex_A self) {\n    return self.v;\n}\n\nlong A__greet(struct ex_A self) {\n    return self.v + 100;\n}\n";

  check_error "trait default method: required method still enforced"
    "trait Greet {\n\
    \    fn hi(self) -> int;\n\
    \    fn greet(self) -> int { self.hi() }\n\
     }\n\
     struct A { v: int }\n\
     impl Greet for A { }\n\
     fn main() { let a = A { v: 5 }; println(a.v); }\n"
    "missing method 'hi' required by trait 'Greet'";

  check "supertrait: `trait Hash: Eq` accepts when both implemented \
         (order-independent)"
    "trait Eq { fn eq(self, other: Self) -> bool; }\n\
     trait Hash: Eq { fn hash(self) -> int; }\n\
     struct K { v: int }\n\
     impl Hash for K { fn hash(self) -> int { self.v } }\n\
     impl Eq for K { fn eq(self, other: K) -> bool { self.v == other.v } }\n\
     fn main() {\n\
    \    let a = K { v: 7 };\n\
    \    println(a.hash());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_K { long v; };\n\nlong K__hash(struct ex_K self);\nint K__eq(struct ex_K self, struct ex_K other);\n\nint main(void) {\n    struct ex_K a;\n    a.v = 7;\n    printf(\"%ld\\n\", (long)(K__hash(a)));\n    return 0;\n}\n\nlong K__hash(struct ex_K self) {\n    return self.v;\n}\n\nint K__eq(struct ex_K self, struct ex_K other) {\n    return self.v == other.v;\n}\n";

  check_error "supertrait: missing supertrait impl rejected"
    "trait Eq { fn eq(self, other: Self) -> bool; }\n\
     trait Hash: Eq { fn hash(self) -> int; }\n\
     struct K { v: int }\n\
     impl Hash for K { fn hash(self) -> int { self.v } }\n\
     fn main() { let a = K { v: 7 }; println(a.hash()); }\n"
    "'Hash' requires supertrait 'Eq', but 'K' does not implement it (add `impl Eq for K`)";

  check "associated type: `type Item;` + `Self::Item` in sig conforms"
    "trait Iterator {\n\
    \    type Item;\n\
    \    fn next(self) -> Option<Self::Item>;\n\
     }\n\
     struct Counter { n: int }\n\
     impl Iterator for Counter {\n\
    \    type Item = int;\n\
    \    fn next(self) -> Option<int> { Option::Some(self.n) }\n\
     }\n\
     fn main() {\n\
    \    let c = Counter { n: 7 };\n\
    \    match c.next() {\n\
    \        Option::Some(v) => println(v)\n\
    \        | Option::None => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Counter { long n; };\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstruct ex_Option_i32 Counter__next(struct ex_Counter self);\n\nint main(void) {\n    struct ex_Counter c;\n    c.n = 7;\n    {\n        struct ex_Option_i32 __m;\n        __m = Counter__next(c);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long v = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n\nstruct ex_Option_i32 Counter__next(struct ex_Counter self) {\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = self.n;\n        return __exile_ret;\n    }\n}\n";

  check_error "associated type: missing `type X = ...` in impl rejected"
    "trait Iterator { type Item; fn next(self) -> Option<Self::Item>; }\n\
     struct C { n: int }\n\
     impl Iterator for C { fn next(self) -> Option<int> { Option::None } }\n\
     fn main() { let c = C { n: 0 };\n\
    \    match c.next() { Option::Some(v) => println(v) | Option::None => println(-1) } }\n"
    "missing associated type 'type Item = ...;' required by trait 'Iterator'";

  check_error "associated type: binding not in trait rejected"
    "trait Show { fn show(self) -> int; }\n\
     struct C { n: int }\n\
     impl Show for C { type Bogus = int; fn show(self) -> int { self.n } }\n\
     fn main() { let c = C { n: 5 }; println(c.show()); }\n"
    "associated type 'Bogus' is not a member of trait 'Show'";

  check_error "associated type: `Self::Item` mismatch in impl sig rejected"
    "trait Iterator { type Item; fn next(self) -> Option<Self::Item>; }\n\
     struct C { n: int }\n\
     impl Iterator for C { type Item = int; fn next(self) -> Option<bool> { Option::None } }\n\
     fn main() { let c = C { n: 0 };\n\
    \    match c.next() { Option::Some(v) => println(1) | Option::None => println(-1) } }\n"
    "method 'next' return type does not match trait 'Iterator'";

  (* `@derive(Eq, Clone)` synthesizes real `impl Trait for Foo` blocks
     (DECYZJA #1): a struct Eq is `&&` of field `.eq()` (primitive fields
     fold to `==`); `ne` comes from the trait default; Clone is a value
     copy `{ self }`.  Works on enums via nested match too. *)
  check "@derive(Eq) on a struct synthesizes a field-wise eq + default ne"
    "@derive(Eq)\n\
     struct P { x: int, y: int }\n\
     fn main() {\n\
    \    let a = P { x: 1, y: 2 };\n\
    \    let b = P { x: 1, y: 2 };\n\
    \    if a.eq(b) { println(1); } else { println(0); }\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nint P__eq(const struct ex_P *self, const struct ex_P *other);\nint P__ne(const struct ex_P *self, const struct ex_P *other);\n\nint main(void) {\n    struct ex_P a;\n    struct ex_P b;\n    a.x = 1;\n    a.y = 2;\n    b.x = 1;\n    b.y = 2;\n    if (P__eq(&a, &b)) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n\nint P__eq(const struct ex_P *self, const struct ex_P *other) {\n    return self->x == other->x && self->y == other->y;\n}\n\nint P__ne(const struct ex_P *self, const struct ex_P *other) {\n    return !(P__eq(&*self, &*other));\n}\n";

  (* `@move` — affine / use-at-most-once marker for heap-owning
     structs.  Parser accepts and records on struct_sig
     (`ss_is_move = true`); the DR-002 move-pass reads it in a
     follow-up commit.  Today's commit is parsing only — no
     enforcement, no codegen change. *)
  check "@move struct parses; codegen unchanged"
    "@move\n\
     struct Owner { ptr: int, len: u32 }\n\
     fn main() {\n\
    \    let o = Owner { ptr: 0, len: 0 as u32 };\n\
    \    println(o.ptr);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Owner { long ptr; unsigned long len; };\n\nint main(void) {\n    struct ex_Owner o;\n    o.ptr = 0;\n    o.len = ((unsigned long)0);\n    printf(\"%ld\\n\", (long)(o.ptr));\n    return 0;\n}\n";

  check_error "@move on an enum rejected (struct-only marker)"
    "@move\n\
     enum E { A | B }\n\
     fn main() { println(1); }\n"
    "'@move' can only decorate struct decls";

  (* DR-002 move-pass: @move binding consumed by `let b = a` then
     read at `take(a)` is a use-after-consume, reported with both
     positions (the move and the use). *)
  check_error "@move binding read after `let b = a` rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 42 };\n\
    \    let b = a;\n\
    \    println(take(a));\n\
    \    println(take(b));\n\
     }\n"
    "use of 'a' after it was consumed at <input>:6:13 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "@move binding consumed by method receiver, error on subsequent use"
    "@move\n\
     struct Owner { tag: int }\n\
     impl Owner {\n\
    \    pub fn take(self) -> int { return self.tag; }\n\
    \    pub fn peek(*const self) -> int { return self.tag; }\n\
     }\n\
     fn main() {\n\
    \    let a = Owner { tag: 7 };\n\
    \    println(a.take());\n\
    \    println(a.peek());\n\
     }\n"
    "use of 'a' after it was consumed at <input>:9:13 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  (* Borrow paths (`&a`, `*const self` receivers) leave the binding
     live — same source can be read repeatedly and only consumed at
     the end. *)
  check "@move binding stays live across `&a` borrows"
    "@move\n\
     struct Owner { tag: int }\n\
     fn read(o: *const Owner) -> int { return o.tag; }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 7 };\n\
    \    println(read(&a));\n\
    \    println(read(&a));\n\
    \    println(take(a));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Owner { long tag; };\n\nstatic long ex_read(const struct ex_Owner *o);\nstatic long ex_take(struct ex_Owner o);\n\nstatic long ex_read(const struct ex_Owner *o) {\n    return o->tag;\n}\n\nstatic long ex_take(struct ex_Owner o) {\n    return o.tag;\n}\n\nint main(void) {\n    struct ex_Owner a;\n    a.tag = 7;\n    printf(\"%ld\\n\", (long)(ex_read(&a)));\n    printf(\"%ld\\n\", (long)(ex_read(&a)));\n    printf(\"%ld\\n\", (long)(ex_take(a)));\n    return 0;\n}\n";

  (* Divergence oracle: a TIf branch that early-returns / breaks /
     continues doesn't reach the post-branch program point, so its
     consume can't make the binding Consumed post-merge.  Without
     this, `if c { take(a); return; } take(a)` would false-positive
     the second `take(a)` as use-after-consume. *)
  check "@move binding consumed on early-return branch stays Live after the if"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn helper(a: Owner) -> int {\n\
    \    if a.tag < 0 {\n\
    \        return take(a);\n\
    \    }\n\
    \    return take(a);\n\
     }\n\
     fn main() { println(helper(Owner { tag: 1 })); }\n"
    "#include <stdio.h>\n\nstruct ex_Owner { long tag; };\n\nstatic long ex_take(struct ex_Owner o);\nstatic long ex_helper(struct ex_Owner a);\n\nstatic long ex_take(struct ex_Owner o) {\n    return o.tag;\n}\n\nstatic long ex_helper(struct ex_Owner a) {\n    if (a.tag < 0) {\n        return ex_take(a);\n    }\n    return ex_take(a);\n}\n\nint main(void) {\n    struct ex_Owner __lift_0;\n    __lift_0.tag = 1;\n    printf(\"%ld\\n\", (long)(ex_helper(__lift_0)));\n    return 0;\n}\n";

  (* TMatch arm fork+merge: every non-diverging arm contributes to
     the post-match state.  All arms consume + fall through →
     Consumed post-match → use-after-consume on subsequent read. *)
  (* TDefer is checked last against the end-of-scope state (LIFO).
     A `defer drop(&a)` that captures `a` errors if `a` is consumed
     anywhere in the surrounding scope — defer can't fire on a moved
     binding without re-using freed memory. *)
  (* `Vec<T>` is `@move`: the silent `let v2 = v` alias is rejected
     before it could cause a double-free or double-grow. *)
  check_error "double `Vec::len` via aliased let-rebind rejected"
    "mod raw { extern fn make_a() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
    \    let v2 = v;\n\
    \    println(v.len() as int);\n\
     }\n"
    "use of 'v' after it was consumed at <input>:5:14 (move-marked types are use-at-most-once — borrow with '&v' / take '*const Vec_i32' or clone to keep the source live)";

  check_error "`Vec::push` rejects an arg of the wrong element type"
    "mod raw { extern fn make_a() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
    \    v.push(\"oops\");\n\
     }\n"
    "type parameter 'T' inferred as both 'i32' and 'str'";

  (* Prelude `String` and `StringBuilder` are `@move`: the silent
     `let s2 = s; s.free()` double-free pattern is rejected
     structurally now that they wear the affine marker. *)
  check_error "double `String::free` via aliased let-rebind rejected"
    "mod raw { extern fn make_a() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let s = String::with_str(a, \"x\");\n\
    \    let s2 = s;\n\
    \    s.free();\n\
    \    s2.free();\n\
     }\n"
    "use of 's' after it was consumed at <input>:5:14 (move-marked types are use-at-most-once — borrow with '&s' / take '*const String' or clone to keep the source live)";

  (* `String::build(sb)` consumes the StringBuilder.  Subsequent use
     of `sb` (`push_*`, `length`, etc.) errors — the buffer ownership
     has transferred. *)
  check_error "StringBuilder consumed by `String::build`, subsequent push_str rejected"
    "mod raw { extern fn make_a() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let mut sb = StringBuilder::with_capacity(a, 4 as u32);\n\
    \    sb.push_str(\"hi\");\n\
    \    let s = String::build(sb);\n\
    \    sb.push_str(\"!\");\n\
    \    println(s.length() as int);\n\
     }\n"
    "use of 'sb' after it was consumed at <input>:6:27 (move-marked types are use-at-most-once — borrow with '&sb' / take '*const StringBuilder' or clone to keep the source live)";

  check_error "@move binding consumed in scope, defer that reads it rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn drop(o: *const Owner) { println(o.tag); }\n\
     fn main() {\n\
    \    let a = Owner { tag: 42 };\n\
    \    defer drop(&a);\n\
    \    println(take(a));\n\
     }\n"
    "use of 'a' after it was consumed at <input>:8:18 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "@move binding consumed on every match arm errors on post-match use"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn run(a: Owner) {\n\
    \    match Option::Some(a.tag) {\n\
    \        Option::Some(_) => { println(take(a)); }\n\
    \        | Option::None  => { println(take(a)); }\n\
    \    }\n\
    \    println(take(a));\n\
     }\n\
     fn main() { run(Owner { tag: 42 }); }\n"
    "use of 'a' after it was consumed at <input>:6:43 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check "@derive(Clone) synthesizes a value-copy clone"
    "@derive(Clone)\n\
     struct P { x: int }\n\
     fn main() {\n\
    \    let a = P { x: 7 };\n\
    \    let b = a.clone();\n\
    \    println(b.x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; };\n\nstruct ex_P P__clone(const struct ex_P *self);\n\nint main(void) {\n    struct ex_P a;\n    struct ex_P b;\n    a.x = 7;\n    b = P__clone(&a);\n    printf(\"%ld\\n\", (long)(b.x));\n    return 0;\n}\n\nstruct ex_P P__clone(const struct ex_P *self) {\n    return *self;\n}\n";

  check "@derive(Hash) synthesizes a multiplicative field fold"
    "@derive(Eq, Hash)\n\
     struct P { x: int, y: int }\n\
     fn main() {\n\
    \    let a = P { x: 1, y: 2 };\n\
    \    println(a.hash() as int);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nint P__eq(const struct ex_P *self, const struct ex_P *other);\nint P__ne(const struct ex_P *self, const struct ex_P *other);\nunsigned long P__hash(const struct ex_P *self);\n\nint main(void) {\n    struct ex_P a;\n    a.x = 1;\n    a.y = 2;\n    printf(\"%ld\\n\", (long)(((long)P__hash(&a))));\n    return 0;\n}\n\nint P__eq(const struct ex_P *self, const struct ex_P *other) {\n    return self->x == other->x && self->y == other->y;\n}\n\nint P__ne(const struct ex_P *self, const struct ex_P *other) {\n    return !(P__eq(&*self, &*other));\n}\n\nunsigned long P__hash(const struct ex_P *self) {\n    return ((unsigned long)self->x) * 31 + ((unsigned long)self->y);\n}\n";

  check_error "@derive(Hash) without Eq rejected (supertrait)"
    "@derive(Hash)\n\
     struct P { x: int }\n\
     fn main() { let a = P { x: 1 }; println(a.x); }\n"
    "'Hash' requires supertrait 'Eq', but 'P' does not implement it (add `impl Eq for P`)";

  check_error "@derive(Hash) on a str field rejected"
    "@derive(Eq, Hash)\n\
     struct P { name: str }\n\
     fn main() { let a = P { name: \"x\" }; println(a.name); }\n"
    "`hash` is not built-in for str (str / pointer content hashing is not supported yet)";

  check_error "@derive of an unknown trait rejected"
    "@derive(Ord)\n\
     struct P { x: int }\n\
     fn main() { let a = P { x: 1 }; println(a.x); }\n"
    "cannot derive 'Ord' (supported: Eq, Hash, Clone, Debug)";

  (* `Display` / `Debug` — prelude traits, writer pattern.  The user
     writes `impl Display for Foo { fn fmt( * self, out: *StringBuilder)
     { ... } }` by hand; threading the same `out` through nested
     `child.fmt(out)` calls composes without intermediate allocs.
     `@derive(Display)` is deliberately not supported (Display is
     manual surface — the design intent is user-controlled output);
     `@derive(Debug)` lands in a follow-up commit. *)
  check_error "@derive(Display) rejected (Display is manual surface)"
    "@derive(Display)\n\
     struct P { x: int }\n\
     fn main() { println(1); }\n"
    "cannot derive 'Display' — Display is a hand-written surface; use `@derive(Debug)` for an automatically-generated formatter";

  check_error "impl Display with the wrong fmt signature rejected"
    "struct P { x: int }\n\
     impl Display for P {\n\
    \    fn fmt(*self, out: int) {}\n\
     }\n\
     fn main() { println(1); }\n"
    "method 'fmt': parameter 'out' type does not match trait 'Display' (expected *StringBuilder, got i32)";

  (* @derive(Debug) synthesizes an `impl Debug for T` whose fmt body
     pushes the Rust-style `{:?}` rendering into the caller's
     StringBuilder.  Primitive fields land via push_int / push_byte
     for bool literal / quoted push_str for str; struct/enum fields
     recurse through their own Debug impls. *)
  check_error "@derive(Debug) on a generic struct rejected (MVP)"
    "@derive(Debug)\n\
     struct Box<T> { v: T }\n\
     fn main() { println(1); }\n"
    "@derive(Debug) on a generic struct 'Box' is not supported yet";

  check_error "@derive(Debug) on a struct with a non-Debug field rejected"
    "struct Inner { v: int }\n\
     @derive(Debug)\n\
     struct Outer { i: Inner }\n\
     fn main() { println(1); }\n"
    "no method 'fmt' on type 'Inner'";

  check_error "@derive on a generic struct rejected (MVP)"
    "@derive(Eq)\n\
     struct Box<T> { v: T }\n\
     fn main() { println(1); }\n"
    "@derive(Eq) on a generic struct 'Box' is not supported yet";

  (* `break` inside a `match` inside a loop must exit the loop, not the C
     `switch` the match would otherwise compile to.  Such a match routes to
     the if-else decision chain (no switch to capture the break). *)
  check "break inside match-in-loop routes to if-else (break hits the loop)"
    "enum E { A | B }\n\
     fn main() {\n\
    \    let mut i: int = 0;\n\
    \    loop {\n\
    \        let e = E::A;\n\
    \        match e {\n\
    \            E::A => { if i >= 3 { break; } println(i); i = i + 1; }\n\
    \            | E::B => { break; }\n\
    \        }\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; };\n\nint main(void) {\n    long i;\n    struct ex_E e;\n    i = 0;\n    while (1) {\n        e.tag = ex_E_A;\n        {\n            struct ex_E __m;\n            __m = e;\n            if (__m.tag == ex_E_A) {\n                if (i >= 3) {\n                    break;\n                }\n                printf(\"%ld\\n\", (long)(i));\n                i = i + 1;\n            }\n            else {\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  (* Range as a value: `a..b` / `a..=b` desugar to the prelude struct
     `Range<T>` / `RangeInclusive<T>`, so a range can be bound, passed and
     returned.  `for v in <Range value>` pulls `.lo` / `.hi` off the value
     through a temp; a literal `..`/`..=` in for-head still takes the
     direct fast path (no struct alloc). *)
  check "`for v in <Range value>` pulls bounds off a struct field"
    "fn main() {\n    let r = 0..3;\n    let mut s = 0;\n    for i in r { s = s + i; }\n    println(s);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Range_i32 { long lo; long hi; };\n\nint main(void) {\n    struct ex_Range_i32 r;\n    long s;\n    struct ex_Range_i32 __fr0;\n    long __fv0;\n    long __fe0;\n    r.lo = 0;\n    r.hi = 3;\n    s = 0;\n    __fr0 = r;\n    __fe0 = __fr0.hi;\n    __fv0 = __fr0.lo;\n    for (; __fv0 < __fe0; __fv0 = __fv0 + 1) {\n        s = s + __fv0;\n    }\n    printf(\"%ld\\n\", (long)(s));\n    return 0;\n}\n";

  check_error "`for v in <non-Range value>` rejected"
    "fn main() {\n    let n = 5;\n    for i in n { println(i); }\n}\n"
    "`for v in ...` needs a `..` / `..=` range, a `Range<T>` / `RangeInclusive<T>` value, or a type that `impl Iterator`, got i32";

  (* `for x in <iterator>` over a type that `impl Iterator` (prelude trait).
     Desugars to a mutable iterator temp + `loop { match it.next() {
     Some(x) => body | None => break } }`.  `next` (a pointer receiver)
     advances the iterator via auto-ref. *)
  check "for-in-iterator: desugars to loop + match over next()"
    "struct UpTo { cur: int, stop: int }\n\
     impl Iterator for UpTo {\n\
    \    type Item = int;\n\
    \    fn next(*self) -> Option<int> {\n\
    \        if self.cur >= self.stop { return Option::None; }\n\
    \        let v = self.cur;\n\
    \        self.cur = self.cur + 1;\n\
    \        Option::Some(v)\n\
    \    }\n\
     }\n\
     fn main() {\n\
    \    let up = UpTo { cur: 0, stop: 3 };\n\
    \    for x in up { println(x); }\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_UpTo { long cur; long stop; };\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self);\n\nint main(void) {\n    struct ex_UpTo up;\n    struct ex_UpTo __it0;\n    up.cur = 0;\n    up.stop = 3;\n    __it0 = up;\n    while (1) {\n        {\n            struct ex_Option_i32 __m;\n            __m = UpTo__next(&__it0);\n            if (__m.tag == ex_Option_i32_Some) {\n                long __fv0 = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(__fv0));\n            }\n            else {\n                break;\n            }\n        }\n    }\n    return 0;\n}\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self) {\n    long v;\n    if (self->cur >= self->stop) {\n        {\n            struct ex_Option_i32 __exile_ret;\n            __exile_ret.tag = ex_Option_i32_None;\n            return __exile_ret;\n        }\n    }\n    v = self->cur;\n    self->cur = self->cur + 1;\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v;\n        return __exile_ret;\n    }\n}\n";

  (* Associated-type projection `I::Item` in a generic fn signature.
     Skeleton-time produces a `TAssocProj { head = TVar I; assoc = Item }`;
     at the `first(&up)` call site, mono substitutes `I → UpTo`, and
     `normalize_apps` projects `Item` via the impl's `type Item = int;`
     to a concrete `Option<int>` — same flat mono instance as
     [for-in-iterator] (no second `Option<I::Item>` instantiation
     appears in the C output). *)
  check "I::Item projects to the impl's associated type at the call site"
    "struct UpTo { cur: int, stop: int }\n\
     impl Iterator for UpTo {\n\
    \    type Item = int;\n\
    \    fn next(*self) -> Option<int> {\n\
    \        if self.cur >= self.stop { return Option::None; }\n\
    \        let v = self.cur;\n\
    \        self.cur = self.cur + 1;\n\
    \        Option::Some(v)\n\
    \    }\n\
     }\n\
     fn first<I: Iterator>(it: *I) -> Option<I::Item> {\n\
    \    return it.next();\n\
     }\n\
     fn main() {\n\
    \    let up = UpTo { cur: 0, stop: 2 };\n\
    \    match first(&up) {\n\
    \        Option::Some(v) => { println(v); }\n\
    \        | Option::None => { println(99); }\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_UpTo { long cur; long stop; };\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self);\nstatic struct ex_Option_i32 ex_first_ex_UpTo(struct ex_UpTo *it);\n\nint main(void) {\n    struct ex_UpTo up;\n    up.cur = 0;\n    up.stop = 2;\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_first_ex_UpTo(&up);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long v = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(99));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self) {\n    long v;\n    if (self->cur >= self->stop) {\n        {\n            struct ex_Option_i32 __exile_ret;\n            __exile_ret.tag = ex_Option_i32_None;\n            return __exile_ret;\n        }\n    }\n    v = self->cur;\n    self->cur = self->cur + 1;\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v;\n        return __exile_ret;\n    }\n}\n\nstatic struct ex_Option_i32 ex_first_ex_UpTo(struct ex_UpTo *it) {\n    return UpTo__next(it);\n}\n";

  (* Ambiguous projection: two traits define `type Item` and the same
     struct implements both.  `Counter::Item` without `<Counter as
     Trait>::Item` qualification is unresolvable; we reject at use. *)
  check_error "ambiguous associated-type projection rejected"
    "trait Container { type Item; fn first(self) -> Self::Item; }\n\
     struct Counter { n: int }\n\
     impl Iterator for Counter {\n\
    \    type Item = int;\n\
    \    fn next(*self) -> Option<int> { return Option::Some(self.n); }\n\
     }\n\
     impl Container for Counter {\n\
    \    type Item = str;\n\
    \    fn first(self) -> str { return \"hi\"; }\n\
     }\n\
     fn main() {\n\
    \    let x: Counter::Item = 5;\n\
    \    println(x);\n\
     }\n"
    "ambiguous associated-type projection 'Counter::Item' (multiple traits define 'Item' — qualified `<Counter as Trait>::Item` is not yet supported)";

  (* Regression: a match-arm bind must stay in scope after a nested `if`
     in the same multi-statement arm body.  (walk_stmt used to reset the
     env to param_env+decls after an `if`, dropping arm binds.) *)
  check "multi-stmt arm: bind stays in scope after a nested `if`"
    "enum E { A(int) | B }\n\
     fn main() {\n\
    \    let e = E::A(5);\n\
    \    match e {\n\
    \        E::A(n) => { if n > 0 { println(99); } println(n); }\n\
    \        | E::B => { println(0); }\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } A; } data; };\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_A;\n    e.data.A._0 = 5;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                long n = __m.data.A._0;\n                if (n > 0) {\n                    printf(\"%ld\\n\", (long)(99));\n                }\n                printf(\"%ld\\n\", (long)(n));\n                break;\n            }\n        case ex_E_B:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  (* Short-circuit logical `&&` / `||` — both `bool`-only, `&&` tighter than
     `||`, both looser than comparisons (Rust-order matches C, so no extra
     parens are needed in the emitted C). *)
  check "logical `&&` / `||` compose with comparisons"
    "fn main() {\n    let a = 3;\n    let b = 5;\n    if a < b && b > 0 || a == 0 {\n        println(1);\n    } else {\n        println(0);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long a;\n    long b;\n    a = 3;\n    b = 5;\n    if ((a < b && b > 0) || a == 0) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check_error "logical `&&` rejects non-bool operands"
    "fn main() {\n    if 5 && true { println(1); }\n}\n"
    "logical '&&' requires bool operands, got i32 and bool";

  check "unary minus on literal var and call"
    "fn id(x: int) -> int {\n    return x;\n}\nfn main() {\n    let a = -5;\n    let b = -a;\n    println(b);\n    println(-id(7));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_id(long x);\n\nstatic long ex_id(long x) {\n    return x;\n}\n\nint main(void) {\n    long a;\n    long b;\n    a = -5;\n    b = -a;\n    printf(\"%ld\\n\", (long)(b));\n    printf(\"%ld\\n\", (long)(-(ex_id(7))));\n    return 0;\n}\n";

  check_error "undefined variable in if cond"
    "fn main() {\n    if nope > 0 {\n        println(1);\n    }\n}\n"
    "undefined variable 'nope'";

  check_error "comparison '<' between str and int rejected"
    "fn cmp(a: str, b: int) -> bool { return a < b; }\n\
     fn main() { println(1); }\n"
    "operator '<' requires integer operands, got str and i32";

  check_error "equality '==' between str and int rejected"
    "fn main() { if \"x\" == 5 { println(1); } }\n"
    "equality '==' between incompatible types str and i32";

  check_error "arithmetic '+' between str and int rejected"
    "fn main() { let x = \"x\" + 5; println(1); }\n"
    "operator '+' requires integer operands, got str and i32";

  check_error "return: tuple element type mismatch rejected"
    "fn pair(a: int, b: str) -> (int, int) { return (a, b); }\n\
     fn main() { println(1); }\n"
    "return: expected (i32, i32), got (i32, str)";

  check_error "return: scalar type mismatch rejected"
    "fn id(x: int) -> str { return x; }\n\
     fn main() { println(1); }\n"
    "return: expected str, got i32";

  check_error "duplicate let"
    "fn main() {\n    let x = 1;\n    let x = 2;\n    println(x);\n}\n"
    "variable 'x' already declared in this function";

  check_error "let shadows parameter"
    "fn foo(x: int) -> int {\n    let x = 5;\n    return x;\n}\nfn main() {\n    println(foo(1));\n}\n"
    "variable 'x' shadows a parameter";

  check_error "let annotation mismatch"
    "fn main() {\n    let x: str = 5;\n    println(x);\n}\n"
    "variable 'x' declared as str but initializer has type i32";

  check_error "wrong arg count"
    "fn add(a: int, b: int) -> int {\n    return a + b;\n}\nfn main() {\n    println(add(1));\n}\n"
    "function 'add' expects 2 argument(s), got 1";

  check_error "wrong arg type"
    "fn greet(name: str) {\n    println(name);\n}\nfn main() {\n    greet(42);\n}\n"
    "argument 1 of 'greet': expected str, got i32";

  check_error "void used as value"
    "fn greet(name: str) {\n    println(name);\n}\nfn main() {\n    let x = greet(\"hi\");\n    println(x);\n}\n"
    "'greet' returns void, cannot use as a value";

  check_error "assignment to undefined"
    "fn main() {\n    x = 5;\n}\n"
    "assignment to undefined variable 'x'";

  (* Immutable-by-default: bindings and parameters need `mut` to be
     reassigned or have an owned value mutated.  Mutability is
     compile-time-only — a `mut` binding emits identical C (no `const`). *)
  check_error "reassign immutable let rejected"
    "fn main() {\n    let x = 1;\n    x = 2;\n    println(x);\n}\n"
    "cannot assign to immutable 'x' — declare it with `let mut`";

  check_error "mutate field of immutable value struct rejected"
    "struct P { x: int, y: int }\n\
     fn main() {\n    let p = P { x: 1, y: 2 };\n    p.x = 9;\n    println(p.x);\n}\n"
    "cannot mutate field of immutable 'p' — declare it with `let mut`";

  check_error "reassign immutable parameter rejected"
    "fn f(n: int) -> int {\n    n = n + 1;\n    return n;\n}\n\
     fn main() {\n    println(f(5));\n}\n"
    "cannot assign to immutable 'n' — declare it with `let mut` (or mark the parameter `mut`)";

  check "mut parameter allows mutation (emits plain C, no const)"
    "fn inc(mut n: int) -> int {\n    n = n + 1;\n    return n;\n}\n\
     fn main() {\n    println(inc(5));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_inc(long n);\n\nstatic long ex_inc(long n) {\n    n = n + 1;\n    return n;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_inc(5)));\n    return 0;\n}\n";

  check "mutating a value struct field through `let mut`"
    "struct P { x: int, y: int }\n\
     fn main() {\n    let mut p = P { x: 1, y: 2 };\n    p.x = 9;\n    println(p.x);\n}\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nint main(void) {\n    struct ex_P p;\n    p.x = 1;\n    p.y = 2;\n    p.x = 9;\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n";

  check_error "main with params"
    "fn main(x: int) {\n    println(x);\n}\n"
    "'main' must take no parameters";

  check_error "duplicate function"
    "fn foo() -> int {\n    return 1;\n}\nfn foo() -> int {\n    return 2;\n}\nfn main() {\n    println(foo());\n}\n"
    "function 'foo' already defined";

  check_error "stray ';' after if block reported clearly"
    "fn main() {\n\
    \    if true { println(1); };\n\
     }\n"
    "stray ';' — `if`/`while`/`match` and inner blocks are self-terminating, no trailing semicolon needed";

  check_error "stray ';' after while block reported clearly"
    "fn main() {\n\
    \    let i = 0;\n\
    \    while i > 0 { i = i - 1; };\n\
     }\n"
    "stray ';' — `if`/`while`/`match` and inner blocks are self-terminating, no trailing semicolon needed";

  check_error "unknown type name in let annotation rejected"
    "fn main() { let x: NoSuchType = 1; println(x); }\n"
    "unknown type 'NoSuchType'";

  check_error "unknown type name in param annotation rejected"
    "fn take(x: NoSuchType) { println(1); }\n\
     fn main() { println(1); }\n"
    "unknown type 'NoSuchType'";

  check_error "self-recursive value struct rejected"
    "struct Node { next: Node }\n\
     fn main() { println(1); }\n"
    "recursive value type 'Node' (cycle: Node -> Node) — a field embeds \
     the type by value, making it infinitely sized; break the cycle with \
     a pointer (`*T`)";

  check_error "mutually-recursive value structs rejected"
    "struct A { b: B }\n\
     struct B { a: A }\n\
     fn main() { println(1); }\n"
    "recursive value type 'A' (cycle: A -> B -> A) — a field embeds the \
     type by value, making it infinitely sized; break the cycle with a \
     pointer (`*T`)";

  check_error "self-recursive value enum rejected"
    "enum List { Nil | Cons(int, List) }\n\
     fn main() { println(1); }\n"
    "recursive value type 'List' (cycle: List -> List) — a field embeds \
     the type by value, making it infinitely sized; break the cycle with \
     a pointer (`*T`)";

  check_error "mutually-recursive GENERIC value structs rejected"
    "struct A<T> { b: B<T> }\n\
     struct B<T> { a: A<T> }\n\
     fn use_it(x: A<int>) -> int { return 1; }\n\
     fn main() { println(1); }\n"
    "recursive value type 'A' (cycle: A -> B -> A) — a field embeds the \
     type by value, making it infinitely sized; break the cycle with a \
     pointer (`*T`)";

  check "recursive struct through pointer is allowed"
    "struct Node { val: int, next: *Node }\n\
     fn main() {\n\
    \    let n = Node { val: 1, next: null };\n\
    \    println(n.val);\n\
     }\n"
    "#include <stdio.h>\n\n\
     struct ex_Node { long val; struct ex_Node *next; };\n\n\
     int main(void) {\n\
    \    struct ex_Node n;\n\
    \    n.val = 1;\n\
    \    n.next = ((void *)0);\n\
    \    printf(\"%ld\\n\", (long)(n.val));\n\
    \    return 0;\n\
     }\n";

  check_error "bare no-effect expression statement rejected"
    "fn main() { 42; }\n"
    "expression statement has no effect — its result is discarded; \
     remove it, or bind it with `let _x = ...`";

  check_error "discarded type_name() statement rejected"
    "fn main() { type_name(42); }\n"
    "expression statement has no effect — its result is discarded; \
     remove it, or bind it with `let _x = ...`";

  check_error "constant division by zero rejected"
    "fn main() { let x = 1 / 0; println(x); }\n"
    "division by zero";

  check_error "duplicate match arm for same variant rejected"
    "enum E { A | B }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    let r = match e { E::A => 1 | E::A => 2 | E::B => 3 };\n\
    \    println(r);\n\
     }\n"
    "unreachable match arm: earlier arms already cover this case";

  check_error "match arm after catch-all rejected as unreachable"
    "enum E { A | B }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    let r = match e { _ => 1 | E::A => 2 };\n\
    \    println(r);\n\
     }\n"
    "unreachable match arm: earlier arms already cover this case";

  check_error "runaway monomorphization rejected"
    "fn f<T>(x: T) { f((x, x)); }\n\
     fn main() { f(1); }\n"
    "monomorphization produced a type with more than 10000 nodes — a \
     generic function is recursing with a growing type argument; make \
     the recursion type-stable or add a non-generic base case";

  check "bare 'return' early-exits void fn, flushing defers"
    "fn f(b: bool) {\n\
    \    defer { println(9); }\n\
    \    if b { return; }\n\
    \    println(1);\n\
     }\n\
     fn main() { f(true); }\n"
    "#include <stdio.h>\n\n\
     static void ex_f(int b);\n\n\
     static void ex_f(int b) {\n\
    \    if (b) {\n\
    \        printf(\"%ld\\n\", (long)(9));\n\
    \        return;\n\
    \    }\n\
    \    printf(\"%ld\\n\", (long)(1));\n\
    \    printf(\"%ld\\n\", (long)(9));\n\
     }\n\n\
     int main(void) {\n\
    \    ex_f(1);\n\
    \    return 0;\n\
     }\n";

  check "bare 'return' in main means exit 0"
    "fn main() {\n\
    \    if true { return; }\n\
    \    println(1);\n\
     }\n"
    "#include <stdio.h>\n\n\
     int main(void) {\n\
    \    if (1) {\n\
    \        return 0;\n\
    \    }\n\
    \    printf(\"%ld\\n\", (long)(1));\n\
    \    return 0;\n\
     }\n";

  check_error "bare 'return' in value-returning fn rejected"
    "fn f() -> int {\n\
    \    if true { return; }\n\
    \    return 5;\n\
     }\n\
     fn main() { println(f()); }\n"
    "`return` needs a value — function returns i32";

  check_error "top-level fn shadowing builtin 'print' rejected"
    "fn print(s: str) { }\n\
     fn main() { print(\"x\"); }\n"
    "'print' is a compiler builtin and cannot be redefined as a top-level function — pick a different name";

  check_error "top-level fn shadowing builtin 'println' rejected"
    "fn println(s: str) { }\n\
     fn main() { println(\"x\"); }\n"
    "'println' is a compiler builtin and cannot be redefined as a top-level function — pick a different name";

  check_error "top-level fn shadowing builtin 'free' rejected"
    "fn free(p: *int) { }\n\
     fn main() { println(1); }\n"
    "'free' is a compiler builtin and cannot be redefined as a top-level function — pick a different name";

  check "module fn named like a builtin is allowed (qualified path)"
    "mod m { pub fn println(_s: str) { } }\n\
     fn main() { m::println(\"x\"); }\n"
    "#include <stdio.h>\n\nvoid m__println(const char *_s);\n\nvoid m__println(const char *_s) {\n}\n\nint main(void) {\n    m__println(\"x\");\n    return 0;\n}\n";

  check_error "duplicate parameter"
    "fn add(x: int, x: int) -> int {\n    return x;\n}\nfn main() {\n    println(add(1, 2));\n}\n"
    "duplicate parameter 'x' in function 'add'";

  check_error "unknown escape"
    "fn main() {\n    println(\"hi \\q there\");\n}\n"
    "unknown escape \\q";

  check_error "negative literal in unsigned"
    "fn main() {\n    let x: u8 = -1;\n    println(x);\n}\n"
    "negative literal -1 cannot fit in u8";

  check_error "negative literal out of signed range"
    "fn main() {\n    let x: i8 = -200;\n    println(x);\n}\n"
    "literal -200 does not fit in i8";

  check "negative literal fits in signed"
    "fn main() {\n    let x: i8 = -1;\n    println(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    signed char x;\n    x = -1;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check_error "C keyword as variable name"
    "fn main() {\n    let unsigned: u32 = 5;\n    println(unsigned);\n}\n"
    "variable 'unsigned' is a reserved C keyword";

  check_error "C keyword as parameter name"
    "fn foo(static: int) -> int {\n    return static;\n}\nfn main() {\n    println(foo(1));\n}\n"
    "parameter 'static' is a reserved C keyword";

  check_error "C keyword as top-level function name"
    "fn signed() -> int {\n    return 1;\n}\nfn main() {\n    println(signed());\n}\n"
    "function 'signed' is a reserved C keyword";

  check "C keyword as function name inside module"
    "mod m {\n    pub fn unsigned() -> int {\n        return 7;\n    }\n}\nfn main() {\n    println(m::unsigned());\n}\n"
    "#include <stdio.h>\n\nlong m__unsigned(void);\n\nlong m__unsigned(void) {\n    return 7;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(m__unsigned()));\n    return 0;\n}\n";

  check_error "print arity zero"
    "fn main() {\n    println();\n}\n"
    "println() takes exactly one argument, got 0";

  check_error "print arity two"
    "fn main() {\n    println(1, 2);\n}\n"
    "println() takes exactly one argument, got 2";

  check "defer LIFO at fall-through"
    "fn main() {\n    defer println(\"A\");\n    defer println(\"B\");\n    println(\"body\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"body\");\n    printf(\"%s\\n\", \"B\");\n    printf(\"%s\\n\", \"A\");\n    return 0;\n}\n";

  check "defer with explicit return uses temp"
    "fn compute() -> int {\n    defer println(\"cleanup\");\n    return 42;\n}\nfn main() {\n    println(compute());\n}\n"
    "#include <stdio.h>\n\nstatic long ex_compute(void);\n\nstatic long ex_compute(void) {\n    {\n        long __exile_ret;\n        __exile_ret = 42;\n        printf(\"%s\\n\", \"cleanup\");\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_compute()));\n    return 0;\n}\n";

  check "defer block fires its stmts in source order"
    "fn main() {\n    defer { println(\"a\"); println(\"b\"); }\n    defer println(\"c\");\n    println(\"body\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"body\");\n    printf(\"%s\\n\", \"c\");\n    printf(\"%s\\n\", \"a\");\n    printf(\"%s\\n\", \"b\");\n    return 0;\n}\n";

  check "defer in if branch chains outer cleanup on return"
    "fn process(n: int) -> int {\n    defer println(\"outer\");\n    if n > 0 {\n        defer println(\"inner\");\n        return n;\n    }\n    return 0;\n}\nfn main() {\n    println(process(5));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_process(long n);\n\nstatic long ex_process(long n) {\n    if (n > 0) {\n        {\n            long __exile_ret;\n            __exile_ret = n;\n            printf(\"%s\\n\", \"inner\");\n            printf(\"%s\\n\", \"outer\");\n            return __exile_ret;\n        }\n    }\n    {\n        long __exile_ret;\n        __exile_ret = 0;\n        printf(\"%s\\n\", \"outer\");\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_process(5)));\n    return 0;\n}\n";

  check "defer in while fires per iteration"
    "fn main() {\n    let mut i = 0;\n    while i < 2 {\n        defer println(\"end\");\n        println(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    i = 0;\n    while (i < 2) {\n        printf(\"%ld\\n\", (long)(i));\n        i = i + 1;\n        printf(\"%s\\n\", \"end\");\n    }\n    return 0;\n}\n";

  check_error "return inside defer body rejected"
    "fn foo() -> int {\n    defer { return 5; }\n    return 10;\n}\nfn main() {\n    println(foo());\n}\n"
    "'return' inside a defer body is not supported";

  check_error "defer inside defer body rejected"
    "fn main() {\n    defer { defer println(\"x\"); }\n}\n"
    "'defer' inside a defer body is not supported";

  check "top-level fn name like a C stdlib symbol still works (ex_ prefix)"
    "fn pow(base: int, exp: int) -> int {\n    return base * exp;\n}\nfn main() {\n    println(pow(2, 3));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_pow(long base, long exp);\n\nstatic long ex_pow(long base, long exp) {\n    return base * exp;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_pow(2, 3)));\n    return 0;\n}\n";

  check "tuple return + destructuring (homogeneous)"
    "fn split(n: int) -> (int, int) {\n    return (n * 2, n * 3);\n}\nfn main() {\n    let (a, b) = split(5);\n    println(a);\n    println(b);\n}\n"
    "#include <stdio.h>\n\nstruct ex_tup2_i32_i32 { long _0; long _1; };\n\nstatic struct ex_tup2_i32_i32 ex_split(long n);\n\nstatic struct ex_tup2_i32_i32 ex_split(long n) {\n    {\n        struct ex_tup2_i32_i32 __exile_ret;\n        __exile_ret._0 = n * 2;\n        __exile_ret._1 = n * 3;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    long a;\n    long b;\n    {\n        struct ex_tup2_i32_i32 __t;\n        __t = ex_split(5);\n        a = __t._0;\n        b = __t._1;\n    }\n    printf(\"%ld\\n\", (long)(a));\n    printf(\"%ld\\n\", (long)(b));\n    return 0;\n}\n";

  check "tuple literal RHS in destructuring"
    "fn main() {\n    let (x, y) = (10, 20);\n    println(x);\n    println(y);\n}\n"
    "#include <stdio.h>\n\nstruct ex_tup2_i32_i32 { long _0; long _1; };\n\nint main(void) {\n    long x;\n    long y;\n    {\n        struct ex_tup2_i32_i32 __t;\n        __t._0 = 10;\n        __t._1 = 20;\n        x = __t._0;\n        y = __t._1;\n    }\n    printf(\"%ld\\n\", (long)(x));\n    printf(\"%ld\\n\", (long)(y));\n    return 0;\n}\n";

  check "tuple variable bound from literal, passed as fn arg"
    "fn show(t: (int, int)) {\n    let (a, b) = t;\n    println(a);\n    println(b);\n}\nfn main() {\n    let t = (10, 20);\n    show(t);\n}\n"
    "#include <stdio.h>\n\nstruct ex_tup2_i32_i32 { long _0; long _1; };\n\nstatic void ex_show(struct ex_tup2_i32_i32 t);\n\nstatic void ex_show(struct ex_tup2_i32_i32 t) {\n    long a;\n    long b;\n    {\n        struct ex_tup2_i32_i32 __t;\n        __t = t;\n        a = __t._0;\n        b = __t._1;\n    }\n    printf(\"%ld\\n\", (long)(a));\n    printf(\"%ld\\n\", (long)(b));\n}\n\nint main(void) {\n    struct ex_tup2_i32_i32 t;\n    t._0 = 10;\n    t._1 = 20;\n    ex_show(t);\n    return 0;\n}\n";

  check_error "empty tuple type rejected"
    "fn foo() -> () {\n    return (1, 2);\n}\nfn main() {\n    foo();\n}\n"
    "empty tuple type '()' is not supported";

  check_error "destructuring arity mismatch"
    "fn split() -> (int, int) {\n    return (1, 2);\n}\nfn main() {\n    let (a, b, c) = split();\n    println(a);\n}\n"
    "destructuring 'let (...)' has 3 names but value is a 2-tuple";

  check_error "destructuring non-tuple value"
    "fn main() {\n    let (a, b) = 5;\n    println(a);\n}\n"
    "destructuring 'let (...)' expects a tuple value, got i32";

  check_error "destructuring single name rejected"
    "fn main() {\n    let (a) = (1, 2);\n    println(a);\n}\n"
    "destructuring 'let (...)' needs at least two names";

  check_error "duplicate name in destructuring"
    "fn split() -> (int, int) {\n    return (1, 2);\n}\nfn main() {\n    let (a, a) = split();\n    println(a);\n}\n"
    "duplicate name 'a' in 'let (...)'";

  check_error "tuple cannot be printed"
    "fn split() -> (int, int) {\n    return (1, 2);\n}\nfn main() {\n    println(split());\n}\n"
    "cannot print a tuple; destructure with 'let (...)' first";

  check "struct decl + literal + field access + by-value param/return"
    "struct Point {\n    x: int,\n    y: int,\n}\nfn make(a: int, b: int) -> Point {\n    return Point { x: a, y: b };\n}\nfn main() {\n    let p = make(3, 4);\n    println(p.x);\n    println(p.y);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic struct ex_Point ex_make(long a, long b);\n\nstatic struct ex_Point ex_make(long a, long b) {\n    {\n        struct ex_Point __exile_ret;\n        __exile_ret.x = a;\n        __exile_ret.y = b;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Point p;\n    p = ex_make(3, 4);\n    printf(\"%ld\\n\", (long)(p.x));\n    printf(\"%ld\\n\", (long)(p.y));\n    return 0;\n}\n";

  check "struct field assignment"
    "struct Point { x: int, y: int, }\nfn main() {\n    let mut p = Point { x: 1, y: 2 };\n    p.x = 99;\n    println(p.x);\n    println(p.y);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 1;\n    p.y = 2;\n    p.x = 99;\n    printf(\"%ld\\n\", (long)(p.x));\n    printf(\"%ld\\n\", (long)(p.y));\n    return 0;\n}\n";

  check_error "unknown struct name"
    "fn main() {\n    let p = Foo { x: 1 };\n    println(p.x);\n}\n"
    "unknown struct 'Foo'";

  check_error "struct literal missing field"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1 };\n    println(p.x);\n}\n"
    "struct literal 'Point' missing field(s): y";

  check_error "struct literal extra field"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2, z: 3 };\n    println(p.x);\n}\n"
    "struct literal 'Point' has unknown field(s): z";

  check_error "struct literal wrong field type"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: \"hi\" };\n    println(p.x);\n}\n"
    "field 'y' of struct 'Point': expected i32, got str";

  check_error "struct literal duplicate field"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, x: 2, y: 3 };\n    println(p.x);\n}\n"
    "duplicate field 'x' in struct literal 'Point'";

  check_error "field access on non-struct"
    "fn main() {\n    let x = 5;\n    println(x.foo);\n}\n"
    "field access '.foo' requires a struct value or pointer to struct, got i32";

  check_error "unknown field on struct"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    println(p.z);\n}\n"
    "struct 'Point' has no field 'z'";

  check_error "duplicate field in struct decl"
    "struct Point { x: int, x: int, }\nfn main() {\n    let p = Point { x: 1 };\n    println(p.x);\n}\n"
    "duplicate field 'x' in struct 'Point'";

  check_error "print of struct rejected"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    println(p);\n}\n"
    "cannot print a struct value (Point); print individual fields, or mark the struct with `@debug`";

  check "pointer to struct: ref + auto-deref field access + assign"
    "struct Point { x: int, y: int, }\nfn shift(p: *Point, dx: int) {\n    p.x = p.x + dx;\n}\nfn main() {\n    let p = Point { x: 0, y: 0 };\n    shift(&p, 10);\n    println(p.x);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic void ex_shift(struct ex_Point *p, long dx);\n\nstatic void ex_shift(struct ex_Point *p, long dx) {\n    p->x = p->x + dx;\n}\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 0;\n    p.y = 0;\n    ex_shift(&p, 10);\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n";

  (* `( * p).field` used to emit `* p.field` — C parses `.` tighter
     than `*`, so cc rejected it ("`p` is a pointer; did you mean
     `->`?").  Codegen now detects the explicit `Deref → Field`
     shape and emits arrow notation directly. *)
  check "explicit `(*p).field` lowers to `p->field` (precedence fix)"
    "struct P { x: int, y: int }\n\
     fn main() {\n\
    \    let p = P { x: 7, y: 3 };\n\
    \    let q: *const P = &p;\n\
    \    println((*q).x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nint main(void) {\n    struct ex_P p;\n    const struct ex_P *q;\n    p.x = 7;\n    p.y = 3;\n    q = &p;\n    printf(\"%ld\\n\", (long)(q->x));\n    return 0;\n}\n";

  check "pointer to int: ref, deref-load, deref-store"
    "fn main() {\n    let n = 5;\n    let pn = &n;\n    println(*pn);\n    *pn = 99;\n    println(n);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long n;\n    long *pn;\n    n = 5;\n    pn = &n;\n    printf(\"%ld\\n\", (long)(*pn));\n    *pn = 99;\n    printf(\"%ld\\n\", (long)(n));\n    return 0;\n}\n";

  check_error "deref of non-pointer"
    "fn main() {\n    let n = 5;\n    let x = *n;\n    println(x);\n}\n"
    "deref '*' requires a pointer, got i32";

  check_error "assign through deref of non-pointer"
    "fn main() {\n    let n = 5;\n    *n = 7;\n    println(n);\n}\n"
    "assignment through '*' requires a pointer, got i32";

  check_error "pointer cannot be printed"
    "fn main() {\n    let n = 5;\n    println(&n);\n}\n"
    "cannot print a pointer value (*i32); deref or print a field";

  check "new + free + defer-free for heap struct"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = new Point { x: 1, y: 2 };\n    defer free(p);\n    println(p.x);\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point *p;\n    p = malloc(sizeof(struct ex_Point));\n    p->x = 1;\n    p->y = 2;\n    printf(\"%ld\\n\", (long)(p->x));\n    free(p);\n    return 0;\n}\n";

  check "fn returning *Point via new"
    "struct Point { x: int, y: int, }\nfn make() -> *Point {\n    return new Point { x: 0, y: 0 };\n}\nfn main() {\n    let p = make();\n    defer free(p);\n    println(p.x);\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic struct ex_Point *ex_make(void);\n\nstatic struct ex_Point *ex_make(void) {\n    {\n        struct ex_Point * __exile_ret;\n        __exile_ret = malloc(sizeof(struct ex_Point));\n        __exile_ret->x = 0;\n        __exile_ret->y = 0;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Point *p;\n    p = ex_make();\n    printf(\"%ld\\n\", (long)(p->x));\n    free(p);\n    return 0;\n}\n";

  check_error "free of non-pointer"
    "fn main() {\n    let n = 5;\n    free(n);\n}\n"
    "'free' expects a pointer, got i32";

  check_error "free used as value"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = new Point { x: 0, y: 0 };\n    let x = free(p);\n    println(x);\n}\n"
    "'free' returns void, cannot use as a value";

  check_error "new of unknown struct"
    "fn main() {\n    let p = new Foo { x: 1 };\n    println(p.x);\n}\n"
    "unknown struct 'Foo'";

  check "functional update copies base then overrides"
    "struct Point { x: int, y: int, z: int, }\nfn main() {\n    let p = Point { x: 1, y: 2, z: 3 };\n    let q = Point { x: 99, ..p };\n    println(q.x);\n    println(q.y);\n    println(q.z);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; long z; };\n\nint main(void) {\n    struct ex_Point p;\n    struct ex_Point q;\n    p.x = 1;\n    p.y = 2;\n    p.z = 3;\n    q = p;\n    q.x = 99;\n    printf(\"%ld\\n\", (long)(q.x));\n    printf(\"%ld\\n\", (long)(q.y));\n    printf(\"%ld\\n\", (long)(q.z));\n    return 0;\n}\n";

  check "functional update with new copies through deref"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    let r = new Point { y: 50, ..p };\n    defer free(r);\n    println(r.x);\n    println(r.y);\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point p;\n    struct ex_Point *r;\n    p.x = 1;\n    p.y = 2;\n    r = malloc(sizeof(struct ex_Point));\n    *r = p;\n    r->y = 50;\n    printf(\"%ld\\n\", (long)(r->x));\n    printf(\"%ld\\n\", (long)(r->y));\n    free(r);\n    return 0;\n}\n";

  check_error "functional update with mismatched base type"
    "struct Point { x: int, y: int, }\nstruct Other { z: int, }\nfn main() {\n    let o = Other { z: 7 };\n    let p = Point { x: 1, ..o };\n    println(p.x);\n}\n"
    "'..base' in struct literal 'Point' expects a value of type Point, got Other";

  check "functional update on a generic struct (base pins the instance)"
    "struct Pair<A, B> { fst: A, snd: B }\n\
     fn main() {\n\
    \    let p = Pair { fst: 1, snd: true };\n\
    \    let q = Pair { fst: 99, ..p };\n\
    \    println(q.fst);\n\
     }\n"
    "#include <stdio.h>\n\n\
     struct ex_Pair_i32_bool { long fst; int snd; };\n\n\
     int main(void) {\n\
    \    struct ex_Pair_i32_bool p;\n\
    \    struct ex_Pair_i32_bool q;\n\
    \    p.fst = 1;\n\
    \    p.snd = 1;\n\
    \    q = p;\n\
    \    q.fst = 99;\n\
    \    printf(\"%ld\\n\", (long)(q.fst));\n\
    \    return 0;\n\
     }\n";

  check "null literal in struct field + equality check"
    "struct Node { value: int, next: *Node, }\nfn main() {\n    let n = new Node { value: 5, next: null };\n    defer free(n);\n    if n.next == null {\n        println(n.value);\n    }\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Node { long value; struct ex_Node *next; };\n\nint main(void) {\n    struct ex_Node *n;\n    n = malloc(sizeof(struct ex_Node));\n    n->value = 5;\n    n->next = ((void *)0);\n    if (n->next == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(n->value));\n    }\n    free(n);\n    return 0;\n}\n";

  check "null with typed let binding"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p: *Point = null;\n    if p == null {\n        println(42);\n    }\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point *p;\n    p = ((void *)0);\n    if (p == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(42));\n    }\n    return 0;\n}\n";

  check_error "naked null without type ann rejected"
    "fn main() {\n    let p = null;\n    println(p);\n}\n"
    "cannot infer pointer type for 'null'; add a type annotation like 'let p: *T = null;'";

  check_error "deref of null rejected"
    "fn main() {\n    let x = *null;\n    println(x);\n}\n"
    "cannot deref 'null'";

  check_error "print of null rejected"
    "fn main() {\n    println(null);\n}\n"
    "cannot print 'null'";

  check_multi "wildcard import inlines pub items, hides private"
    [ ("lib.exl",
       "pub fn hello() -> int {\n    return 42;\n}\n\
        fn priv() -> int {\n    return 99;\n}\n");
      ("main.exl",
       "use lib::*;\n\nfn main() {\n    println(hello());\n}\n") ]
    "main.exl"
    "#include <stdio.h>\n\nlong ex_hello(void);\n\nlong ex_hello(void) {\n    return 42;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_hello()));\n    return 0;\n}\n";

  check "by-value method called via dot form"
    "struct Point { x: int, y: int }\n\
     impl Point {\n\
    \    pub fn area(self: Point) -> int { return self.x * self.y; }\n\
     }\n\
     fn main() {\n\
    \    let p = Point { x: 3, y: 4 };\n\
    \    println(p.area());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nlong Point__area(struct ex_Point self);\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 3;\n    p.y = 4;\n    printf(\"%ld\\n\", (long)(Point__area(p)));\n    return 0;\n}\n\nlong Point__area(struct ex_Point self) {\n    return self.x * self.y;\n}\n";

  check "ptr-self method auto-refs receiver, mutates fields"
    "struct Point { x: int, y: int }\n\
     impl Point {\n\
    \    pub fn shift(self: *Point, dx: int) { self.x = self.x + dx; }\n\
     }\n\
     fn main() {\n\
    \    let p = Point { x: 1, y: 2 };\n\
    \    p.shift(10);\n\
    \    println(p.x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nvoid Point__shift(struct ex_Point *self, long dx);\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 1;\n    p.y = 2;\n    Point__shift(&p, 10);\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n\nvoid Point__shift(struct ex_Point *self, long dx) {\n    self->x = self->x + dx;\n}\n";

  check "UFCS call + auto-deref via ptr receiver to value-self method"
    "struct P { x: int }\n\
     impl P { pub fn get(self: P) -> int { return self.x; } }\n\
     fn main() {\n\
    \    let p = P { x: 7 };\n\
    \    let q: *P = &p;\n\
    \    println(P::get(p));\n\
    \    println(q.get());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; };\n\nlong P__get(struct ex_P self);\n\nint main(void) {\n    struct ex_P p;\n    struct ex_P *q;\n    p.x = 7;\n    q = &p;\n    printf(\"%ld\\n\", (long)(P__get(p)));\n    printf(\"%ld\\n\", (long)(P__get(*q)));\n    return 0;\n}\n\nlong P__get(struct ex_P self) {\n    return self.x;\n}\n";

  check "static method (no self) invoked as Foo::name(...)"
    "struct P { x: int }\n\
     impl P {\n\
    \    pub fn make(v: int) -> P { return P { x: v }; }\n\
     }\n\
     fn main() {\n\
    \    let p = P::make(42);\n\
    \    println(p.x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; };\n\nstruct ex_P P__make(long v);\n\nint main(void) {\n    struct ex_P p;\n    p = P__make(42);\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n\nstruct ex_P P__make(long v) {\n    {\n        struct ex_P __exile_ret;\n        __exile_ret.x = v;\n        return __exile_ret;\n    }\n}\n";

  check_error "method on unknown struct rejected"
    "impl Nope { fn x(self: Nope) {} }\nfn main() {}\n"
    "unknown type 'Nope' in 'impl' block";

  check_error "method 'self' must have struct type"
    "struct P { x: int }\nimpl P { fn foo(self: int) {} }\nfn main() {}\n"
    "first parameter 'self' must have type 'P', '*P', or '*const P', got i32";

  (* `*const self` receiver — read-only borrow.  Lowers to a
     `TConstPtr (TStruct target)` parameter the same way `*self`
     lowers to `TPtr (TStruct target)`.  Method dispatch auto-refs
     a value receiver (`p.sum()`) and a `*T` is implicitly coercible
     to `*const T` at the receiver slot. *)
  check "`*const self` receiver parses, auto-refs and dispatches"
    "struct P { x: int, y: int }\n\
     impl P {\n\
    \    pub fn sum(*const self) -> int { return self.x + self.y; }\n\
     }\n\
     fn main() {\n\
    \    let p = P { x: 3, y: 5 };\n\
    \    println(p.sum());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nlong P__sum(const struct ex_P *self);\n\nint main(void) {\n    struct ex_P p;\n    p.x = 3;\n    p.y = 5;\n    printf(\"%ld\\n\", (long)(P__sum(&p)));\n    return 0;\n}\n\nlong P__sum(const struct ex_P *self) {\n    return self->x + self->y;\n}\n";

  check_error "writing through `*const self` rejected"
    "struct P { x: int }\n\
     impl P {\n\
    \    pub fn bad(*const self) { self.x = 9; }\n\
     }\n\
     fn main() { let p = P { x: 1 }; p.bad(); }\n"
    "cannot assign field 'x' through '*const' pointer *const P (pointee is read-only)";;

  check_error "method name clashes with field rejected"
    "struct P { v: int }\nimpl P { fn v(self: P) -> int { return self.v; } }\nfn main() {}\n"
    "method name 'v' clashes with a field on 'P'";

  check_error "duplicate method across impl blocks rejected"
    "struct P { x: int }\nimpl P { fn foo(self: P) {} }\nimpl P { fn foo(self: P) {} }\nfn main() {}\n"
    "method 'foo' on 'P' already defined in another 'impl' block";

  check_error "method call on non-struct rejected"
    "fn main() { let x: int = 5; println(x.foo()); }\n"
    "method call '.foo()' requires a struct or enum value (or a pointer to one), got i32";

  check_error "unknown method rejected"
    "struct P { x: int }\nimpl P { pub fn foo(self: P) -> int { return self.x; } }\nfn main() { let p = P { x: 1 }; println(p.bar()); }\n"
    "no method 'bar' on type 'P'";

  (* Generic impl (impl<T> Foo<T>) — methods on a generic struct,
     monomorphized per concrete receiver; runtime coverage lives in
     examples/generic_impl.exl. *)
  check "generic impl method monomorphizes per receiver instance"
    "struct Box<T> { v: T }\n\
     impl<T> Box<T> {\n\
    \    pub fn get(self) -> T { return self.v; }\n\
     }\n\
     fn main() {\n\
    \    let b = Box { v: 5 };\n\
    \    println(b.get());\n\
     }\n"
    "#include <stdio.h>\n\n\
     struct ex_Box_i32 { long v; };\n\n\
     long Box__get_i32(struct ex_Box_i32 self);\n\n\
     int main(void) {\n\
    \    struct ex_Box_i32 b;\n\
    \    b.v = 5;\n\
    \    printf(\"%ld\\n\", (long)(Box__get_i32(b)));\n\
    \    return 0;\n\
     }\n\n\
     long Box__get_i32(struct ex_Box_i32 self) {\n\
    \    return self.v;\n\
     }\n";

  check "nested generic struct field normalizes to a flat instance"
    "struct Box<T> { v: T }\n\
     struct Wrapper<T> { inner: Box<T> }\n\
     fn main() {\n\
    \    let w = Wrapper { inner: Box { v: 7 } };\n\
    \    println(w.inner.v);\n\
     }\n"
    "#include <stdio.h>\n\n\
     struct ex_Box_i32 { long v; };\n\
     struct ex_Wrapper_i32 { struct ex_Box_i32 inner; };\n\n\
     int main(void) {\n\
    \    struct ex_Wrapper_i32 w;\n\
    \    struct ex_Box_i32 __lift_0;\n\
    \    __lift_0.v = 7;\n\
    \    w.inner = __lift_0;\n\
    \    printf(\"%ld\\n\", (long)(w.inner.v));\n\
    \    return 0;\n\
     }\n";

  check_error "bare 'self' outside an impl rejected"
    "fn foo(self) -> int { return 1; }\nfn main() { println(1); }\n"
    "bare 'self' is only allowed as the receiver of an 'impl' method";

  check_error "generic impl target args must match declared params"
    "struct Pair<A, B> { fst: A, snd: B }\n\
     impl<A, B> Pair<B, A> { pub fn f(self) -> A { return self.snd; } }\n\
     fn main() { println(1); }\n"
    "'impl<A, B>' target must be 'Pair<A, B>' — the type arguments must \
     match the declared parameters in order";

  check_error "private method called from outside rejected"
    "struct P { x: int }\nimpl P { fn priv(self: P) -> int { return self.x; } }\nfn main() { let p = P { x: 1 }; println(p.priv()); }\n"
    "method 'priv' is private to 'P'";

  check "enum unit variants + match with explicit arms"
    "enum Color {\n\
    \    Red\n\
    \    | Green\n\
    \    | Blue\n\
     }\n\
     fn main() {\n\
    \    let c = Color::Green;\n\
    \    match c {\n\
    \        Color::Red => println(\"r\")\n\
    \        | Color::Green => println(\"g\")\n\
    \        | Color::Blue => println(\"b\")\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Color_tag { ex_Color_Red, ex_Color_Green, ex_Color_Blue };\nstruct ex_Color { enum ex_Color_tag tag; };\n\nint main(void) {\n    struct ex_Color c;\n    c.tag = ex_Color_Green;\n    {\n        struct ex_Color __m;\n        __m = c;\n        switch (__m.tag) {\n        case ex_Color_Red:\n            {\n                printf(\"%s\\n\", \"r\");\n                break;\n            }\n        case ex_Color_Green:\n            {\n                printf(\"%s\\n\", \"g\");\n                break;\n            }\n        case ex_Color_Blue:\n            {\n                printf(\"%s\\n\", \"b\");\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "match with wildcard arm covers remaining variants"
    "enum E { A | B | C }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    match e {\n\
    \        E::A => println(\"a\")\n\
    \        | _ => println(\"other\")\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B, ex_E_C };\nstruct ex_E { enum ex_E_tag tag; };\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_A;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                printf(\"%s\\n\", \"a\");\n                break;\n            }\n        default:\n            {\n                printf(\"%s\\n\", \"other\");\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "tuple variants + match with bind patterns"
    "enum Shape {\n\
    \    Square\n\
    \    | Circle(int)\n\
    \    | Rect(int, int)\n\
     }\n\
     fn main() {\n\
    \    let s = Shape::Rect(3, 4);\n\
    \    match s {\n\
    \        Shape::Square => println(\"sq\")\n\
    \        | Shape::Circle(r) => println(r)\n\
    \        | Shape::Rect(w, h) => println(w + h)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Shape_tag { ex_Shape_Square, ex_Shape_Circle, ex_Shape_Rect };\nstruct ex_Shape { enum ex_Shape_tag tag; union { struct { long _0; } Circle; struct { long _0; long _1; } Rect; } data; };\n\nint main(void) {\n    struct ex_Shape s;\n    s.tag = ex_Shape_Rect;\n    s.data.Rect._0 = 3;\n    s.data.Rect._1 = 4;\n    {\n        struct ex_Shape __m;\n        __m = s;\n        switch (__m.tag) {\n        case ex_Shape_Square:\n            {\n                printf(\"%s\\n\", \"sq\");\n                break;\n            }\n        case ex_Shape_Circle:\n            {\n                long r = __m.data.Circle._0;\n                printf(\"%ld\\n\", (long)(r));\n                break;\n            }\n        case ex_Shape_Rect:\n            {\n                long w = __m.data.Rect._0;\n                long h = __m.data.Rect._1;\n                printf(\"%ld\\n\", (long)(w + h));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "wrong arg count for tuple variant rejected"
    "enum E { A(int) }\nfn main() { let e = E::A(1, 2); }\n"
    "variant 'E::A' takes 1 argument(s), got 2";

  check_error "wrong arg type for tuple variant rejected"
    "enum E { A(str) }\nfn main() { let e = E::A(5); }\n"
    "argument 1 of 'E::A': expected str, got i32";

  check_error "wrong bind count in pattern rejected"
    "enum E { A(int, int) }\nfn main() { let e = E::A(1, 2); match e { E::A(x) => println(x) } }\n"
    "variant 'A' has 2 field(s), pattern binds 1";

  check_error "duplicate bind name in pattern rejected"
    "enum E { A(int, int) }\nfn main() { let e = E::A(1, 2); match e { E::A(x, x) => println(x) } }\n"
    "duplicate bind name 'x' in pattern";

  check "match as expression in let RHS"
    "enum E { A | B(int) }\n\
     fn main() {\n\
    \    let e = E::B(7);\n\
    \    let v = match e {\n\
    \        E::A => 0\n\
    \        | E::B(n) => n + 1\n\
    \    };\n\
    \    println(v);\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nint main(void) {\n    struct ex_E e;\n    long v;\n    e.tag = ex_E_B;\n    e.data.B._0 = 7;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                v = 0;\n                break;\n            }\n        case ex_E_B:\n            {\n                long n = __m.data.B._0;\n                v = n + 1;\n                break;\n            }\n        }\n    }\n    printf(\"%ld\\n\", (long)(v));\n    return 0;\n}\n";

  check "match as expression in return position"
    "enum E { A | B(int) }\n\
     fn classify(e: E) -> int {\n\
    \    return match e {\n\
    \        E::A => 0\n\
    \        | E::B(n) => n\n\
    \    };\n\
     }\n\
     fn main() {\n\
    \    let e = E::B(42);\n\
    \    println(classify(e));\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nstatic long ex_classify(struct ex_E e);\n\nstatic long ex_classify(struct ex_E e) {\n    {\n        long __exile_ret;\n        {\n            struct ex_E __m;\n            __m = e;\n            switch (__m.tag) {\n            case ex_E_A:\n                {\n                    __exile_ret = 0;\n                    break;\n                }\n            case ex_E_B:\n                {\n                    long n = __m.data.B._0;\n                    __exile_ret = n;\n                    break;\n                }\n            }\n        }\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_B;\n    e.data.B._0 = 42;\n    printf(\"%ld\\n\", (long)(ex_classify(e)));\n    return 0;\n}\n";

  check_error "match arms with inconsistent types rejected"
    "enum E { A | B }\nfn main() { let v = match E::A { E::A => 1 | E::B => true }; println(v); }\n"
    "match arms have inconsistent types: i32 vs bool";

  check "EnumLit as fn arg lifts via __lift_N temp"
    "enum E { A | B(int) }\n\
     fn show(e: E) {\n\
    \    match e {\n\
    \        E::A => println(0)\n\
    \        | E::B(n) => println(n)\n\
    \    }\n\
     }\n\
     fn main() {\n\
    \    show(E::B(7));\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nstatic void ex_show(struct ex_E e);\n\nstatic void ex_show(struct ex_E e) {\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        case ex_E_B:\n            {\n                long n = __m.data.B._0;\n                printf(\"%ld\\n\", (long)(n));\n                break;\n            }\n        }\n    }\n}\n\nint main(void) {\n    struct ex_E __lift_0;\n    __lift_0.tag = ex_E_B;\n    __lift_0.data.B._0 = 7;\n    ex_show(__lift_0);\n    return 0;\n}\n";

  check "Match as sub-expression in BinOp lifts to __lift_N"
    "enum E { A | B(int) }\n\
     fn main() {\n\
    \    let e = E::B(2);\n\
    \    let total = 1 + match e {\n\
    \        E::A => 0\n\
    \        | E::B(n) => n\n\
    \    };\n\
    \    println(total);\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nint main(void) {\n    struct ex_E e;\n    long total;\n    long __lift_0;\n    e.tag = ex_E_B;\n    e.data.B._0 = 2;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                __lift_0 = 0;\n                break;\n            }\n        case ex_E_B:\n            {\n                long n = __m.data.B._0;\n                __lift_0 = n;\n                break;\n            }\n        }\n    }\n    total = 1 + __lift_0;\n    printf(\"%ld\\n\", (long)(total));\n    return 0;\n}\n";

  check "struct-like variant: shorthand bind in pattern"
    "enum E {\n\
    \    A\n\
    \    | B { x: int, y: int }\n\
     }\n\
     fn main() {\n\
    \    let e = E::B { x: 3, y: 4 };\n\
    \    match e {\n\
    \        E::A => println(0)\n\
    \        | E::B { x, y } => println(x + y)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long x; long y; } B; } data; };\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_B;\n    e.data.B.x = 3;\n    e.data.B.y = 4;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        case ex_E_B:\n            {\n                long x = __m.data.B.x;\n                long y = __m.data.B.y;\n                printf(\"%ld\\n\", (long)(x + y));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "struct-syntax for tuple variant rejected"
    "enum E { A(int) }\nfn main() { let e = E::A { x: 1 }; }\n"
    "variant 'E::A' is a tuple variant; construct it with '(...)', not with '{ field: ... }'";

  check_error "tuple-syntax for struct variant rejected"
    "enum E { A { x: int } }\nfn main() { let e = E::A(1); }\n"
    "variant 'E::A' is a struct variant; construct it with '{ field: ... }', not with '(...)'";

  check_error "missing field in struct variant ctor rejected"
    "enum E { A { x: int, y: int } }\nfn main() { let e = E::A { x: 1 }; }\n"
    "missing field 'y' in 'E::A' construction";

  check_error "extra field in struct variant ctor rejected"
    "enum E { A { x: int } }\nfn main() { let e = E::A { x: 1, y: 2 }; }\n"
    "variant 'E::A' has no field 'y'";

  check_error "tuple pattern on struct variant rejected"
    "enum E { A { x: int } }\nfn main() { let e = E::A { x: 1 }; match e { E::A(x) => println(x) } }\n"
    "variant 'A' is a struct variant; match it with '{ field: pat }', not '(...)'";

  check_error "unknown field in variant pattern rejected"
    "enum E { A { x: int } }\nfn main() { let e = E::A { x: 1 }; match e { E::A { y } => println(y) } }\n"
    "variant 'A' has no field 'y'";

  check "StructLit as fn arg lifts to __lift_N"
    "struct P { x: int, y: int }\n\
     fn sum(p: P) -> int { return p.x + p.y; }\n\
     fn main() {\n\
    \    println(sum(P { x: 3, y: 4 }));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nstatic long ex_sum(struct ex_P p);\n\nstatic long ex_sum(struct ex_P p) {\n    return p.x + p.y;\n}\n\nint main(void) {\n    struct ex_P __lift_0;\n    __lift_0.x = 3;\n    __lift_0.y = 4;\n    printf(\"%ld\\n\", (long)(ex_sum(__lift_0)));\n    return 0;\n}\n";

  check_error "non-exhaustive match rejected"
    "enum E { A | B }\nfn main() { let e = E::A; match e { E::A => println(\"a\") } }\n"
    "non-exhaustive 'match': pattern 'B' is not covered (add an arm or '_')";

  check_error "non-exhaustive nested match names the missing pattern"
    "enum Inner { A(int) | B }\n\
     enum Outer { Wrap(Inner) | Empty }\n\
     fn main() {\n\
    \    let o = Outer::Empty;\n\
    \    let r = match o { Outer::Wrap(Inner::A(n)) => n | Outer::Empty => 0 };\n\
    \    println(r);\n\
     }\n"
    "non-exhaustive 'match': pattern 'Wrap(B)' is not covered (add an arm or '_')";

  check_error "redundant nested match arm rejected"
    "enum Inner { A(int) | B }\n\
     enum Outer { Wrap(Inner) | Empty }\n\
     fn main() {\n\
    \    let o = Outer::Empty;\n\
    \    let r = match o { Outer::Wrap(Inner::A(n)) => n \
                          | Outer::Wrap(Inner::A(m)) => m | _ => 0 };\n\
    \    println(r);\n\
     }\n"
    "unreachable match arm: earlier arms already cover this case";

  (* Nested-pattern codegen correctness is exercised by execution in
     examples/enums.exl (verify-host) — more robust than pinning the
     decision-chain's exact C here. *)

  check_error "unknown variant in constructor rejected"
    "enum E { A }\nfn main() { let e = E::Nope; }\n"
    "enum 'E' has no variant 'Nope'";

  check_error "unknown enum in constructor rejected"
    "fn main() { let e = Nope::A; }\n"
    "unknown enum 'Nope'";

  check_error "duplicate variant in enum decl rejected"
    "enum E { A | A }\nfn main() {}\n"
    "duplicate variant 'A' in enum 'E'";

  check_error "match on non-enum rejected"
    "fn main() { let x = 5; match x { _ => println(\"x\") } }\n"
    "'match' requires an enum value, got i32";

  check_error "pattern enum mismatch with scrutinee rejected"
    "enum A { X }\nenum B { Y }\nfn main() { let a = A::X; match a { B::Y => println(\"b\") } }\n"
    "pattern matches 'B' but the value has type 'A'";

  check_error "print of enum value rejected"
    "enum E { A }\nfn main() { let e = E::A; println(e); }\n"
    "cannot print an enum value (E); match on it and print per variant, or mark the enum with `@debug`";

  check_error "unknown generic type rejected"
    "fn main() { let x: Box<int> = 0; }\n"
    "unknown generic type 'Box'";

  check "generic decls without instantiation emit nothing"
    "enum Option<T> { None | Some(T) }\n\
     struct Pair<A, B> { fst: A, snd: B }\n\
     fn id<T>(x: T) -> T { return x; }\n\
     fn main() {}\n"
    "#include <stdio.h>\n\nint main(void) {\n    return 0;\n}\n";

  check "generic struct construction infers type args from fields"
    "struct Pair<A, B> { fst: A, snd: B }\n\
     fn main() {\n\
    \    let p = Pair { fst: 5, snd: \"hi\" };\n\
    \    println(p.fst);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Pair_i32_str { long fst; const char *snd; };\n\nint main(void) {\n    struct ex_Pair_i32_str p;\n    p.fst = 5;\n    p.snd = \"hi\";\n    printf(\"%ld\\n\", (long)(p.fst));\n    return 0;\n}\n";

  check "Result<T, E>: bidirectional typing infers E from return type"
    "enum Result<T, E> { Ok(T) | Err(E) }\n\
     enum IoErr { NotFound }\n\
     fn make() -> Result<int, IoErr> { return Result::Ok(42); }\n\
     fn main() {\n\
    \    let r = make();\n\
    \    match r {\n\
    \        Result::Ok(v) => println(v)\n\
    \        | Result::Err(_) => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_IoErr_tag { ex_IoErr_NotFound };\nstruct ex_IoErr { enum ex_IoErr_tag tag; };\nenum ex_Result_i32_ex_IoErr_tag { ex_Result_i32_ex_IoErr_Ok, ex_Result_i32_ex_IoErr_Err };\nstruct ex_Result_i32_ex_IoErr { enum ex_Result_i32_ex_IoErr_tag tag; union { struct { long _0; } Ok; struct { struct ex_IoErr _0; } Err; } data; };\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void);\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void) {\n    {\n        struct ex_Result_i32_ex_IoErr __exile_ret;\n        __exile_ret.tag = ex_Result_i32_ex_IoErr_Ok;\n        __exile_ret.data.Ok._0 = 42;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Result_i32_ex_IoErr r;\n    r = ex_make();\n    {\n        struct ex_Result_i32_ex_IoErr __m;\n        __m = r;\n        switch (__m.tag) {\n        case ex_Result_i32_ex_IoErr_Ok:\n            {\n                long v = __m.data.Ok._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Result_i32_ex_IoErr_Err:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "Option::None: type-ann pins T when payload doesn't"
    "enum Option<T> { None | Some(T) }\n\
     fn main() {\n\
    \    let o: Option<int> = Option::None;\n\
    \    match o {\n\
    \        Option::None => println(0)\n\
    \        | Option::Some(x) => println(x)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_None;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "generic enum: tuple ctor infers payload + match destructures"
    "enum Option<T> { None | Some(T) }\n\
     fn main() {\n\
    \    let o = Option::Some(42);\n\
    \    match o {\n\
    \        Option::Some(x) => println(x)\n\
    \        | Option::None => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_Some;\n    o.data.Some._0 = 42;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "?T sugar: ?int parses as Option<int>"
    "fn main() {\n\
    \    let o: ?int = Option::Some(5);\n\
    \    match o {\n\
    \        Option::Some(x) => println(x)\n\
    \        | Option::None    => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_Some;\n    o.data.Some._0 = 5;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "orelse: Option<T> orelse default unwraps Some, falls back on None"
    "fn first_or(o: ?int, d: int) -> int {\n\
    \    return o orelse d;\n\
     }\n\
     fn main() {\n\
    \    let some = Option::Some(7);\n\
    \    let none: ?int = Option::None;\n\
    \    println(first_or(some, 99));\n\
    \    println(first_or(none, 99));\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstatic long ex_first_or(struct ex_Option_i32 o, long d);\n\nstatic long ex_first_or(struct ex_Option_i32 o, long d) {\n    {\n        long __exile_ret;\n        {\n            struct ex_Option_i32 __m;\n            __m = o;\n            switch (__m.tag) {\n            case ex_Option_i32_Some:\n                {\n                    long __orelse_v = __m.data.Some._0;\n                    __exile_ret = __orelse_v;\n                    break;\n                }\n            case ex_Option_i32_None:\n                {\n                    __exile_ret = d;\n                    break;\n                }\n            }\n        }\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Option_i32 some;\n    struct ex_Option_i32 none;\n    some.tag = ex_Option_i32_Some;\n    some.data.Some._0 = 7;\n    none.tag = ex_Option_i32_None;\n    printf(\"%ld\\n\", (long)(ex_first_or(some, 99)));\n    printf(\"%ld\\n\", (long)(ex_first_or(none, 99)));\n    return 0;\n}\n";

  check_error "orelse on non-enum rejected"
    "fn main() { let x = 5 orelse 0; println(x); }\n"
    "'orelse' requires an Option or Result value, got i32";

  check "try: Option<T> early-returns None from enclosing fn"
    "fn incr(o: ?int) -> ?int {\n\
    \    let v = try o;\n\
    \    return Option::Some(v + 1);\n\
     }\n\
     fn main() {\n\
    \    let some = Option::Some(7);\n\
    \    let none: ?int = Option::None;\n\
    \    match incr(some) {\n\
    \        Option::Some(x) => println(x)\n\
    \        | Option::None    => println(0)\n\
    \    }\n\
    \    match incr(none) {\n\
    \        Option::Some(x) => println(x)\n\
    \        | Option::None    => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstatic struct ex_Option_i32 ex_incr(struct ex_Option_i32 o);\n\nstatic struct ex_Option_i32 ex_incr(struct ex_Option_i32 o) {\n    long v;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long __try_v = __m.data.Some._0;\n                v = __try_v;\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                struct ex_Option_i32 __try_ret;\n                __try_ret.tag = ex_Option_i32_None;\n                return __try_ret;\n            }\n        }\n    }\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v + 1;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Option_i32 some;\n    struct ex_Option_i32 none;\n    some.tag = ex_Option_i32_Some;\n    some.data.Some._0 = 7;\n    none.tag = ex_Option_i32_None;\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_incr(some);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_incr(none);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "try inside main rejected (main returns an int exit code)"
    "fn main() {\n\
    \    let v = try Option::Some(5);\n\
    \    println(v);\n\
     }\n"
    "'try' on Option_i32 value but enclosing fn returns i32 — they must share the same Option/Result shape";

  check_error "try outside Option/Result-returning fn rejected"
    "fn helper() {\n\
    \    let v = try Option::Some(5);\n\
    \    println(v);\n\
     }\n\
     fn main() { helper(); }\n"
    "'try' is only allowed in fns that return Option or Result (this fn has no return type)";

  check_error "try shape mismatch (Option in Result-returning fn)"
    "fn f() -> Result<int, int> {\n\
    \    let v = try Option::Some(5);\n\
    \    return Result::Ok(v);\n\
     }\n\
     fn main() { match f() { Result::Ok(_) => println(1) | Result::Err(_) => println(0) } }\n"
    "'try' on Option_i32 value but enclosing fn returns Result_i32_i32 \
     — they must share the same Option/Result shape";

  check "extern fn: forward decl + raw call site name (no ex_ prefix)"
    "pub mod raw { extern fn my_add(a: int, b: int) -> int; }\n\
     fn main() {\n\
         println(raw::my_add(2, 3));\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern long my_add(long a, long b);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(my_add(2, 3)));\n    return 0;\n}\n";

  check "extern fn: void return (no -> T)"
    "pub mod raw { extern fn my_init(); }\n\
     fn main() {\n\
         raw::my_init();\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern void my_init(void);\n\nint main(void) {\n    my_init();\n    return 0;\n}\n";

  check_error "extern fn rejects body block"
    "pub mod raw { extern fn foo() { println(1); } }\n\
     fn main() {}\n\
     "
    "'extern fn foo' must end with ';', not a body — extern declares an \
     existing C symbol";

  check_error "extern fn rejects generic params"
    "pub mod raw { extern fn map<T>(x: T) -> T; }\n\
     fn main() {}\n\
     "
    "'extern fn map' cannot have generic parameters — C signatures must \
     be concrete";

  check_error "extern fn rejects 'pub'"
    "pub extern fn foo();\n\
     fn main() {}\n"
    "'pub' is redundant on 'extern' — extern items are always callable / \
     referenceable";

  check_no_cc "c_short / c_long / c_char / c_void: full int alias suite"
    "pub mod raw {\n\
         extern fn alloc(n: c_ulong) -> *c_void;\n\
         extern fn free_p(p: *c_void);\n\
         extern fn read_byte(p: *c_void) -> c_uchar;\n\
         extern fn small(x: c_short, y: c_ushort) -> c_long;\n\
     }\n\
     fn main() {\n\
         let buf: *c_void = raw::alloc(1024);\n\
         let _b: c_uchar = raw::read_byte(buf);\n\
         let _r: c_long = raw::small(-100, 200);\n\
         raw::free_p(buf);\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern void *alloc(unsigned long n);\nextern void free_p(void *p);\nextern unsigned char read_byte(void *p);\nextern long small(short x, unsigned short y);\n\nint main(void) {\n    void *buf;\n    unsigned char _b;\n    long _r;\n    buf = alloc(1024);\n    _b = read_byte(buf);\n    _r = small(-100, 200);\n    free_p(buf);\n    return 0;\n}\n";

  check_error "c_void cannot be used as a value type"
    "pub mod raw { extern fn weird() -> c_void; }\n\
     fn main() {}\n\
     "
    "'c_void' has no values — only `*c_void` is usable as a type";

  check "@c_include emits #include line in generated C"
    "@c_include(\"stdio.h\")\n\
     fn main() {}\n"
    "#include <stdio.h>\n#include \"stdio.h\"\n\nint main(void) {\n    return 0;\n}\n";

  check_no_cc "extern type T: alias visible as a name in fn signatures"
    "pub mod raw {\n\
         extern type LONG;\n\
         extern fn add(a: LONG, b: LONG) -> LONG;\n\
     }\n\
     fn main() {\n\
         let r: LONG = raw::add(40, 2);\n\
         println(r as int);\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern LONG add(LONG a, LONG b);\n\nint main(void) {\n    LONG r;\n    r = add(40, 2);\n    printf(\"%ld\\n\", (long)(((long)r)));\n    return 0;\n}\n";

  check_error "extern type rejects bare (non-pointer-or-by-value) misuse... actually allowed"
    "pub mod raw { extern type LONG; }\n\
     fn main() { println(LONG); }\n\
     "
    "undefined variable 'LONG'";

  check_no_cc "extern const NAME: T — value resolved by linker"
    "pub mod raw { extern const VERSION: c_int; }\n\
     fn main() { println(VERSION as int); }\n\
     "
    "#include <stdio.h>\n\nextern const int VERSION;\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(((long)VERSION)));\n    return 0;\n}\n";

  check_error "extern const requires explicit type annotation"
    "pub mod raw { extern const FOO; }\n\
     fn main() {}\n\
     "
    "'extern const FOO' must declare its type with `: T`, got ';'";

  check_no_cc "extern var: declared as mutable global, read+write via raw::"
    "pub mod raw {\n\
    \    extern struct Library;\n\
    \    extern var DOSBase: *Library;\n\
    \    extern fn lib_open() -> *Library;\n\
     }\n\
     fn main() {\n\
    \    raw::DOSBase = raw::lib_open();\n\
    \    if raw::DOSBase == null { println(0); } else { println(1); }\n\
     }\n"
    "#include <stdio.h>\n\nextern struct Library *DOSBase;\n\nextern struct Library *lib_open(void);\n\nint main(void) {\n    DOSBase = lib_open();\n    if (DOSBase == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(0));\n    } else {\n        printf(\"%ld\\n\", (long)(1));\n    }\n    return 0;\n}\n";

  check_error "extern var rejects assignment to extern const"
    "pub mod raw { extern const FOO: c_int; }\n\
     fn main() { raw::FOO = 5; }\n"
    "cannot assign to 'raw::FOO' — it's an `extern const` (use `extern var` for mutable globals)";

  check_error "extern var requires type annotation"
    "pub mod raw { extern var X; }\n\
     fn main() {}\n"
    "'extern var X' must declare its type with `: T`, got ';'";

  check_error "extern var must live in mod raw"
    "extern var DOSBase: c_int;\n\
     fn main() {}\n"
    "'extern var DOSBase' must live inside a `mod raw { ... }` block (FFI hygiene rule); wrap with `mod raw { ... }` and call as `raw::DOSBase` or import via `use raw::*;`";

  check_no_cc "@reg / @amiga_lib parse + validate, emitted prototype stays plain"
    (* The annotations are documentation; Bebbo's amiga.lib stubs do
       the register loading at the stub level (read args off the
       stack, load into registers, JSR through SysBase/DOSBase).
       Emitting `register T x __asm("X")` on the prototype would
       short-circuit the stack convention and break the stub call. *)
    "pub mod raw {\n\
    \    extern struct Library;\n\
    \    @amiga_lib(SysBase)\n\
    \    extern fn open_library as OpenLibrary(\n\
    \        @reg(a1) name: *c_char,\n\
    \        @reg(d0) version: c_uint\n\
    \    ) -> *Library;\n\
     }\n\
     fn main() { println(0); }\n"
    "#include <stdio.h>\n\nextern struct Library *OpenLibrary(char *name, unsigned int version);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(0));\n    return 0;\n}\n";

  check_error "@reg(...) rejected on non-extern fn"
    "fn bad(@reg(d0) x: int) -> int { return x; }\n\
     fn main() { println(0); }\n"
    "'@reg(...)' on parameter 'x' is only allowed on `extern fn`";

  check_error "@reg(...) rejects invalid m68k register name"
    "pub mod raw { extern fn foo(@reg(zzz) x: c_int); }\n\
     fn main() {}\n"
    "invalid m68k register 'zzz' in '@reg(zzz)' on parameter 'x' — expected one of d0..d7 or a0..a6";

  check_error "@amiga_lib rejected on non-extern fn"
    "@amiga_lib(SysBase)\n\
     fn bad() -> int { return 0; }\n\
     fn main() {}\n"
    "'@amiga_lib' can only decorate `extern fn` declarations";

  check "extern fn name mapping: `as <C-name>` decouples exile and C names"
    "pub mod raw { extern fn put_char as putchar(c: c_int) -> c_int; }\n\
     fn main() {\n\
         raw::put_char(72);\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern int putchar(int c);\n\nint main(void) {\n    putchar(72);\n    return 0;\n}\n";

  check "fn pointer as type: typedef + bare-name reference + indirect call"
    "fn add(a: c_int, b: c_int) -> c_int { return a + b; }\n\
     fn main() {\n\
    \    let f: fn(c_int, c_int) -> c_int = add;\n\
    \    println(f(40, 2) as int);\n\
     }\n"
    "#include <stdio.h>\n\ntypedef int (*fn2_cint_cint_to_cint)(int, int);\n\nstatic int ex_add(int a, int b);\n\nstatic int ex_add(int a, int b) {\n    return a + b;\n}\n\nint main(void) {\n    fn2_cint_cint_to_cint f;\n    f = ex_add;\n    printf(\"%ld\\n\", (long)(((long)f(40, 2))));\n    return 0;\n}\n";

  check_no_cc "fn pointer as extern fn parameter: signal-style callback"
    "pub mod raw { extern fn signal(sig: c_int, handler: fn(c_int)) -> fn(c_int); }\n\
     fn my_handler(s: c_int) { println(s as int); }\n\
     fn main() {\n\
         let _prev: fn(c_int) = raw::signal(2, my_handler);\n\
     }\n\
     "
    "#include <stdio.h>\n\ntypedef void (*fn1_cint_to_void)(int);\n\nextern fn1_cint_to_void signal(int sig, fn1_cint_to_void handler);\nstatic void ex_my_handler(int s);\n\nstatic void ex_my_handler(int s) {\n    printf(\"%ld\\n\", (long)(((long)s)));\n}\n\nint main(void) {\n    fn1_cint_to_void _prev;\n    _prev = signal(2, ex_my_handler);\n    return 0;\n}\n";

  check_error "fn pointer call with wrong arity rejected"
    "fn add(a: c_int, b: c_int) -> c_int { return a + b; }\n\
     fn main() {\n\
    \    let f: fn(c_int, c_int) -> c_int = add;\n\
    \    let _ = f(1);\n\
     }\n"
    "function pointer 'f' expects 2 argument(s), got 1";

  check "extern fn variadic: trailing `, ...` emits C-style varargs"
    "pub mod raw { extern fn printf(fmt: str, ...) -> c_int; }\n\
     fn main() {\n\
         raw::printf(\"x = %d\\n\", 42);\n\
         raw::printf(\"two: %d %d\\n\", 7, 13);\n\
         raw::printf(\"no args\\n\");\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern int printf(const char *fmt, ...);\n\nint main(void) {\n    printf(\"x = %d\\n\", 42);\n    printf(\"two: %d %d\\n\", 7, 13);\n    printf(\"no args\\n\");\n    return 0;\n}\n";

  check_error "variadic call: too few args (below fixed-param count) rejected"
    "pub mod raw { extern fn printf(fmt: str, ...) -> c_int; }\n\
     fn main() { raw::printf(); }\n\
     "
    "function 'raw::printf' expects at least 1 argument(s), got 0";

  check_error "variadic '...' as only param rejected"
    "pub mod raw { extern fn weird(...); }\n\
     fn main() {}\n\
     "
    "'extern fn weird' variadic '...' must come after at least one fixed \
     parameter (e.g. `(fmt: str, ...)`)";

  check "c_int / c_uint: extern fn signature emits raw `int` / `unsigned int`"
    "pub mod raw {\n\
         extern fn putchar(c: c_int) -> c_int;\n\
         extern fn rand_seed(s: c_uint);\n\
     }\n\
     fn main() {\n\
         raw::putchar(72);\n\
         raw::rand_seed(42);\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern int putchar(int c);\nextern void rand_seed(unsigned int s);\n\nint main(void) {\n    putchar(72);\n    rand_seed(42);\n    return 0;\n}\n";

  check_error "c_int does not implicitly convert to int"
    "pub mod raw { extern fn need_cint(x: c_int); }\n\
     fn main() {\n\
         let n: int = 5;\n\
         raw::need_cint(n);\n\
     }\n\
     "
    "argument 1 of 'raw::need_cint': expected c_int, got i32";

  check "extern struct: opaque, used through pointer in extern fn signatures"
    "pub mod raw {\n\
         extern struct Library;\n\
         extern fn lib_open() -> *Library;\n\
         extern fn lib_close(lib: *Library);\n\
     }\n\
     fn main() {\n\
         let lib: *Library = raw::lib_open();\n\
         raw::lib_close(lib);\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern struct Library *lib_open(void);\nextern void lib_close(struct Library *lib);\n\nint main(void) {\n    struct Library *lib;\n    lib = lib_open();\n    lib_close(lib);\n    return 0;\n}\n";

  check_error "extern struct: bare (non-pointer) use rejected"
    "pub mod raw { extern struct Library; }\n\
     fn take(_l: Library) {}\n\
     fn main() {}\n\
     "
    "opaque type 'Library' can only be used through a pointer (`*Library`) \
     — exile doesn't know its layout (use `extern struct Library { ... }` \
     to expose fields)";

  check_error "opaque extern struct as a struct field by value rejected"
    "pub mod raw { extern struct Win; }\n\
     struct S { w: raw::Win }\n\
     fn use_s(_x: S) -> int { return 1; }\n\
     fn main() { println(1); }\n"
    "opaque type 'Win' can only be used through a pointer (`*Win`) \
     — exile doesn't know its layout (use `extern struct Win { ... }` \
     to expose fields)";

  check_error "opaque extern struct as a generic type argument by value rejected"
    "pub mod raw { extern struct Win; }\n\
     struct Pair<A, B> { fst: A, snd: B }\n\
     fn f(p: Pair<raw::Win, int>) -> int { return p.snd; }\n\
     fn main() { println(1); }\n"
    "opaque type 'Win' can only be used through a pointer (`*Win`) \
     — exile doesn't know its layout (use `extern struct Win { ... }` \
     to expose fields)";

  check_no_cc "extern struct Foo { fields }: field read through pointer"
    "pub mod raw {\n\
    \    extern struct Library { lib_OpenCnt: c_uint, lib_Version: c_uint }\n\
    \    extern fn open_lib() -> *Library;\n\
     }\n\
     fn main() {\n\
    \    let lib: *Library = raw::open_lib();\n\
    \    if lib != null {\n\
    \        println(lib.lib_OpenCnt as int);\n\
    \        println(lib.lib_Version as int);\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nextern struct Library *open_lib(void);\n\nint main(void) {\n    struct Library *lib;\n    lib = open_lib();\n    if (lib != ((void *)0)) {\n        printf(\"%ld\\n\", (long)(((long)lib->lib_OpenCnt)));\n        printf(\"%ld\\n\", (long)(((long)lib->lib_Version)));\n    }\n    return 0;\n}\n";

  check_no_cc "extern struct: by-value type allowed when fields exposed"
    "pub mod raw {\n\
    \    extern struct Point { x: c_int, y: c_int }\n\
    \    extern fn make_pt() -> Point;\n\
     }\n\
     fn main() {\n\
    \    let p: Point = raw::make_pt();\n\
    \    println(p.x as int);\n\
     }\n"
    "#include <stdio.h>\n\nextern struct Point make_pt(void);\n\nint main(void) {\n    struct Point p;\n    p = make_pt();\n    printf(\"%ld\\n\", (long)(((long)p.x)));\n    return 0;\n}\n";

  check_no_cc "extern struct: field write through pointer"
    "pub mod raw {\n\
    \    extern struct Cfg { count: c_int }\n\
    \    extern fn get_cfg() -> *Cfg;\n\
     }\n\
     fn main() {\n\
    \    let c: *Cfg = raw::get_cfg();\n\
    \    c.count = 42 as c_int;\n\
     }\n"
    "#include <stdio.h>\n\nextern struct Cfg *get_cfg(void);\n\nint main(void) {\n    struct Cfg *c;\n    c = get_cfg();\n    c->count = ((int)42);\n    return 0;\n}\n";

  check_error "extern struct opaque rejects field access"
    "pub mod raw {\n\
    \    extern struct Library;\n\
    \    extern fn open_lib() -> *Library;\n\
     }\n\
     fn main() {\n\
    \    let lib: *Library = raw::open_lib();\n\
    \    println(lib.lib_Version as int);\n\
     }\n"
    "field access '.lib_Version' on opaque type 'Library' — declare fields with `extern struct Library { ... }` to access them";

  check_error "extern struct: unknown field rejected"
    "pub mod raw {\n\
    \    extern struct Lib { x: c_int }\n\
    \    extern fn open_lib() -> *Lib;\n\
     }\n\
     fn main() {\n\
    \    let l: *Lib = raw::open_lib();\n\
    \    println(l.y as int);\n\
     }\n"
    "extern struct 'Lib' has no field 'y'";

  check_error "extern struct empty body { } rejected"
    "pub mod raw { extern struct Library { } }\n\
     fn main() {}\n\
     "
    "'extern struct Library {}' is empty — use `extern struct Library;` for opaque types";

  check_error "extern struct rejects generic params"
    "pub mod raw { extern struct Pair<T>; }\n\
     fn main() {}\n\
     "
    "'extern struct Pair' cannot have generic parameters — extern types \
     live on the C side";

  check_error "extern struct outside `mod raw` rejected"
    "mod m {\n\
    \    extern struct Library;\n\
     }\n\
     fn main() {}\n"
    "'extern struct Library' must live inside a `mod raw { ... }` block \
     (FFI hygiene rule); wrap with `mod raw { ... }` and call as \
     `raw::Library` or import via `use raw::*;`";

  check_error "extern fn outside `mod raw` rejected (top-level)"
    "extern fn foo();\n\
     fn main() {}\n"
    "'extern fn foo' must live inside a `mod raw { ... }` block \
     (FFI hygiene rule); wrap with `mod raw { ... }` and call as \
     `raw::foo` or import via `use raw::*;`";

  check_error "extern fn inside non-raw module rejected"
    "mod m {\n\
    \    extern fn foo();\n\
     }\n\
     fn main() {}\n"
    "'extern fn foo' must live inside a `mod raw { ... }` block \
     (FFI hygiene rule); wrap with `mod raw { ... }` and call as \
     `raw::foo` or import via `use raw::*;`";

  check "prelude: Option<T> usable without explicit declaration"
    "fn main() {\n\
    \    let o = Option::Some(42);\n\
    \    match o {\n\
    \        Option::Some(x) => println(x)\n\
    \        | Option::None => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_Some;\n    o.data.Some._0 = 42;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "prelude: Result<T, E> usable without explicit declaration"
    "enum IoErr { NotFound }\n\
     fn make() -> Result<int, IoErr> { return Result::Ok(42); }\n\
     fn main() {\n\
    \    let r = make();\n\
    \    match r {\n\
    \        Result::Ok(v) => println(v)\n\
    \        | Result::Err(_) => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_IoErr_tag { ex_IoErr_NotFound };\nstruct ex_IoErr { enum ex_IoErr_tag tag; };\nenum ex_Result_i32_ex_IoErr_tag { ex_Result_i32_ex_IoErr_Ok, ex_Result_i32_ex_IoErr_Err };\nstruct ex_Result_i32_ex_IoErr { enum ex_Result_i32_ex_IoErr_tag tag; union { struct { long _0; } Ok; struct { struct ex_IoErr _0; } Err; } data; };\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void);\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void) {\n    {\n        struct ex_Result_i32_ex_IoErr __exile_ret;\n        __exile_ret.tag = ex_Result_i32_ex_IoErr_Ok;\n        __exile_ret.data.Ok._0 = 42;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Result_i32_ex_IoErr r;\n    r = ex_make();\n    {\n        struct ex_Result_i32_ex_IoErr __m;\n        __m = r;\n        switch (__m.tag) {\n        case ex_Result_i32_ex_IoErr_Ok:\n            {\n                long v = __m.data.Ok._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Result_i32_ex_IoErr_Err:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "prelude: user-declared Option<T> overrides built-in"
    "enum Option<T> { Empty | Full(T) }\n\
     fn main() {\n\
    \    let o = Option::Full(7);\n\
    \    match o {\n\
    \        Option::Full(x) => println(x)\n\
    \        | Option::Empty => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_Empty, ex_Option_i32_Full };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Full; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_Full;\n    o.data.Full._0 = 7;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Full:\n            {\n                long x = __m.data.Full._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_Empty:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "mod-local struct as fn param and self resolves to absolute path"
    "mod geom {\n\
    \    pub struct Point { x: int, y: int }\n\
    \    pub fn area(p: Point) -> int { return p.x * p.y; }\n\
    \    impl Point {\n\
    \        pub fn sum(self: Point) -> int { return self.x + self.y; }\n\
    \    }\n\
     }\n\
     fn main() {\n\
    \    let p = geom::Point { x: 3, y: 4 };\n\
    \    println(geom::area(p));\n\
    \    println(p.sum());\n\
     }\n"
    "#include <stdio.h>\n\nstruct geom__Point { long x; long y; };\n\nlong geom__area(struct geom__Point p);\nlong geom__Point__sum(struct geom__Point self);\n\nlong geom__area(struct geom__Point p) {\n    return p.x * p.y;\n}\n\nint main(void) {\n    struct geom__Point p;\n    p.x = 3;\n    p.y = 4;\n    printf(\"%ld\\n\", (long)(geom__area(p)));\n    printf(\"%ld\\n\", (long)(geom__Point__sum(p)));\n    return 0;\n}\n\nlong geom__Point__sum(struct geom__Point self) {\n    return self.x + self.y;\n}\n";

  check "generic free fn: T inferred from positional arg, one instance per T"
    "fn id<T>(x: T) -> T {\n\
    \    return x;\n\
     }\n\
     fn main() {\n\
    \    println(id(42));\n\
    \    println(id(7));\n\
     }\n"
    "#include <stdio.h>\n\nstatic long ex_id_i32(long x);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_id_i32(42)));\n    printf(\"%ld\\n\", (long)(ex_id_i32(7)));\n    return 0;\n}\n\nstatic long ex_id_i32(long x) {\n    return x;\n}\n";

  check "generic free fn: distinct T values produce distinct instances"
    "fn id<T>(x: T) -> T {\n\
    \    return x;\n\
     }\n\
     fn main() {\n\
    \    println(id(42));\n\
    \    println(id(true));\n\
     }\n"
    "#include <stdio.h>\n\nstatic long ex_id_i32(long x);\nstatic int ex_id_bool(int x);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_id_i32(42)));\n    printf(\"%d\\n\", ex_id_bool(1));\n    return 0;\n}\n\nstatic long ex_id_i32(long x) {\n    return x;\n}\n\nstatic int ex_id_bool(int x) {\n    return x;\n}\n";

  check "generic free fn: T inferred bidirectionally from let annotation"
    "fn make<T>() -> *T {\n\
    \    return null;\n\
     }\n\
     fn main() {\n\
    \    let p: *int = make();\n\
    \    if p == null { println(1); } else { println(0); }\n\
     }\n"
    "#include <stdio.h>\n\nstatic long *ex_make_i32(void);\n\nint main(void) {\n    long *p;\n    p = ex_make_i32();\n    if (p == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n\nstatic long *ex_make_i32(void) {\n    return ((void *)0);\n}\n";

  check_error "generic free fn: under-determined T errors with hint"
    "fn make<T>() -> *T {\n\
    \    return null;\n\
     }\n\
     fn main() {\n\
    \    let p = make();\n\
    \    if p == null { println(1); } else { println(0); }\n\
     }\n"
    "could not infer type parameter 'T' from arguments (add a type annotation on the surrounding let / return)";

  check "generic method: tparam on method of mono struct, two instances"
    "struct Box { value: int }\n\
     impl Box {\n\
    \    pub fn first<T>(self: Box, x: T) -> T {\n\
    \        return x;\n\
    \    }\n\
     }\n\
     fn main() {\n\
    \    let b = Box { value: 99 };\n\
    \    println(b.first(42));\n\
    \    println(b.first(true));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Box { long value; };\n\nlong Box__first_i32(struct ex_Box self, long x);\nint Box__first_bool(struct ex_Box self, int x);\n\nint main(void) {\n    struct ex_Box b;\n    b.value = 99;\n    printf(\"%ld\\n\", (long)(Box__first_i32(b, 42)));\n    printf(\"%d\\n\", Box__first_bool(b, 1));\n    return 0;\n}\n\nlong Box__first_i32(struct ex_Box self, long x) {\n    return x;\n}\n\nint Box__first_bool(struct ex_Box self, int x) {\n    return x;\n}\n";

  check "size_of yields a c_uint via C sizeof"
    "fn main() {\n\
    \    println(size_of(int) as int);\n\
    \    println(size_of(*int) as int);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(((long)sizeof(long))));\n    printf(\"%ld\\n\", (long)(((long)sizeof(long *))));\n    return 0;\n}\n";

  check "size_of substitutes T per generic instance"
    "fn sz<T>(_p: *T) -> c_uint { return size_of(T); }\n\
     fn main() {\n\
    \    let p: *int = null;\n\
    \    let q: *bool = null;\n\
    \    println(sz(p) as int);\n\
    \    println(sz(q) as int);\n\
     }\n"
    "#include <stdio.h>\n\nstatic unsigned int ex_sz_i32(long *_p);\nstatic unsigned int ex_sz_bool(int *_p);\n\nint main(void) {\n    long *p;\n    int *q;\n    p = ((void *)0);\n    q = ((void *)0);\n    printf(\"%ld\\n\", (long)(((long)ex_sz_i32(p))));\n    printf(\"%ld\\n\", (long)(((long)ex_sz_bool(q))));\n    return 0;\n}\n\nstatic unsigned int ex_sz_i32(long *_p) {\n    return sizeof(long);\n}\n\nstatic unsigned int ex_sz_bool(int *_p) {\n    return sizeof(int);\n}\n";

  check "pointer-to-pointer cast with `as` accepted"
    "fn main() {\n\
    \    let p: *c_void = null;\n\
    \    let q: *int = p as *int;\n\
    \    if q == null { println(1); } else { println(0); }\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    void *p;\n    long *q;\n    p = ((void *)0);\n    q = ((long *)p);\n    if (q == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check_error "non-pointer-non-int cast still rejected"
    "fn main() {\n\
    \    let b = true;\n\
    \    let _ = b as *int;\n\
     }\n"
    "cannot cast bool to *i32 (supported: int↔int, ptr↔ptr, int→ptr)";

  check "prelude Allocator: dropped from emitted C when unused"
    "fn main() { println(1); }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(1));\n    return 0;\n}\n";

  (* DR-004 size-on-free: `Allocator.free_fn` carries the byte-count
     back to the allocator.  `alloc.free(p)` lowers to
     `free_fn(state, p, size_of(T))` — the size is a compile-time
     constant per monomorphic instance, so libc backends ignore it
     while Amiga FreeMem / arena / kernel ward-region backends use it. *)
  check "Allocator::free threads size_of(T) through free_fn"
    "mod raw {\n\
    \    extern fn make_a() -> Allocator;\n\
     }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let p: *int = a.alloc();\n\
    \    a.free(p);\n\
    \    println(1);\n\
     }\n"
    "#include <stdio.h>\n\ntypedef void *(*fn2_ptr_cvoid_u32_to_ptr_cvoid)(void *, unsigned long);\ntypedef void (*fn3_ptr_cvoid_ptr_cvoid_u32_to_void)(void *, void *, unsigned long);\n\nstruct ex_Allocator { void *state; fn2_ptr_cvoid_u32_to_ptr_cvoid alloc_fn; fn3_ptr_cvoid_ptr_cvoid_u32_to_void free_fn; };\n\nextern struct ex_Allocator make_a(void);\nlong *Allocator__alloc_i32(struct ex_Allocator self);\nvoid Allocator__free_i32(struct ex_Allocator self, long *p);\n\nint main(void) {\n    struct ex_Allocator a;\n    long *p;\n    a = make_a();\n    p = Allocator__alloc_i32(a);\n    Allocator__free_i32(a, p);\n    printf(\"%ld\\n\", (long)(1));\n    return 0;\n}\n\nlong *Allocator__alloc_i32(struct ex_Allocator self) {\n    return ((long *)(self.alloc_fn)(self.state, ((unsigned long)sizeof(long))));\n}\n\nvoid Allocator__free_i32(struct ex_Allocator self, long *p) {\n    (self.free_fn)(self.state, ((void *)p), ((unsigned long)sizeof(long)));\n}\n";

  (* StringBuilder prelude type — same DCE guarantee as Allocator: a
     hello-world that never names `StringBuilder` must not carry its
     struct decl, methods, or the `Slice<u8>` instance `as_slice`
     produces. *)
  check "prelude StringBuilder: dropped from emitted C when unused"
    "fn main() { println(1); }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(1));\n    return 0;\n}\n";

  check_error "StringBuilder::with_capacity rejects non-Allocator first arg"
    "fn main() { let sb = StringBuilder::with_capacity(5, 16 as u32); println(1); }\n"
    "argument 1 of 'StringBuilder::with_capacity': expected Allocator, got i32";

  check_error "StringBuilder::push_byte rejects a non-u8 byte"
    "fn touch(sb: *StringBuilder) {\n\
    \    sb.push_byte(257 as int);\n\
     }\n\
     fn main() { println(1); }\n"
    "argument 1 of 'StringBuilder::push_byte': expected u8, got i32";

  check_error "StringBuilder::push_str rejects a non-str argument"
    "fn touch(sb: *StringBuilder) {\n\
    \    sb.push_str(42);\n\
     }\n\
     fn main() { println(1); }\n"
    "argument 1 of 'StringBuilder::push_str': expected str, got i32";

  check_error "StringBuilder::push_int rejects a str argument"
    "fn touch(sb: *StringBuilder) {\n\
    \    sb.push_int(\"42\");\n\
     }\n\
     fn main() { println(1); }\n"
    "argument 1 of 'StringBuilder::push_int': expected i32, got str";

  (* prelude String — Faza 1: same DCE story as SB.  Hello-world must
     not drag in the struct, its methods, or the `Slice<u8>` instance
     emerging from `as_slice`. *)
  check "prelude String: dropped from emitted C when unused"
    "fn main() { println(1); }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(1));\n    return 0;\n}\n";

  check_error "String::with_str rejects a non-str second argument"
    "fn main() { let s: String = String::with_str(make(), 42); println(1); }\n\
     fn make() -> Allocator { return make(); }\n"
    "argument 2 of 'String::with_str': expected str, got i32";

  check_error "String::empty rejects extra arguments"
    "fn main() {\n\
    \    let s: String = String::empty();\n\
    \    println(1);\n\
     }\n"
    "function 'String::empty' expects 1 argument(s), got 0";

  let contains hay needle =
    let hn = String.length needle in
    let hh = String.length hay in
    let rec go i =
      if i + hn > hh then false
      else if String.sub hay i hn = needle then true
      else go (i + 1)
    in
    go 0
  in
  (* String impls Eq/Hash/Clone by delegating to `str::*` content
     ops over `as_str()`.  `s1.eq(s2)` on equal-content Strings
     lowers to `str::eq(s1.as_str(), s2.as_str())`; the emission
     pulls `str__eq` in via the reachability DCE even though no
     user code calls `str::eq` directly. *)
  check_assert "`String::eq` delegates to `str::eq` over `as_str`"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make_a() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make_a();\n\
         \    let s1 = String::with_str(a, \"x\");\n\
         \    let s2 = String::with_str(a, \"x\");\n\
         \    if s1.eq(s2) { println(1); } else { println(0); }\n\
         \    s1.free(); s2.free();\n\
          }\n"
     in
     contains c "String__eq("
     && contains c "str__eq("
     && contains c "String__as_str");

  check_assert "`String::hash` delegates to `str::hash` and clone deep-copies"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make_a() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make_a();\n\
         \    let s = String::with_str(a, \"x\");\n\
         \    let h = s.hash();\n\
         \    let c = s.clone();\n\
         \    println(h as int);\n\
         \    s.free(); c.free();\n\
          }\n"
     in
     contains c "String__hash("
     && contains c "str__hash("
     && contains c "String__clone("
     && contains c "String__with_str(self->alloc");

  check "user `mod Allocator { fn ... }` not confused with prelude impl"
    "mod Allocator {\n\
    \    pub fn helper() -> int { return 1; }\n\
     }\n\
     fn main() {\n\
    \    println(Allocator::helper());\n\
     }\n"
    "#include <stdio.h>\n\nlong Allocator__helper(void);\n\nlong Allocator__helper(void) {\n    return 1;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(Allocator__helper()));\n    return 0;\n}\n";

  check "pub use: struct re-export — Inner visible without raw:: prefix"
    "pub mod raw { pub struct Inner { x: int } }\n\
     pub use raw::Inner;\n\
     fn make() -> Inner { return Inner { x: 42 }; }\n\
     fn main() { println(make().x); }\n"
    "#include <stdio.h>\n\nstruct raw__Inner { long x; };\n\nstatic struct raw__Inner ex_make(void);\n\nstatic struct raw__Inner ex_make(void) {\n    {\n        struct raw__Inner __exile_ret;\n        __exile_ret.x = 42;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_make().x));\n    return 0;\n}\n";

  check "pub use: fn re-export — helper visible without raw:: prefix"
    "pub mod raw { pub fn helper() -> int { return 7; } }\n\
     pub use raw::helper;\n\
     fn main() { println(helper()); }\n"
    "#include <stdio.h>\n\nlong raw__helper(void);\n\nlong raw__helper(void) {\n    return 7;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(raw__helper()));\n    return 0;\n}\n";

  check_error "pub use of unknown name rejected at decl site"
    "pub mod raw { pub fn helper() -> int { return 7; } }\n\
     pub use raw::nonexistent;\n\
     fn main() { println(nonexistent()); }\n"
    "'pub use raw::nonexistent' refers to unknown item — no fn, struct, or enum with that path is visible from this scope";

  check_multi "pub use foo::* re-exports a file module's public items"
    [ ("foo.exl", "pub fn ping() -> int { return 42; }\n");
      ("bar.exl", "pub use foo::*;\n");
      ("main.exl", "use bar;\nfn main() { println(bar::ping()); }\n") ]
    "main.exl"
    "#include <stdio.h>\n\n\
     long bar__ping(void);\n\n\
     long bar__ping(void) {\n\
    \    return 42;\n\
     }\n\n\
     int main(void) {\n\
    \    printf(\"%ld\\n\", (long)(bar__ping()));\n\
    \    return 0;\n\
     }\n";

  check "fn-ptr field call: recv.field(args) routes through TIndirectCall"
    "struct Op { f: fn(int) -> int }\n\
     fn id(x: int) -> int { return x; }\n\
     fn dbl(x: int) -> int { return x + x; }\n\
     fn main() {\n\
    \    let a = Op { f: id };\n\
    \    let b = Op { f: dbl };\n\
    \    println(a.f(7));\n\
    \    println(b.f(21));\n\
     }\n"
    "#include <stdio.h>\n\ntypedef long (*fn1_i32_to_i32)(long);\n\nstruct ex_Op { fn1_i32_to_i32 f; };\n\nstatic long ex_id(long x);\nstatic long ex_dbl(long x);\n\nstatic long ex_id(long x) {\n    return x;\n}\n\nstatic long ex_dbl(long x) {\n    return x + x;\n}\n\nint main(void) {\n    struct ex_Op a;\n    struct ex_Op b;\n    a.f = ex_id;\n    b.f = ex_dbl;\n    printf(\"%ld\\n\", (long)((a.f)(7)));\n    printf(\"%ld\\n\", (long)((b.f)(21)));\n    return 0;\n}\n";

  check "defer fires on `try` early-return path"
    "fn maybe(ok: bool) -> ?int {\n\
    \    if ok { return Option::Some(42); }\n\
    \    return Option::None;\n\
     }\n\
     fn run(ok: bool) -> ?int {\n\
    \    defer println(-1);\n\
    \    let v = try maybe(ok);\n\
    \    return Option::Some(v + 1);\n\
     }\n\
     fn main() {\n\
    \    match run(false) {\n\
    \        Option::Some(x) => println(x)\n\
    \        | Option::None    => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstatic struct ex_Option_i32 ex_maybe(int ok);\nstatic struct ex_Option_i32 ex_run(int ok);\n\nstatic struct ex_Option_i32 ex_maybe(int ok) {\n    if (ok) {\n        {\n            struct ex_Option_i32 __exile_ret;\n            __exile_ret.tag = ex_Option_i32_Some;\n            __exile_ret.data.Some._0 = 42;\n            return __exile_ret;\n        }\n    }\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_None;\n        return __exile_ret;\n    }\n}\n\nstatic struct ex_Option_i32 ex_run(int ok) {\n    long v;\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_maybe(ok);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long __try_v = __m.data.Some._0;\n                v = __try_v;\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                struct ex_Option_i32 __try_ret;\n                __try_ret.tag = ex_Option_i32_None;\n                printf(\"%ld\\n\", (long)(-1));\n                return __try_ret;\n            }\n        }\n    }\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v + 1;\n        printf(\"%ld\\n\", (long)(-1));\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_run(0);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_lint "lint: per-call-site origin pos (not prelude decl pos)"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { println(id(42)); }\n"
    ~profile:Exile_lang.Profile.Core
    ["generic fn 'id'"];

  check_assert "lint origin pos points to call site, not skeleton decl"
    (let ws = lint_warnings ~profile:Exile_lang.Profile.Core
       "fn id<T>(x: T) -> T { return x; }\n\
        fn main() { println(id(42)); }\n"
     in
     match ws with
     | [w] ->
         w.Exile_lang.Lint.pos.line = 2
         && w.Exile_lang.Lint.pos.file = "<input>"
     | _ -> false);

  check_assert "Profile.of_string round-trip"
    (Exile_lang.Profile.of_string "core" = Some Exile_lang.Profile.Core
     && Exile_lang.Profile.of_string "standard" = Some Exile_lang.Profile.Standard
     && Exile_lang.Profile.of_string "full" = Some Exile_lang.Profile.Full
     && Exile_lang.Profile.of_string "bogus" = None);

  check_assert "Profile.to_string canonical names"
    (Exile_lang.Profile.to_string Exile_lang.Profile.Core = "core"
     && Exile_lang.Profile.to_string Exile_lang.Profile.Standard = "standard"
     && Exile_lang.Profile.to_string Exile_lang.Profile.Full = "full");

  (* Bloat report must populate after a successful compile, with one
     entry per defined fn (extern fns excluded — they have no body). *)
  let _ = Exile_lang.Compiler.compile
    "fn helper() -> int { return 7; }\n\
     fn main() { println(helper()); }\n"
  in
  let bloat = Exile_lang.Codegen.last_bloat () in
  check_assert "bloat report: one entry per defined fn"
    (List.length bloat = 2);
  check_assert "bloat report: entries are mangled names"
    (List.exists (fun (n, _) -> n = "ex_helper") bloat
     && List.exists (fun (n, _) -> n = "main") bloat);
  check_assert "bloat report: byte counts are positive"
    (List.for_all (fun (_, b) -> b > 0) bloat);

  check_assert "Tier.of_string round-trip + ord"
    (Exile_lang.Tier.of_string "core" = Some Exile_lang.Tier.Core
     && Exile_lang.Tier.of_string "standard" = Some Exile_lang.Tier.Standard
     && Exile_lang.Tier.of_string "full" = Some Exile_lang.Tier.Full
     && Exile_lang.Tier.to_int Exile_lang.Tier.Core
        < Exile_lang.Tier.to_int Exile_lang.Tier.Standard
     && Exile_lang.Tier.to_int Exile_lang.Tier.Standard
        < Exile_lang.Tier.to_int Exile_lang.Tier.Full);

  check_lint "lint: generic fn under --profile=full is silent"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { println(id(42)); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: generic fn under --profile=core warns once per fn"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { println(id(42)); println(id(7)); }\n"
    ~profile:Exile_lang.Profile.Core
    ["generic fn 'id' is tier=full but compiled under --profile=core"];

  check_lint "lint: @tier(core) override silences warning under core"
    "@tier(core)\n\
     fn id<T>(x: T) -> T { return x; }\n\
     fn main() { println(id(42)); }\n"
    ~profile:Exile_lang.Profile.Core
    [];

  check_lint "lint: @tier(standard) warns under core but not standard"
    "@tier(standard)\n\
     fn id<T>(x: T) -> T { return x; }\n\
     fn main() { println(id(42)); }\n"
    ~profile:Exile_lang.Profile.Core
    ["generic fn 'id' is tier=standard"];

  check_lint "lint: mono fn never warns regardless of profile"
    "fn add(a: int, b: int) -> int { return a + b; }\n\
     fn main() { println(add(1, 2)); }\n"
    ~profile:Exile_lang.Profile.Core
    [];

  check_lint "lint: unused generic fn does not warn (no instances)"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { println(1); }\n"
    ~profile:Exile_lang.Profile.Core
    [];

  check_error "@tier rejects unknown tier name"
    "@tier(bogus)\n\
     fn foo() -> int { return 1; }\n\
     fn main() { println(foo()); }\n"
    "unknown tier 'bogus' (expected: core, standard, full)";

  check_error "@tier rejects placement on non-decoratable items"
    "@tier(core)\n\
     impl Allocator { }\n\
     fn main() { println(1); }\n"
    "'@tier' can only decorate fn / struct / enum decls";

  check_lint "lint: unused let warns once per binding"
    "fn main() {\n\
    \    let a: int = 1;\n\
    \    let b: int = 2;\n\
    \    println(a);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    ["unused variable 'b'"];

  check_lint "lint: '_' prefix silences unused-let warning"
    "fn main() {\n\
    \    let _scratch: int = 99;\n\
    \    println(1);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: fn-ptr local called via TCall counts as used"
    "fn add(a: c_int, b: c_int) -> c_int { return a + b; }\n\
     fn main() {\n\
    \    let f: fn(c_int, c_int) -> c_int = add;\n\
    \    println(f(1, 2) as int);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: prelude code doesn't emit unused-let warnings"
    "fn main() { println(1); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: unused parameter warns"
    "pub fn printt(s: str) { println(\"hi\"); }\n\
     fn main() { printt(\"lol\"); }\n"
    ~profile:Exile_lang.Profile.Full
    ["unused parameter 's'"];

  check_lint "lint: used parameter does not warn"
    "fn dbl(n: int) -> int { return n + n; }\n\
     fn main() { println(dbl(21)); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: '_' prefix silences unused-parameter warning"
    "fn cb(_event: int) { println(1); }\n\
     fn main() { cb(99); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: unused 'self' receiver is exempt"
    "struct P { x: int }\n\
     impl P {\n\
    \    pub fn kind(self: P) -> int { return 7; }\n\
     }\n\
     fn main() { let p = P { x: 1 }; println(p.kind()); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: unused mono fn warns at decl pos"
    "fn dead() -> int { return 1; }\n\
     fn main() { println(2); }\n"
    ~profile:Exile_lang.Profile.Full
    ["unused function 'dead'"];

  check_lint "lint: called mono fn does not warn"
    "fn helper() -> int { return 7; }\n\
     fn main() { println(helper()); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: pub fn never warns (may be called by importer)"
    "pub fn api() -> int { return 1; }\n\
     fn main() { println(2); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: self-recursive but otherwise unused fn still warns"
    "fn loop_self(n: int) -> int { return loop_self(n); }\n\
     fn main() { println(1); }\n"
    ~profile:Exile_lang.Profile.Full
    ["unused function 'loop_self'"];

  check_lint "lint: fn referenced via TFnRef (taken as fn-ptr) counts as used"
    "fn cb(s: c_int) { println(s as int); }\n\
     fn main() {\n\
    \    let _h: fn(c_int) = cb;\n\
    \    println(1);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: @must_use fn discarded in statement position warns"
    "@must_use\n\
     fn classify(n: int) -> int { return n; }\n\
     fn main() { classify(19); }\n"
    ~profile:Exile_lang.Profile.Full
    ["call result is `@must_use`"];

  check_lint "lint: @must_use fn captured in let does not warn"
    "@must_use\n\
     fn classify(n: int) -> int { return n; }\n\
     fn main() { let r: int = classify(19); println(r); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: `let _ = ...` silences must_use"
    "@must_use\n\
     fn classify(n: int) -> int { return n; }\n\
     fn main() { let _ignored: int = classify(19); println(1); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: plain fn discarded in statement position does not warn"
    "fn classify(n: int) -> int { return n; }\n\
     fn main() { classify(19); println(classify(20)); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: discarded Result<T,E> from prelude warns (type-level must_use)"
    "fn try_div(n: int, d: int) -> Result<int, str> {\n\
    \    if d == 0 { return Result::Err(\"div by zero\"); }\n\
    \    return Result::Ok(n / d);\n\
     }\n\
     fn main() { try_div(10, 0); }\n"
    ~profile:Exile_lang.Profile.Full
    ["unused 'Result<i32, str>' value"];

  check_error "@must_use rejects placement on non-decoratable items"
    "@must_use\n\
     impl Allocator { }\n\
     fn main() { println(1); }\n"
    "'@must_use' can only decorate fn / enum decls";

  check "@debug on a generic struct: printer synthesized per instance"
    "@debug\n\
     struct Box<T> { v: T }\n\
     fn main() { println(Box { v: 5 }); }\n"
    "#include <stdio.h>\n\n\
     struct ex_Box_i32 { long v; };\n\n\
     static void ex_Box_i32__debug(struct ex_Box_i32 self);\n\n\
     static void ex_Box_i32__debug(struct ex_Box_i32 self) {\n\
    \    printf(\"Box<i32> { \");\n\
    \    printf(\"v: \");\n\
    \    printf(\"%ld\", (long)(self.v));\n\
    \    printf(\" }\");\n\
     }\n\n\n\
     int main(void) {\n\
    \    struct ex_Box_i32 __lift_0;\n\
    \    __lift_0.v = 5;\n\
    \    ex_Box_i32__debug(__lift_0); printf(\"\\n\");\n\
    \    return 0;\n\
     }\n";

  check_error "@debug generic struct: non-debug-able field instance rejected"
    "@debug\n\
     struct W<T> { f: T }\n\
     fn main() { let w = W { f: (1, 2) }; println(w); }\n"
    "'@debug' struct 'W<(i32, i32)>': field 'f' of type (i32, i32) is not \
     debug-able (mark the type `@debug`, or remove `@debug` from the struct)";

  check_error "@debug rejects field of non-debug-able type"
    "@debug\n\
     struct Bad { cb: fn(int) -> int }\n\
     fn main() { println(1); }\n"
    "'@debug' struct 'Bad': field 'cb' of type fn(i32) -> i32 is not debug-able (mark the type `@debug`, or remove `@debug` from the struct)";

  check_error "@debug rejects field whose type is not @debug"
    "struct Plain { x: int }\n\
     @debug\n\
     struct Outer { inner: Plain }\n\
     fn main() { println(1); }\n"
    "'@debug' struct 'Outer': field 'inner' of type Plain is not debug-able (mark the type `@debug`, or remove `@debug` from the struct)";

  check_error "non-@debug struct still rejects print"
    "struct Plain { x: int }\n\
     fn main() { let p = Plain { x: 1 }; println(p); }\n"
    "cannot print a struct value (Plain); print individual fields, or mark the struct with `@debug`";

  check "type_name(expr): scalar types rendered at compile time"
    "fn main() {\n\
    \    let a: i8 = 5;\n\
    \    let b: u16 = 100;\n\
    \    println(type_name(a));\n\
    \    println(type_name(b));\n\
    \    println(type_name(\"hi\"));\n\
    \    println(type_name(42));\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    signed char a;\n    unsigned short b;\n    a = 5;\n    b = 100;\n    printf(\"%s\\n\", ((void)sizeof(a), \"i8\"));\n    printf(\"%s\\n\", ((void)sizeof(b), \"u16\"));\n    printf(\"%s\\n\", ((void)sizeof(\"hi\"), \"str\"));\n    printf(\"%s\\n\", ((void)sizeof(42), \"i32\"));\n    return 0;\n}\n";

  check_error "type_name rejects null literal"
    "fn main() { println(type_name(null)); }\n"
    "type_name() needs a typed expression — 'null' has no statically-known target type";

  check_error "type_name rejects wrong arity"
    "fn main() { println(type_name(1, 2)); }\n"
    "type_name() takes exactly one argument, got 2";

  (* `cstr_len(s)` — narrow strlen seam (DR-001).  Lowers to
     `((unsigned long)strlen(...))` and pulls `<string.h>` into the
     output.  Width-pinned to `u32` at the call so the seam never
     surfaces C's `size_t`. *)
  check "cstr_len(s) lowers to strlen and pulls <string.h>"
    "fn main() {\n\
    \    let s = \"hi\";\n\
    \    let n: u32 = cstr_len(s);\n\
    \    println(n as int);\n\
     }\n"
    "#include <stdio.h>\n#include <string.h>\n\nint main(void) {\n    const char *s;\n    unsigned long n;\n    s = \"hi\";\n    n = ((unsigned long)strlen(s));\n    printf(\"%ld\\n\", (long)(((long)n)));\n    return 0;\n}\n";

  check_error "cstr_len rejects non-str argument"
    "fn main() { let n = cstr_len(42); println(n as int); }\n"
    "'cstr_len' expects a `str`, got i32";

  check_error "cstr_len rejects wrong arity"
    "fn main() { let n = cstr_len(\"a\", \"b\"); println(n as int); }\n"
    "cstr_len() takes exactly one argument, got 2";

  (* `str == str` and `str != str` lower to `str::eq` content compare
     instead of C pointer compare (the long-standing footgun: even
     identical literals could land in different `.rodata` slots and
     pointer compare would lie).  Per str ops design 2026-05-31. *)
  let contains hay needle =
    let hn = String.length needle in
    let hh = String.length hay in
    let rec go i =
      if i + hn > hh then false
      else if String.sub hay i hn = needle then true
      else go (i + 1)
    in
    go 0
  in
  check_assert "`str == str` lowers to `str::eq` content compare"
    (let c =
       Exile_lang.Compiler.compile
         "fn same(a: str, b: str) -> bool { return a == b; }\n\
          fn main() { if same(\"foo\", \"foo\") { println(1); } else { println(0); } }\n"
     in
     contains c "str__eq(a, b)" && not (contains c "return a == b"));

  check_assert "`str != str` wraps `str::eq` in logical NOT"
    (let c =
       Exile_lang.Compiler.compile
         "fn diff(a: str, b: str) -> bool { return a != b; }\n\
          fn main() { if diff(\"x\", \"y\") { println(1); } else { println(0); } }\n"
     in
     contains c "!(str__eq(a, b))");

  check_assert "`str::hash(s)` emits a call to the synthesized fold"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() { println(str::hash(\"a\") as int); }\n"
     in
     contains c "str__hash(\"a\")"
     && contains c "acc * 31 + ((unsigned long)bytes.ptr[i])");

  check "prelude `mod str` dropped from hello-world emission"
    "fn main() { println(1); }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(1));\n    return 0;\n}\n";

  check "type_name() folds into compile-time '++' concat"
    "fn main() {\n\
    \    println(type_name(42) ++ \" literal\");\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"i32 literal\");\n    return 0;\n}\n";

  check "type_name() in '++' folds per generic instance"
    "fn label<T>(x: T) -> str { return type_name(x) ++ \"!\"; }\n\
     fn main() {\n\
    \    println(label(42));\n\
    \    println(label(\"hi\"));\n\
     }\n"
    "#include <stdio.h>\n\n\
     static const char *ex_label_i32(long x);\n\
     static const char *ex_label_str(const char *x);\n\n\
     int main(void) {\n\
    \    printf(\"%s\\n\", ex_label_i32(42));\n\
    \    printf(\"%s\\n\", ex_label_str(\"hi\"));\n\
    \    return 0;\n\
     }\n\n\
     static const char *ex_label_i32(long x) {\n\
    \    return \"i32!\";\n\
     }\n\n\
     static const char *ex_label_str(const char *x) {\n\
    \    return \"str!\";\n\
     }\n";

  check_error "free(&local) rejected at typecheck (would corrupt allocator)"
    "fn main() {\n\
    \    let x = 20;\n\
    \    defer free(&x);\n\
    \    println(x);\n\
     }\n"
    "'free' expects a heap-allocated pointer (from 'new'); got '&...' which is a stack or field address — this would corrupt the allocator";

  check_error "missing return on if-without-else path rejected"
    "fn early_exit(n: int) -> int {\n\
    \    if n > 0 { return n * 2; }\n\
     }\n\
     fn main() { println(early_exit(5)); }\n"
    "function 'early_exit' declared with return type i32, but not every control-flow path ends in `return`";

  check_error "missing return when no return at end of body"
    "fn f(n: int) -> int {\n\
    \    let x: int = n;\n\
     }\n\
     fn main() { println(f(5)); }\n"
    "function 'f' declared with return type i32, but not every control-flow path ends in `return`";

  check_error "if-else where only one branch returns rejected"
    "fn f(n: int) -> int {\n\
    \    if n > 0 { return 1; } else { let x: int = 0; }\n\
     }\n\
     fn main() { println(f(5)); }\n"
    "function 'f' declared with return type i32, but not every control-flow path ends in `return`";

  check "if-else where both branches return: accepted"
    "fn f(n: int) -> int {\n\
    \    if n > 0 { return 1; } else { return 0; }\n\
     }\n\
     fn main() { println(f(5)); }\n"
    "#include <stdio.h>\n\nstatic long ex_f(long n);\n\nstatic long ex_f(long n) {\n    if (n > 0) {\n        return 1;\n    } else {\n        return 0;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_f(5)));\n    return 0;\n}\n";

  check_error "return non-int from main rejected"
    "fn main() {\n\
    \    println(1);\n\
    \    return \"lol\";\n\
     }\n"
    "return: expected i32, got str";

  check "return int from main: accepted as exit code"
    "fn main() {\n\
    \    println(1);\n\
    \    return 5;\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(1));\n    return 5;\n    return 0;\n}\n";

  check_error "return value from non-main void fn rejected"
    "fn helper() {\n\
    \    println(1);\n\
    \    return 5;\n\
     }\n\
     fn main() { helper(); }\n"
    "cannot return a value from a function with no return type (declare `-> i32` if the value is intended)";

  check_error "free(&field) rejected (same rule applies to field address)"
    "struct P { x: int }\n\
     fn main() {\n\
    \    let p = P { x: 1 };\n\
    \    free(&p.x);\n\
     }\n"
    "'free' expects a heap-allocated pointer (from 'new'); got '&...' which is a stack or field address — this would corrupt the allocator";

  check "string ++ literal concat: folded to single rodata"
    "fn main() {\n\
    \    let g: str = \"Hello, \" ++ \"World\" ++ \"!\";\n\
    \    println(g);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    const char *g;\n    g = \"Hello, World!\";\n    printf(\"%s\\n\", g);\n    return 0;\n}\n";

  check_error "string ++ rejects runtime str on left"
    "fn main() {\n\
    \    let name: str = \"Bob\";\n\
    \    let g: str = name ++ \"!\";\n\
    \    println(g);\n\
     }\n"
    "'++' requires a compile-time string literal on both sides; got str on the left (for runtime concat use an Allocator method)";

  check_error "string ++ rejects runtime str on right"
    "fn main() {\n\
    \    let name: str = \"Bob\";\n\
    \    let g: str = \"Hi \" ++ name;\n\
    \    println(g);\n\
     }\n"
    "'++' requires a compile-time string literal on both sides; got str on the right (for runtime concat use an Allocator method)";

  check_error "++ between non-string operands rejected"
    "fn main() { let x = 1 ++ 2; println(x); }\n"
    "'++' requires a compile-time string literal on both sides; got i32 on the left (for runtime concat use an Allocator method)";

  check "print of @debug struct: emits __debug call"
    "@debug\n\
     struct Point { x: int, y: int }\n\
     fn main() {\n\
    \    let p = Point { x: 3, y: 4 };\n\
    \    println(p);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic void ex_Point__debug(struct ex_Point self);\n\nstatic void ex_Point__debug(struct ex_Point self) {\n    printf(\"Point { \");\n    printf(\"x: \");\n    printf(\"%ld\", (long)(self.x));\n    printf(\", \");\n    printf(\"y: \");\n    printf(\"%ld\", (long)(self.y));\n    printf(\" }\");\n}\n\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 3;\n    p.y = 4;\n    ex_Point__debug(p); printf(\"\\n\");\n    return 0;\n}\n";

  check "hex literal: 0xDEAD parses and emits as decimal"
    "fn main() {\n\
    \    let x: u32 = 0xDEAD;\n\
    \    println(x as int);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    unsigned long x;\n    x = 57005;\n    printf(\"%ld\\n\", (long)(((long)x)));\n    return 0;\n}\n";

  check "hex literal: case-insensitive prefix and digits"
    "fn main() {\n\
    \    let a: u32 = 0xff;\n\
    \    let b: u32 = 0XFF;\n\
    \    let c: u32 = 0xAbCd;\n\
    \    println(a as int);\n\
    \    println(b as int);\n\
    \    println(c as int);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    unsigned long a;\n    unsigned long b;\n    unsigned long c;\n    a = 255;\n    b = 255;\n    c = 43981;\n    printf(\"%ld\\n\", (long)(((long)a)));\n    printf(\"%ld\\n\", (long)(((long)b)));\n    printf(\"%ld\\n\", (long)(((long)c)));\n    return 0;\n}\n";

  check_error "hex literal: 0x with no digits rejected"
    "fn main() { let x = 0x; print(x); }\n"
    "hex literal '0x' has no digits";

  check "pipe: x |> f(a) desugars to f(x, a)"
    "fn add(a: int, b: int) -> int { a + b }\n\
     fn main() {\n\
    \    let r = 3 |> add(4);\n\
    \    println(r);\n\
     }\n"
    "#include <stdio.h>\n\nstatic long ex_add(long a, long b);\n\nstatic long ex_add(long a, long b) {\n    return a + b;\n}\n\nint main(void) {\n    long r;\n    r = ex_add(3, 4);\n    printf(\"%ld\\n\", (long)(r));\n    return 0;\n}\n";

  check "pipe: bare ident `x |> f` ≡ `f(x)`"
    "fn dbl(x: int) -> int { x + x }\n\
     fn main() { println(5 |> dbl); }\n"
    "#include <stdio.h>\n\nstatic long ex_dbl(long x);\n\nstatic long ex_dbl(long x) {\n    return x + x;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_dbl(5)));\n    return 0;\n}\n";

  check "pipe: left-assoc chaining `x |> f() |> g()`"
    "fn dbl(x: int) -> int { x + x }\n\
     fn inc(x: int) -> int { x + 1 }\n\
     fn main() { println(5 |> dbl() |> inc()); }\n"
    "#include <stdio.h>\n\nstatic long ex_dbl(long x);\nstatic long ex_inc(long x);\n\nstatic long ex_dbl(long x) {\n    return x + x;\n}\n\nstatic long ex_inc(long x) {\n    return x + 1;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_inc(ex_dbl(5))));\n    return 0;\n}\n";

  check_error "pipe: missing ident after `|>` rejected"
    "fn main() { let x = 1 |> ; }\n"
    "expected function name after '|>', got ';'";

  check "cast: int literal to *T accepted (MMIO-style)"
    "struct Custom { vposr: u32 }\n\
     fn main() {\n\
    \    let cust = 0xDFF000 as *Custom;\n\
    \    let is_null: bool = cust == null;\n\
    \    if is_null { println(1); } else { println(0); }\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Custom { unsigned long vposr; };\n\nint main(void) {\n    struct ex_Custom *cust;\n    int is_null;\n    cust = ((struct ex_Custom *)14675968);\n    is_null = cust == ((void *)0);\n    if (is_null) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check_error "cast: *T to int still rejected"
    "fn main() {\n\
    \    let p: *i32 = null;\n\
    \    let n = p as int;\n\
    \    println(n);\n\
     }\n"
    "cannot cast *i32 to i32 (supported: int↔int, ptr↔ptr, int→ptr)";

  check "or-pattern: A | B in match arm head emits stacked case labels"
    "enum Color { Red | Green | Blue }\n\
     fn main() {\n\
    \    let c = Color::Green;\n\
    \    match c {\n\
    \        Color::Red | Color::Green => println(1)\n\
    \        | Color::Blue => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Color_tag { ex_Color_Red, ex_Color_Green, ex_Color_Blue };\nstruct ex_Color { enum ex_Color_tag tag; };\n\nint main(void) {\n    struct ex_Color c;\n    c.tag = ex_Color_Green;\n    {\n        struct ex_Color __m;\n        __m = c;\n        switch (__m.tag) {\n        case ex_Color_Red:\n        case ex_Color_Green:\n            {\n                printf(\"%ld\\n\", (long)(1));\n                break;\n            }\n        case ex_Color_Blue:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "or-pattern: still must cover every variant"
    "enum E { A | B | C }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    match e {\n\
    \        E::A | E::B => println(1)\n\
    \    }\n\
     }\n"
    "non-exhaustive 'match': pattern 'C' is not covered (add an arm or '_')";

  check_error "or-pattern: alternatives may not bind variables"
    "enum E { Some(int) | None }\n\
     fn main() {\n\
    \    let e = E::Some(7);\n\
    \    match e {\n\
    \        E::Some(x) | E::None => println(1)\n\
    \    }\n\
     }\n"
    "or-pattern alternatives must bind zero variables (got bind 'x'); use separate arms if you need to bind";

  check "guard: pat if cond => emits goto-based decision chain"
    "enum Maybe { Some(int) | None }\n\
     fn main() {\n\
    \    let m = Maybe::Some(5);\n\
    \    let r = match m {\n\
    \        Maybe::Some(n) if n > 0 => 1\n\
    \        | Maybe::Some(_) => 2\n\
    \        | Maybe::None => 0\n\
    \    };\n\
    \    println(r);\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Maybe_tag { ex_Maybe_Some, ex_Maybe_None };\nstruct ex_Maybe { enum ex_Maybe_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Maybe m;\n    long r;\n    m.tag = ex_Maybe_Some;\n    m.data.Some._0 = 5;\n    {\n        struct ex_Maybe __m;\n        __m = m;\n        if (__m.tag == ex_Maybe_Some) {\n            long n = __m.data.Some._0;\n            if (n > 0) {\n                r = 1;\n                goto __mdone0;\n            }\n        }\n        if (__m.tag == ex_Maybe_Some) {\n            r = 2;\n            goto __mdone0;\n        }\n        if (__m.tag == ex_Maybe_None) {\n            r = 0;\n            goto __mdone0;\n        }\n        __mdone0: ;\n    }\n    printf(\"%ld\\n\", (long)(r));\n    return 0;\n}\n";

  check_error "guard: must be bool"
    "enum E { A(int) | B }\n\
     fn main() {\n\
    \    let e = E::A(1);\n\
    \    match e {\n\
    \        E::A(n) if n => println(1)\n\
    \        | E::B => println(0)\n\
    \    }\n\
     }\n"
    "match-arm guard `if ...` must be of type bool, got i32";

  check "doc-comments: `///` lines are silently accepted (lexer skip)"
    "/// Adds two integers.\n\
     /// First argument is the left operand.\n\
     fn add(a: int, b: int) -> int { a + b }\n\
     fn main() { println(add(2, 3)); }\n"
    "#include <stdio.h>\n\nstatic long ex_add(long a, long b);\n\nstatic long ex_add(long a, long b) {\n    return a + b;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_add(2, 3)));\n    return 0;\n}\n";

  check "doc-comments: `@doc(\"...\")` attribute accepted (no-op)"
    "@doc(\"Adds two integers.\")\n\
     fn add(a: int, b: int) -> int { a + b }\n\
     fn main() { println(add(2, 3)); }\n"
    "#include <stdio.h>\n\nstatic long ex_add(long a, long b);\n\nstatic long ex_add(long a, long b) {\n    return a + b;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_add(2, 3)));\n    return 0;\n}\n";

  check "multi-stmt arm body: value position with let + trailing expr"
    "enum E { A(int) | B }\n\
     fn main() {\n\
    \    let e = E::A(7);\n\
    \    let r = match e {\n\
    \        E::A(n) => {\n\
    \            let doubled = n + n;\n\
    \            doubled + 1\n\
    \        }\n\
    \        | E::B => 0\n\
    \    };\n\
    \    println(r);\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } A; } data; };\n\nint main(void) {\n    struct ex_E e;\n    long doubled;\n    long r;\n    e.tag = ex_E_A;\n    e.data.A._0 = 7;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                long n = __m.data.A._0;\n                doubled = n + n;\n                r = doubled + 1;\n                break;\n            }\n        case ex_E_B:\n            {\n                r = 0;\n                break;\n            }\n        }\n    }\n    printf(\"%ld\\n\", (long)(r));\n    return 0;\n}\n";

  check "multi-stmt arm body: void position (no trailing expr)"
    "enum E { A(int) | B }\n\
     fn main() {\n\
    \    let e = E::A(7);\n\
    \    match e {\n\
    \        E::A(n) => {\n\
    \            println(n);\n\
    \            println(n + 1);\n\
    \        }\n\
    \        | E::B => println(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } A; } data; };\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_A;\n    e.data.A._0 = 7;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                long n = __m.data.A._0;\n                printf(\"%ld\\n\", (long)(n));\n                printf(\"%ld\\n\", (long)(n + 1));\n                break;\n            }\n        case ex_E_B:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "multi-stmt arm body: value position requires trailing expr"
    "enum E { A | B }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    let r = match e {\n\
    \        E::A => { let x = 1; }\n\
    \        | E::B => 2\n\
    \    };\n\
    \    println(r);\n\
     }\n"
    "block expression `{ ... }` must end with a trailing value expression (no `;` after the last expression) when used in a value position";

  check_error "multi-stmt arm body: arm-local `let` shares fn decl namespace"
    "enum E { A | B }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    match e {\n\
    \        E::A => { let x = 1; println(x); }\n\
    \        | E::B => { let x = 2; println(x); }\n\
    \    }\n\
     }\n"
    "variable 'x' already declared in this function";

  check "const-ptr: read through *const T emits `const T *`"
    "fn main() {\n\
    \    let x: int = 42;\n\
    \    let p: *const int = &x;\n\
    \    println(*p);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    const long *p;\n    x = 42;\n    p = &x;\n    printf(\"%ld\\n\", (long)(*p));\n    return 0;\n}\n";

  check "const-ptr: *T coerces to *const T at fn-arg site"
    "fn read_int(p: *const int) -> int { *p }\n\
     fn main() {\n\
    \    let x: int = 42;\n\
    \    let q: *int = &x;\n\
    \    println(read_int(q));\n\
     }\n"
    "#include <stdio.h>\n\nstatic long ex_read_int(const long *p);\n\nstatic long ex_read_int(const long *p) {\n    return *p;\n}\n\nint main(void) {\n    long x;\n    long *q;\n    x = 42;\n    q = &x;\n    printf(\"%ld\\n\", (long)(ex_read_int(q)));\n    return 0;\n}\n";

  check_error "const-ptr: writing through *const rejected"
    "fn main() {\n\
    \    let x: int = 1;\n\
    \    let p: *const int = &x;\n\
    \    *p = 99;\n\
     }\n"
    "cannot assign through '*const' pointer *const i32 (pointee is read-only)";

  check_error "const-ptr: writing field through *const rejected"
    "struct Point { x: int, y: int }\n\
     fn main() {\n\
    \    let p = Point { x: 1, y: 2 };\n\
    \    let pp: *const Point = &p;\n\
    \    pp.x = 99;\n\
     }\n"
    "cannot assign field 'x' through '*const' pointer *const Point (pointee is read-only)";

  check_error "const-ptr: implicit *const T -> *T rejected"
    "fn main() {\n\
    \    let x: int = 1;\n\
    \    let p: *const int = &x;\n\
    \    let q: *int = p;\n\
    \    println(*q);\n\
     }\n"
    "variable 'q' declared as *i32 but initializer has type *const i32";

  (* Regression for a build_struct_index ordering crash: a top-level
     `struct H { s: Slice<int> }` instantiates `Slice<int>` while the
     prelude `Slice` skeleton's fields haven't been resolved yet (the
     pass is names-first, fields-later).  Mono cached an empty
     `Slice_i32` and every subsequent field-access on it crashed with
     `Not_found` or hit "field 'len' missing".  Post-pass refresh
     reseeds empty mono instances from the now-resolved skeleton, and
     the orphan-drop closure walks top-level structs too so the
     instance survives DCE even when only a struct decl references
     it. *)
  check "Slice<int> as a struct field — instance fields refreshed post-resolve"
    "struct H { s: Slice<int> }\n\
     fn touch(h: *H) -> u32 { return h.s.len; }\n\
     fn main() {\n\
    \    let arr: [int; 2] = [10, 20];\n\
    \    let v: Slice<int> = Slice { ptr: &arr[0], len: 2 as u32 };\n\
    \    let h: H = H { s: v };\n\
    \    println(touch(&h) as int);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Slice_i32 { const long *ptr; unsigned long len; };\nstruct ex_H { struct ex_Slice_i32 s; };\nstruct ex_arr2_i32 { long data[2]; };\n\nstatic unsigned long ex_touch(struct ex_H *h);\n\nstatic unsigned long ex_touch(struct ex_H *h) {\n    return h->s.len;\n}\n\nint main(void) {\n    struct ex_arr2_i32 arr;\n    struct ex_Slice_i32 v;\n    struct ex_H h;\n    arr.data[0] = 10;\n    arr.data[1] = 20;\n    v.ptr = &(arr.data[0]);\n    v.len = ((unsigned long)2);\n    h.s = v;\n    printf(\"%ld\\n\", (long)(((long)ex_touch(&h))));\n    return 0;\n}\n";

  check "slice: indexing s[i] lowers to s.ptr[i]"
    "fn main() {\n\
    \    let arr: [int; 4] = [10, 20, 30, 40];\n\
    \    let s: Slice<int> = Slice { ptr: &arr[0], len: 4 };\n\
    \    println(s[0]);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Slice_i32 { const long *ptr; unsigned long len; };\nstruct ex_arr4_i32 { long data[4]; };\n\nint main(void) {\n    struct ex_arr4_i32 arr;\n    struct ex_Slice_i32 s;\n    arr.data[0] = 10;\n    arr.data[1] = 20;\n    arr.data[2] = 30;\n    arr.data[3] = 40;\n    s.ptr = &(arr.data[0]);\n    s.len = 4;\n    printf(\"%ld\\n\", (long)(s.ptr[0]));\n    return 0;\n}\n";

  check "slice: .len + iter via while loop"
    "fn sum(s: Slice<int>) -> int {\n\
    \    let mut total: int = 0;\n\
    \    let mut i: u32 = 0;\n\
    \    while i < s.len {\n\
    \        total = total + s[i];\n\
    \        i = i + 1;\n\
    \    }\n\
    \    total\n\
     }\n\
     fn main() {\n\
    \    let arr: [int; 4] = [10, 20, 30, 40];\n\
    \    let s: Slice<int> = Slice { ptr: &arr[0], len: 4 };\n\
    \    println(sum(s));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Slice_i32 { const long *ptr; unsigned long len; };\nstruct ex_arr4_i32 { long data[4]; };\n\nstatic long ex_sum(struct ex_Slice_i32 s);\n\nstatic long ex_sum(struct ex_Slice_i32 s) {\n    long total;\n    unsigned long i;\n    total = 0;\n    i = 0;\n    while (i < s.len) {\n        total = total + s.ptr[i];\n        i = i + 1;\n    }\n    return total;\n}\n\nint main(void) {\n    struct ex_arr4_i32 arr;\n    struct ex_Slice_i32 s;\n    arr.data[0] = 10;\n    arr.data[1] = 20;\n    arr.data[2] = 30;\n    arr.data[3] = 40;\n    s.ptr = &(arr.data[0]);\n    s.len = 4;\n    printf(\"%ld\\n\", (long)(ex_sum(s)));\n    return 0;\n}\n";

  check_error "doc-comments: `@doc(non-string)` rejected"
    "@doc(123)\n\
     fn add(a: int, b: int) -> int { a + b }\n\
     fn main() { println(add(2, 3)); }\n"
    "expected string literal in '@doc(\"...\")', got integer 123";

  check_error "guard: does not count toward exhaustiveness"
    "enum E { A | B }\n\
     fn main() {\n\
    \    let x = 0;\n\
    \    let e = E::A;\n\
    \    match e {\n\
    \        E::A if x == 0 => println(1)\n\
    \        | E::B => println(0)\n\
    \    }\n\
     }\n"
    "non-exhaustive 'match': pattern 'A' is not covered (add an arm or '_')";

  check_error "or-pattern: nested or inside a variant bind rejected"
    "enum Color { Red | Green | Blue }\n\
     enum Boxed { B(Color) }\n\
     fn main() {\n\
    \    let b = Boxed::B(Color::Red);\n\
    \    match b {\n\
    \        Boxed::B(Color::Red | Color::Green) => println(1)\n\
    \        | Boxed::B(Color::Blue) => println(0)\n\
    \    }\n\
     }\n"
    "or-pattern only allowed at the top of a match arm (nested `pat1 | pat2` inside a variant bind is not supported yet)"
