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

(* DR self-host bring-up Faza −1 — differential-harness dumps.  The
   three canonical forms (tokens / AST / typed IR) are golden-input
   for the future exile port: porting the lexer in exile means
   making its --emit-tokens output match this one byte-for-byte. *)
let dump_tokens src =
  src
  |> Exile_lang.Lexer.tokenize ~file:"<input>"
  |> Exile_lang.Dump.dump_tokens ~file:"<input>"

let dump_ast src =
  src
  |> Exile_lang.Lexer.tokenize ~file:"<input>"
  |> Exile_lang.Parser.parse_program
  |> Exile_lang.Dump.dump_ast ~file:"<input>"

let dump_typed_ir ?(user_only = true) src =
  src
  |> Exile_lang.Lexer.tokenize ~file:"<input>"
  |> Exile_lang.Parser.parse_program
  |> Exile_lang.Typecheck.check_program
  |> Exile_lang.Dump.dump_typed_ir ~file:"<input>" ~user_only

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

  (* `String::free` takes `self` by value (not `* self`), so the
     move-pass consumes the binding the moment the destructor fires.
     Pre-refactor the signature was `fn free( * self)` — `s.free()`
     auto-ref'd to `TPtr String` and `consume_var` skipped it, so
     `s.free(); s.free();` (double free) and `s.free(); s.length();`
     (use-after-free) compiled cleanly and segfaulted at runtime. *)
  check_error "double `s.free()` rejected — free consumes the binding"
    "mod raw { extern fn make_a() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let s = String::with_str(a, \"x\");\n\
    \    s.free();\n\
    \    s.free();\n\
     }\n"
    "use of 's' after it was consumed at <input>:5:5 (move-marked types are use-at-most-once — borrow with '&s' / take '*const String' or clone to keep the source live)";

  check_error "use-after-`free` rejected — read on consumed String"
    "mod raw { extern fn make_a() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_a();\n\
    \    let s = String::with_str(a, \"x\");\n\
    \    s.free();\n\
    \    println(s.length() as int);\n\
     }\n"
    "use of 's' after it was consumed at <input>:5:5 (move-marked types are use-at-most-once — borrow with '&s' / take '*const String' or clone to keep the source live)";

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

  (* DR-002 S0 — may-consume merge.  When ONE branch of a fork
     consumes an @move binding and the other leaves it Live, the
     post-merge state is Consumed (not Live).  Pre-fix `merge_states`
     used `Consumed,Consumed→Consumed | _→Live` — the inverse of the
     DR-002 contract — and let `defer s.free(); if c { sink(s); }`
     silently double-fire.  Four forms exercised: defer-after-if,
     match-arm conditional, if-expr value-position, if-stmt. *)
  check_error "S0: defer body rejects @move after conditional consume in if-stmt"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn drop(o: *const Owner) { println(o.tag); }\n\
     fn run(c: bool) {\n\
    \    let a = Owner { tag: 1 };\n\
    \    defer drop(&a);\n\
    \    if c { println(take(a)); }\n\
     }\n\
     fn main() { run(true); }\n"
    "use of 'a' after it was consumed at <input>:8:25 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S0: post-match use rejected when ONE arm consumes @move"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn run(o: Option<int>) {\n\
    \    let a = Owner { tag: 1 };\n\
    \    match o {\n\
    \        Option::Some(_) => { println(take(a)); }\n\
    \        | Option::None  => {}\n\
    \    }\n\
    \    println(take(a));\n\
     }\n\
     fn main() { run(Option::Some(1)); }\n"
    "use of 'a' after it was consumed at <input>:7:43 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S0: post-if-expr use rejected when then-value consumes @move"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn run(c: bool) {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let n = if c { take(a) } else { 0 };\n\
    \    println(n + take(a));\n\
     }\n\
     fn main() { run(true); }\n"
    "use of 'a' after it was consumed at <input>:6:25 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S0: post-if-stmt use rejected when then-body consumes @move"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn run(c: bool) {\n\
    \    let a = Owner { tag: 1 };\n\
    \    if c { println(take(a)); }\n\
    \    println(take(a));\n\
     }\n\
     fn main() { run(true); }\n"
    "use of 'a' after it was consumed at <input>:6:25 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  (* DR-002 S1 — aggregate literals consume @move fields/elements.
     Each shape (TStructLit / TTupleLit / TEnumLit / TArrayLit /
     TNew) shallow-copies the bare-TVar arg into the fresh value;
     pre-fix the source stayed Live and a subsequent reuse would
     silently double-fire at runtime.  TArrayRepeat with an @move
     element is banned outright — the fill-loop lowering N-aliases
     the same source. *)
  check_error "S1: struct literal consumes @move field, post-lit use rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     struct Wrap { f: Owner }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let w = Wrap { f: a };\n\
    \    println(take(a));\n\
     }\n"
    "use of 'a' after it was consumed at <input>:7:23 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S1: tuple literal consumes @move element, post-lit use rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let _t = (a, 1);\n\
    \    println(take(a));\n\
     }\n"
    "use of 'a' after it was consumed at <input>:6:15 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S1: enum literal payload consumes @move arg, post-lit use rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let _o = Option::Some(a);\n\
    \    println(take(a));\n\
     }\n"
    "use of 'a' after it was consumed at <input>:6:27 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S1: array literal consumes @move element, post-lit use rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let _arr: [Owner; 1] = [a];\n\
    \    println(take(a));\n\
     }\n"
    "use of 'a' after it was consumed at <input>:6:29 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S1: `new` heap-init consumes @move field, post-lit use rejected"
    "@move\n\
     struct Owner { tag: int }\n\
     struct Wrap { f: Owner }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let p = new Wrap { f: a };\n\
    \    println(take(a));\n\
    \    free(p);\n\
     }\n"
    "use of 'a' after it was consumed at <input>:7:27 (move-marked types are use-at-most-once — borrow with '&a' / take '*const Owner' or clone to keep the source live)";

  check_error "S1: `[expr; N]` with @move element rejected outright"
    "@move\n\
     struct Owner { tag: int }\n\
     fn main() {\n\
    \    let a = Owner { tag: 1 };\n\
    \    let _arr: [Owner; 3] = [a; 3];\n\
    \    println(_arr[0].tag);\n\
     }\n"
    "cannot use a @move value in `[expr; N]` — the array-repeat lowering shallow-copies the same value into every slot, aliasing the heap-owning source N times (build each element explicitly or use a non-@move element type)";

  (* DR-002 S2 — pattern-bound @move tracked in the arm's live map.
     The move-pass used to seed `live` only from `TLet` RHS and fn
     params; arm binds (`H::Has(inner)`) were invisible, so a body
     that consumed `inner` twice silently double-fired.  Fix walks
     the pattern against the scrutinee's enum sig, threads each
     affine bind into the arm's live map, and filters them out of
     the post-arm contribution so they don't survive into the
     post-match state. *)
  check_error "S2: pattern-bound @move consumed twice in arm body rejected"
    "@move struct Owner { tag: int }\n\
     enum H { Has(Owner) | Empty }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn main() {\n\
    \    let h = H::Has(Owner { tag: 42 });\n\
    \    match h {\n\
    \        H::Has(inner) => {\n\
    \            let _x = take(inner);\n\
    \            let _y = take(inner);\n\
    \            println(_x + _y);\n\
    \        }\n\
    \        | H::Empty => {}\n\
    \    }\n\
     }\n"
    "use of 'inner' after it was consumed at <input>:8:27 (move-marked types are use-at-most-once — borrow with '&inner' / take '*const Owner' or clone to keep the source live)";

  (* DR-002 W1 — prelude struct used only as a field/payload must
     still emit its C definition.  The mono-prelude DCE pass used
     to seed `keep` only from non-prelude tfunc signatures + body
     texpr types; field reachability fired only for KEPT prelude
     structs, so a user struct/enum whose field referenced an
     otherwise-unused prelude type left the C decl with
     `incomplete type`.  Fix sweeps non-generic user structs/enums
     (and mono prelude-collection instances like `Vec_i32`) for
     prelude mentions, while skipping generic skeletons whose TVar
     payload can't false-trigger anything. *)
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
  check_assert "W1: user struct with `Allocator` field pulls in `ex_Allocator` def"
    (let c =
       Exile_lang.Compiler.compile
         "struct H { a: Allocator }\n\
          fn pick(h: H) -> H { return h; }\n\
          fn main() { println(0); }\n"
     in
     contains c "struct ex_Allocator {" && contains c "struct ex_H {");

  check_assert "W1: user enum with `Allocator` variant payload pulls in `ex_Allocator` def"
    (let c =
       Exile_lang.Compiler.compile
         "enum H { Has(Allocator) | Empty }\n\
          fn pick(h: H) -> H { return h; }\n\
          fn main() { println(0); }\n"
     in
     contains c "struct ex_Allocator {" && contains c "struct ex_H {");

  check_assert "W1: user struct with `Vec<int>` field pulls in `ex_Allocator` transitively"
    (let c =
       Exile_lang.Compiler.compile
         "struct A { v: Vec<int> }\n\
          fn pick(a: A) -> A { return a; }\n\
          fn main() { println(0); }\n"
     in
     contains c "struct ex_Allocator {" && contains c "struct ex_Vec_i32 {"
     && contains c "struct ex_A {");

  (* DR-002 W2 — `==` / `!=` on a struct/enum lowers to `T__eq`.
     Pre-fix the operator fell through to raw `a == b` in C and cc
     rejected aggregate equality.  `.eq()` dispatch was already
     wired; W2 hooks the operator into the same path through
     `trait_impl_table`.  Without an Eq impl, the operator errors
     upfront with a directive to derive or impl Eq. *)
  check_assert "W2: `s1 == s2` on String lowers to `String__eq(&s1, &s2)`"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let s1 = String::with_str(a, \"x\");\n\
         \    let s2 = String::with_str(a, \"x\");\n\
         \    if s1 == s2 { println(1); } else { println(0); }\n\
         \    s1.free(); s2.free();\n\
          }\n"
     in
     contains c "String__eq(&s1, &s2)");

  check_assert "W2: `@derive(Eq)` struct dispatches `==` through `T__eq`"
    (let c =
       Exile_lang.Compiler.compile
         "@derive(Eq)\n\
          struct Pt { x: int, y: int }\n\
          fn main() {\n\
         \    let p1 = Pt { x: 1, y: 2 };\n\
         \    let p2 = Pt { x: 1, y: 2 };\n\
         \    if p1 == p2 { println(1); } else { println(0); }\n\
          }\n"
     in
     contains c "Pt__eq(&p1, &p2)");

  check_assert "W2: `!=` on an `@derive(Eq)` enum negates the `T__eq` call"
    (let c =
       Exile_lang.Compiler.compile
         "@derive(Eq)\n\
          enum H { Has(int) | Empty }\n\
          fn main() {\n\
         \    let a = H::Has(7);\n\
         \    let b = H::Empty;\n\
         \    if a != b { println(1); } else { println(0); }\n\
          }\n"
     in
     contains c "!(H__eq(&a, &b))");

  check_error "W2: `==` on a struct without Eq impl errors with derive hint"
    "struct Pt { x: int, y: int }\n\
     fn main() {\n\
    \    let p1 = Pt { x: 1, y: 2 };\n\
    \    let p2 = Pt { x: 1, y: 2 };\n\
    \    if p1 == p2 { println(1); } else { println(0); }\n\
     }\n"
    "type 'Pt' does not implement Eq, so `==` cannot compare two values of it (add `@derive(Eq)` to the decl or write `impl Eq for Pt` to define content equality)";

  check "@derive(Clone) synthesizes a field-wise deep copy"
    "@derive(Clone)\n\
     struct P { x: int }\n\
     fn main() {\n\
    \    let a = P { x: 7 };\n\
    \    let b = a.clone();\n\
    \    println(b.x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; };\n\nstruct ex_P P__clone(const struct ex_P *self);\n\nint main(void) {\n    struct ex_P a;\n    struct ex_P b;\n    a.x = 7;\n    b = P__clone(&a);\n    printf(\"%ld\\n\", (long)(b.x));\n    return 0;\n}\n\nstruct ex_P P__clone(const struct ex_P *self) {\n    {\n        struct ex_P __exile_ret;\n        __exile_ret.x = self->x;\n        return __exile_ret;\n    }\n}\n";

  (* DR-002 S4 — `@derive(Clone)` on a struct with a @move field
     used to emit `return *self;` (shallow ptr-copy), aliasing
     every heap-owning field with the source so a later `free()`
     on either side double-fired (ASan double-free).  Fix mirrors
     `@derive(Eq)` / `@derive(Hash)`: per-field recurse through
     `self.f.clone()`; primitive fields hit the built-in identity
     clone, aggregate fields dispatch through their own `T__clone`
     impl (`String::clone` deep-copies via `with_str`).  An enum
     payload variant maps to a per-arm match that constructs the
     same variant with each bound name cloned. *)
  check_assert "S4: `@derive(Clone)` on a String field recurses through `String__clone`"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          @derive(Clone)\n\
          struct H { name: String }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let h1 = H { name: String::with_str(a, \"x\") };\n\
         \    let h2 = h1.clone();\n\
         \    h1.name.free(); h2.name.free();\n\
          }\n"
     in
     contains c "String__clone(&self->name)");

  check_assert "S4: `@derive(Clone)` on an enum String payload recurses through `String__clone`"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          @derive(Clone)\n\
          enum E { Has(String) | Empty }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let e1 = E::Has(String::with_str(a, \"x\"));\n\
         \    let _e2 = e1.clone();\n\
          }\n"
     in
     contains c "E__clone" && contains c "String__clone(&__dc_a0)");

  check_error "S4: `@derive(Clone)` on a generic struct rejected (MVP)"
    "@derive(Clone)\n\
     struct Box<T> { v: T }\n\
     fn main() { println(1); }\n"
    "@derive(Clone) on a generic struct 'Box' is not supported yet";

  (* DR-002 C1 — `print_like_bcheck`'s catch-all `[_]→t_i32` let
     every non-explicit singleton (`*const T`, fn-ptr, array,
     unresolved-TVar, …) through to codegen, where
     `emit_print_impl` asserted false because `printf_int_spec`
     returned None.  Replace the catch-all with an allowlist
     (TInt / TCInt / TBool / TString) and add explicit reject
     arms for `TConstPtr`, `TFnPtr`, `TArray` that point users at
     the right idiom.  Closes the ICE class for print/println
     argument types — anything not on the allowlist now errors
     with a typed diagnostic instead of crashing the compiler. *)
  check_error "C1: `println(*const T)` errors with a deref hint (no ICE)"
    "fn main() {\n\
    \    let x: int = 5;\n\
    \    let p: *const int = &x;\n\
    \    println(p);\n\
     }\n"
    "cannot print a const-pointer value (*const i32); deref or print a field";

  check_error "C1: `println(fn_ptr)` errors with a call/cast hint (no ICE)"
    "fn helper() -> int { return 42; }\n\
     fn main() {\n\
    \    let f: fn() -> int = helper;\n\
    \    println(f);\n\
     }\n"
    "cannot print a function-pointer value (fn() -> i32); call it or cast to an int first";

  check_error "C1: `println([T; N])` errors with an iterate hint (no ICE)"
    "fn main() {\n\
    \    let arr: [int; 3] = [1, 2, 3];\n\
    \    println(arr);\n\
     }\n"
    "cannot print an array value ([i32; 3]); iterate and print each element";

  (* DR-002 C2 — `collect_tuple_types_of`'s `add_tuple` /
     `add_fnptr` / `add_array` walked every tfunc's signature and
     every struct/enum's field types, including generic skeletons
     carrying TVar in a tuple/fnptr/array slot.  `mangle_typ` has
     no encoding for TVar and asserted false on the declaration
     alone (`fn wrap<T>(x: T) -> (T, int)`, `struct Pair<T> { p:
     (T, int) }`).  Guard each `add_*` with `contains_tvar`:
     skeletons skip aggregate registration; their monomorphic
     instances (walked separately) carry concrete args verbatim
     and register correctly. *)
  check_assert "C2: tuple-returning generic fn declaration compiles (no codegen ICE)"
    (let c =
       Exile_lang.Compiler.compile
         "fn wrap<T>(x: T) -> (T, int) { return (x, 1); }\n\
          fn main() { println(0); }\n"
     in
     contains c "int main");

  check_assert "C2: generic struct with tuple field declaration compiles (no codegen ICE)"
    (let c =
       Exile_lang.Compiler.compile
         "struct Pair<T> { p: (T, int) }\n\
          fn main() { println(0); }\n"
     in
     contains c "int main");

  check_assert "C2: concrete tuple-returning generic instance still emits a tuple typedef"
    (let c =
       Exile_lang.Compiler.compile
         "fn wrap<T>(x: T, y: int) -> (T, int) { return (x, y); }\n\
          fn main() {\n\
         \    let p = wrap(42, 7);\n\
         \    let (a, b) = p;\n\
         \    println(a); println(b);\n\
          }\n"
     in
     contains c "tup2_i32_i32" && contains c "ex_wrap_i32");

  (* DR-002 C3 — wrong-arity multi-arg generic call used to crash
     in `List.combine skel.param_tys (take ... arg_tys)` because the
     callsite's `check_call_args` arity diagnostic only ran AFTER
     `resolve_call_dispatch`'s tparam inference.  Arity now checks
     up front (`'fn' expects N argument(s), got M`) and the inference
     loop only sees length-aligned (param, arg) pairs.  Display strips
     `ex_` prefix so top-level fns read naturally in the error. *)
  check_error "C3: under-supplied generic fn call errors with arity, not ICE"
    "fn pair<A, B>(a: A, b: B) -> A { return a; }\n\
     fn main() {\n\
    \    let x = pair(1);\n\
    \    println(x);\n\
     }\n"
    "'pair' expects 2 argument(s), got 1";

  check_error "C3: over-supplied generic fn call errors with arity, not ICE"
    "fn pair<A, B>(a: A, b: B) -> A { return a; }\n\
     fn main() {\n\
    \    let x = pair(1, 2, 3);\n\
    \    println(x);\n\
     }\n"
    "'pair' expects 2 argument(s), got 3";

  check_assert "C3: correctly-arity'd generic fn call still infers + dispatches"
    (let c =
       Exile_lang.Compiler.compile
         "fn pair<A, B>(a: A, b: B) -> A { return a; }\n\
          fn main() {\n\
         \    let x = pair(1, 2);\n\
         \    println(x);\n\
          }\n"
     in
     contains c "ex_pair_i32_i32");

  (* DR-002 W3 — cross-arm or-pattern redundancy.  Pre-fix the
     redundant-arm check asked "is any row in this arm useful?",
     so `A | B => ... | B | C => ...` passed (the second arm's
     `C` is new) and codegen emitted duplicate `case ex_T_B:`
     labels — cc rejected.  Walk alternatives in source order
     and reject the first row already covered by accepted earlier
     rows; the intra-arm `A | A` check continues to fire separately
     at parser-level (`or-pattern lists 'A' more than once`). *)
  check_error "W3: cross-arm or-pattern with overlap rejected as unreachable"
    "enum E { A | B | C | D }\n\
     fn classify(e: E) {\n\
    \    match e {\n\
    \        E::A | E::B => { println(1); }\n\
    \        | E::B | E::C => { println(2); }\n\
    \        | E::D => { println(3); }\n\
    \    }\n\
     }\n\
     fn main() { classify(E::A); }\n"
    "unreachable match arm: earlier arms already cover this case";

  check_error "W3: fully-covered second arm after or-pattern rejected"
    "enum E { A | B | C }\n\
     fn classify(e: E) {\n\
    \    match e {\n\
    \        E::A | E::B => { println(1); }\n\
    \        | E::B => { println(2); }\n\
    \        | E::C => { println(3); }\n\
    \    }\n\
     }\n\
     fn main() { classify(E::A); }\n"
    "unreachable match arm: earlier arms already cover this case";

  check_assert "W3: non-overlapping cross-arm or-patterns still compile"
    (let c =
       Exile_lang.Compiler.compile
         "enum E { A | B | C | D }\n\
          fn classify(e: E) {\n\
         \    match e {\n\
         \        E::A | E::B => { println(1); }\n\
         \        | E::C | E::D => { println(2); }\n\
         \    }\n\
          }\n\
          fn main() { classify(E::A); classify(E::C); }\n"
     in
     contains c "case ex_E_A" && contains c "case ex_E_C");

  (* DR-002 W4 — `Vec<*T>` grow used to expand the slice-routed
     copy as `new_ptr[i] = src.ptr[i]` (LHS `U **`, RHS `U * const
     *`), which cc flagged `-Wdiscarded-qualifiers`.  `Vec<int>`
     dropped const trivially and never warned.  Wrap the RHS in
     `as T`: cast is a no-op at the C level for plain T and
     silences the discard for pointer T (cc treats explicit casts
     as intent). *)
  check_assert "W4: `Vec<*int>` grow body carries an explicit `(T)` cast"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut v: Vec<*int> = Vec::with_capacity(a, 2 as u32);\n\
         \    let mut x: int = 1;\n\
         \    v.push(&x);\n\
          }\n"
     in
     contains c "new_ptr[i] = ((long *)src.ptr[i])");

  check_assert "W4: `Vec<int>` grow body still emits the cast (no-op for plain T)"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 2 as u32);\n\
         \    v.push(1);\n\
          }\n"
     in
     contains c "new_ptr[i] = ((long)src.ptr[i])");

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

  (* @derive(Hash) on a struct with a `str` field now folds through
     the content hash `str::hash` (DR-007 follow-up enabling
     `HashMap<str, _>`).  Pre-fix this errored with "hash not built
     in for str"; with the dispatch in place the `.hash()` call on
     the str field lowers to `str__hash(self.name)`. *)
  check_assert "@derive(Hash) on a str field folds through `str::hash`"
    (let c =
       Exile_lang.Compiler.compile
         "@derive(Eq, Hash)\n\
          struct P { name: str }\n\
          fn main() {\n\
         \    let a = P { name: \"x\" };\n\
         \    println(a.hash() as int);\n\
          }\n"
     in
     contains c "P__hash" && contains c "str__hash(self->name)");

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
    \    fn fmt(*const self, out: int) {}\n\
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
    "#include <stdio.h>\n\ntypedef void *(*fn2_ptr_cvoid_u32_to_ptr_cvoid)(void *, unsigned long);\ntypedef void (*fn3_ptr_cvoid_ptr_cvoid_u32_to_void)(void *, void *, unsigned long);\n\nstruct ex_Allocator { void *state; fn2_ptr_cvoid_u32_to_ptr_cvoid alloc_fn; fn3_ptr_cvoid_ptr_cvoid_u32_to_void free_fn; };\nstruct ex_UpTo { long cur; long stop; };\nstruct ex_Take_ex_UpTo { struct ex_UpTo inner; unsigned long remaining; };\nstruct ex_Enumerate_ex_UpTo { struct ex_UpTo inner; unsigned long idx; };\nstruct ex_Vec_i32 { long *ptr; unsigned long count; unsigned long cap; struct ex_Allocator alloc; };\nstruct ex_Slice_i32 { const long *ptr; unsigned long len; };\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self);\nstruct ex_Take_ex_UpTo UpTo__take(struct ex_UpTo self, unsigned long n);\nstruct ex_Enumerate_ex_UpTo UpTo__enumerate(struct ex_UpTo self);\nstruct ex_Vec_i32 UpTo__collect(struct ex_UpTo self, struct ex_Allocator a);\nstruct ex_Vec_i32 Vec__with_capacity_i32(struct ex_Allocator a, unsigned long hint);\nvoid Vec__push_i32(struct ex_Vec_i32 *self, long x);\nstatic void Vec__grow_i32(struct ex_Vec_i32 *self, unsigned long new_cap);\n\nint main(void) {\n    struct ex_UpTo up;\n    struct ex_UpTo __it0;\n    up.cur = 0;\n    up.stop = 3;\n    __it0 = up;\n    while (1) {\n        {\n            struct ex_Option_i32 __m;\n            __m = UpTo__next(&__it0);\n            if (__m.tag == ex_Option_i32_Some) {\n                long __fv0 = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(__fv0));\n            }\n            else {\n                break;\n            }\n        }\n    }\n    return 0;\n}\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self) {\n    long v;\n    if (self->cur >= self->stop) {\n        {\n            struct ex_Option_i32 __exile_ret;\n            __exile_ret.tag = ex_Option_i32_None;\n            return __exile_ret;\n        }\n    }\n    v = self->cur;\n    self->cur = self->cur + 1;\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v;\n        return __exile_ret;\n    }\n}\n\nstruct ex_Take_ex_UpTo UpTo__take(struct ex_UpTo self, unsigned long n) {\n    {\n        struct ex_Take_ex_UpTo __exile_ret;\n        __exile_ret.inner = self;\n        __exile_ret.remaining = n;\n        return __exile_ret;\n    }\n}\n\nstruct ex_Enumerate_ex_UpTo UpTo__enumerate(struct ex_UpTo self) {\n    {\n        struct ex_Enumerate_ex_UpTo __exile_ret;\n        __exile_ret.inner = self;\n        __exile_ret.idx = ((unsigned long)0);\n        return __exile_ret;\n    }\n}\n\nstruct ex_Vec_i32 UpTo__collect(struct ex_UpTo self, struct ex_Allocator a) {\n    struct ex_Vec_i32 out;\n    struct ex_UpTo __it1;\n    out = Vec__with_capacity_i32(a, ((unsigned long)8));\n    __it1 = self;\n    while (1) {\n        {\n            struct ex_Option_i32 __m;\n            __m = UpTo__next(&__it1);\n            if (__m.tag == ex_Option_i32_Some) {\n                long __fv1 = __m.data.Some._0;\n                Vec__push_i32(&out, __fv1);\n            }\n            else {\n                break;\n            }\n        }\n    }\n    return out;\n}\n\nstruct ex_Vec_i32 Vec__with_capacity_i32(struct ex_Allocator a, unsigned long hint) {\n    unsigned long cap;\n    unsigned long bytes;\n    long *p;\n    if (hint < ((unsigned long)8)) {\n        cap = ((unsigned long)8);\n    } else {\n        cap = hint;\n    }\n    bytes = ((unsigned long)sizeof(long)) * cap;\n    p = ((long *)(a.alloc_fn)(a.state, bytes));\n    {\n        struct ex_Vec_i32 __exile_ret;\n        __exile_ret.ptr = p;\n        __exile_ret.count = ((unsigned long)0);\n        __exile_ret.cap = cap;\n        __exile_ret.alloc = a;\n        return __exile_ret;\n    }\n}\n\nvoid Vec__push_i32(struct ex_Vec_i32 *self, long x) {\n    if (self->count + ((unsigned long)1) > self->cap) {\n        Vec__grow_i32(self, self->cap * ((unsigned long)2));\n    }\n    self->ptr[self->count] = x;\n    self->count = self->count + ((unsigned long)1);\n}\n\nstatic void Vec__grow_i32(struct ex_Vec_i32 *self, unsigned long new_cap) {\n    unsigned long bytes;\n    long *new_ptr;\n    struct ex_Slice_i32 src;\n    unsigned long i;\n    unsigned long old_bytes;\n    bytes = ((unsigned long)sizeof(long)) * new_cap;\n    new_ptr = ((long *)(self->alloc.alloc_fn)(self->alloc.state, bytes));\n    src.ptr = ((const long *)self->ptr);\n    src.len = self->count;\n    i = ((unsigned long)0);\n    while (i < self->count) {\n        new_ptr[i] = ((long)src.ptr[i]);\n        i = i + ((unsigned long)1);\n    }\n    old_bytes = ((unsigned long)sizeof(long)) * self->cap;\n    (self->alloc.free_fn)(self->alloc.state, ((void *)self->ptr), old_bytes);\n    self->ptr = new_ptr;\n    self->cap = new_cap;\n}\n";

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
    "#include <stdio.h>\n\ntypedef void *(*fn2_ptr_cvoid_u32_to_ptr_cvoid)(void *, unsigned long);\ntypedef void (*fn3_ptr_cvoid_ptr_cvoid_u32_to_void)(void *, void *, unsigned long);\n\nstruct ex_Allocator { void *state; fn2_ptr_cvoid_u32_to_ptr_cvoid alloc_fn; fn3_ptr_cvoid_ptr_cvoid_u32_to_void free_fn; };\nstruct ex_UpTo { long cur; long stop; };\nstruct ex_Take_ex_UpTo { struct ex_UpTo inner; unsigned long remaining; };\nstruct ex_Enumerate_ex_UpTo { struct ex_UpTo inner; unsigned long idx; };\nstruct ex_Vec_i32 { long *ptr; unsigned long count; unsigned long cap; struct ex_Allocator alloc; };\nstruct ex_Slice_i32 { const long *ptr; unsigned long len; };\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self);\nstruct ex_Take_ex_UpTo UpTo__take(struct ex_UpTo self, unsigned long n);\nstruct ex_Enumerate_ex_UpTo UpTo__enumerate(struct ex_UpTo self);\nstruct ex_Vec_i32 UpTo__collect(struct ex_UpTo self, struct ex_Allocator a);\nstatic struct ex_Option_i32 ex_first_ex_UpTo(struct ex_UpTo *it);\nstruct ex_Vec_i32 Vec__with_capacity_i32(struct ex_Allocator a, unsigned long hint);\nvoid Vec__push_i32(struct ex_Vec_i32 *self, long x);\nstatic void Vec__grow_i32(struct ex_Vec_i32 *self, unsigned long new_cap);\n\nint main(void) {\n    struct ex_UpTo up;\n    up.cur = 0;\n    up.stop = 2;\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_first_ex_UpTo(&up);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long v = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(99));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n\nstruct ex_Option_i32 UpTo__next(struct ex_UpTo *self) {\n    long v;\n    if (self->cur >= self->stop) {\n        {\n            struct ex_Option_i32 __exile_ret;\n            __exile_ret.tag = ex_Option_i32_None;\n            return __exile_ret;\n        }\n    }\n    v = self->cur;\n    self->cur = self->cur + 1;\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v;\n        return __exile_ret;\n    }\n}\n\nstruct ex_Take_ex_UpTo UpTo__take(struct ex_UpTo self, unsigned long n) {\n    {\n        struct ex_Take_ex_UpTo __exile_ret;\n        __exile_ret.inner = self;\n        __exile_ret.remaining = n;\n        return __exile_ret;\n    }\n}\n\nstruct ex_Enumerate_ex_UpTo UpTo__enumerate(struct ex_UpTo self) {\n    {\n        struct ex_Enumerate_ex_UpTo __exile_ret;\n        __exile_ret.inner = self;\n        __exile_ret.idx = ((unsigned long)0);\n        return __exile_ret;\n    }\n}\n\nstruct ex_Vec_i32 UpTo__collect(struct ex_UpTo self, struct ex_Allocator a) {\n    struct ex_Vec_i32 out;\n    struct ex_UpTo __it0;\n    out = Vec__with_capacity_i32(a, ((unsigned long)8));\n    __it0 = self;\n    while (1) {\n        {\n            struct ex_Option_i32 __m;\n            __m = UpTo__next(&__it0);\n            if (__m.tag == ex_Option_i32_Some) {\n                long __fv0 = __m.data.Some._0;\n                Vec__push_i32(&out, __fv0);\n            }\n            else {\n                break;\n            }\n        }\n    }\n    return out;\n}\n\nstatic struct ex_Option_i32 ex_first_ex_UpTo(struct ex_UpTo *it) {\n    return UpTo__next(it);\n}\n\nstruct ex_Vec_i32 Vec__with_capacity_i32(struct ex_Allocator a, unsigned long hint) {\n    unsigned long cap;\n    unsigned long bytes;\n    long *p;\n    if (hint < ((unsigned long)8)) {\n        cap = ((unsigned long)8);\n    } else {\n        cap = hint;\n    }\n    bytes = ((unsigned long)sizeof(long)) * cap;\n    p = ((long *)(a.alloc_fn)(a.state, bytes));\n    {\n        struct ex_Vec_i32 __exile_ret;\n        __exile_ret.ptr = p;\n        __exile_ret.count = ((unsigned long)0);\n        __exile_ret.cap = cap;\n        __exile_ret.alloc = a;\n        return __exile_ret;\n    }\n}\n\nvoid Vec__push_i32(struct ex_Vec_i32 *self, long x) {\n    if (self->count + ((unsigned long)1) > self->cap) {\n        Vec__grow_i32(self, self->cap * ((unsigned long)2));\n    }\n    self->ptr[self->count] = x;\n    self->count = self->count + ((unsigned long)1);\n}\n\nstatic void Vec__grow_i32(struct ex_Vec_i32 *self, unsigned long new_cap) {\n    unsigned long bytes;\n    long *new_ptr;\n    struct ex_Slice_i32 src;\n    unsigned long i;\n    unsigned long old_bytes;\n    bytes = ((unsigned long)sizeof(long)) * new_cap;\n    new_ptr = ((long *)(self->alloc.alloc_fn)(self->alloc.state, bytes));\n    src.ptr = ((const long *)self->ptr);\n    src.len = self->count;\n    i = ((unsigned long)0);\n    while (i < self->count) {\n        new_ptr[i] = ((long)src.ptr[i]);\n        i = i + ((unsigned long)1);\n    }\n    old_bytes = ((unsigned long)sizeof(long)) * self->cap;\n    (self->alloc.free_fn)(self->alloc.state, ((void *)self->ptr), old_bytes);\n    self->ptr = new_ptr;\n    self->cap = new_cap;\n}\n";

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
    \    let mut p = Point { x: 1, y: 2 };\n\
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
    "cannot cast bool to *i32 (supported: int↔int, int↔float, float↔float, ptr↔ptr, int→ptr)";

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
  (* HashMap v1 (DR-007): open-addressing linear probe + cached
     hash + content `K::eq`.  String keys are the headline use case;
     `String::hash` delegates to `str::hash` so equal-content
     Strings hit the same slot. *)
  check_assert "`HashMap<int,int>` insert+get roundtrips"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<int,int> = HashMap::with_capacity(a, 8 as u32);\n\
         \    m.insert(42, 7);\n\
          }\n"
     in
     contains c "HashMap__insert" && contains c "HashMap__with_capacity"
     && contains c "memset" && contains c "Slot");

  check_assert "`HashMap<String,_>` pulls in String::hash / str::hash for key dispatch"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<String,int> = HashMap::with_capacity(a, 8 as u32);\n\
         \    m.insert(String::with_str(a, \"x\"), 1);\n\
          }\n"
     in
     contains c "String__hash" && contains c "str__hash"
     && contains c "String__eq");

  (* `insert` checks load > 0.75 before probing and calls the
     private `grow` method, which alloc's a fresh buffer, walks
     the old slots and re-probes every Occupied entry into the
     new layout using each slot's cached hash. *)
  check_assert "`HashMap::insert` rehashes via private `grow` on load > 0.75"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<int,int> = HashMap::with_capacity(a, 4 as u32);\n\
         \    m.insert(1, 100);\n\
          }\n"
     in
     contains c "HashMap__grow" && contains c "memset"
     && contains c "HashMap__insert");

  check_assert "`HashMap::remove` lowers to a tombstone-mark on the matching slot"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<int,int> = HashMap::with_capacity(a, 8 as u32);\n\
         \    m.insert(1, 100);\n\
         \    m.remove(1);\n\
          }\n"
     in
     contains c "HashMap__remove"
     (* Tombstone state byte (2) is written into the matching slot. *)
     && contains c ".state = ((unsigned char)2)");

  check_assert "`HashMap::iter` yields `(K, V)` tuples for `for kv in m.iter()`"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<int,int> = HashMap::with_capacity(a, 8 as u32);\n\
         \    m.insert(1, 10);\n\
         \    let mut sum: int = 0;\n\
         \    let it = m.iter();\n\
         \    for kv in it { let (k, v) = kv; sum = sum + v; }\n\
         \    println(sum);\n\
          }\n"
     in
     contains c "HashMap__iter"
     && contains c "HashMapIter__next"
     && contains c "tup2_i32_i32");

  (* DR-007 follow-up — `HashMap<str, _>` is the second motivating
     use-case (alongside `HashMap<String, _>`).  `str.hash()` had no
     content-hash dispatch and the `Hash` conformance check rejected
     `str` keys; `str.eq()` lowered to pointer-compare via the BinOp
     path, which would let two distinct-pointer keys with the same
     content miss each other.  Fixed by dispatching `.hash()` on
     `TString` to `str__hash` and `.eq()`/`.ne()` to `str__eq` (same
     path the `==` / `!=` operator already takes). *)
  check_assert "DR-007: `str.hash()` dispatches to the content-hash `str::hash`"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() { println(\"hello\".hash() as int); }\n"
     in
     contains c "str__hash(\"hello\")");

  check_assert "DR-007: `str.eq(other)` content-compares via `str::eq`"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    if \"foo\".eq(\"foo\") { println(1); } else { println(0); }\n\
          }\n"
     in
     contains c "str__eq(\"foo\", \"foo\")");

  check_assert "DR-007: `HashMap<str, int>` insert + get round-trips"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut kw: HashMap<str, int> = HashMap::with_capacity(a, 8 as u32);\n\
         \    kw.insert(\"fn\", 1);\n\
         \    kw.insert(\"let\", 2);\n\
         \    println(kw.len() as int);\n\
         \    match kw.get(\"fn\") {\n\
         \        Option::Some(v) => { println(v); }\n\
         \        | Option::None => { println(-1); }\n\
         \    }\n\
          }\n"
     in
     contains c "HashMap__with_capacity_str_i32"
     && contains c "HashMap__insert_str_i32"
     && contains c "HashMap__get_str_i32"
     && contains c "str__hash");

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

  (* DR-010 Faza A — Tier-1 escape floor.  Returning an aggregate
     literal (struct / `new`) whose field embeds an address of a
     local binding is a hard error.  Tier-1 propagates `&local`
     provenance through composite literals (TStructLit / TNew),
     catching the wrap-in-struct hole that defeats cc's
     `-Wreturn-local-addr`.  Pointer-typed params are exempt —
     their root lives outside the frame, so embedding them in a
     returned struct is the canonical view-exposure idiom. *)
  let escape_return_msg =
    "returning a value that embeds the address of a local binding — \
     the local goes out of scope at the end of its enclosing block, \
     leaving the caller with a dangling borrow.  Wrap the storage in \
     a caller-owned region, return a copy / `String::with_str(...)` \
     instead of a borrow, or — for arena/region-allocated returns — \
     mark the fn `@escapes` (forward-compat hatch)"
  in
  check_error "DR-010: returning `Slice { ptr: &local[..] }` is a hard error"
    "fn make() -> Slice<int> {\n\
    \    let arr: [int; 3] = [1, 2, 3];\n\
    \    return Slice { ptr: &arr[0], len: 3 as u32 };\n\
     }\n\
     fn main() { let s = make(); println(s[0]); }\n"
    escape_return_msg;

  check_error "DR-010: returning `new Box { f: &local }` is a hard error"
    "struct Box { p: *const int }\n\
     fn make() -> *Box {\n\
    \    let x: int = 7;\n\
    \    return new Box { p: &x };\n\
     }\n\
     fn main() { let _b = make(); println(0); }\n"
    escape_return_msg;

  check_lint "DR-010: returning `Slice { ptr: arr_param }` (ptr-typed param) is silent"
    "fn make(arr: *const int) -> Slice<int> {\n\
    \    return Slice { ptr: arr, len: 3 as u32 };\n\
     }\n\
     fn main() {\n\
    \    let arr: [int; 3] = [1, 2, 3];\n\
    \    let s = make(&arr[0]);\n\
    \    println(s[0]);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  (* DR-010 — laundering through `let` (the case S5a-lint missed).
     `return s` where `s = Slice { ptr: &local, ... }` is rejected
     because Tier-1 floor propagates Local prov through the let-
     binding state and re-meets it at the return site.  Closes the
     hole that motivated DR-010 even past Tier-2/3. *)
  check_error "DR-010: laundering &local through let then returning is a hard error"
    "fn make() -> Slice<int> {\n\
    \    let arr: [int; 3] = [1, 2, 3];\n\
    \    let s = Slice { ptr: &arr[0], len: 3 as u32 };\n\
    \    return s;\n\
     }\n\
     fn main() { let s = make(); println(s[0]); }\n"
    escape_return_msg;

  (* DR-010 — bare `return &local` (the case `-Wreturn-local-addr`
     does see).  Tier-1 covers it too via TRef → Local prov. *)
  check_error "DR-010: bare `return &local` is a hard error"
    "fn make() -> *int {\n\
    \    let mut x: int = 7;\n\
    \    return &x;\n\
     }\n\
     fn main() { let _p = make(); println(0); }\n"
    escape_return_msg;

  (* DR-010 `@escapes` hatch — function-level opt-out.  An arena/
     region-allocated fn that legitimately returns a borrow rooted
     in caller-owned-but-analyser-opaque storage can suppress the
     floor.  Strukturalny skeleton — swappable predykat. *)
  check_assert "DR-010: `@escapes` suppresses the floor for the marked fn"
    (try
       ignore (Exile_lang.Compiler.compile
         "@escapes\n\
          fn make() -> Slice<int> {\n\
         \    let arr: [int; 3] = [1, 2, 3];\n\
         \    return Slice { ptr: &arr[0], len: 3 as u32 };\n\
          }\n\
          fn main() { let _s = make(); println(0); }\n");
       true
     with _ -> false);

  (* DR-010 Faza B — param-SET summary across calls.  A trivial
     pass-through fn carries `summary = {0}` (returns arg 0); the
     caller's `return passthrough(&local)` then surfaces Local prov
     at the return site through the call, structurally rejecting
     the launder.  Closes the S5b residual that Tier-1 missed. *)
  check_error "DR-010-B: laundering &local through a pass-through call rejected"
    "fn passthrough(p: *int) -> *int { p }\n\
     fn make() -> *int {\n\
    \    let mut x: int = 7;\n\
    \    return passthrough(&x);\n\
     }\n\
     fn main() { let _p = make(); println(0); }\n"
    escape_return_msg;

  (* DR-010 Faza B — pass-through chain `f(g(p))` propagates param
     summary across two hops; param-rooted call still surfaces as
     CallerOrStatic at the return site (sound positive case). *)
  check_assert "DR-010-B: chained pass-through of a param is silent"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn passthrough(p: *int) -> *int { p }\n\
          fn relay(q: *int) -> *int { passthrough(q) }\n\
          fn main() {\n\
         \    let mut x: int = 7;\n\
         \    let p = relay(&x);\n\
         \    println(*p);\n\
          }\n");
       true
     with _ -> false);

  (* DR-010 Faza B — `Vec::as_slice` reads `self.ptr` / `self.count`
     and packs them into a `Slice<T>`; its summary computes to
     `{0}` (self-param).  A `return local_vec.as_slice()` thus
     surfaces the receiver's Local prov through the call and trips
     the return-floor.  This is THE motivating S5b case (closes
     "laundering through methods like `vec.as_slice()`"). *)
  check_error "DR-010-B: returning `local_vec.as_slice()` rejected (S5b laundering)"
    "pub mod raw { extern fn make_c_allocator() -> Allocator; }\n\
     fn make() -> Slice<int> {\n\
    \    let a = raw::make_c_allocator();\n\
    \    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
    \    v.push(1);\n\
    \    return v.as_slice();\n\
     }\n\
     fn main() { let _s = make(); println(0); }\n"
    escape_return_msg;

  (* DR-010 Faza B — recursive fn (AST-traversal pattern: the
     canonical exilc.exl shape) where the body returns a value
     mixing the recursive call's result with a param-rooted
     expression.  SCC-least-fixpoint converges to `{0}`; reject
     would force `@escapes`, masking real `&local` bugs (Decyzja
     #3).  Should compile silently. *)
  check_assert "DR-010-B: recursive param-derived return converges silently"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Node { next: *const Node, val: int }\n\
          fn last(n: *const Node) -> *const Node {\n\
         \    if n.next == null { return n; }\n\
         \    return last(n.next);\n\
          }\n\
          fn main() {\n\
         \    let leaf = Node { next: null, val: 1 };\n\
         \    let _p = last(&leaf);\n\
         \    println(0);\n\
          }\n");
       true
     with _ -> false);

  (* DR-010 Phase C — use-after-invalidation.  A `let s = v.as_slice()`
     records `s` as a borrow rooted in `v`; a subsequent
     `v.push(x)` / `v.grow()` reallocates the buffer the borrow
     points into, so reading `s` afterwards dangles.  Closes S5c. *)
  check_error "DR-010-C: use of `v.as_slice()` after `v.push(...)` rejected (S5c)"
    "pub mod raw { extern fn make_c_allocator() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_c_allocator();\n\
    \    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
    \    v.push(10);\n\
    \    let s = v.as_slice();\n\
    \    v.push(99);\n\
    \    println(s[0]);\n\
     }\n"
    "use of borrow 's' after it was invalidated by 'Vec::push' at <input>:7:6 — growing / freeing the owner reallocates the buffer the borrow pointed into, so subsequent reads dangle (rebuild the borrow after the mutation, or use a copy that doesn't share the buffer)";

  (* DR-010 Phase C — String::free invalidates `s.as_str()` borrow.
     Closes S5d (use-after-free).  String::free is in the invalidating
     list because freeing the buffer dangles every outstanding view. *)
  check_error "DR-010-C: use of `s.as_str()` after `s.free()` rejected (S5d)"
    "pub mod raw { extern fn make_c_allocator() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make_c_allocator();\n\
    \    let mut s: String = String::with_str(a, \"hello\");\n\
    \    let vw = s.as_str();\n\
    \    s.free();\n\
    \    println(vw);\n\
     }\n"
    "use of borrow 'vw' after it was invalidated by 'String::free' at <input>:6:6 — growing / freeing the owner reallocates the buffer the borrow pointed into, so subsequent reads dangle (rebuild the borrow after the mutation, or use a copy that doesn't share the buffer)";

  (* DR-010 Phase C — rebuilding the borrow after a mutation is fine.
     Push first, then take the slice — the slice points at the
     post-grow buffer, no dangling. *)
  check_assert "DR-010-C: rebuilding the borrow after a mutation is silent"
    (try
       ignore (Exile_lang.Compiler.compile
         "pub mod raw { extern fn make_c_allocator() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make_c_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
         \    v.push(10);\n\
         \    v.push(99);\n\
         \    let s = v.as_slice();\n\
         \    println(s[0]);\n\
          }\n");
       true
     with _ -> false);

  (* DR-010 Phase C — borrowing two distinct owners doesn't cross
     invalidation: mutating `v1` should not kill `s2 = v2.as_slice()`.
     Owner tracking is per-binding, so the kill scopes precisely. *)
  check_assert "DR-010-C: mutation on one container leaves another's borrow alive"
    (try
       ignore (Exile_lang.Compiler.compile
         "pub mod raw { extern fn make_c_allocator() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make_c_allocator();\n\
         \    let mut v1: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
         \    let mut v2: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
         \    v1.push(10);\n\
         \    v2.push(20);\n\
         \    let s2 = v2.as_slice();\n\
         \    v1.push(99);\n\
         \    println(s2[0]);\n\
          }\n");
       true
     with _ -> false);

  (* DR-010 Phase C — `h2 = h1.clone()` produces independent storage;
     `h1.name.free()` must NOT invalidate `h2`.  Verifies that the
     hardcoded borrowing-returns list (as_slice/as_str/iter only) is
     narrow enough — `clone` allocates a fresh String, the result is
     not a borrow.  Regression guard for the false positive that
     drove the design (test failed during impl). *)
  check_assert "DR-010-C: `clone()` does NOT propagate owner ownership"
    (try
       ignore (Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          @derive(Clone)\n\
          struct H { name: String }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let h1 = H { name: String::with_str(a, \"x\") };\n\
         \    let h2 = h1.clone();\n\
         \    h1.name.free();\n\
         \    h2.name.free();\n\
          }\n");
       true
     with _ -> false);

  (* DR-009 active patterns (`view`).  `view Name(p: T) -> A | B {
     body }` synthesises a nominal enum `Name { A | B }` plus a
     function `Name(p: T) -> Name { body }`.  A `match scr { Name::A
     => ... }` against a scrutinee of type `T` (not `Name`) gets
     rewritten to wrap the scrutinee in the view-fn call so the
     match sees the synthesised enum.  Maranget exhaustiveness on
     the choice-enum comes for free (`Name::*` is nominal/closed). *)
  check_assert "DR-009: view with explicit ctor — `Name(scr)` returns case"
    (let c =
       Exile_lang.Compiler.compile
         "view Sign(n: int) -> Negative | Zero | Positive {\n\
         \    if n < 0 { return Sign::Negative; }\n\
         \    if n == 0 { return Sign::Zero; }\n\
         \    return Sign::Positive;\n\
          }\n\
          fn main() {\n\
         \    let s = Sign(7);\n\
         \    match s {\n\
         \        Sign::Negative => { println(-1); }\n\
         \        | Sign::Zero => { println(0); }\n\
         \        | Sign::Positive => { println(1); }\n\
         \    }\n\
          }\n"
     in
     contains c "ex_Sign(7)" && contains c "case ex_Sign_Positive");

  check_assert "DR-009: match-site view-call inserted automatically"
    (let c =
       Exile_lang.Compiler.compile
         "view Sign(n: int) -> Negative | Zero | Positive {\n\
         \    if n < 0 { return Sign::Negative; }\n\
         \    if n == 0 { return Sign::Zero; }\n\
         \    return Sign::Positive;\n\
          }\n\
          fn classify(n: int) {\n\
         \    match n {\n\
         \        Sign::Negative => { println(-1); }\n\
         \        | Sign::Zero => { println(0); }\n\
         \        | Sign::Positive => { println(1); }\n\
         \    }\n\
          }\n\
          fn main() { classify(-5); classify(0); classify(42); }\n"
     in
     (* The classify match is over an `int`, but the view-rewrite
        inserts a Sign(n) call so the match runs against ex_Sign. *)
     contains c "ex_Sign(n)");

  check_assert "DR-009: tuple-payload case parses, destructures, and yields"
    (let c =
       Exile_lang.Compiler.compile
         "view Parse(s: int) -> Big(int) | Small(int) {\n\
         \    if s >= 100 { return Parse::Big(s); }\n\
         \    return Parse::Small(s);\n\
          }\n\
          fn classify(n: int) {\n\
         \    match n {\n\
         \        Parse::Big(v) => { println(v * 10); }\n\
         \        | Parse::Small(v) => { println(v); }\n\
         \    }\n\
          }\n\
          fn main() { classify(7); classify(150); }\n"
     in
     contains c "ex_Parse_Big" && contains c "ex_Parse_Small");

  check_error "DR-009: view match exhaustiveness enforced via the choice-enum"
    "view Sign(n: int) -> Negative | Zero | Positive {\n\
    \    if n < 0 { return Sign::Negative; }\n\
    \    if n == 0 { return Sign::Zero; }\n\
    \    return Sign::Positive;\n\
     }\n\
     fn classify(n: int) {\n\
    \    match n {\n\
    \        Sign::Negative => { println(-1); }\n\
    \        | Sign::Zero => { println(0); }\n\
     }\n\
     }\n\
     fn main() { classify(5); }\n"
    "non-exhaustive 'match': pattern 'Positive' is not covered (add an arm or '_')";

  check_error "DR-009: duplicate case in a view rejected at parse time"
    "view Sign(n: int) -> A | B | A {\n\
    \    return Sign::A;\n\
     }\n\
     fn main() {}\n"
    "duplicate case 'A' in view 'Sign'";

  (* DR-floats — f32 / f64 with IEEE built-in operators.  The
     distinctive choice (Q2 design 2026-05-31) is that arithmetic
     and comparison operators are built-in / IEEE on float, but
     `Eq` / `Ord` / `Hash` traits are NOT implemented — so float
     can't `@derive(Eq)`, can't be a HashMap key, and `f.eq(g)`
     falls through to the struct/enum path (which rejects it).
     This is exile-distinctive vs Rust's PartialEq/PartialOrd
     split. *)
  check_assert "DR-floats: arithmetic + comparison work on f64"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let x: f64 = 3.14;\n\
         \    let y: f64 = 2.0;\n\
         \    println(x + y);\n\
         \    println(x * y);\n\
         \    if x > y { println(1); } else { println(0); }\n\
          }\n"
     in
     contains c "double x"
     && contains c "double y"
     && contains c "x + y"
     && contains c "x > y");

  check_assert "DR-floats: f32 literal carries the `f` suffix and emits `float`"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a: f32 = 1.5f32;\n\
         \    let b: f32 = 2.25f32;\n\
         \    println(a + b);\n\
          }\n"
     in
     contains c "float a"
     && contains c "float b"
     && contains c "1.5f"
     && contains c "2.25f");

  check_assert "DR-floats: `as` casts cross int<->float and f32<->f64"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let i: int = 7;\n\
         \    let f: f64 = i as f64;\n\
         \    let g: f32 = f as f32;\n\
         \    let j: int = g as int;\n\
         \    println(j);\n\
          }\n"
     in
     contains c "(double)" && contains c "(float)" && contains c "(long)");

  check_error "DR-floats: `%%` on float rejected (libm fmod is deferred)"
    "fn main() {\n\
    \    let a: f64 = 5.0;\n\
    \    let b: f64 = 2.0;\n\
    \    let _ = a % b;\n\
     }\n"
    "operator '%' is not built-in for float (use the libm `fmod`/`fmodf` extern fn when binding it lands)";

  check_error "DR-floats: float.eq() not built-in (no Eq trait on float)"
    "fn main() {\n\
    \    let a: f64 = 1.0;\n\
    \    if a.eq(a) { println(1); } else { println(0); }\n\
     }\n"
    "method call '.eq()' requires a struct or enum value (or a pointer to one), got f64";

  check_error "DR-floats: float.hash() not built-in (no Hash trait on float)"
    "fn main() {\n\
    \    let a: f64 = 1.0;\n\
    \    println(a.hash() as int);\n\
     }\n"
    "method call '.hash()' requires a struct or enum value (or a pointer to one), got f64";

  check_error "DR-floats: @derive(Eq) on a struct with a float field cascades to reject"
    "@derive(Eq)\n\
     struct P { x: f64 }\n\
     fn main() {}\n"
    "method call '.eq()' requires a struct or enum value (or a pointer to one), got f64";

  (* DR-012 scoped projection (`with <name> in <lvalue> { body }`)
     — binds a borrow to the lvalue for the body block.  The borrow
     is `*T` (mutable pointer-honest) when the lvalue is owned
     storage (array element, struct field, local), or `*const T`
     when reached through a `Slice<T>` (whose `.ptr` is `*const T`
     so the borrow inherits read-only-ness automatically).  Existing
     escape pass catches in-block leaks. *)
  check_assert "DR-012: `with x in arr[i] { *x = ... }` mutates the element in place"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let mut arr: [int; 4] = [10, 20, 30, 40];\n\
         \    with x in arr[2] { *x = 999; }\n\
         \    println(arr[2]);\n\
          }\n"
     in
     contains c "x__with0 = &(arr.data[2])"
     && contains c "*x__with0 = 999");

  check_assert "DR-012: `with` over a Slice element inherits *const T"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 2 as u32);\n\
         \    v.push(7);\n\
         \    let sl = v.as_slice();\n\
         \    with e in sl[0] { println(*e); }\n\
          }\n"
     in
     contains c "const long *e__with");

  check_error "DR-012: `with` over a fn-call result rejected (rvalue)"
    "pub mod raw { extern fn make() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make();\n\
    \    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
    \    with sl in v.as_slice() { println(sl.len as int); }\n\
     }\n"
    "`with` target must be an lvalue — a local binding, a field, an index, or a deref.  Got an expression that produces a fresh value (e.g. a fn call returning by value); bind it to a `let mut` first and `with` over that.";

  (* DR-008 A1 captureless-decay (`|x: T| -> R body`).  A
     pre-typecheck pass lifts each lambda to a fresh top-level fn
     `__lambda_N`; the lambda expression becomes a Var referring to
     that fn, which decays to a C fn-pointer at the use site.
     Captureless is enforced by construction — the lifted body
     lives at top level, so any reference to an enclosing local
     errors as "undefined variable". *)
  check_assert "DR-008: lambda decays to fn-ptr; `apply(|x: int| -> int x*2, n)` works"
    (let c =
       Exile_lang.Compiler.compile
         "fn apply(f: fn(int) -> int, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    let twice = |x: int| -> int x * 2;\n\
         \    println(apply(twice, 7));\n\
          }\n"
     in
     contains c "ex___lambda_"
     && contains c "long ex_apply"
     && contains c "twice = ex___lambda_");

  check_assert "DR-008: lambda inlined at the call site works"
    (let c =
       Exile_lang.Compiler.compile
         "fn apply(f: fn(int) -> int, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    println(apply(|x: int| -> int x + 1, 10));\n\
          }\n"
     in
     contains c "apply(ex___lambda_");

  check_assert "DR-008: two lambdas in the same fn get distinct __lambda_N names"
    (let c =
       Exile_lang.Compiler.compile
         "fn apply(f: fn(int) -> int, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    println(apply(|x: int| -> int x + 1, 1));\n\
         \    println(apply(|x: int| -> int x * 2, 2));\n\
          }\n"
     in
     contains c "ex___lambda_0" && contains c "ex___lambda_1");

  (* Perf quick-win M2 (Gap-B audit 2026-06-02) — HashMap caps round
     up to the next power of 2 in `with_capacity`, and the probe step
     uses `h & (cap - 1)` instead of `h % cap` so 68k DIVU (~140 cy)
     drops to AND (~4 cy).  The generated C reflects this by emitting
     a bitwise-AND with `cap - 1` in every probe. *)
  check_assert "M2: HashMap probe uses `& (cap - 1)` instead of `% cap`"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<int, int> = HashMap::with_capacity(a, 16 as u32);\n\
         \    m.insert(1, 100);\n\
         \    if m.contains(1) { println(1); } else { println(0); }\n\
          }\n"
     in
     contains c "h & (self->cap - "
     && not (contains c "% self->cap"));

  check_assert "M2: HashMap with_capacity rounds up to the next power of 2"
    (let c =
       Exile_lang.Compiler.compile
         "pub mod raw { extern fn make() -> Allocator; }\n\
          fn main() {\n\
         \    let a = raw::make();\n\
         \    let mut m: HashMap<int, int> = HashMap::with_capacity(a, 10 as u32);\n\
         \    println(1);\n\
          }\n"
     in
     (* The pow-2 loop body shifts cap left by 1; the cast +
        u32-encoded shift amount is the signature of the round-up. *)
     contains c "cap = cap << ((unsigned long)1)");

  (* Perf quick-win M1 (Gap-B audit) — warn when a growable collection
     is built with a hint < 8.  The prelude floor itself clamps to 8,
     so a smaller hint can't actually shrink the buffer — it just
     hides intent.  Folding to ≥ 8 also saves one or two `grow` calls
     on a fresh collection that takes more than 4-7 elements. *)
  check_lint "M1: with_capacity hint < 8 warns about grow-thrashing"
    "pub mod raw { extern fn make() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make();\n\
    \    let mut v: Vec<int> = Vec::with_capacity(a, 4 as u32);\n\
    \    v.push(1);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    ["called with hint 4"];

  check_lint "M1: with_capacity hint = 8 is silent (matches the floor)"
    "pub mod raw { extern fn make() -> Allocator; }\n\
     fn main() {\n\
    \    let a = raw::make();\n\
    \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
    \    v.push(1);\n\
     }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  (* DR-006 — `pub mod sys` is the one seam between exile stdlib and
     the host platform.  Width-pinned extern fns (sys_alloc / sys_free
     / sys_write / sys_read) are exposed in the prelude; the compiler
     drops unreferenced ones at emit time (DCE) so plain programs
     don't carry the seam declarations.  `default_allocator()`
     plugs through this seam by default. *)
  check_assert "DR-006: a plain program does not emit any `sys_*` extern decls"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() { println(\"hi\"); }\n"
     in
     not (contains c "sys_alloc")
     && not (contains c "sys_write"));

  check_assert "DR-006: program calling sys_alloc emits its extern decl"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let p = sys::sys_alloc(null as *c_void, 16 as c_ulong);\n\
         \    sys::sys_free(null as *c_void, p, 16 as c_ulong);\n\
         \    println(1);\n\
          }\n"
     in
     contains c "extern void *sys_alloc"
     && contains c "extern void sys_free"
     && contains c "p = sys_alloc"
     && contains c "sys_free(");

  check_assert "DR-006: extern fns inside `mod sys` are exempt from the raw-quarantine"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let n: c_long = sys::sys_write(1 as c_int, null as *const c_uchar, 0 as c_ulong);\n\
         \    println(n as int);\n\
          }\n");
       true
     with _ -> false);

  (* DR-008 A1 originally rejected this with "undefined variable 'y'";
     DR-024 A2 closures-with-capture turn the SAME source into an
     env-struct + impl Fn1 synthesis at expand_lambdas time, so the
     compile now succeeds and the captured `y` flows through. *)
  check_assert "DR-024: lambda capturing annotated outer local compiles"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let y: int = 5;\n\
         \    let f = |x: int| -> int x + y;\n\
         \    println(f(7));\n\
          }\n");
       true
     with _ -> false);

  check_error "DR-012: borrow can't escape the `with` block (out-of-scope after)"
    "fn main() {\n\
    \    let mut arr: [int; 3] = [1, 2, 3];\n\
    \    with x in arr[0] { *x = 100; }\n\
    \    println(*x);\n\
     }\n"
    "undefined variable 'x'";

  check_error "DR-floats: mixed-width comparison rejected without explicit cast"
    "fn main() {\n\
    \    let a: f32 = 1.0f32;\n\
    \    let b: f64 = 1.0;\n\
    \    if a < b { println(1); } else { println(0); }\n\
     }\n"
    "comparison '<' between f32 and f64 — mixed-width float comparison requires an explicit `as` cast";

  (* DR-002 S3 — partial-move scrutinee.  S2 tracks pattern-bound
     @move locally per arm but the scrutinee binding stays Live, so
     re-using it after an arm that consumed its payload (`let _b =
     h`, `take(h)`, `return h`) silently aliases a freed payload
     pointer.  Fix: after walking each arm body, if ANY affine
     pattern-bind ended Consumed and the scrutinee is a bare
     TVar `sn`, splice `(sn, Consumed)` into the arm's
     contribution; the may-consume merge (S0) propagates Consumed
     to the post-match state, so a later read of the scrutinee
     errors.  Read-only arms (`Has(inner) => println(inner.tag)`)
     leave the bind Live and don't trigger the splice — h stays
     reusable. *)
  check_error "S3: scrutinee reused after partial-move arm rejected"
    "@move struct Owner { tag: int }\n\
     enum H { Has(Owner) | Empty }\n\
     fn take(o: Owner) -> int { return o.tag; }\n\
     fn run(h: H) {\n\
    \    match h {\n\
    \        H::Has(inner) => { let _x = take(inner); }\n\
    \        | H::Empty => {}\n\
    \    }\n\
    \    let _b = h;\n\
     }\n\
     fn main() { run(H::Has(Owner { tag: 1 })); }\n"
    "use of 'h' after it was consumed at <input>:5:11 (move-marked types are use-at-most-once — borrow with '&h' / take '*const H' or clone to keep the source live)";

  check_assert "S3: scrutinee reusable after read-only arm (no payload consume)"
    (try
       ignore (Exile_lang.Compiler.compile
         "@move struct Owner { tag: int }\n\
          enum H { Has(Owner) | Empty }\n\
          fn run(h: H) -> int {\n\
         \    let mut sum: int = 0;\n\
         \    match h {\n\
         \        H::Has(inner) => { sum = inner.tag; }\n\
         \        | H::Empty => {}\n\
         \    }\n\
         \    let _b = h;\n\
         \    return sum;\n\
          }\n\
          fn main() { println(run(H::Has(Owner { tag: 5 }))); }\n");
       true
     with _ -> false);

  (* `default_allocator()` — zero-arg prelude builtin that returns
     a libc-backed `Allocator`.  Enables `println(x)` Display
     dispatch without threading an Allocator binding through every
     call site; standalone use also works for any prelude collection
     (String / StringBuilder / Vec / HashMap) that needs an
     Allocator. *)
  check_assert "default_allocator(): plugs the sys:: seam into the Allocator helper"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let s = String::with_str(a, \"hi\");\n\
         \    println(s.length() as int);\n\
         \    s.free();\n\
          }\n"
     in
     (* Post-DR-006 the helper wires sys_alloc / sys_free (from the
        prelude `pub mod sys`) directly — no more inline libc
        thunks; the host backend lives in runtime/sys_host.c. *)
     contains c "static struct ex_Allocator exile_default_allocator(void)"
     && contains c "a.alloc_fn = sys_alloc;"
     && contains c "a.free_fn = sys_free;"
     && contains c "extern void *sys_alloc"
     && contains c "extern void sys_free");

  check_error "default_allocator(): rejects extra arguments"
    "fn main() { let _a = default_allocator(0); }\n"
    "default_allocator() takes no arguments, got 1";

  (* `println(x)` / `print(x)` on a struct/enum with `impl Display
     for T` registered: desugar to the writer pattern.  Without this
     `print_like_bcheck` would reject every aggregate that isn't
     `@debug`-marked — Display was a manual surface the operator
     never reached. *)
  check_assert "println(enum) with impl Display dispatches through the writer pattern"
    (let c =
       Exile_lang.Compiler.compile
         "enum Tok { Num(int) | Plus | Minus }\n\
          impl Display for Tok {\n\
         \    fn fmt(*const self, out: *StringBuilder) {\n\
         \        match *self {\n\
         \            Tok::Num(n) => { out.push_str(\"Num(\"); out.push_int(n); out.push_str(\")\"); }\n\
         \            | Tok::Plus  => { out.push_str(\"+\"); }\n\
         \            | Tok::Minus => { out.push_str(\"-\"); }\n\
         \        }\n\
          }\n\
          }\n\
          fn main() {\n\
         \    let t = Tok::Num(42);\n\
         \    println(t);\n\
          }\n"
     in
     contains c "Tok__fmt(&t, &__disp_sb_"
     && contains c "String__build(__disp_sb_"
     && contains c "String__as_str(&__disp_s_"
     && contains c "String__free(__disp_s_");

  check_error "println(struct) without impl Display still rejected with helpful hint"
    "struct Pt { x: int }\n\
     fn main() { let p = Pt { x: 1 }; println(p); }\n"
    "cannot print a struct value (Pt); print individual fields, or mark the struct with `@debug`";

  (* FP-1 — `type Name<T...> = Type;` pure (transparent) alias.
     Substitution at `resolve_type_ann_raw`, before struct/enum
     lookup; tparams bound to call-site args; cycle-guard rejects
     `type A = B; type B = A;` deterministically; primitive names
     reserved (`type int = ...` errors). *)
  check_assert "type alias: `type NodeId = int` substitutes into let annotation"
    (try
       ignore (Exile_lang.Compiler.compile
         "type NodeId = int;\n\
          fn main() {\n\
         \    let x: NodeId = 42;\n\
         \    println(x);\n\
          }\n");
       true
     with _ -> false);

  check_assert "type alias: generic alias `type ParseRes<T> = Result<T, str>` resolves"
    (try
       ignore (Exile_lang.Compiler.compile
         "type ParseRes<T> = Result<T, str>;\n\
          fn make() -> ParseRes<int> { return Result::Ok(42); }\n\
          fn main() {\n\
         \    match make() {\n\
         \        Result::Ok(v) => { println(v); }\n\
         \        | Result::Err(_) => { println(0); }\n\
         \    }\n\
          }\n");
       true
     with _ -> false);

  check_error "type alias: `type A = B; type B = A;` cycle rejected"
    "type A = B;\n\
     type B = A;\n\
     fn main() {\n\
    \    let x: A = 0;\n\
    \    println(x);\n\
     }\n"
    "type alias cycle through 'A' — alias resolution would loop";

  check_error "type alias: shadowing a primitive type name rejected"
    "type int = u32;\n\
     fn main() { println(1); }\n"
    "'type int' shadows a built-in type — pick a different alias name";

  check_error "type alias: wrong generic arity rejected"
    "type Pair<A, B> = A;\n\
     fn main() { let _x: Pair<int> = 0; println(0); }\n"
    "type alias 'Pair' expects 2 generic argument(s), got 1";

  (* FP-2 — `let <refutable-pat> = expr else { divergent };` MVP:
     pattern is a single qualified variant ctor with flat-name
     binds; enum must have >=2 variants; else-block must diverge
     (return/break/continue).  Desugar: TLet / TLetTuple wrapping
     a TMatch whose success arm extracts the binds and whose
     wildcard arm emits the else stmts verbatim through the
     `gen_block` path (so TReturn flushes defers, TBreak/Continue
     reach their loop). *)
  check_assert "let-else: success path binds escape to enclosing scope"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn parse() -> Option<int> { return Option::Some(42); }\n\
          fn main() {\n\
         \    let Option::Some(v) = parse() else { return; };\n\
         \    println(v);\n\
          }\n");
       true
     with _ -> false);

  check_assert "let-else: multi-bind tuple variant extracts both names"
    (try
       ignore (Exile_lang.Compiler.compile
         "enum Pair { Two(int, int) | One(int) }\n\
          fn parse() -> Pair { return Pair::Two(7, 11); }\n\
          fn main() {\n\
         \    let Pair::Two(a, b) = parse() else { return; };\n\
         \    println(a + b);\n\
          }\n");
       true
     with _ -> false);

  check_error "let-else: else without divergence rejected"
    "fn parse() -> Option<int> { return Option::None; }\n\
     fn main() {\n\
    \    let Option::Some(v) = parse() else { println(0); };\n\
    \    println(v);\n\
     }\n"
    "let-else else-block must diverge (return / break / continue / never-returning fn)";

  check_error "let-else: single-variant enum rejected (else unreachable)"
    "enum One { Only(int) }\n\
     fn parse() -> One { return One::Only(7); }\n\
     fn main() {\n\
    \    let One::Only(v) = parse() else { return; };\n\
    \    println(v);\n\
     }\n"
    "let-else else-branch is unreachable: enum 'One' has only one variant — use a plain `let` instead";

  (* Receiver-mutability per design 2026-05-28.
     `*self` requires a mutable place; the auto-ref matrix rejects
     a `*const T` receiver outright (DECYZJA #2/#3).
     `*const self` stays callable on every shape — by-value
     bindings, mut bindings, `*T`, and `*const T` all auto-ref
     into TConstPtr. *)
  check_error "receiver-mutability: `*self` on immutable binding rejected"
    "struct C { n: int }\n\
     impl C {\n\
    \    pub fn bump(*self) { self.n = self.n + 1; }\n\
     }\n\
     fn main() {\n\
    \    let c = C { n: 0 };\n\
    \    c.bump();\n\
     }\n"
    "method 'C::bump' takes a mutable receiver (`*self`); the call expression is not a mutable place — declare the binding `let mut` (or mark the parameter `mut`), or use a `*const self` method if no mutation is needed";

  check_error "receiver-mutability: `*self` through `*const T` receiver rejected"
    "struct C { n: int }\n\
     impl C {\n\
    \    pub fn bump(*self) { self.n = self.n + 1; }\n\
     }\n\
     fn run(p: *const C) { p.bump(); }\n\
     fn main() {\n\
    \    let mut c = C { n: 0 };\n\
    \    run(&c);\n\
     }\n"
    "method 'C::bump' takes a mutable receiver (`*self`) but receiver is *const C (read-only) — call a `*const self` method, or pass a `*T` to the value";

  check_assert "receiver-mutability: `*self` on `let mut` binding compiles"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct C { n: int }\n\
          impl C {\n\
         \    pub fn bump(*self) { self.n = self.n + 1; }\n\
          }\n\
          fn main() {\n\
         \    let mut c = C { n: 0 };\n\
         \    c.bump();\n\
         \    println(c.n);\n\
          }\n");
       true
     with _ -> false);

  check_assert "receiver-mutability: `*const self` callable on `let` binding"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct C { n: int }\n\
          impl C {\n\
         \    pub fn read(*const self) -> int { return self.n; }\n\
          }\n\
          fn main() {\n\
         \    let c = C { n: 7 };\n\
         \    println(c.read());\n\
          }\n");
       true
     with _ -> false);

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
    "cannot cast *i32 to i32 (supported: int↔int, int↔float, float↔float, ptr↔ptr, int→ptr)";

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
  (* Same defect on the enum side: `struct H { o: Option<int> }`
     elaborated `Option<int>` against the placeholder enum_skeleton
     (variant names known, payloads still `[]`).  Without refreshing
     the cached enum instance after `build_enum_index` resolves the
     real variants, `Option::Some(42)` was rejected with "variant
     takes 0 argument(s), got 1". *)
  check "Option<int> as a struct field — variant payloads refreshed post-resolve"
    "struct H { o: Option<int> }\n\
     fn main() {\n\
    \    let h: H = H { o: Option::Some(42) };\n\
    \    match h.o {\n\
    \        Option::Some(v) => { println(v); }\n\
    \        | Option::None    => { println(0); }\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\nstruct ex_H { struct ex_Option_i32 o; };\n\nint main(void) {\n    struct ex_H h;\n    struct ex_Option_i32 __lift_0;\n    __lift_0.tag = ex_Option_i32_Some;\n    __lift_0.data.Some._0 = 42;\n    h.o = __lift_0;\n    {\n        struct ex_Option_i32 __m;\n        __m = h.o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long v = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

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

  (* DR-011 sub-slicing — `a[lo..hi]` / `a[lo..=hi]` on an array or
     existing Slice produces a fresh `Slice<T>` view rather than a
     scalar element.  Length is `hi - lo` (or `hi - lo + 1` for
     inclusive).  Bounds-check is omitted in v1, consistent with the
     rest of `[i]`. *)
  check_assert "DR-011: sub-slice of an array produces a Slice<T> with ptr+len"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let arr: [int; 5] = [10, 20, 30, 40, 50];\n\
         \    let s = arr[1..4];\n\
         \    println(s.len as int);\n\
         \    println(s[0]);\n\
         \    println(s[2]);\n\
          }\n"
     in
     contains c "s.ptr = &(arr.data[1])");

  check_assert "DR-011: sub-slice with `..=` is one element longer than `..`"
    (let c_excl =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let arr: [int; 4] = [1, 2, 3, 4];\n\
         \    let s = arr[0..2];\n\
         \    println(s.len as int);\n\
          }\n"
     and c_incl =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let arr: [int; 4] = [1, 2, 3, 4];\n\
         \    let s = arr[0..=2];\n\
         \    println(s.len as int);\n\
          }\n"
     in
     (* Inclusive form emits an explicit `+ 1` somewhere in len computation. *)
     contains c_incl "+ 1"
     && not (contains c_excl "+ 1"));

  check_assert "DR-011: sub-slice of an existing Slice produces a nested Slice"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let arr: [int; 6] = [0, 1, 2, 3, 4, 5];\n\
         \    let s1: Slice<int> = arr[0..6];\n\
         \    let s2 = s1[2..5];\n\
         \    println(s2.len as int);\n\
         \    println(s2[0]);\n\
          }\n"
     in
     contains c "s2.ptr = &(s1.ptr[2])");

  check_assert "DR-011: sub-slice as fn arg type-checks (Slice<int> param)"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn first(s: Slice<int>) -> int { s[0] }\n\
          fn main() {\n\
         \    let arr: [int; 4] = [1, 2, 3, 4];\n\
         \    println(first(arr[1..4]));\n\
          }\n");
       true
     with _ -> false);

  check_error "DR-011: indexing with a non-integer non-Range rejected"
    "fn main() {\n\
    \    let arr: [int; 4] = [1, 2, 3, 4];\n\
    \    let s = arr[true];\n\
    \    println(s);\n\
     }\n"
    "index must be an integer or a Range, got bool";

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
    "or-pattern only allowed at the top of a match arm (nested `pat1 | pat2` inside a variant bind is not supported yet)";

  (* ===== DR-014 generic trait methods =====
     `fn map<F>( *const self, ...)` and friends.  Empirically zero
     perf overhead (mono direct-call same as generic free fns), so
     this is purely a scope-cut from trait-step-1.  Pre-req for
     iterator combinators (DR-015) but useful on its own. *)

  check_assert "DR-014: generic trait method called with T inferred from arg"
    (try
       ignore (Exile_lang.Compiler.compile
         "trait Boxed { fn echo<T>(*const self, x: T) -> T; }\n\
          struct A {}\n\
          impl Boxed for A { fn echo<T>(*const self, x: T) -> T { x } }\n\
          fn main() { let a = A {}; println(a.echo(7)); }\n");
       true
     with _ -> false);

  check_assert "DR-014: generic default method synthesises on a bare impl"
    (try
       ignore (Exile_lang.Compiler.compile
         "trait Boxed { fn echo<T>(*const self, x: T) -> T { x } }\n\
          struct A {}\n\
          impl Boxed for A {}\n\
          fn main() { let a = A {}; println(a.echo(99)); }\n");
       true
     with _ -> false);

  check_assert "DR-014: bound on method tparam dispatches through the bound"
    (let c =
       Exile_lang.Compiler.compile
         "trait Shape { fn area(*const self) -> int; }\n\
          trait Apply { fn run<S: Shape>(*const self, s: S) -> int; }\n\
          struct Circle { r: int }\n\
          impl Shape for Circle { fn area(*const self) -> int { self.r * self.r * 3 } }\n\
          struct Runner {}\n\
          impl Apply for Runner {\n\
         \    fn run<S: Shape>(*const self, s: S) -> int { s.area() }\n\
          }\n\
          fn main() { let r = Runner {}; println(r.run(Circle { r: 5 })); }\n"
     in
     (* Mono produces a per-call instance that direct-calls
        Circle::area — no vtable, no indirection. *)
     contains c "Runner__run_ex_Circle" && contains c "Circle__area");

  check_error "DR-014: arity mismatch between trait and impl rejected"
    "trait Boxed { fn echo<T>(*const self, x: T) -> T; }\n\
     struct A {}\n\
     impl Boxed for A {\n\
    \    fn echo(*const self, x: int) -> int { x }\n\
     }\n\
     fn main() {}\n"
    "method 'echo' has 0 type parameter(s) but trait 'Boxed' declares 1";

  (* ===== DR-016 bounded generic impls =====

     `impl<T: Bound>` was a gap in the trait system: only free fns
     could carry tparam bounds, so a generic impl over a wrapper
     couldn't require its T to satisfy a trait.  This blocked the
     final lazy-iterator pattern (`impl<I: Iterator, F: Fn1>
     Iterator for Map<I, F>`).  The fix is purely structural — the
     parser already had `parse_tparams_bounded`, the bound check
     already fired at instantiation time on `func.tbounds`; this
     adds `impl_block.itbounds` and splices it into the lifted
     method's tbounds so the same check covers both. *)

  check_assert "DR-016: impl<T: Bound> body can call trait method on T"
    (try
       ignore (Exile_lang.Compiler.compile
         "trait Show { fn show(*const self) -> int; }\n\
          struct Inner { x: int }\n\
          impl Show for Inner { fn show(*const self) -> int { self.x } }\n\
          struct Box<T> { v: T }\n\
          impl<T: Show> Show for Box<T> {\n\
         \    fn show(*const self) -> int { self.v.show() + 1 }\n\
          }\n\
          fn main() {\n\
         \    let b = Box { v: Inner { x: 41 } };\n\
         \    println(b.show());\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-016: multiple bounds on one tparam (T: A + B)"
    (try
       ignore (Exile_lang.Compiler.compile
         "trait Show { fn show(*const self) -> int; }\n\
          trait Tag { fn tag(*const self) -> int; }\n\
          struct Foo { x: int }\n\
          impl Show for Foo { fn show(*const self) -> int { self.x } }\n\
          impl Tag for Foo { fn tag(*const self) -> int { 99 } }\n\
          struct Pair<T> { a: T, b: T }\n\
          impl<T: Show + Tag> Show for Pair<T> {\n\
         \    fn show(*const self) -> int { self.a.show() + self.b.tag() }\n\
          }\n\
          fn main() {\n\
         \    let p = Pair { a: Foo { x: 1 }, b: Foo { x: 2 } };\n\
         \    println(p.show());\n\
          }\n");
       true
     with _ -> false);

  check_error "DR-016: bound rejects type that does not implement the trait"
    "trait Show { fn show(*const self) -> int; }\n\
     struct Bare { x: int }\n\
     struct Box<T> { v: T }\n\
     impl<T: Show> Show for Box<T> {\n\
    \    fn show(*const self) -> int { self.v.show() + 1 }\n\
     }\n\
     fn main() {\n\
    \    let b = Box { v: Bare { x: 7 } };\n\
    \    println(b.show());\n\
     }\n"
    "type 'Bare' does not implement trait 'Show' (required by bound 'T: Show' on 'show')";

  check_assert "DR-016: impl<T> without bound still compiles (regression)"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Box<T> { v: T }\n\
          impl<T> Box<T> {\n\
         \    pub fn into_inner(self) -> T { self.v }\n\
          }\n\
          fn main() {\n\
         \    let b = Box { v: 42 };\n\
         \    println(b.into_inner());\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-016: bound on one of multiple impl tparams"
    (try
       ignore (Exile_lang.Compiler.compile
         "trait Show { fn show(*const self) -> int; }\n\
          struct N { v: int }\n\
          impl Show for N { fn show(*const self) -> int { self.v } }\n\
          struct Cell<K, V> { key: K, val: V }\n\
          impl<K, V: Show> Show for Cell<K, V> {\n\
         \    fn show(*const self) -> int { self.val.show() }\n\
          }\n\
          fn main() {\n\
         \    let c = Cell { key: 7, val: N { v: 100 } };\n\
         \    println(c.show());\n\
          }\n");
       true
     with _ -> false);

  (* ===== DR-017 Fn-trait prelude + mono-instance trait recognition =====

     Two related ships bundled in one commit because they jointly unlock
     the DR-015 lazy adapter pattern (`Map<I: Iterator, F: Fn1>`):

     1. `type_impls_trait` (typecheck.ml:1166) used exact `List.mem` on
        the (trait, target-path) registry, so a mono-instance receiver
        like `VecIter_i32` failed to match the skeleton-keyed entry
        `("Iterator", ["VecIter"])`.  Same fix the `for x in iter`
        desugar already had — match via `Mono.is_instance_of`.  This
        is a pre-existing bug; `<I: Iterator>` bound on any free fn
        called with `v.iter()` errored "VecIter_i32 does not implement
        Iterator" before this fix.

     2. Fn1 / Fn2 traits land in the prelude (per DR-015 reality-check
        2026-06-04: Fn = real per-arity trait with associated Arg /
        Output types, not a generic-trait surface).  Body shape is
        `trait FnN { type Arg{1..N}; type Output;
        fn call(self-const-ptr, args...) -> Self::Output }`.  User
        writes `impl Fn1 for AddOne { type Arg = int; type Output = int;
        fn call(self-const-ptr, a: int) -> int { a + 1 } }` then can be
        passed through a `<F: Fn1>`-bounded free fn or adapter
        struct. *)

  check_assert "DR-017: <I: Iterator> bound accepts mono Vec iterator"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn count_iter<I: Iterator>(mut it: I) -> int {\n\
         \    let mut n: int = 0;\n\
         \    for _x in it { n = n + 1; }\n\
         \    n\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20); v.push(30);\n\
         \    println(count_iter(v.iter()));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-017: prelude Fn1 trait + assoc-type projection through bound"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn apply<F: Fn1>(f: F, x: F::Arg) -> F::Output { f.call(x) }\n\
          fn main() {\n\
         \    let a = AddOne { _tag: 0 };\n\
         \    println(apply(a, 41));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-017: prelude Fn2 trait with two arg associated types"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Add { _tag: int }\n\
          impl Fn2 for Add {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int, b: int) -> int { a + b }\n\
          }\n\
          fn apply2<F: Fn2>(f: F, x: F::Arg1, y: F::Arg2) -> F::Output {\n\
         \    f.call(x, y)\n\
          }\n\
          fn main() {\n\
         \    let f = Add { _tag: 0 };\n\
         \    println(apply2(f, 13, 29));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-018: f(x) sugar on Fn1-bound tparam desugars to f.call(x)"
    (let c =
       Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn apply<F: Fn1>(f: F, x: F::Arg) -> F::Output { f(x) }\n\
          fn main() { let a = AddOne { _tag: 0 }; println(apply(a, 41)); }\n"
     in
     contains c "AddOne__call");

  check_assert "DR-018: f(x, y) sugar on Fn2-bound tparam"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Add { _tag: int }\n\
          impl Fn2 for Add {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int, b: int) -> int { a + b }\n\
          }\n\
          fn apply2<F: Fn2>(f: F, x: F::Arg1, y: F::Arg2) -> F::Output {\n\
         \    f(x, y)\n\
          }\n\
          fn main() {\n\
         \    let f = Add { _tag: 0 };\n\
         \    println(apply2(f, 13, 29));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-018: f(x) sugar on concrete struct with Fn1 impl"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn main() {\n\
         \    let f = AddOne { _tag: 0 };\n\
         \    println(f(41));\n\
          }\n");
       true
     with _ -> false);

  check_error "DR-018: x(5) on non-Fn type stays an 'unknown function' error"
    "fn main() { let x: int = 10; println(x(5)); }\n"
    "unknown function 'x'";

  check_assert "DR-019: h.f(v) sugar on field carrying Fn1-impl struct"
    (let c =
       Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          struct Holder { f: AddOne }\n\
          fn main() {\n\
         \    let h = Holder { f: AddOne { _tag: 0 } };\n\
         \    println(h.f(41));\n\
          }\n"
     in
     contains c "AddOne__call");

  check_assert "DR-019: self.f(v) sugar in Map<I, F>::next adapter body"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          struct Map<I, F> { inner: I, f: F }\n\
          impl<I: Iterator, F: Fn1> Iterator for Map<I, F> {\n\
         \    type Item = F::Output;\n\
         \    fn next(*self) -> Option<F::Output> {\n\
         \        let v = try self.inner.next();\n\
         \        Option::Some(self.f(v))\n\
         \    }\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10);\n\
         \    let m: Map<VecIter<int>, AddOne> =\n\
         \        Map { inner: v.iter(), f: AddOne { _tag: 0 } };\n\
         \    let mut acc: int = 0;\n\
         \    for x in m { acc = acc + x; }\n\
         \    println(acc);\n\
          }\n");
       true
     with _ -> false);

  check_error "DR-019: non-Fn field still errors 'no method'"
    "struct Holder { v: int }\n\
     fn main() {\n\
    \    let h = Holder { v: 10 };\n\
    \    println(h.v(5));\n\
     }\n"
    "no method 'v' on type 'Holder'";

  check_assert "DR-020: Option::None in match arm picks up TEnumApp expected"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          struct Map<I, F> { inner: I, f: F }\n\
          impl<I: Iterator, F: Fn1> Iterator for Map<I, F> {\n\
         \    type Item = F::Output;\n\
         \    fn next(*self) -> Option<F::Output> {\n\
         \        match self.inner.next() {\n\
         \            Option::Some(v) => Option::Some(self.f(v))\n\
         \            | Option::None => Option::None\n\
         \        }\n\
         \    }\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20);\n\
         \    let m: Map<VecIter<int>, AddOne> =\n\
         \        Map { inner: v.iter(), f: AddOne { _tag: 0 } };\n\
         \    let mut acc: int = 0;\n\
         \    for x in m { acc = acc + x; }\n\
         \    println(acc);\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-021: |A|->R bound sugar lowers to Fn1 + assoc bindings"
    (let c =
       Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn apply<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() { let a = AddOne { _tag: 0 }; println(apply(a, 41)); }\n"
     in
     contains c "AddOne__call");

  check_assert "DR-021: |A, B|->R bound sugar lowers to Fn2 with Arg1/Arg2"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Add { _tag: int }\n\
          impl Fn2 for Add {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int, b: int) -> int { a + b }\n\
          }\n\
          fn apply2<F: |int, int|->int>(f: F, x: int, y: int) -> int {\n\
         \    f(x, y)\n\
          }\n\
          fn main() {\n\
         \    let f = Add { _tag: 0 };\n\
         \    println(apply2(f, 13, 29));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-021: F::Arg / F::Output project via bound assoc shortcut"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn apply<F: |int|->int>(f: F, x: F::Arg) -> F::Output { f(x) }\n\
          fn main() { let a = AddOne { _tag: 0 }; println(apply(a, 41)); }\n");
       true
     with _ -> false);

  check_assert "DR-023: ||->R bound sugar lowers to Fn0 + Output assoc"
    (let c =
       Exile_lang.Compiler.compile
         "struct Forty { _tag: int }\n\
          impl Fn0 for Forty {\n\
         \    type Output = int;\n\
         \    fn call(*const self) -> int { 42 }\n\
          }\n\
          fn run<F: ||->int>(f: F) -> int { f() }\n\
          fn main() { let f = Forty { _tag: 0 }; println(run(f)); }\n"
     in
     contains c "Forty__call");

  check_assert "DR-023: F::Output projects via bound on Fn0"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Forty { _tag: int }\n\
          impl Fn0 for Forty {\n\
         \    type Output = int;\n\
         \    fn call(*const self) -> int { 42 }\n\
          }\n\
          fn run<F: ||->int>(f: F) -> F::Output { f() }\n\
          fn main() { let f = Forty { _tag: 0 }; println(run(f)); }\n");
       true
     with _ -> false);

  check_error "DR-023: Fn0 Output mismatch rejected at call site"
    "struct Forty { _tag: int }\n\
     impl Fn0 for Forty {\n\
    \    type Output = u32;\n\
    \    fn call(*const self) -> u32 { 42 as u32 }\n\
     }\n\
     fn run<F: ||->int>(f: F) -> int { f() }\n\
     fn main() { let f = Forty { _tag: 0 }; println(run(f)); }\n"
    "bound 'F: Fn0' on 'run' requires 'F::Output = i32' but type 'Forty' has 'Fn0::Output = u32'";

  (* DR-024 A2 closures with capture inference.  expand_lambdas
     synthesises an env-struct + auto-impl Fn{arity} when a lambda
     references locals; capture types come from the surrounding fn's
     params or annotated lets. *)

  check_assert "DR-024: A2 single-capture from annotated let"
    (let c =
       Exile_lang.Compiler.compile
         "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    let t: int = 10;\n\
         \    let f = |x: int| -> int x + t;\n\
         \    println(run(f, 5));\n\
          }\n"
     in
     contains c "__closure_0__call" || contains c "__closure_0");

  check_assert "DR-024: A2 multi-capture from annotated lets"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    let a: int = 10;\n\
         \    let b: int = 20;\n\
         \    let f = |x: int| -> int x + a + b;\n\
         \    println(run(f, 5));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-024: A2 binary lambda capturing → Fn2 impl"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn run<F: |int, int|->int>(f: F, x: int, y: int) -> int {\n\
         \    f(x, y)\n\
          }\n\
          fn main() {\n\
         \    let base: int = 100;\n\
         \    let g = |x: int, y: int| -> int x + y + base;\n\
         \    println(run(g, 3, 4));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-024: lambda capturing fn param compiles"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn caller(base: int) -> int {\n\
         \    let f = |x: int| -> int x + base;\n\
         \    run(f, 5)\n\
          }\n\
          fn main() { println(caller(100)); }\n");
       true
     with _ -> false);

  check_assert "DR-024: captureless lambda still takes A1 fn-ptr path"
    (let c =
       Exile_lang.Compiler.compile
         "fn run(f: fn(int) -> int, x: int) -> int { f(x) }\n\
          fn main() { println(run(|x: int| -> int x + 1, 41)); }\n"
     in
     contains c "__lambda_0");

  (* DR-025 — trait-decl assoc shortcut.  When a tparam carries a
     bound `<F: Trait>` and the trait declares `assoc` in its
     `trassoc`, `F::assoc` projects through the bound's trait decl
     even when no `impl Trait for X` is registered yet.  Prereq for
     prelude-synthesised adapter impls (combinator-stdlib) where
     impl-side entries land later in user code. *)

  check_assert "DR-025: F::Output compiles with NO impl Fn1 anywhere"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn echo<F: Fn1>(f: F, x: F::Arg) -> F::Output { f(x) }\n\
          fn main() { println(42); }\n");
       true
     with _ -> false);

  check_assert "DR-025: F::Arg + F::Output usable in fn sig without impl in scope"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Wrap<F> { inner: F }\n\
          fn make<F: Fn1>(f: F) -> Wrap<F> { Wrap { inner: f } }\n\
          fn main() { println(42); }\n");
       true
     with _ -> false);

  (* DR-026 combinator stdlib v1 — Step A: prelude Map<I, F> adapter
     + impl<I: Iterator, F: Fn1> Iterator for Map<I, F>.  Manual
     construction shape; the .map() dot-chain ships in Step B. *)

  check_assert "DR-026 A: Map<I, F> manual construction iterates user Fn1 impl"
    (let c =
       Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20); v.push(30);\n\
         \    let m: Map<VecIter<int>, AddOne> =\n\
         \        Map { inner: v.iter(), f: AddOne { _tag: 0 } };\n\
         \    let mut acc: int = 0;\n\
         \    for x in m { acc = acc + x; }\n\
         \    println(acc);\n\
          }\n"
     in
     (* Verify the Map<VecIter_i32, AddOne> instance was mono'd and
        AddOne's call dispatches directly inside Map's next. *)
     contains c "AddOne__call");

  check_assert "DR-026 A: Map<I, F> chains with A2 captured closure"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20);\n\
         \    let bump: int = 1;\n\
         \    let m = Map { inner: v.iter(), f: |x: int| -> int x + bump };\n\
         \    let mut acc: int = 0;\n\
         \    for x in m { acc = acc + x; }\n\
         \    println(acc);\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-026 B: v.iter().map(closure) dot-chain (Iterator.map default)"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20); v.push(30);\n\
         \    let bump: int = 1;\n\
         \    let m = v.iter().map(|x: int| -> int x + bump);\n\
         \    let mut acc: int = 0;\n\
         \    for x in m { acc = acc + x; }\n\
         \    println(acc);\n\
          }\n");
       true
     with _ -> false);

  (* DR-027 - bound-order independence in tbound resolution.  When
     `<I: Iterator, P: |I::Item|->bool>` lists I before P, the
     resolver now sees I's Iterator bound in ctx.tbounds when
     resolving P's assoc binding `I::Item`.  Pre-fix the resolution
     iterated all bounds with the same empty ctx.tbounds, so
     `I::Item` inside P's bound was looked up before I's Iterator
     bound was visible - "unknown type 'I::Item'" even when the
     same code shape worked on free fns. *)
  check_assert "DR-027: |I::Item|->bool bound parses + resolves on free fn"
    (try
       ignore (Exile_lang.Compiler.compile
         "trait Has { type Item; }\n\
          struct Box {}\n\
          impl Has for Box { type Item = int; }\n\
          fn id<I: Has, P: |I::Item|->bool>(_x: int) -> int { 0 }\n\
          fn main() { println(42); }\n");
       true
     with _ -> false);

  (* DR-028 - `|A|->R` / `||->R` / `|A, B|->R` in type-ann position
     is sugar for `fn(...) -> R` (TyFnPtr).  Mirror of the DR-021
     sugar in bound position, but with concrete fn-ptr semantics
     here (the bound form is an existential `<F: Fn{N}<Arg=A,
     Output=R>>`).  Captureless lambdas decay through the A1
     pathway so `let f: |int|->int = |x| x + 1` works; a captured
     closure has its own struct type and would error - correct
     because a fn-ptr slot can't hold an env-struct. *)

  check_assert "DR-028: |int|->int let-ann sugar = fn(int) -> int"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn inc(x: int) -> int { x + 1 }\n\
          fn main() {\n\
         \    let f: |int|->int = inc;\n\
         \    println(f(41));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-028: ||->R zero-arg sugar"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn forty_two() -> int { 42 }\n\
          fn main() {\n\
         \    let f: ||->int = forty_two;\n\
         \    println(f());\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-028: |A, B|->R multi-arg sugar in fn parameter"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn run(f: |int, int|->int, a: int, b: int) -> int { f(a, b) }\n\
          fn add(a: int, b: int) -> int { a + b }\n\
          fn main() { println(run(add, 13, 29)); }\n");
       true
     with _ -> false);

  check_assert "DR-028: captureless lambda decays into |int|->int slot"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let f: |int|->int = |x: int| -> int x * 2;\n\
         \    println(f(21));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-028: |A|->R sugar in a struct field type"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Hooks { on_x: |int|->int }\n\
          fn inc(x: int) -> int { x + 1 }\n\
          fn main() {\n\
         \    let h = Hooks { on_x: inc };\n\
         \    println(h.on_x(41));\n\
          }\n");
       true
     with _ -> false);

  (* DR-029 - prelude Fn3 / Fn4 traits.  Parser was already
     arity-agnostic (`Fn{N}` + `Arg{N}` numbering); shipping the
     trait declarations lights the bound sugar up for arities 3
     and 4, plus the DR-024 A2 closure synthesis (which picks
     Fn{arity} from the lambda's param count) reaches further. *)

  check_assert "DR-029: Fn3 prelude trait + |A, B, C|->R bound dispatch"
    (let c =
       Exile_lang.Compiler.compile
         "struct Triple { _tag: int }\n\
          impl Fn3 for Triple {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Arg3 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int, b: int, c: int) -> int { a + b + c }\n\
          }\n\
          fn apply3<F: |int, int, int|->int>(f: F, x: int, y: int, z: int) -> int { f(x, y, z) }\n\
          fn main() {\n\
         \    let f = Triple { _tag: 0 };\n\
         \    println(apply3(f, 10, 13, 19));\n\
          }\n"
     in
     contains c "Triple__call");

  check_assert "DR-029: Fn4 prelude trait + |A, B, C, D|->R bound dispatch"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct Quad { _tag: int }\n\
          impl Fn4 for Quad {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Arg3 = int;\n\
         \    type Arg4 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int, b: int, c: int, d: int) -> int { a + b + c + d }\n\
          }\n\
          fn apply4<F: |int, int, int, int|->int>(f: F, w: int, x: int, y: int, z: int) -> int { f(w, x, y, z) }\n\
          fn main() {\n\
         \    let f = Quad { _tag: 0 };\n\
         \    println(apply4(f, 5, 10, 13, 14));\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-029: A2 captured closure with 3 params synthesises Fn3 impl"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn apply3<F: |int, int, int|->int>(f: F, x: int, y: int, z: int) -> int { f(x, y, z) }\n\
          fn main() {\n\
         \    let base: int = 100;\n\
         \    let g = |a: int, b: int, c: int| -> int a + b + c + base;\n\
         \    println(apply3(g, 13, 19, 10));\n\
          }\n");
       true
     with _ -> false);

  (* DR-031 - `new Path::Variant(args)` heap-boxes an enum tuple-
     variant.  Faithful OCaml-variant -> Exile-enum-AST port needs
     this for recursive enum trees (the Add variant carries two
     pointer-to-Expr payloads).  Parser dispatches on `(` vs `{`
     after `new <path>`; the elab path reuses EnumLit machinery and
     rewraps the result IR as TNewEnum with `*Enum` type; codegen
     emits malloc + writes through `->`. *)

  check_assert "DR-031: new Enum::Variant(args) heap-boxes single tuple-variant"
    (try
       ignore (Exile_lang.Compiler.compile
         "enum E { Lit(int) | Add(int, int) }\n\
          fn main() {\n\
         \    let p = new E::Add(13, 29);\n\
         \    match *p {\n\
         \        E::Lit(n) => println(n)\n\
         \        | E::Add(a, b) => println(a + b)\n\
         \    }\n\
         \    free(p);\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-031: recursive enum AST via *const Self payload"
    (try
       ignore (Exile_lang.Compiler.compile
         "enum Expr { Lit(int) | Add(*const Expr, *const Expr) }\n\
          fn eval(e: *const Expr) -> int {\n\
         \    match *e {\n\
         \        Expr::Lit(n) => n\n\
         \        | Expr::Add(a, b) => eval(a) + eval(b)\n\
         \    }\n\
          }\n\
          fn main() {\n\
         \    let l = new Expr::Lit(13);\n\
         \    let r = new Expr::Lit(29);\n\
         \    let s = new Expr::Add(l, r);\n\
         \    println(eval(s));\n\
         \    free(s); free(r); free(l);\n\
          }\n");
       true
     with _ -> false);

  check_error "DR-031: `new Path` without ( or { rejected"
    "enum E { Z }\n\
     fn main() { let _p = new E::Z; }\n"
    "expected '{' (struct heap-init) or '(' (enum tuple-variant heap-box) \
     after 'new E::Z', got ';'";

  (* DR-032 - prelude `sys::sys_open` + `sys::sys_close` extern decls.
     The host-backend wraps libc `open`/`close`; the amiga-backend
     stubs to -1 until BPTR<->fd bookkeeping lands.  Future self-host
     module-loading (`use foo;` resolution) reads source files via
     this seam.  These tests cover the prelude wiring; the runtime
     thunks are covered by the host-target verify-host run. *)

  check_assert "DR-032: sys::sys_open is in scope with extern c_int signature"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let p = \"/tmp/__exilc_dr032_unused\" as *const c_char;\n\
         \    let _fd = sys::sys_open(p, 0 as c_int);\n\
         \    println(0);\n\
          }\n"
     in
     contains c "sys_open");

  check_assert "DR-032: sys::sys_close takes c_int fd and returns c_int"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let _r: c_int = sys::sys_close(0 as c_int);\n\
         \    println(0);\n\
          }\n"
     in
     contains c "sys_close");

  (* DR-033 - `[&x] |y| body` explicit by-ref capture list.  Lowers
     a capture from by-value (env field T) to by-reference (env
     field *const T); each body reference to x desugars to *self.x.
     Captures not listed keep the implicit by-value path.  Escape-
     pass DR-010 owners_of picks up the ptr-field rooted in x for
     free, so returning such a closure trips the existing S5a
     reject. *)
  check_assert "DR-033: [&n] |x: int| x + n lowers n to const ptr in env"
    (let c =
       Exile_lang.Compiler.compile
         "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    let n: int = 10;\n\
         \    let add_n = [&n] |x: int| -> int x + n;\n\
         \    println(run(add_n, 5));\n\
          }\n"
     in
     contains c "const long *n");

  check_assert "DR-033: by-ref struct capture lowers to ptr field"
    (let c =
       Exile_lang.Compiler.compile
         "struct Ctx { field: int }\n\
          fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    let c: Ctx = Ctx { field: 100 };\n\
         \    let f = [&c] |x: int| -> int c.field + x;\n\
         \    println(run(f, 3));\n\
          }\n"
     in
     contains c "const struct ex_Ctx *c");

  check_assert "DR-033: mixed by-ref + implicit by-value compiles"
    (let c =
       Exile_lang.Compiler.compile
         "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() {\n\
         \    let a: int = 20;\n\
         \    let b: int = 5;\n\
         \    let f = [&a] |x: int| -> int x + a + b;\n\
         \    println(run(f, 1));\n\
          }\n"
     in
     (* env-struct has `const long *a;` (by-ref) and plain `long b;`. *)
     contains c "const long *a" && contains c "long b");

  check_error "DR-033: by-ref capture of name not in scope rejected"
    "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
     fn main() {\n\
    \    let f = [&missing] |x: int| -> int x + 1;\n\
    \    println(run(f, 0));\n\
     }\n"
    "by-ref capture `&missing`: name not in scope at lambda \
     (captures must be fn params or type-annotated lets)";

  check_error "DR-033: by-ref capture not referenced in body rejected"
    "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
     fn main() {\n\
    \    let n: int = 10;\n\
    \    let f = [&n] |x: int| -> int x + 1;\n\
    \    println(run(f, 0));\n\
     }\n"
    "by-ref capture `&n` has no reference in lambda body";

  check_error "DR-033: plain `[t]` (no `&`) before lambda rejected"
    "fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
     fn main() {\n\
    \    let t: int = 1;\n\
    \    let f = [t] |x: int| -> int x + t;\n\
    \    println(run(f, 0));\n\
     }\n"
    "capture list before lambda must contain only `&name` items \
     (by-value captures are implicit; only `&name` belongs in \
     a capture list)";

  check_error "DR-033: closure with by-ref capture is type-distinct from raw struct"
    "struct C0 { p: *const int }\n\
     fn run<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
     fn make() -> C0 {\n\
    \    let n: int = 5;\n\
    \    let f = [&n] |x: int| -> int x + n;\n\
    \    f\n\
     }\n\
     fn main() { println(run(make(), 1)); }\n"
    "trailing expression: expected C0, got __closure_0";

  (* DR-026 Step D - `Take<I>` adapter struct + `Iterator.take(n)`
     default-method + `Enumerate<I>` adapter struct +
     `Iterator.enumerate()` default-method.  Both ride the DR-027
     site-1 multi-hop assoc-projection fix (skeleton-tparam subst in
     normalize_apps): `Take::Item` and `Enumerate::Item`'s inner
     `I::Item` projects through every concrete iterator at use site.
     Take is single-tparam (Self pinned by impl target), so mono
     instantiates eagerly per Iterator-implementor.  Enumerate ships
     a `(u32, I::Item)` tuple item — the same single-hop machinery
     handles tuple sub-types unchanged. *)
  check_assert "DR-026 Step D: Take<I> compiles + take(3) sums first three"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(1); v.push(2); v.push(3); v.push(4);\n\
         \    let mut s: int = 0;\n\
         \    for x in v.iter().take(3 as u32) { s = s + x; }\n\
         \    println(s);\n\
          }\n"
     in
     contains c "Take_ex_VecIter_i32"
     && contains c "remaining");

  check_assert "DR-026 Step D: Enumerate<I> compiles + emits (u32, Item) tuple"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20);\n\
         \    let mut s: int = 0;\n\
         \    for pair in v.iter().enumerate() {\n\
         \        let (i, x) = pair;\n\
         \        s = s + (i as int) + x;\n\
         \    }\n\
         \    println(s);\n\
          }\n"
     in
     contains c "Enumerate_ex_VecIter_i32"
     && contains c "idx");

  check_assert "DR-026 Step D: take(n).enumerate() chain mono-inlines"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(100); v.push(200); v.push(300);\n\
         \    let mut s: int = 0;\n\
         \    for pair in v.iter().take(2 as u32).enumerate() {\n\
         \        let (i, x) = pair;\n\
         \        s = s + (i as int) * 10 + x;\n\
         \    }\n\
         \    println(s);\n\
          }\n"
     in
     (* Take pinned by VecIter_i32 then Enumerate pinned by Take. *)
     contains c "Take_ex_VecIter_i32"
     && contains c "Enumerate_ex_Take_ex_VecIter_i32");

  check_assert "DR-026 Step D: manual Take constructor works"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(7); v.push(8); v.push(9);\n\
         \    let t: Take<VecIter<int>> =\n\
         \        Take { inner: v.iter(), remaining: 2 as u32 };\n\
         \    let mut s: int = 0;\n\
         \    for x in t { s = s + x; }\n\
         \    println(s);\n\
          }\n"
     in
     contains c "Take_ex_VecIter_i32");

  (* DR-026 Step E - `Iterator.fold(init, f)` and
     `Iterator.collect(a)` consuming terminals.  `fold` walks the
     iterator into a single accumulator via `Fn2.call(acc, x)`;
     `collect` drains the iterator into a fresh `Vec<Self::Item>`.
     Both consume `self` by value.

     Method-level tparams use Acc/G (fold) to avoid name clashes
     with impl-level tparams like Map<I, F> - the previous fold
     definition with method-tparam `F` made dot-chained `map(...).
     fold(...)` collapse the two `F`s during inference. *)
  check_assert "DR-026 Step E: fold(0, sum) collapses iterator to scalar"
    (let c =
       Exile_lang.Compiler.compile
         "struct Sum { _t: int }\n\
          impl Fn2 for Sum {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, acc: int, x: int) -> int { acc + x }\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(1); v.push(2); v.push(3);\n\
         \    let r: int = v.iter().fold(0, Sum { _t: 0 });\n\
         \    println(r);\n\
          }\n"
     in
     (* fold gets monomorphised per (Self, Acc, G) shape. *)
     contains c "VecIter__fold_i32_i32_ex_Sum");

  check_assert "DR-026 Step E: collect(a) drains iterator into Vec<Item>"
    (let c =
       Exile_lang.Compiler.compile
         "fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(1); v.push(2);\n\
         \    let out: Vec<int> = v.iter().collect(a);\n\
         \    println(out.len());\n\
          }\n"
     in
     contains c "VecIter__collect_i32");

  check_assert "DR-026 Step E: take(n).map(f).fold(init, g) chain fuses"
    (let c =
       Exile_lang.Compiler.compile
         "struct Sum { _t: int }\n\
          impl Fn2 for Sum {\n\
         \    type Arg1 = int;\n\
         \    type Arg2 = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, acc: int, x: int) -> int { acc + x }\n\
          }\n\
          struct Double { _t: int }\n\
          impl Fn1 for Double {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, x: int) -> int { x + x }\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(1); v.push(2); v.push(3); v.push(4);\n\
         \    let r: int =\n\
         \        v.iter().take(2 as u32)\n\
         \                .map(Double { _t: 0 })\n\
         \                .fold(0, Sum { _t: 0 });\n\
         \    println(r);\n\
          }\n"
     in
     (* Take and Map adapters both pinned, fold consumes the chain. *)
     contains c "Take_ex_VecIter_i32"
     && contains c "Map_ex_Take_ex_VecIter_i32_ex_Double");

  check_error "DR-022: bound assoc mismatch rejected at call site (Output)"
    "struct AddOne { _tag: int }\n\
     impl Fn1 for AddOne {\n\
    \    type Arg = int;\n\
    \    type Output = u32;\n\
    \    fn call(*const self, a: int) -> u32 { a as u32 + 1 as u32 }\n\
     }\n\
     fn apply<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
     fn main() {\n\
    \    let a = AddOne { _tag: 0 };\n\
    \    println(apply(a, 41));\n\
     }\n"
    "bound 'F: Fn1' on 'apply' requires 'F::Output = i32' but type 'AddOne' has 'Fn1::Output = u32'";

  check_error "DR-022: bound assoc mismatch rejected at call site (Arg)"
    "struct Sink { _tag: int }\n\
     impl Fn1 for Sink {\n\
    \    type Arg = u32;\n\
    \    type Output = int;\n\
    \    fn call(*const self, _a: u32) -> int { 0 }\n\
     }\n\
     fn apply<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
     fn main() {\n\
    \    let s = Sink { _tag: 0 };\n\
    \    println(apply(s, 1));\n\
     }\n"
    "bound 'F: Fn1' on 'apply' requires 'F::Arg = i32' but type 'Sink' has 'Fn1::Arg = u32'";

  check_assert "DR-022: matching bound + impl assoc types compile"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          fn apply<F: |int|->int>(f: F, x: int) -> int { f(x) }\n\
          fn main() { let a = AddOne { _tag: 0 }; println(apply(a, 41)); }\n");
       true
     with _ -> false);

  check_assert "DR-020: Result::Err in match arm picks up TEnumApp expected"
    (try
       ignore (Exile_lang.Compiler.compile
         "fn fwd<T>(r: Result<T, str>) -> Result<T, str> {\n\
         \    match r {\n\
         \        Result::Ok(v) => Result::Ok(v)\n\
         \        | Result::Err(e) => Result::Err(e)\n\
         \    }\n\
          }\n\
          fn main() {\n\
         \    let r: Result<int, str> = Result::Ok(42);\n\
         \    match fwd(r) {\n\
         \        Result::Ok(v) => println(v)\n\
         \        | Result::Err(_e) => println(0)\n\
         \    }\n\
          }\n");
       true
     with _ -> false);

  check_assert "DR-017: Map<I: Iterator, F: Fn1> lazy adapter"
    (try
       ignore (Exile_lang.Compiler.compile
         "struct AddOne { _tag: int }\n\
          impl Fn1 for AddOne {\n\
         \    type Arg = int;\n\
         \    type Output = int;\n\
         \    fn call(*const self, a: int) -> int { a + 1 }\n\
          }\n\
          struct Map<I, F> { inner: I, f: F }\n\
          impl<I: Iterator, F: Fn1> Iterator for Map<I, F> {\n\
         \    type Item = F::Output;\n\
         \    fn next(*self) -> Option<F::Output> {\n\
         \        let v = try self.inner.next();\n\
         \        Option::Some(self.f.call(v))\n\
         \    }\n\
          }\n\
          fn main() {\n\
         \    let a = default_allocator();\n\
         \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
         \    v.push(10); v.push(20); v.push(30);\n\
         \    let m: Map<VecIter<int>, AddOne> =\n\
         \        Map { inner: v.iter(), f: AddOne { _tag: 0 } };\n\
         \    let mut acc: int = 0;\n\
         \    for x in m { acc = acc + x; }\n\
         \    println(acc);\n\
          }\n");
       true
     with _ -> false);

  (* ===== Self-host bring-up Faza −1 — differential-harness dumps =====

     The three canonical dump forms are emitted from the OCaml exilc
     as oracles for the future exile port.  Tests below cover the
     format-stability invariants (header, deterministic ordering,
     bit-stable across runs) rather than locking the full dump text
     — the goal here is: when the port runs, its dump must match
     this byte-for-byte, so any drift inside our own emitter is a
     bug we want a test to catch. *)

  let starts_with hay prefix =
    let lh = String.length hay and lp = String.length prefix in
    lh >= lp && String.sub hay 0 lp = prefix
  in

  check_assert "Faza -1: --emit-tokens header + EOF sentinel"
    (let d = dump_tokens "fn main() {}\n" in
     starts_with d ";; exile-tokens-dump v1 <input>\n"
     && contains d "Fn @<input>:1:1"
     && contains d "Eof @<input>:");

  check_assert "Faza -1: --emit-tokens captures float suffix"
    (let d = dump_tokens "fn main() { let a: f32 = 1.5f32; }\n" in
     contains d "(Float "
     && contains d " f32)");

  check_assert "Faza -1: --emit-ast header + structural fn body"
    (let d = dump_ast "fn add(x: int, y: int) -> int { x + y }\n" in
     starts_with d ";; exile-ast-dump v1 <input>\n"
     && contains d "(fn add"
     && contains d "(param x (int i32))"
     && contains d "(param y (int i32))"
     && contains d "(binop + (var x) (var y))");

  check_assert "Faza -1: --emit-ast lambda survives the parse"
    (let d = dump_ast
       "fn main() { let f = |x: int| -> int x * 2; println(f(7)); }\n"
     in
     contains d "(lambda "
     && contains d "(param x (int i32))");

  check_assert "Faza -1: --emit-typed-ir carries :ty on every node"
    (let d = dump_typed_ir
       "fn add(x: int, y: int) -> int { x + y }\n\
        fn main() { println(add(2, 3)); }\n"
     in
     starts_with d ";; exile-typed-ir-dump v1 <input>\n"
     && contains d ":ty (int i32)"
     && contains d "(tfn ex_add"
     && contains d "(call ex_add");

  check_assert "Faza -1: --emit-typed-ir mono-instances section is sorted"
    (let d = dump_typed_ir
       "pub mod raw { extern fn make() -> Allocator; }\n\
        fn main() {\n\
       \    let a = raw::make();\n\
       \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
       \    v.push(1);\n\
        }\n"
     in
     contains d "(mono-instances"
     (* Alphabetical: Allocator < Vec_i32, both should appear in
        that relative order in the sorted list. *)
     && let find sub =
          let lh = String.length d and ls = String.length sub in
          let rec loop i =
            if i + ls > lh then None
            else if String.sub d i ls = sub then Some i
            else loop (i + 1)
          in
          loop 0
        in
        let alloc_pos = find "(struct Allocator)" in
        let vec_pos = find "(struct Vec_i32)" in
        match alloc_pos, vec_pos with
        | Some a, Some v -> a < v
        | _ -> false);

  check_assert "Faza -1: --emit-typed-ir is deterministic across runs"
    (let src =
       "pub mod raw { extern fn make() -> Allocator; }\n\
        fn main() {\n\
       \    let a = raw::make();\n\
       \    let mut v: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
       \    let mut m: HashMap<int, int> = HashMap::with_capacity(a, 8 as u32);\n\
       \    v.push(1);\n\
       \    m.insert(7, 100);\n\
        }\n"
     in
     dump_typed_ir src = dump_typed_ir src);

  (* ===== DR-013 perf-report v1 =====

     The collector folds typed-IR for cost-sites (i32 mul/div/mod
     soft-call, indirect call, Vec/HashMap with_capacity(_,<8) =
     no-cap, aggregate by-value copy) and groups codegen-emitted fns
     by skeleton.  Tests pin the wire-level invariants — kinds, hot
     flag heuristic, folds-at-O2 marker, JSON envelope shape — so
     downstream consumers (the v2 heatmap, perf quick-win lints M1/M2)
     can rely on them. *)

  let perf_collect src =
    let (tp, _c) = Exile_lang.Compiler.compile_capture src in
    let bloat = Exile_lang.Codegen.last_bloat () in
    Exile_lang.Perf_report.collect tp bloat
  in
  let dr013_contains hay sub =
    let lh = String.length hay and ls = String.length sub in
    let rec loop i =
      if i + ls > lh then false
      else if String.sub hay i ls = sub then true
      else loop (i + 1)
    in loop 0
  in
  let find_fn (r : Exile_lang.Perf_report.report) mangled =
    List.find_opt (fun f -> f.Exile_lang.Perf_report.fm_mangled = mangled)
      r.r_fns
  in
  let kinds_for f =
    List.map (fun s -> s.Exile_lang.Perf_report.cs_kind) f.Exile_lang.Perf_report.fm_sites
  in

  check_assert "DR-013: i32 mul/div/mod detected as soft-call cost-sites"
    (let r = perf_collect
       "fn calc3(x: int, y: int) -> int { x * y + x / y + x % y }\n\
        fn main() { println(calc3(10, 3)); }\n"
     in
     match find_fn r "ex_calc3" with
     | None -> false
     | Some f ->
         let ks = kinds_for f in
         List.mem Exile_lang.Perf_report.Mul32 ks
         && List.mem Exile_lang.Perf_report.DivuDiv ks
         && List.mem Exile_lang.Perf_report.DivuMod ks);

  check_assert "DR-013: folds-at-O2 marker on literal × literal, not on var × var"
    (let r = perf_collect
       "fn foldlit(x: int) -> int {\n\
       \    let a: int = 3 * 4;\n\
       \    let b: int = x * 4;\n\
       \    a + b\n\
        }\n\
        fn main() { println(foldlit(7)); }\n"
     in
     match find_fn r "ex_foldlit" with
     | None -> false
     | Some f ->
         let sites = f.Exile_lang.Perf_report.fm_sites in
         let muls =
           List.filter (fun s ->
             s.Exile_lang.Perf_report.cs_kind = Exile_lang.Perf_report.Mul32) sites
         in
         List.length muls = 2
         && List.exists (fun s -> s.Exile_lang.Perf_report.cs_folds_at_o2) muls
         && List.exists (fun s -> not s.Exile_lang.Perf_report.cs_folds_at_o2) muls);

  check_assert "DR-013: hot flag fires on i32 modulo anywhere"
    (let r = perf_collect
       "fn spicy(x: int) -> int { x % 7 }\n\
        fn cooled(x: int) -> int { x + 1 }\n\
        fn main() { println(spicy(10) + cooled(2)); }\n"
     in
     match find_fn r "ex_spicy", find_fn r "ex_cooled" with
     | Some hot, Some cool ->
         hot.Exile_lang.Perf_report.fm_hot && not cool.Exile_lang.Perf_report.fm_hot
     | _ -> false);

  check_assert "DR-013: in_loop tagged for cost-sites inside while-body"
    (let r = perf_collect
       "fn looped(n: int) -> int {\n\
       \    let mut acc: int = 0;\n\
       \    let mut i: int = 0;\n\
       \    while i < n {\n\
       \        acc = acc + i % 5;\n\
       \        i = i + 1;\n\
       \    }\n\
       \    acc\n\
        }\n\
        fn main() { println(looped(10)); }\n"
     in
     match find_fn r "ex_looped" with
     | None -> false
     | Some f ->
         List.exists (fun s ->
           s.Exile_lang.Perf_report.cs_kind = Exile_lang.Perf_report.DivuMod
           && s.Exile_lang.Perf_report.cs_in_loop) f.Exile_lang.Perf_report.fm_sites);

  check_assert "DR-013: no-capacity site on Vec::with_capacity(_, <8)"
    (let r = perf_collect
       "fn main() {\n\
       \    let a = default_allocator();\n\
       \    let mut v: Vec<int> = Vec::with_capacity(a, 2 as u32);\n\
       \    v.push(1);\n\
        }\n"
     in
     match find_fn r "main" with
     | None -> false
     | Some f ->
         List.exists (fun s ->
           s.Exile_lang.Perf_report.cs_kind = Exile_lang.Perf_report.NoCapacity)
           f.Exile_lang.Perf_report.fm_sites);

  check_assert "DR-013: skeleton grouping collapses mono instances"
    (let r = perf_collect
       "fn main() {\n\
       \    let a = default_allocator();\n\
       \    let mut vi: Vec<int> = Vec::with_capacity(a, 8 as u32);\n\
       \    let mut vu: Vec<u32> = Vec::with_capacity(a, 8 as u32);\n\
       \    vi.push(1);\n\
       \    vu.push(1 as u32);\n\
        }\n"
     in
     (* Both Vec_i32 and Vec_u32 mono-instances must collapse into one
        group keyed on the skeleton's source name `Vec::push`. *)
     let push = List.find_opt (fun g ->
       g.Exile_lang.Perf_report.sg_name = "push"
       && g.Exile_lang.Perf_report.sg_path = ["Vec"]) r.r_groups in
     match push with
     | Some g -> List.length g.Exile_lang.Perf_report.sg_instances = 2
     | None -> false);

  check_assert "DR-013: JSON envelope carries version, total_bytes, fn_count"
    (let r = perf_collect
       "fn main() { println(1 + 2); }\n"
     in
     let j = Exile_lang.Perf_report.to_json r in
     dr013_contains j "\"version\":1"
     && dr013_contains j "\"total_bytes\":"
     && dr013_contains j "\"fn_count\":"
     && dr013_contains j "\"groups\":["
     && dr013_contains j "\"fns\":[")
