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
    "fn main() {\n    print(\"Hello, World!\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"Hello, World!\");\n    return 0;\n}\n";

  check "let int + print"
    "fn main() {\n    let x = 6 * 7;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 6 * 7;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "let string + print"
    "fn main() {\n    let msg = \"hi\";\n    print(msg);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    const char *msg;\n    msg = \"hi\";\n    printf(\"%s\\n\", msg);\n    return 0;\n}\n";

  check "arithmetic precedence"
    "fn main() {\n    let x = 1 + 2 * 3;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 1 + 2 * 3;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "if without else"
    "fn main() {\n    let x = 10;\n    if x < 5 {\n        print(x);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 10;\n    if (x < 5) {\n        printf(\"%ld\\n\", (long)(x));\n    }\n    return 0;\n}\n";

  check "if with else"
    "fn main() {\n    let x = 10;\n    if x < 5 {\n        print(x);\n    } else {\n        print(0);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 10;\n    if (x < 5) {\n        printf(\"%ld\\n\", (long)(x));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check "multi-function call"
    "fn add(a: int, b: int) -> int {\n    return a + b;\n}\n\nfn main() {\n    let x = add(3, 4);\n    print(x);\n}\n"
    "#include <stdio.h>\n\nstatic long ex_add(long a, long b);\n\nstatic long ex_add(long a, long b) {\n    return a + b;\n}\n\nint main(void) {\n    long x;\n    x = ex_add(3, 4);\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "assignment"
    "fn main() {\n    let x = 1;\n    x = x + 41;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 1;\n    x = x + 41;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "while loop"
    "fn main() {\n    let i = 0;\n    while i < 3 {\n        print(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    i = 0;\n    while (i < 3) {\n        printf(\"%ld\\n\", (long)(i));\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "while with hoisted inner let"
    "fn main() {\n    let i = 0;\n    while i < 2 {\n        let doubled = i * 2;\n        print(doubled);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    long doubled;\n    i = 0;\n    while (i < 2) {\n        doubled = i * 2;\n        printf(\"%ld\\n\", (long)(doubled));\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "bool literals"
    "fn main() {\n    let x = true;\n    let y = false;\n    print(x);\n    print(y);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    int y;\n    x = 1;\n    y = 0;\n    printf(\"%d\\n\", x);\n    printf(\"%d\\n\", y);\n    return 0;\n}\n";

  check "else if chain"
    "fn main() {\n    let x = 2;\n    if x < 1 {\n        print(1);\n    } else if x < 3 {\n        print(2);\n    } else {\n        print(3);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 2;\n    if (x < 1) {\n        printf(\"%ld\\n\", (long)(1));\n    } else if (x < 3) {\n        printf(\"%ld\\n\", (long)(2));\n    } else {\n        printf(\"%ld\\n\", (long)(3));\n    }\n    return 0;\n}\n";

  check "line and block comments"
    "// top comment\nfn main() {\n    /* block\n       comment */\n    let x = 1; // trailing\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long x;\n    x = 1;\n    printf(\"%ld\\n\", (long)(x));\n    return 0;\n}\n";

  check "all comparison operators"
    "fn main() {\n    let a = 5;\n    if a == 5 {\n        print(1);\n    }\n    if a != 0 {\n        print(2);\n    }\n    if a <= 5 {\n        print(3);\n    }\n    if a >= 5 {\n        print(4);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long a;\n    a = 5;\n    if (a == 5) {\n        printf(\"%ld\\n\", (long)(1));\n    }\n    if (a != 0) {\n        printf(\"%ld\\n\", (long)(2));\n    }\n    if (a <= 5) {\n        printf(\"%ld\\n\", (long)(3));\n    }\n    if (a >= 5) {\n        printf(\"%ld\\n\", (long)(4));\n    }\n    return 0;\n}\n";

  check "unary minus on literal var and call"
    "fn id(x: int) -> int {\n    return x;\n}\nfn main() {\n    let a = -5;\n    let b = -a;\n    print(b);\n    print(-id(7));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_id(long x);\n\nstatic long ex_id(long x) {\n    return x;\n}\n\nint main(void) {\n    long a;\n    long b;\n    a = -5;\n    b = -a;\n    printf(\"%ld\\n\", (long)(b));\n    printf(\"%ld\\n\", (long)(-(ex_id(7))));\n    return 0;\n}\n";

  check_error "undefined variable in if cond"
    "fn main() {\n    if nope > 0 {\n        print(1);\n    }\n}\n"
    "undefined variable 'nope'";

  check_error "duplicate let"
    "fn main() {\n    let x = 1;\n    let x = 2;\n    print(x);\n}\n"
    "variable 'x' already declared in this function";

  check_error "let shadows parameter"
    "fn foo(x: int) -> int {\n    let x = 5;\n    return x;\n}\nfn main() {\n    print(foo(1));\n}\n"
    "variable 'x' shadows a parameter";

  check_error "let annotation mismatch"
    "fn main() {\n    let x: str = 5;\n    print(x);\n}\n"
    "variable 'x' declared as str but initializer has type i32";

  check_error "wrong arg count"
    "fn add(a: int, b: int) -> int {\n    return a + b;\n}\nfn main() {\n    print(add(1));\n}\n"
    "function 'add' expects 2 argument(s), got 1";

  check_error "wrong arg type"
    "fn greet(name: str) {\n    print(name);\n}\nfn main() {\n    greet(42);\n}\n"
    "argument 1 of 'greet': expected str, got i32";

  check_error "void used as value"
    "fn greet(name: str) {\n    print(name);\n}\nfn main() {\n    let x = greet(\"hi\");\n    print(x);\n}\n"
    "'greet' returns void, cannot use as a value";

  check_error "assignment to undefined"
    "fn main() {\n    x = 5;\n}\n"
    "assignment to undefined variable 'x'";

  check_error "main with params"
    "fn main(x: int) {\n    print(x);\n}\n"
    "'main' must take no parameters";

  check_error "duplicate function"
    "fn foo() -> int {\n    return 1;\n}\nfn foo() -> int {\n    return 2;\n}\nfn main() {\n    print(foo());\n}\n"
    "function 'foo' already defined";

  check_error "duplicate parameter"
    "fn add(x: int, x: int) -> int {\n    return x;\n}\nfn main() {\n    print(add(1, 2));\n}\n"
    "duplicate parameter 'x' in function 'add'";

  check_error "unknown escape"
    "fn main() {\n    print(\"hi \\q there\");\n}\n"
    "unknown escape \\q";

  check_error "negative literal in unsigned"
    "fn main() {\n    let x: u8 = -1;\n    print(x);\n}\n"
    "negative literal -1 cannot fit in u8";

  check_error "negative literal out of signed range"
    "fn main() {\n    let x: i8 = -200;\n    print(x);\n}\n"
    "literal -200 does not fit in i8";

  check "negative literal fits in signed"
    "fn main() {\n    let x: i8 = -1;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    signed char x;\n    x = -1;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check_error "C keyword as variable name"
    "fn main() {\n    let unsigned: u32 = 5;\n    print(unsigned);\n}\n"
    "variable 'unsigned' is a reserved C keyword";

  check_error "C keyword as parameter name"
    "fn foo(static: int) -> int {\n    return static;\n}\nfn main() {\n    print(foo(1));\n}\n"
    "parameter 'static' is a reserved C keyword";

  check_error "C keyword as top-level function name"
    "fn signed() -> int {\n    return 1;\n}\nfn main() {\n    print(signed());\n}\n"
    "function 'signed' is a reserved C keyword";

  check "C keyword as function name inside module"
    "mod m {\n    pub fn unsigned() -> int {\n        return 7;\n    }\n}\nfn main() {\n    print(m::unsigned());\n}\n"
    "#include <stdio.h>\n\nlong m__unsigned(void);\n\nlong m__unsigned(void) {\n    return 7;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(m__unsigned()));\n    return 0;\n}\n";

  check_error "print arity zero"
    "fn main() {\n    print();\n}\n"
    "print() takes exactly one argument, got 0";

  check_error "print arity two"
    "fn main() {\n    print(1, 2);\n}\n"
    "print() takes exactly one argument, got 2";

  check "defer LIFO at fall-through"
    "fn main() {\n    defer print(\"A\");\n    defer print(\"B\");\n    print(\"body\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"body\");\n    printf(\"%s\\n\", \"B\");\n    printf(\"%s\\n\", \"A\");\n    return 0;\n}\n";

  check "defer with explicit return uses temp"
    "fn compute() -> int {\n    defer print(\"cleanup\");\n    return 42;\n}\nfn main() {\n    print(compute());\n}\n"
    "#include <stdio.h>\n\nstatic long ex_compute(void);\n\nstatic long ex_compute(void) {\n    {\n        long __exile_ret;\n        __exile_ret = 42;\n        printf(\"%s\\n\", \"cleanup\");\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_compute()));\n    return 0;\n}\n";

  check "defer block fires its stmts in source order"
    "fn main() {\n    defer { print(\"a\"); print(\"b\"); }\n    defer print(\"c\");\n    print(\"body\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"body\");\n    printf(\"%s\\n\", \"c\");\n    printf(\"%s\\n\", \"a\");\n    printf(\"%s\\n\", \"b\");\n    return 0;\n}\n";

  check "defer in if branch chains outer cleanup on return"
    "fn process(n: int) -> int {\n    defer print(\"outer\");\n    if n > 0 {\n        defer print(\"inner\");\n        return n;\n    }\n    return 0;\n}\nfn main() {\n    print(process(5));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_process(long n);\n\nstatic long ex_process(long n) {\n    if (n > 0) {\n        {\n            long __exile_ret;\n            __exile_ret = n;\n            printf(\"%s\\n\", \"inner\");\n            printf(\"%s\\n\", \"outer\");\n            return __exile_ret;\n        }\n    }\n    {\n        long __exile_ret;\n        __exile_ret = 0;\n        printf(\"%s\\n\", \"outer\");\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_process(5)));\n    return 0;\n}\n";

  check "defer in while fires per iteration"
    "fn main() {\n    let i = 0;\n    while i < 2 {\n        defer print(\"end\");\n        print(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long i;\n    i = 0;\n    while (i < 2) {\n        printf(\"%ld\\n\", (long)(i));\n        i = i + 1;\n        printf(\"%s\\n\", \"end\");\n    }\n    return 0;\n}\n";

  check_error "return inside defer body rejected"
    "fn foo() -> int {\n    defer { return 5; }\n    return 10;\n}\nfn main() {\n    print(foo());\n}\n"
    "'return' inside a defer body is not supported";

  check_error "defer inside defer body rejected"
    "fn main() {\n    defer { defer print(\"x\"); }\n}\n"
    "'defer' inside a defer body is not supported";

  check "top-level fn name like a C stdlib symbol still works (ex_ prefix)"
    "fn pow(base: int, exp: int) -> int {\n    return base * exp;\n}\nfn main() {\n    print(pow(2, 3));\n}\n"
    "#include <stdio.h>\n\nstatic long ex_pow(long base, long exp);\n\nstatic long ex_pow(long base, long exp) {\n    return base * exp;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_pow(2, 3)));\n    return 0;\n}\n";

  check "tuple return + destructuring (homogeneous)"
    "fn split(n: int) -> (int, int) {\n    return (n * 2, n * 3);\n}\nfn main() {\n    let (a, b) = split(5);\n    print(a);\n    print(b);\n}\n"
    "#include <stdio.h>\n\nstruct ex_tup2_i32_i32 { long _0; long _1; };\n\nstatic struct ex_tup2_i32_i32 ex_split(long n);\n\nstatic struct ex_tup2_i32_i32 ex_split(long n) {\n    {\n        struct ex_tup2_i32_i32 __exile_ret;\n        __exile_ret._0 = n * 2;\n        __exile_ret._1 = n * 3;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    long a;\n    long b;\n    {\n        struct ex_tup2_i32_i32 __t;\n        __t = ex_split(5);\n        a = __t._0;\n        b = __t._1;\n    }\n    printf(\"%ld\\n\", (long)(a));\n    printf(\"%ld\\n\", (long)(b));\n    return 0;\n}\n";

  check "tuple literal RHS in destructuring"
    "fn main() {\n    let (x, y) = (10, 20);\n    print(x);\n    print(y);\n}\n"
    "#include <stdio.h>\n\nstruct ex_tup2_i32_i32 { long _0; long _1; };\n\nint main(void) {\n    long x;\n    long y;\n    {\n        struct ex_tup2_i32_i32 __t;\n        __t._0 = 10;\n        __t._1 = 20;\n        x = __t._0;\n        y = __t._1;\n    }\n    printf(\"%ld\\n\", (long)(x));\n    printf(\"%ld\\n\", (long)(y));\n    return 0;\n}\n";

  check "tuple variable bound from literal, passed as fn arg"
    "fn show(t: (int, int)) {\n    let (a, b) = t;\n    print(a);\n    print(b);\n}\nfn main() {\n    let t = (10, 20);\n    show(t);\n}\n"
    "#include <stdio.h>\n\nstruct ex_tup2_i32_i32 { long _0; long _1; };\n\nstatic void ex_show(struct ex_tup2_i32_i32 t);\n\nstatic void ex_show(struct ex_tup2_i32_i32 t) {\n    long a;\n    long b;\n    {\n        struct ex_tup2_i32_i32 __t;\n        __t = t;\n        a = __t._0;\n        b = __t._1;\n    }\n    printf(\"%ld\\n\", (long)(a));\n    printf(\"%ld\\n\", (long)(b));\n}\n\nint main(void) {\n    struct ex_tup2_i32_i32 t;\n    t._0 = 10;\n    t._1 = 20;\n    ex_show(t);\n    return 0;\n}\n";

  check_error "empty tuple type rejected"
    "fn foo() -> () {\n    return (1, 2);\n}\nfn main() {\n    foo();\n}\n"
    "empty tuple type '()' is not supported";

  check_error "destructuring arity mismatch"
    "fn split() -> (int, int) {\n    return (1, 2);\n}\nfn main() {\n    let (a, b, c) = split();\n    print(a);\n}\n"
    "destructuring 'let (...)' has 3 names but value is a 2-tuple";

  check_error "destructuring non-tuple value"
    "fn main() {\n    let (a, b) = 5;\n    print(a);\n}\n"
    "destructuring 'let (...)' expects a tuple value, got i32";

  check_error "destructuring single name rejected"
    "fn main() {\n    let (a) = (1, 2);\n    print(a);\n}\n"
    "destructuring 'let (...)' needs at least two names";

  check_error "duplicate name in destructuring"
    "fn split() -> (int, int) {\n    return (1, 2);\n}\nfn main() {\n    let (a, a) = split();\n    print(a);\n}\n"
    "duplicate name 'a' in 'let (...)'";

  check_error "tuple cannot be printed"
    "fn split() -> (int, int) {\n    return (1, 2);\n}\nfn main() {\n    print(split());\n}\n"
    "cannot print a tuple; destructure with 'let (...)' first";

  check "struct decl + literal + field access + by-value param/return"
    "struct Point {\n    x: int,\n    y: int,\n}\nfn make(a: int, b: int) -> Point {\n    return Point { x: a, y: b };\n}\nfn main() {\n    let p = make(3, 4);\n    print(p.x);\n    print(p.y);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic struct ex_Point ex_make(long a, long b);\n\nstatic struct ex_Point ex_make(long a, long b) {\n    {\n        struct ex_Point __exile_ret;\n        __exile_ret.x = a;\n        __exile_ret.y = b;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Point p;\n    p = ex_make(3, 4);\n    printf(\"%ld\\n\", (long)(p.x));\n    printf(\"%ld\\n\", (long)(p.y));\n    return 0;\n}\n";

  check "struct field assignment"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    p.x = 99;\n    print(p.x);\n    print(p.y);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 1;\n    p.y = 2;\n    p.x = 99;\n    printf(\"%ld\\n\", (long)(p.x));\n    printf(\"%ld\\n\", (long)(p.y));\n    return 0;\n}\n";

  check_error "unknown struct name"
    "fn main() {\n    let p = Foo { x: 1 };\n    print(p.x);\n}\n"
    "unknown struct 'Foo'";

  check_error "struct literal missing field"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1 };\n    print(p.x);\n}\n"
    "struct literal 'Point' missing field(s): y";

  check_error "struct literal extra field"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2, z: 3 };\n    print(p.x);\n}\n"
    "struct literal 'Point' has unknown field(s): z";

  check_error "struct literal wrong field type"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: \"hi\" };\n    print(p.x);\n}\n"
    "field 'y' of struct 'Point': expected i32, got str";

  check_error "struct literal duplicate field"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, x: 2, y: 3 };\n    print(p.x);\n}\n"
    "duplicate field 'x' in struct literal 'Point'";

  check_error "field access on non-struct"
    "fn main() {\n    let x = 5;\n    print(x.foo);\n}\n"
    "field access '.foo' requires a struct value or pointer to struct, got i32";

  check_error "unknown field on struct"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    print(p.z);\n}\n"
    "struct 'Point' has no field 'z'";

  check_error "duplicate field in struct decl"
    "struct Point { x: int, x: int, }\nfn main() {\n    let p = Point { x: 1 };\n    print(p.x);\n}\n"
    "duplicate field 'x' in struct 'Point'";

  check_error "print of struct rejected"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    print(p);\n}\n"
    "cannot print a struct value (Point); print individual fields instead";

  check "pointer to struct: ref + auto-deref field access + assign"
    "struct Point { x: int, y: int, }\nfn shift(p: *Point, dx: int) {\n    p.x = p.x + dx;\n}\nfn main() {\n    let p = Point { x: 0, y: 0 };\n    shift(&p, 10);\n    print(p.x);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic void ex_shift(struct ex_Point *p, long dx);\n\nstatic void ex_shift(struct ex_Point *p, long dx) {\n    p->x = p->x + dx;\n}\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 0;\n    p.y = 0;\n    ex_shift(&p, 10);\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n";

  check "pointer to int: ref, deref-load, deref-store"
    "fn main() {\n    let n = 5;\n    let pn = &n;\n    print(*pn);\n    *pn = 99;\n    print(n);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    long n;\n    long *pn;\n    n = 5;\n    pn = &n;\n    printf(\"%ld\\n\", (long)(*pn));\n    *pn = 99;\n    printf(\"%ld\\n\", (long)(n));\n    return 0;\n}\n";

  check_error "deref of non-pointer"
    "fn main() {\n    let n = 5;\n    let x = *n;\n    print(x);\n}\n"
    "deref '*' requires a pointer, got i32";

  check_error "assign through deref of non-pointer"
    "fn main() {\n    let n = 5;\n    *n = 7;\n    print(n);\n}\n"
    "assignment through '*' requires a pointer, got i32";

  check_error "pointer cannot be printed"
    "fn main() {\n    let n = 5;\n    print(&n);\n}\n"
    "cannot print a pointer value (*i32); deref or print a field";

  check "new + free + defer-free for heap struct"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = new Point { x: 1, y: 2 };\n    defer free(p);\n    print(p.x);\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point *p;\n    p = malloc(sizeof(struct ex_Point));\n    p->x = 1;\n    p->y = 2;\n    printf(\"%ld\\n\", (long)(p->x));\n    free(p);\n    return 0;\n}\n";

  check "fn returning *Point via new"
    "struct Point { x: int, y: int, }\nfn make() -> *Point {\n    return new Point { x: 0, y: 0 };\n}\nfn main() {\n    let p = make();\n    defer free(p);\n    print(p.x);\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Point { long x; long y; };\n\nstatic struct ex_Point *ex_make(void);\n\nstatic struct ex_Point *ex_make(void) {\n    {\n        struct ex_Point * __exile_ret;\n        __exile_ret = malloc(sizeof(struct ex_Point));\n        __exile_ret->x = 0;\n        __exile_ret->y = 0;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Point *p;\n    p = ex_make();\n    printf(\"%ld\\n\", (long)(p->x));\n    free(p);\n    return 0;\n}\n";

  check_error "free of non-pointer"
    "fn main() {\n    let n = 5;\n    free(n);\n}\n"
    "'free' expects a pointer, got i32";

  check_error "free used as value"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = new Point { x: 0, y: 0 };\n    let x = free(p);\n    print(x);\n}\n"
    "'free' returns void, cannot use as a value";

  check_error "new of unknown struct"
    "fn main() {\n    let p = new Foo { x: 1 };\n    print(p.x);\n}\n"
    "unknown struct 'Foo'";

  check "functional update copies base then overrides"
    "struct Point { x: int, y: int, z: int, }\nfn main() {\n    let p = Point { x: 1, y: 2, z: 3 };\n    let q = Point { x: 99, ..p };\n    print(q.x);\n    print(q.y);\n    print(q.z);\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; long z; };\n\nint main(void) {\n    struct ex_Point p;\n    struct ex_Point q;\n    p.x = 1;\n    p.y = 2;\n    p.z = 3;\n    q = p;\n    q.x = 99;\n    printf(\"%ld\\n\", (long)(q.x));\n    printf(\"%ld\\n\", (long)(q.y));\n    printf(\"%ld\\n\", (long)(q.z));\n    return 0;\n}\n";

  check "functional update with new copies through deref"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p = Point { x: 1, y: 2 };\n    let r = new Point { y: 50, ..p };\n    defer free(r);\n    print(r.x);\n    print(r.y);\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point p;\n    struct ex_Point *r;\n    p.x = 1;\n    p.y = 2;\n    r = malloc(sizeof(struct ex_Point));\n    *r = p;\n    r->y = 50;\n    printf(\"%ld\\n\", (long)(r->x));\n    printf(\"%ld\\n\", (long)(r->y));\n    free(r);\n    return 0;\n}\n";

  check_error "functional update with mismatched base type"
    "struct Point { x: int, y: int, }\nstruct Other { z: int, }\nfn main() {\n    let o = Other { z: 7 };\n    let p = Point { x: 1, ..o };\n    print(p.x);\n}\n"
    "'..base' in struct literal 'Point' expects a value of type Point, got Other";

  check "null literal in struct field + equality check"
    "struct Node { value: int, next: *Node, }\nfn main() {\n    let n = new Node { value: 5, next: null };\n    defer free(n);\n    if n.next == null {\n        print(n.value);\n    }\n}\n"
    "#include <stdio.h>\n#include <stdlib.h>\n\nstruct ex_Node { long value; struct ex_Node *next; };\n\nint main(void) {\n    struct ex_Node *n;\n    n = malloc(sizeof(struct ex_Node));\n    n->value = 5;\n    n->next = ((void *)0);\n    if (n->next == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(n->value));\n    }\n    free(n);\n    return 0;\n}\n";

  check "null with typed let binding"
    "struct Point { x: int, y: int, }\nfn main() {\n    let p: *Point = null;\n    if p == null {\n        print(42);\n    }\n}\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nint main(void) {\n    struct ex_Point *p;\n    p = ((void *)0);\n    if (p == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(42));\n    }\n    return 0;\n}\n";

  check_error "naked null without type ann rejected"
    "fn main() {\n    let p = null;\n    print(p);\n}\n"
    "cannot infer pointer type for 'null'; add a type annotation like 'let p: *T = null;'";

  check_error "deref of null rejected"
    "fn main() {\n    let x = *null;\n    print(x);\n}\n"
    "cannot deref 'null'";

  check_error "print of null rejected"
    "fn main() {\n    print(null);\n}\n"
    "cannot print 'null'";

  check_multi "wildcard import inlines pub items, hides private"
    [ ("lib.exl",
       "pub fn hello() -> int {\n    return 42;\n}\n\
        fn priv() -> int {\n    return 99;\n}\n");
      ("main.exl",
       "use lib::*;\n\nfn main() {\n    print(hello());\n}\n") ]
    "main.exl"
    "#include <stdio.h>\n\nlong ex_hello(void);\n\nlong ex_hello(void) {\n    return 42;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_hello()));\n    return 0;\n}\n";

  check "by-value method called via dot form"
    "struct Point { x: int, y: int }\n\
     impl Point {\n\
    \    pub fn area(self: Point) -> int { return self.x * self.y; }\n\
     }\n\
     fn main() {\n\
    \    let p = Point { x: 3, y: 4 };\n\
    \    print(p.area());\n\
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
    \    print(p.x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Point { long x; long y; };\n\nvoid Point__shift(struct ex_Point *self, long dx);\n\nint main(void) {\n    struct ex_Point p;\n    p.x = 1;\n    p.y = 2;\n    Point__shift(&p, 10);\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n\nvoid Point__shift(struct ex_Point *self, long dx) {\n    self->x = self->x + dx;\n}\n";

  check "UFCS call + auto-deref via ptr receiver to value-self method"
    "struct P { x: int }\n\
     impl P { pub fn get(self: P) -> int { return self.x; } }\n\
     fn main() {\n\
    \    let p = P { x: 7 };\n\
    \    let q: *P = &p;\n\
    \    print(P::get(p));\n\
    \    print(q.get());\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; };\n\nlong P__get(struct ex_P self);\n\nint main(void) {\n    struct ex_P p;\n    struct ex_P *q;\n    p.x = 7;\n    q = &p;\n    printf(\"%ld\\n\", (long)(P__get(p)));\n    printf(\"%ld\\n\", (long)(P__get(*q)));\n    return 0;\n}\n\nlong P__get(struct ex_P self) {\n    return self.x;\n}\n";

  check "static method (no self) invoked as Foo::name(...)"
    "struct P { x: int }\n\
     impl P {\n\
    \    pub fn make(v: int) -> P { return P { x: v }; }\n\
     }\n\
     fn main() {\n\
    \    let p = P::make(42);\n\
    \    print(p.x);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; };\n\nstruct ex_P P__make(long v);\n\nint main(void) {\n    struct ex_P p;\n    p = P__make(42);\n    printf(\"%ld\\n\", (long)(p.x));\n    return 0;\n}\n\nstruct ex_P P__make(long v) {\n    {\n        struct ex_P __exile_ret;\n        __exile_ret.x = v;\n        return __exile_ret;\n    }\n}\n";

  check_error "method on unknown struct rejected"
    "impl Nope { fn x(self: Nope) {} }\nfn main() {}\n"
    "unknown struct 'Nope' in 'impl' block";

  check_error "method 'self' must have struct type"
    "struct P { x: int }\nimpl P { fn foo(self: int) {} }\nfn main() {}\n"
    "first parameter 'self' must have type 'P' or '*P', got i32";

  check_error "method name clashes with field rejected"
    "struct P { v: int }\nimpl P { fn v(self: P) -> int { return self.v; } }\nfn main() {}\n"
    "method name 'v' clashes with a field on 'P'";

  check_error "duplicate method across impl blocks rejected"
    "struct P { x: int }\nimpl P { fn foo(self: P) {} }\nimpl P { fn foo(self: P) {} }\nfn main() {}\n"
    "method 'foo' on 'P' already defined in another 'impl' block";

  check_error "method call on non-struct rejected"
    "fn main() { let x: int = 5; print(x.foo()); }\n"
    "method call '.foo()' requires a struct value or pointer to struct, got i32";

  check_error "unknown method rejected"
    "struct P { x: int }\nimpl P { pub fn foo(self: P) -> int { return self.x; } }\nfn main() { let p = P { x: 1 }; print(p.bar()); }\n"
    "no method 'bar' on type 'P'";

  check_error "private method called from outside rejected"
    "struct P { x: int }\nimpl P { fn priv(self: P) -> int { return self.x; } }\nfn main() { let p = P { x: 1 }; print(p.priv()); }\n"
    "method 'priv' is private to 'P'";

  check "enum unit variants + match with explicit arms"
    "enum Color {\n\
    \    | Red\n\
    \    | Green\n\
    \    | Blue\n\
     }\n\
     fn main() {\n\
    \    let c = Color::Green;\n\
    \    match c {\n\
    \        | Color::Red => print(\"r\")\n\
    \        | Color::Green => print(\"g\")\n\
    \        | Color::Blue => print(\"b\")\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Color_tag { ex_Color_Red, ex_Color_Green, ex_Color_Blue };\nstruct ex_Color { enum ex_Color_tag tag; };\n\nint main(void) {\n    struct ex_Color c;\n    c.tag = ex_Color_Green;\n    {\n        struct ex_Color __m;\n        __m = c;\n        switch (__m.tag) {\n        case ex_Color_Red:\n            {\n                printf(\"%s\\n\", \"r\");\n                break;\n            }\n        case ex_Color_Green:\n            {\n                printf(\"%s\\n\", \"g\");\n                break;\n            }\n        case ex_Color_Blue:\n            {\n                printf(\"%s\\n\", \"b\");\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "match with wildcard arm covers remaining variants"
    "enum E { | A | B | C }\n\
     fn main() {\n\
    \    let e = E::A;\n\
    \    match e {\n\
    \        | E::A => print(\"a\")\n\
    \        | _ => print(\"other\")\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B, ex_E_C };\nstruct ex_E { enum ex_E_tag tag; };\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_A;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                printf(\"%s\\n\", \"a\");\n                break;\n            }\n        default:\n            {\n                printf(\"%s\\n\", \"other\");\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "tuple variants + match with bind patterns"
    "enum Shape {\n\
    \    | Square\n\
    \    | Circle(int)\n\
    \    | Rect(int, int)\n\
     }\n\
     fn main() {\n\
    \    let s = Shape::Rect(3, 4);\n\
    \    match s {\n\
    \        | Shape::Square => print(\"sq\")\n\
    \        | Shape::Circle(r) => print(r)\n\
    \        | Shape::Rect(w, h) => print(w + h)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Shape_tag { ex_Shape_Square, ex_Shape_Circle, ex_Shape_Rect };\nstruct ex_Shape { enum ex_Shape_tag tag; union { struct { long _0; } Circle; struct { long _0; long _1; } Rect; } data; };\n\nint main(void) {\n    struct ex_Shape s;\n    s.tag = ex_Shape_Rect;\n    s.data.Rect._0 = 3;\n    s.data.Rect._1 = 4;\n    {\n        struct ex_Shape __m;\n        __m = s;\n        switch (__m.tag) {\n        case ex_Shape_Square:\n            {\n                printf(\"%s\\n\", \"sq\");\n                break;\n            }\n        case ex_Shape_Circle:\n            {\n                long r = __m.data.Circle._0;\n                printf(\"%ld\\n\", (long)(r));\n                break;\n            }\n        case ex_Shape_Rect:\n            {\n                long w = __m.data.Rect._0;\n                long h = __m.data.Rect._1;\n                printf(\"%ld\\n\", (long)(w + h));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "wrong arg count for tuple variant rejected"
    "enum E { | A(int) }\nfn main() { let e = E::A(1, 2); }\n"
    "variant 'E::A' takes 1 argument(s), got 2";

  check_error "wrong arg type for tuple variant rejected"
    "enum E { | A(str) }\nfn main() { let e = E::A(5); }\n"
    "argument 1 of 'E::A': expected str, got i32";

  check_error "wrong bind count in pattern rejected"
    "enum E { | A(int, int) }\nfn main() { let e = E::A(1, 2); match e { | E::A(x) => print(x) } }\n"
    "variant 'A' has 2 field(s), pattern binds 1";

  check_error "duplicate bind name in pattern rejected"
    "enum E { | A(int, int) }\nfn main() { let e = E::A(1, 2); match e { | E::A(x, x) => print(x) } }\n"
    "duplicate bind name 'x' in pattern";

  check "match as expression in let RHS"
    "enum E { | A | B(int) }\n\
     fn main() {\n\
    \    let e = E::B(7);\n\
    \    let v = match e {\n\
    \        | E::A => 0\n\
    \        | E::B(n) => n + 1\n\
    \    };\n\
    \    print(v);\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nint main(void) {\n    struct ex_E e;\n    long v;\n    e.tag = ex_E_B;\n    e.data.B._0 = 7;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                v = 0;\n                break;\n            }\n        case ex_E_B:\n            {\n                long n = __m.data.B._0;\n                v = n + 1;\n                break;\n            }\n        }\n    }\n    printf(\"%ld\\n\", (long)(v));\n    return 0;\n}\n";

  check "match as expression in return position"
    "enum E { | A | B(int) }\n\
     fn classify(e: E) -> int {\n\
    \    return match e {\n\
    \        | E::A => 0\n\
    \        | E::B(n) => n\n\
    \    };\n\
     }\n\
     fn main() {\n\
    \    let e = E::B(42);\n\
    \    print(classify(e));\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nstatic long ex_classify(struct ex_E e);\n\nstatic long ex_classify(struct ex_E e) {\n    {\n        long __exile_ret;\n        {\n            struct ex_E __m;\n            __m = e;\n            switch (__m.tag) {\n            case ex_E_A:\n                {\n                    __exile_ret = 0;\n                    break;\n                }\n            case ex_E_B:\n                {\n                    long n = __m.data.B._0;\n                    __exile_ret = n;\n                    break;\n                }\n            }\n        }\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_B;\n    e.data.B._0 = 42;\n    printf(\"%ld\\n\", (long)(ex_classify(e)));\n    return 0;\n}\n";

  check_error "match arms with inconsistent types rejected"
    "enum E { | A | B }\nfn main() { let v = match E::A { | E::A => 1 | E::B => true }; print(v); }\n"
    "match arms have inconsistent types: i32 vs bool";

  check "EnumLit as fn arg lifts via __lift_N temp"
    "enum E { | A | B(int) }\n\
     fn show(e: E) {\n\
    \    match e {\n\
    \        | E::A => print(0)\n\
    \        | E::B(n) => print(n)\n\
    \    }\n\
     }\n\
     fn main() {\n\
    \    show(E::B(7));\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nstatic void ex_show(struct ex_E e);\n\nstatic void ex_show(struct ex_E e) {\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        case ex_E_B:\n            {\n                long n = __m.data.B._0;\n                printf(\"%ld\\n\", (long)(n));\n                break;\n            }\n        }\n    }\n}\n\nint main(void) {\n    struct ex_E __lift_0;\n    __lift_0.tag = ex_E_B;\n    __lift_0.data.B._0 = 7;\n    ex_show(__lift_0);\n    return 0;\n}\n";

  check "Match as sub-expression in BinOp lifts to __lift_N"
    "enum E { | A | B(int) }\n\
     fn main() {\n\
    \    let e = E::B(2);\n\
    \    let total = 1 + match e {\n\
    \        | E::A => 0\n\
    \        | E::B(n) => n\n\
    \    };\n\
    \    print(total);\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long _0; } B; } data; };\n\nint main(void) {\n    struct ex_E e;\n    long total;\n    long __lift_0;\n    e.tag = ex_E_B;\n    e.data.B._0 = 2;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                __lift_0 = 0;\n                break;\n            }\n        case ex_E_B:\n            {\n                long n = __m.data.B._0;\n                __lift_0 = n;\n                break;\n            }\n        }\n    }\n    total = 1 + __lift_0;\n    printf(\"%ld\\n\", (long)(total));\n    return 0;\n}\n";

  check "struct-like variant: shorthand bind in pattern"
    "enum E {\n\
    \    | A\n\
    \    | B { x: int, y: int }\n\
     }\n\
     fn main() {\n\
    \    let e = E::B { x: 3, y: 4 };\n\
    \    match e {\n\
    \        | E::A => print(0)\n\
    \        | E::B { x, y } => print(x + y)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_E_tag { ex_E_A, ex_E_B };\nstruct ex_E { enum ex_E_tag tag; union { struct { long x; long y; } B; } data; };\n\nint main(void) {\n    struct ex_E e;\n    e.tag = ex_E_B;\n    e.data.B.x = 3;\n    e.data.B.y = 4;\n    {\n        struct ex_E __m;\n        __m = e;\n        switch (__m.tag) {\n        case ex_E_A:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        case ex_E_B:\n            {\n                long x = __m.data.B.x;\n                long y = __m.data.B.y;\n                printf(\"%ld\\n\", (long)(x + y));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "struct-syntax for tuple variant rejected"
    "enum E { | A(int) }\nfn main() { let e = E::A { x: 1 }; }\n"
    "variant 'E::A' is a tuple variant; construct it with '(...)', not with '{ field: ... }'";

  check_error "tuple-syntax for struct variant rejected"
    "enum E { | A { x: int } }\nfn main() { let e = E::A(1); }\n"
    "variant 'E::A' is a struct variant; construct it with '{ field: ... }', not with '(...)'";

  check_error "missing field in struct variant ctor rejected"
    "enum E { | A { x: int, y: int } }\nfn main() { let e = E::A { x: 1 }; }\n"
    "missing field 'y' in 'E::A' construction";

  check_error "extra field in struct variant ctor rejected"
    "enum E { | A { x: int } }\nfn main() { let e = E::A { x: 1, y: 2 }; }\n"
    "variant 'E::A' has no field 'y'";

  check_error "tuple pattern on struct variant rejected"
    "enum E { | A { x: int } }\nfn main() { let e = E::A { x: 1 }; match e { | E::A(x) => print(x) } }\n"
    "variant 'A' is a struct variant; match it with '{ field: pat }', not '(...)'";

  check_error "unknown field in variant pattern rejected"
    "enum E { | A { x: int } }\nfn main() { let e = E::A { x: 1 }; match e { | E::A { y } => print(y) } }\n"
    "variant 'A' has no field 'y'";

  check "StructLit as fn arg lifts to __lift_N"
    "struct P { x: int, y: int }\n\
     fn sum(p: P) -> int { return p.x + p.y; }\n\
     fn main() {\n\
    \    print(sum(P { x: 3, y: 4 }));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_P { long x; long y; };\n\nstatic long ex_sum(struct ex_P p);\n\nstatic long ex_sum(struct ex_P p) {\n    return p.x + p.y;\n}\n\nint main(void) {\n    struct ex_P __lift_0;\n    __lift_0.x = 3;\n    __lift_0.y = 4;\n    printf(\"%ld\\n\", (long)(ex_sum(__lift_0)));\n    return 0;\n}\n";

  check_error "non-exhaustive match rejected"
    "enum E { | A | B }\nfn main() { let e = E::A; match e { | E::A => print(\"a\") } }\n"
    "non-exhaustive 'match': variant(s) B not covered (add an arm or '_')";

  check_error "unknown variant in constructor rejected"
    "enum E { | A }\nfn main() { let e = E::Nope; }\n"
    "enum 'E' has no variant 'Nope'";

  check_error "unknown enum in constructor rejected"
    "fn main() { let e = Nope::A; }\n"
    "unknown enum 'Nope'";

  check_error "duplicate variant in enum decl rejected"
    "enum E { | A | A }\nfn main() {}\n"
    "duplicate variant 'A' in enum 'E'";

  check_error "match on non-enum rejected"
    "fn main() { let x = 5; match x { | _ => print(\"x\") } }\n"
    "'match' requires an enum value, got i32";

  check_error "pattern enum mismatch with scrutinee rejected"
    "enum A { | X }\nenum B { | Y }\nfn main() { let a = A::X; match a { | B::Y => print(\"b\") } }\n"
    "pattern matches 'B' but scrutinee has type 'A'";

  check_error "print of enum value rejected"
    "enum E { | A }\nfn main() { let e = E::A; print(e); }\n"
    "cannot print an enum value (E); match on it and print per variant";

  check_error "unknown generic type rejected"
    "fn main() { let x: Box<int> = 0; }\n"
    "unknown generic type 'Box'";

  check "generic decls without instantiation emit nothing"
    "enum Option<T> { | None | Some(T) }\n\
     struct Pair<A, B> { fst: A, snd: B }\n\
     fn id<T>(x: T) -> T { return x; }\n\
     fn main() {}\n"
    "#include <stdio.h>\n\nint main(void) {\n    return 0;\n}\n";

  check "generic struct construction infers type args from fields"
    "struct Pair<A, B> { fst: A, snd: B }\n\
     fn main() {\n\
    \    let p = Pair { fst: 5, snd: \"hi\" };\n\
    \    print(p.fst);\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Pair_i32_str { long fst; const char *snd; };\n\nint main(void) {\n    struct ex_Pair_i32_str p;\n    p.fst = 5;\n    p.snd = \"hi\";\n    printf(\"%ld\\n\", (long)(p.fst));\n    return 0;\n}\n";

  check "Result<T, E>: bidirectional typing infers E from return type"
    "enum Result<T, E> { | Ok(T) | Err(E) }\n\
     enum IoErr { | NotFound }\n\
     fn make() -> Result<int, IoErr> { return Result::Ok(42); }\n\
     fn main() {\n\
    \    let r = make();\n\
    \    match r {\n\
    \        | Result::Ok(v) => print(v)\n\
    \        | Result::Err(_) => print(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_IoErr_tag { ex_IoErr_NotFound };\nstruct ex_IoErr { enum ex_IoErr_tag tag; };\nenum ex_Result_i32_ex_IoErr_tag { ex_Result_i32_ex_IoErr_Ok, ex_Result_i32_ex_IoErr_Err };\nstruct ex_Result_i32_ex_IoErr { enum ex_Result_i32_ex_IoErr_tag tag; union { struct { long _0; } Ok; struct { struct ex_IoErr _0; } Err; } data; };\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void);\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void) {\n    {\n        struct ex_Result_i32_ex_IoErr __exile_ret;\n        __exile_ret.tag = ex_Result_i32_ex_IoErr_Ok;\n        __exile_ret.data.Ok._0 = 42;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Result_i32_ex_IoErr r;\n    r = ex_make();\n    {\n        struct ex_Result_i32_ex_IoErr __m;\n        __m = r;\n        switch (__m.tag) {\n        case ex_Result_i32_ex_IoErr_Ok:\n            {\n                long v = __m.data.Ok._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Result_i32_ex_IoErr_Err:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "Option::None: type-ann pins T when payload doesn't"
    "enum Option<T> { | None | Some(T) }\n\
     fn main() {\n\
    \    let o: Option<int> = Option::None;\n\
    \    match o {\n\
    \        | Option::None => print(0)\n\
    \        | Option::Some(x) => print(x)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_None;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "generic enum: tuple ctor infers payload + match destructures"
    "enum Option<T> { | None | Some(T) }\n\
     fn main() {\n\
    \    let o = Option::Some(42);\n\
    \    match o {\n\
    \        | Option::Some(x) => print(x)\n\
    \        | Option::None => print(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_Some;\n    o.data.Some._0 = 42;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "?T sugar: ?int parses as Option<int>"
    "fn main() {\n\
    \    let o: ?int = Option::Some(5);\n\
    \    match o {\n\
    \        | Option::Some(x) => print(x)\n\
    \        | Option::None    => print(0)\n\
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
    \    print(first_or(some, 99));\n\
    \    print(first_or(none, 99));\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstatic long ex_first_or(struct ex_Option_i32 o, long d);\n\nstatic long ex_first_or(struct ex_Option_i32 o, long d) {\n    {\n        long __exile_ret;\n        {\n            struct ex_Option_i32 __m;\n            __m = o;\n            switch (__m.tag) {\n            case ex_Option_i32_Some:\n                {\n                    long __orelse_v = __m.data.Some._0;\n                    __exile_ret = __orelse_v;\n                    break;\n                }\n            case ex_Option_i32_None:\n                {\n                    __exile_ret = d;\n                    break;\n                }\n            }\n        }\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Option_i32 some;\n    struct ex_Option_i32 none;\n    some.tag = ex_Option_i32_Some;\n    some.data.Some._0 = 7;\n    none.tag = ex_Option_i32_None;\n    printf(\"%ld\\n\", (long)(ex_first_or(some, 99)));\n    printf(\"%ld\\n\", (long)(ex_first_or(none, 99)));\n    return 0;\n}\n";

  check_error "orelse on non-enum rejected"
    "fn main() { let x = 5 orelse 0; print(x); }\n"
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
    \        | Option::Some(x) => print(x)\n\
    \        | Option::None    => print(0)\n\
    \    }\n\
    \    match incr(none) {\n\
    \        | Option::Some(x) => print(x)\n\
    \        | Option::None    => print(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstatic struct ex_Option_i32 ex_incr(struct ex_Option_i32 o);\n\nstatic struct ex_Option_i32 ex_incr(struct ex_Option_i32 o) {\n    long v;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long __try_v = __m.data.Some._0;\n                v = __try_v;\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                struct ex_Option_i32 __try_ret;\n                __try_ret.tag = ex_Option_i32_None;\n                return __try_ret;\n            }\n        }\n    }\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v + 1;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Option_i32 some;\n    struct ex_Option_i32 none;\n    some.tag = ex_Option_i32_Some;\n    some.data.Some._0 = 7;\n    none.tag = ex_Option_i32_None;\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_incr(some);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_incr(none);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_error "try outside Option/Result-returning fn rejected"
    "fn main() {\n\
    \    let v = try Option::Some(5);\n\
    \    print(v);\n\
     }\n"
    "'try' is only allowed in fns that return Option or Result (this fn has no return type)";

  check_error "try shape mismatch (Option in Result-returning fn)"
    "fn f() -> Result<int, int> {\n\
    \    let v = try Option::Some(5);\n\
    \    return Result::Ok(v);\n\
     }\n\
     fn main() { match f() { | Result::Ok(_) => print(1) | Result::Err(_) => print(0) } }\n"
    "'try' on Option_i32 value but enclosing fn returns Result_i32_i32 \
     — they must share the same Option/Result shape";

  check "extern fn: forward decl + raw call site name (no ex_ prefix)"
    "pub mod raw { extern fn my_add(a: int, b: int) -> int; }\n\
     fn main() {\n\
         print(raw::my_add(2, 3));\n\
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
    "pub mod raw { extern fn foo() { print(1); } }\n\
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
         print(r as int);\n\
     }\n\
     "
    "#include <stdio.h>\n\nextern LONG add(LONG a, LONG b);\n\nint main(void) {\n    LONG r;\n    r = add(40, 2);\n    printf(\"%ld\\n\", (long)(((long)r)));\n    return 0;\n}\n";

  check_error "extern type rejects bare (non-pointer-or-by-value) misuse... actually allowed"
    "pub mod raw { extern type LONG; }\n\
     fn main() { print(LONG); }\n\
     "
    "undefined variable 'LONG'";

  check_no_cc "extern const NAME: T — value resolved by linker"
    "pub mod raw { extern const VERSION: c_int; }\n\
     fn main() { print(VERSION as int); }\n\
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
    \    if raw::DOSBase == null { print(0); } else { print(1); }\n\
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
     fn main() { print(0); }\n"
    "#include <stdio.h>\n\nextern struct Library *OpenLibrary(char *name, unsigned int version);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(0));\n    return 0;\n}\n";

  check_error "@reg(...) rejected on non-extern fn"
    "fn bad(@reg(d0) x: int) -> int { return x; }\n\
     fn main() { print(0); }\n"
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
    \    print(f(40, 2) as int);\n\
     }\n"
    "#include <stdio.h>\n\ntypedef int (*fn2_cint_cint_to_cint)(int, int);\n\nstatic int ex_add(int a, int b);\n\nstatic int ex_add(int a, int b) {\n    return a + b;\n}\n\nint main(void) {\n    fn2_cint_cint_to_cint f;\n    f = ex_add;\n    printf(\"%ld\\n\", (long)(((long)f(40, 2))));\n    return 0;\n}\n";

  check_no_cc "fn pointer as extern fn parameter: signal-style callback"
    "pub mod raw { extern fn signal(sig: c_int, handler: fn(c_int)) -> fn(c_int); }\n\
     fn my_handler(s: c_int) { print(s as int); }\n\
     fn main() {\n\
         let prev: fn(c_int) = raw::signal(2, my_handler);\n\
     }\n\
     "
    "#include <stdio.h>\n\ntypedef void (*fn1_cint_to_void)(int);\n\nextern fn1_cint_to_void signal(int sig, fn1_cint_to_void handler);\nstatic void ex_my_handler(int s);\n\nstatic void ex_my_handler(int s) {\n    printf(\"%ld\\n\", (long)(((long)s)));\n}\n\nint main(void) {\n    fn1_cint_to_void prev;\n    prev = signal(2, ex_my_handler);\n    return 0;\n}\n";

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

  check_no_cc "extern struct Foo { fields }: field read through pointer"
    "pub mod raw {\n\
    \    extern struct Library { lib_OpenCnt: c_uint, lib_Version: c_uint }\n\
    \    extern fn open_lib() -> *Library;\n\
     }\n\
     fn main() {\n\
    \    let lib: *Library = raw::open_lib();\n\
    \    if lib != null {\n\
    \        print(lib.lib_OpenCnt as int);\n\
    \        print(lib.lib_Version as int);\n\
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
    \    print(p.x as int);\n\
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
    \    print(lib.lib_Version as int);\n\
     }\n"
    "field access '.lib_Version' on opaque type 'Library' — declare fields with `extern struct Library { ... }` to access them";

  check_error "extern struct: unknown field rejected"
    "pub mod raw {\n\
    \    extern struct Lib { x: c_int }\n\
    \    extern fn open_lib() -> *Lib;\n\
     }\n\
     fn main() {\n\
    \    let l: *Lib = raw::open_lib();\n\
    \    print(l.y as int);\n\
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
    \        | Option::Some(x) => print(x)\n\
    \        | Option::None => print(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nint main(void) {\n    struct ex_Option_i32 o;\n    o.tag = ex_Option_i32_Some;\n    o.data.Some._0 = 42;\n    {\n        struct ex_Option_i32 __m;\n        __m = o;\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "prelude: Result<T, E> usable without explicit declaration"
    "enum IoErr { | NotFound }\n\
     fn make() -> Result<int, IoErr> { return Result::Ok(42); }\n\
     fn main() {\n\
    \    let r = make();\n\
    \    match r {\n\
    \        | Result::Ok(v) => print(v)\n\
    \        | Result::Err(_) => print(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_IoErr_tag { ex_IoErr_NotFound };\nstruct ex_IoErr { enum ex_IoErr_tag tag; };\nenum ex_Result_i32_ex_IoErr_tag { ex_Result_i32_ex_IoErr_Ok, ex_Result_i32_ex_IoErr_Err };\nstruct ex_Result_i32_ex_IoErr { enum ex_Result_i32_ex_IoErr_tag tag; union { struct { long _0; } Ok; struct { struct ex_IoErr _0; } Err; } data; };\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void);\n\nstatic struct ex_Result_i32_ex_IoErr ex_make(void) {\n    {\n        struct ex_Result_i32_ex_IoErr __exile_ret;\n        __exile_ret.tag = ex_Result_i32_ex_IoErr_Ok;\n        __exile_ret.data.Ok._0 = 42;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    struct ex_Result_i32_ex_IoErr r;\n    r = ex_make();\n    {\n        struct ex_Result_i32_ex_IoErr __m;\n        __m = r;\n        switch (__m.tag) {\n        case ex_Result_i32_ex_IoErr_Ok:\n            {\n                long v = __m.data.Ok._0;\n                printf(\"%ld\\n\", (long)(v));\n                break;\n            }\n        case ex_Result_i32_ex_IoErr_Err:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check "prelude: user-declared Option<T> overrides built-in"
    "enum Option<T> { | Empty | Full(T) }\n\
     fn main() {\n\
    \    let o = Option::Full(7);\n\
    \    match o {\n\
    \        | Option::Full(x) => print(x)\n\
    \        | Option::Empty => print(0)\n\
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
    \    print(geom::area(p));\n\
    \    print(p.sum());\n\
     }\n"
    "#include <stdio.h>\n\nstruct geom__Point { long x; long y; };\n\nlong geom__area(struct geom__Point p);\nlong geom__Point__sum(struct geom__Point self);\n\nlong geom__area(struct geom__Point p) {\n    return p.x * p.y;\n}\n\nint main(void) {\n    struct geom__Point p;\n    p.x = 3;\n    p.y = 4;\n    printf(\"%ld\\n\", (long)(geom__area(p)));\n    printf(\"%ld\\n\", (long)(geom__Point__sum(p)));\n    return 0;\n}\n\nlong geom__Point__sum(struct geom__Point self) {\n    return self.x + self.y;\n}\n";

  check "generic free fn: T inferred from positional arg, one instance per T"
    "fn id<T>(x: T) -> T {\n\
    \    return x;\n\
     }\n\
     fn main() {\n\
    \    print(id(42));\n\
    \    print(id(7));\n\
     }\n"
    "#include <stdio.h>\n\nstatic long ex_id_i32(long x);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_id_i32(42)));\n    printf(\"%ld\\n\", (long)(ex_id_i32(7)));\n    return 0;\n}\n\nstatic long ex_id_i32(long x) {\n    return x;\n}\n";

  check "generic free fn: distinct T values produce distinct instances"
    "fn id<T>(x: T) -> T {\n\
    \    return x;\n\
     }\n\
     fn main() {\n\
    \    print(id(42));\n\
    \    print(id(true));\n\
     }\n"
    "#include <stdio.h>\n\nstatic long ex_id_i32(long x);\nstatic int ex_id_bool(int x);\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_id_i32(42)));\n    printf(\"%d\\n\", ex_id_bool(1));\n    return 0;\n}\n\nstatic long ex_id_i32(long x) {\n    return x;\n}\n\nstatic int ex_id_bool(int x) {\n    return x;\n}\n";

  check "generic free fn: T inferred bidirectionally from let annotation"
    "fn make<T>() -> *T {\n\
    \    return null;\n\
     }\n\
     fn main() {\n\
    \    let p: *int = make();\n\
    \    if p == null { print(1); } else { print(0); }\n\
     }\n"
    "#include <stdio.h>\n\nstatic long *ex_make_i32(void);\n\nint main(void) {\n    long *p;\n    p = ex_make_i32();\n    if (p == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n\nstatic long *ex_make_i32(void) {\n    return ((void *)0);\n}\n";

  check_error "generic free fn: under-determined T errors with hint"
    "fn make<T>() -> *T {\n\
    \    return null;\n\
     }\n\
     fn main() {\n\
    \    let p = make();\n\
    \    if p == null { print(1); } else { print(0); }\n\
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
    \    print(b.first(42));\n\
    \    print(b.first(true));\n\
     }\n"
    "#include <stdio.h>\n\nstruct ex_Box { long value; };\n\nlong Box__first_i32(struct ex_Box self, long x);\nint Box__first_bool(struct ex_Box self, int x);\n\nint main(void) {\n    struct ex_Box b;\n    b.value = 99;\n    printf(\"%ld\\n\", (long)(Box__first_i32(b, 42)));\n    printf(\"%d\\n\", Box__first_bool(b, 1));\n    return 0;\n}\n\nlong Box__first_i32(struct ex_Box self, long x) {\n    return x;\n}\n\nint Box__first_bool(struct ex_Box self, int x) {\n    return x;\n}\n";

  check "size_of yields a c_uint via C sizeof"
    "fn main() {\n\
    \    print(size_of(int) as int);\n\
    \    print(size_of(*int) as int);\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(((long)sizeof(long))));\n    printf(\"%ld\\n\", (long)(((long)sizeof(long *))));\n    return 0;\n}\n";

  check "size_of substitutes T per generic instance"
    "fn sz<T>(_p: *T) -> c_uint { return size_of(T); }\n\
     fn main() {\n\
    \    let p: *int = null;\n\
    \    let q: *bool = null;\n\
    \    print(sz(p) as int);\n\
    \    print(sz(q) as int);\n\
     }\n"
    "#include <stdio.h>\n\nstatic unsigned int ex_sz_i32(long *_p);\nstatic unsigned int ex_sz_bool(int *_p);\n\nint main(void) {\n    long *p;\n    int *q;\n    p = ((void *)0);\n    q = ((void *)0);\n    printf(\"%ld\\n\", (long)(((long)ex_sz_i32(p))));\n    printf(\"%ld\\n\", (long)(((long)ex_sz_bool(q))));\n    return 0;\n}\n\nstatic unsigned int ex_sz_i32(long *_p) {\n    return sizeof(long);\n}\n\nstatic unsigned int ex_sz_bool(int *_p) {\n    return sizeof(int);\n}\n";

  check "pointer-to-pointer cast with `as` accepted"
    "fn main() {\n\
    \    let p: *c_void = null;\n\
    \    let q: *int = p as *int;\n\
    \    if q == null { print(1); } else { print(0); }\n\
     }\n"
    "#include <stdio.h>\n\nint main(void) {\n    void *p;\n    long *q;\n    p = ((void *)0);\n    q = ((long *)p);\n    if (q == ((void *)0)) {\n        printf(\"%ld\\n\", (long)(1));\n    } else {\n        printf(\"%ld\\n\", (long)(0));\n    }\n    return 0;\n}\n";

  check_error "non-pointer-non-int cast still rejected"
    "fn main() {\n\
    \    let b = true;\n\
    \    let _ = b as *int;\n\
     }\n"
    "cannot cast bool to *i32 (only integer-to-integer or pointer-to-pointer casts supported)";

  check "prelude Allocator: dropped from emitted C when unused"
    "fn main() { print(1); }\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(1));\n    return 0;\n}\n";

  check "user `mod Allocator { fn ... }` not confused with prelude impl"
    "mod Allocator {\n\
    \    pub fn helper() -> int { return 1; }\n\
     }\n\
     fn main() {\n\
    \    print(Allocator::helper());\n\
     }\n"
    "#include <stdio.h>\n\nlong Allocator__helper(void);\n\nlong Allocator__helper(void) {\n    return 1;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(Allocator__helper()));\n    return 0;\n}\n";

  check "pub use: struct re-export — Inner visible without raw:: prefix"
    "pub mod raw { pub struct Inner { x: int } }\n\
     pub use raw::Inner;\n\
     fn make() -> Inner { return Inner { x: 42 }; }\n\
     fn main() { print(make().x); }\n"
    "#include <stdio.h>\n\nstruct raw__Inner { long x; };\n\nstatic struct raw__Inner ex_make(void);\n\nstatic struct raw__Inner ex_make(void) {\n    {\n        struct raw__Inner __exile_ret;\n        __exile_ret.x = 42;\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(ex_make().x));\n    return 0;\n}\n";

  check "pub use: fn re-export — helper visible without raw:: prefix"
    "pub mod raw { pub fn helper() -> int { return 7; } }\n\
     pub use raw::helper;\n\
     fn main() { print(helper()); }\n"
    "#include <stdio.h>\n\nlong raw__helper(void);\n\nlong raw__helper(void) {\n    return 7;\n}\n\nint main(void) {\n    printf(\"%ld\\n\", (long)(raw__helper()));\n    return 0;\n}\n";

  check_error "pub use of unknown name rejected at use site"
    "pub mod raw { pub fn helper() -> int { return 7; } }\n\
     pub use raw::nonexistent;\n\
     fn main() { print(nonexistent()); }\n"
    "unknown function 'nonexistent'";

  check_error "pub use wildcard rejected"
    "pub mod raw { pub fn a() {} pub fn b() {} }\n\
     pub use raw::*;\n\
     fn main() {}\n"
    "'pub use foo::*;' wildcard re-export not supported (re-export individual names)";

  check "fn-ptr field call: recv.field(args) routes through TIndirectCall"
    "struct Op { f: fn(int) -> int }\n\
     fn id(x: int) -> int { return x; }\n\
     fn dbl(x: int) -> int { return x + x; }\n\
     fn main() {\n\
    \    let a = Op { f: id };\n\
    \    let b = Op { f: dbl };\n\
    \    print(a.f(7));\n\
    \    print(b.f(21));\n\
     }\n"
    "#include <stdio.h>\n\ntypedef long (*fn1_i32_to_i32)(long);\n\nstruct ex_Op { fn1_i32_to_i32 f; };\n\nstatic long ex_id(long x);\nstatic long ex_dbl(long x);\n\nstatic long ex_id(long x) {\n    return x;\n}\n\nstatic long ex_dbl(long x) {\n    return x + x;\n}\n\nint main(void) {\n    struct ex_Op a;\n    struct ex_Op b;\n    a.f = ex_id;\n    b.f = ex_dbl;\n    printf(\"%ld\\n\", (long)((a.f)(7)));\n    printf(\"%ld\\n\", (long)((b.f)(21)));\n    return 0;\n}\n";

  check "defer fires on `try` early-return path"
    "fn maybe(ok: bool) -> ?int {\n\
    \    if ok { return Option::Some(42); }\n\
    \    return Option::None;\n\
     }\n\
     fn run(ok: bool) -> ?int {\n\
    \    defer print(-1);\n\
    \    let v = try maybe(ok);\n\
    \    return Option::Some(v + 1);\n\
     }\n\
     fn main() {\n\
    \    match run(false) {\n\
    \        | Option::Some(x) => print(x)\n\
    \        | Option::None    => print(0)\n\
    \    }\n\
     }\n"
    "#include <stdio.h>\n\nenum ex_Option_i32_tag { ex_Option_i32_None, ex_Option_i32_Some };\nstruct ex_Option_i32 { enum ex_Option_i32_tag tag; union { struct { long _0; } Some; } data; };\n\nstatic struct ex_Option_i32 ex_maybe(int ok);\nstatic struct ex_Option_i32 ex_run(int ok);\n\nstatic struct ex_Option_i32 ex_maybe(int ok) {\n    if (ok) {\n        {\n            struct ex_Option_i32 __exile_ret;\n            __exile_ret.tag = ex_Option_i32_Some;\n            __exile_ret.data.Some._0 = 42;\n            return __exile_ret;\n        }\n    }\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_None;\n        return __exile_ret;\n    }\n}\n\nstatic struct ex_Option_i32 ex_run(int ok) {\n    long v;\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_maybe(ok);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long __try_v = __m.data.Some._0;\n                v = __try_v;\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                struct ex_Option_i32 __try_ret;\n                __try_ret.tag = ex_Option_i32_None;\n                printf(\"%ld\\n\", (long)(-1));\n                return __try_ret;\n            }\n        }\n    }\n    {\n        struct ex_Option_i32 __exile_ret;\n        __exile_ret.tag = ex_Option_i32_Some;\n        __exile_ret.data.Some._0 = v + 1;\n        printf(\"%ld\\n\", (long)(-1));\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    {\n        struct ex_Option_i32 __m;\n        __m = ex_run(0);\n        switch (__m.tag) {\n        case ex_Option_i32_Some:\n            {\n                long x = __m.data.Some._0;\n                printf(\"%ld\\n\", (long)(x));\n                break;\n            }\n        case ex_Option_i32_None:\n            {\n                printf(\"%ld\\n\", (long)(0));\n                break;\n            }\n        }\n    }\n    return 0;\n}\n";

  check_lint "lint: per-call-site origin pos (not prelude decl pos)"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { print(id(42)); }\n"
    ~profile:Exile_lang.Profile.Core
    ["generic fn 'id'"];

  check_assert "lint origin pos points to call site, not skeleton decl"
    (let ws = lint_warnings ~profile:Exile_lang.Profile.Core
       "fn id<T>(x: T) -> T { return x; }\n\
        fn main() { print(id(42)); }\n"
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
     fn main() { print(helper()); }\n"
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
     fn main() { print(id(42)); }\n"
    ~profile:Exile_lang.Profile.Full
    [];

  check_lint "lint: generic fn under --profile=core warns once per fn"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { print(id(42)); print(id(7)); }\n"
    ~profile:Exile_lang.Profile.Core
    ["generic fn 'id' is tier=full but compiled under --profile=core"];

  check_lint "lint: @tier(core) override silences warning under core"
    "@tier(core)\n\
     fn id<T>(x: T) -> T { return x; }\n\
     fn main() { print(id(42)); }\n"
    ~profile:Exile_lang.Profile.Core
    [];

  check_lint "lint: @tier(standard) warns under core but not standard"
    "@tier(standard)\n\
     fn id<T>(x: T) -> T { return x; }\n\
     fn main() { print(id(42)); }\n"
    ~profile:Exile_lang.Profile.Core
    ["generic fn 'id' is tier=standard"];

  check_lint "lint: mono fn never warns regardless of profile"
    "fn add(a: int, b: int) -> int { return a + b; }\n\
     fn main() { print(add(1, 2)); }\n"
    ~profile:Exile_lang.Profile.Core
    [];

  check_lint "lint: unused generic fn does not warn (no instances)"
    "fn id<T>(x: T) -> T { return x; }\n\
     fn main() { print(1); }\n"
    ~profile:Exile_lang.Profile.Core
    [];

  check_error "@tier rejects unknown tier name"
    "@tier(bogus)\n\
     fn foo() -> int { return 1; }\n\
     fn main() { print(foo()); }\n"
    "unknown tier 'bogus' (expected: core, standard, full)";

  check_error "@tier rejects placement on non-decoratable items"
    "@tier(core)\n\
     impl Allocator { }\n\
     fn main() { print(1); }\n"
    "'@tier' can only decorate fn / struct / enum decls"
