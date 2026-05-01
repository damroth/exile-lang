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

let () =
  check "hello world"
    "fn main() {\n    print(\"Hello, World!\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"Hello, World!\");\n    return 0;\n}\n";

  check "let int + print"
    "fn main() {\n    let x = 6 * 7;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 6 * 7;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "let string + print"
    "fn main() {\n    let msg = \"hi\";\n    print(msg);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    const char *msg;\n    msg = \"hi\";\n    printf(\"%s\\n\", msg);\n    return 0;\n}\n";

  check "arithmetic precedence"
    "fn main() {\n    let x = 1 + 2 * 3;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 1 + 2 * 3;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "if without else"
    "fn main() {\n    let x = 10;\n    if x < 5 {\n        print(x);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 10;\n    if (x < 5) {\n        printf(\"%d\\n\", x);\n    }\n    return 0;\n}\n";

  check "if with else"
    "fn main() {\n    let x = 10;\n    if x < 5 {\n        print(x);\n    } else {\n        print(0);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 10;\n    if (x < 5) {\n        printf(\"%d\\n\", x);\n    } else {\n        printf(\"%d\\n\", 0);\n    }\n    return 0;\n}\n";

  check "multi-function call"
    "fn add(a: int, b: int) -> int {\n    return a + b;\n}\n\nfn main() {\n    let x = add(3, 4);\n    print(x);\n}\n"
    "#include <stdio.h>\n\nstatic int ex_add(int a, int b);\n\nstatic int ex_add(int a, int b) {\n    return a + b;\n}\n\nint main(void) {\n    int x;\n    x = ex_add(3, 4);\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "assignment"
    "fn main() {\n    let x = 1;\n    x = x + 41;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 1;\n    x = x + 41;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "while loop"
    "fn main() {\n    let i = 0;\n    while i < 3 {\n        print(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int i;\n    i = 0;\n    while (i < 3) {\n        printf(\"%d\\n\", i);\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "while with hoisted inner let"
    "fn main() {\n    let i = 0;\n    while i < 2 {\n        let doubled = i * 2;\n        print(doubled);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int i;\n    int doubled;\n    i = 0;\n    while (i < 2) {\n        doubled = i * 2;\n        printf(\"%d\\n\", doubled);\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "bool literals"
    "fn main() {\n    let x = true;\n    let y = false;\n    print(x);\n    print(y);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    int y;\n    x = 1;\n    y = 0;\n    printf(\"%d\\n\", x);\n    printf(\"%d\\n\", y);\n    return 0;\n}\n";

  check "else if chain"
    "fn main() {\n    let x = 2;\n    if x < 1 {\n        print(1);\n    } else if x < 3 {\n        print(2);\n    } else {\n        print(3);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 2;\n    if (x < 1) {\n        printf(\"%d\\n\", 1);\n    } else if (x < 3) {\n        printf(\"%d\\n\", 2);\n    } else {\n        printf(\"%d\\n\", 3);\n    }\n    return 0;\n}\n";

  check "line and block comments"
    "// top comment\nfn main() {\n    /* block\n       comment */\n    let x = 1; // trailing\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 1;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "all comparison operators"
    "fn main() {\n    let a = 5;\n    if a == 5 {\n        print(1);\n    }\n    if a != 0 {\n        print(2);\n    }\n    if a <= 5 {\n        print(3);\n    }\n    if a >= 5 {\n        print(4);\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int a;\n    a = 5;\n    if (a == 5) {\n        printf(\"%d\\n\", 1);\n    }\n    if (a != 0) {\n        printf(\"%d\\n\", 2);\n    }\n    if (a <= 5) {\n        printf(\"%d\\n\", 3);\n    }\n    if (a >= 5) {\n        printf(\"%d\\n\", 4);\n    }\n    return 0;\n}\n";

  check "unary minus on literal var and call"
    "fn id(x: int) -> int {\n    return x;\n}\nfn main() {\n    let a = -5;\n    let b = -a;\n    print(b);\n    print(-id(7));\n}\n"
    "#include <stdio.h>\n\nstatic int ex_id(int x);\n\nstatic int ex_id(int x) {\n    return x;\n}\n\nint main(void) {\n    int a;\n    int b;\n    a = -5;\n    b = -a;\n    printf(\"%d\\n\", b);\n    printf(\"%d\\n\", -(ex_id(7)));\n    return 0;\n}\n";

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
    "#include <stdio.h>\n\nint m__unsigned(void);\n\nint m__unsigned(void) {\n    return 7;\n}\n\nint main(void) {\n    printf(\"%d\\n\", m__unsigned());\n    return 0;\n}\n";

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
    "#include <stdio.h>\n\nstatic int ex_compute(void);\n\nstatic int ex_compute(void) {\n    {\n        int __exile_ret = 42;\n        printf(\"%s\\n\", \"cleanup\");\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%d\\n\", ex_compute());\n    return 0;\n}\n";

  check "defer block fires its stmts in source order"
    "fn main() {\n    defer { print(\"a\"); print(\"b\"); }\n    defer print(\"c\");\n    print(\"body\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"%s\\n\", \"body\");\n    printf(\"%s\\n\", \"c\");\n    printf(\"%s\\n\", \"a\");\n    printf(\"%s\\n\", \"b\");\n    return 0;\n}\n";

  check "defer in if branch chains outer cleanup on return"
    "fn process(n: int) -> int {\n    defer print(\"outer\");\n    if n > 0 {\n        defer print(\"inner\");\n        return n;\n    }\n    return 0;\n}\nfn main() {\n    print(process(5));\n}\n"
    "#include <stdio.h>\n\nstatic int ex_process(int n);\n\nstatic int ex_process(int n) {\n    if (n > 0) {\n        {\n            int __exile_ret = n;\n            printf(\"%s\\n\", \"inner\");\n            printf(\"%s\\n\", \"outer\");\n            return __exile_ret;\n        }\n    }\n    {\n        int __exile_ret = 0;\n        printf(\"%s\\n\", \"outer\");\n        return __exile_ret;\n    }\n}\n\nint main(void) {\n    printf(\"%d\\n\", ex_process(5));\n    return 0;\n}\n";

  check "defer in while fires per iteration"
    "fn main() {\n    let i = 0;\n    while i < 2 {\n        defer print(\"end\");\n        print(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int i;\n    i = 0;\n    while (i < 2) {\n        printf(\"%d\\n\", i);\n        i = i + 1;\n        printf(\"%s\\n\", \"end\");\n    }\n    return 0;\n}\n";

  check_error "return inside defer body rejected"
    "fn foo() -> int {\n    defer { return 5; }\n    return 10;\n}\nfn main() {\n    print(foo());\n}\n"
    "'return' inside a defer body is not supported";

  check_error "defer inside defer body rejected"
    "fn main() {\n    defer { defer print(\"x\"); }\n}\n"
    "'defer' inside a defer body is not supported";

  check "top-level fn name like a C stdlib symbol still works (ex_ prefix)"
    "fn pow(base: int, exp: int) -> int {\n    return base * exp;\n}\nfn main() {\n    print(pow(2, 3));\n}\n"
    "#include <stdio.h>\n\nstatic int ex_pow(int base, int exp);\n\nstatic int ex_pow(int base, int exp) {\n    return base * exp;\n}\n\nint main(void) {\n    printf(\"%d\\n\", ex_pow(2, 3));\n    return 0;\n}\n";

  check_multi "wildcard import inlines pub items, hides private"
    [ ("lib.exl",
       "pub fn hello() -> int {\n    return 42;\n}\n\
        fn priv() -> int {\n    return 99;\n}\n");
      ("main.exl",
       "use lib::*;\n\nfn main() {\n    print(hello());\n}\n") ]
    "main.exl"
    "#include <stdio.h>\n\nint ex_hello(void);\n\nint ex_hello(void) {\n    return 42;\n}\n\nint main(void) {\n    printf(\"%d\\n\", ex_hello());\n    return 0;\n}\n"
