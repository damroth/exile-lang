let check label src expected =
  let actual = Exile_lang.Compiler.compile src in
  if actual <> expected then begin
    Printf.eprintf "FAIL: %s\n--- expected ---\n%s--- got ---\n%s" label expected actual;
    exit 1
  end;
  Printf.printf "ok: %s\n" label

let () =
  check "hello world"
    "fn main() {\n    print(\"Hello, World!\");\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    printf(\"Hello, World!\\n\");\n    return 0;\n}\n";

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
    "#include <stdio.h>\n\nint add(int a, int b);\n\nint add(int a, int b) {\n    return a + b;\n}\n\nint main(void) {\n    int x;\n    x = add(3, 4);\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "assignment"
    "fn main() {\n    let x = 1;\n    x = x + 41;\n    print(x);\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int x;\n    x = 1;\n    x = x + 41;\n    printf(\"%d\\n\", x);\n    return 0;\n}\n";

  check "while loop"
    "fn main() {\n    let i = 0;\n    while i < 3 {\n        print(i);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int i;\n    i = 0;\n    while (i < 3) {\n        printf(\"%d\\n\", i);\n        i = i + 1;\n    }\n    return 0;\n}\n";

  check "while with hoisted inner let"
    "fn main() {\n    let i = 0;\n    while i < 2 {\n        let doubled = i * 2;\n        print(doubled);\n        i = i + 1;\n    }\n}\n"
    "#include <stdio.h>\n\nint main(void) {\n    int i;\n    int doubled;\n    i = 0;\n    while (i < 2) {\n        doubled = i * 2;\n        printf(\"%d\\n\", doubled);\n        i = i + 1;\n    }\n    return 0;\n}\n"
