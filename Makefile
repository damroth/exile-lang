EXILE   := dune exec --no-print-directory exile-lang --
CC      := cc
CFLAGS  := -ansi -pedantic -Wall

EXAMPLES_SRC := $(wildcard examples/*.exl)
EXAMPLES_BIN := $(EXAMPLES_SRC:.exl=)

.PHONY: all build test clean run hello examples
.PRECIOUS: %.c examples/%

all: build

build:
	dune build

test:
	dune test

# Compile a .exl file to .c via the exile transpiler
%.c: %.exl build
	$(EXILE) $<

# Compile the generated .c into a native binary with strict C89
%: %.c
	$(CC) $(CFLAGS) $< -o $@

examples: $(EXAMPLES_BIN)

hello: examples/hello
	./examples/hello

run-%: examples/%
	./examples/$*

clean:
	dune clean
	rm -f examples/*.c $(EXAMPLES_BIN)
