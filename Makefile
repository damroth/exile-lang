EXILE   := dune exec --no-print-directory exilc --
CC      := cc
CFLAGS  := -ansi -pedantic -Wall

TOOLCHAIN_PREFIX := $(CURDIR)/_build/toolchain
AMIGA_GCC        := $(TOOLCHAIN_PREFIX)/bin/m68k-amigaos-gcc

EXAMPLES_SRC := $(filter-out examples/error_%.exl, $(wildcard examples/*.exl))
EXAMPLES_BIN := $(EXAMPLES_SRC:.exl=)

.PHONY: all build test clean run hello examples toolchain toolchain-clean
.PRECIOUS: %.c examples/%

all: build

build:
	dune build

test:
	dune test

# Build the bundled m68k-amigaos cross-compiler (Bebbo's amiga-gcc).
# First run takes 30-60 minutes. Output lives in _build/toolchain/.
toolchain:
	@if [ ! -f tools/amiga-gcc/Makefile ]; then \
		echo "tools/amiga-gcc submodule missing — run: git submodule update --init"; \
		exit 1; \
	fi
	$(MAKE) -C tools/amiga-gcc min PREFIX=$(TOOLCHAIN_PREFIX)
	@echo "toolchain ready: $(AMIGA_GCC)"

toolchain-clean:
	rm -rf $(TOOLCHAIN_PREFIX)
	$(MAKE) -C tools/amiga-gcc clean

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

# Note: `clean` does NOT touch the toolchain. Use `toolchain-clean` for that.
