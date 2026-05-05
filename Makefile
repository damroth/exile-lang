EXILE   := dune exec --no-print-directory exilc --
CC      := cc
CFLAGS  := -ansi -pedantic -Wall

TOOLCHAIN_PREFIX := $(CURDIR)/_build/toolchain
AMIGA_GCC        := $(TOOLCHAIN_PREFIX)/bin/m68k-amigaos-gcc

# Out-of-tree build artefacts.  `_build/` is already gitignored (dune
# owns it), so generated C and per-target binaries live under it too.
OUT      := _build/out
C_OUT    := $(OUT)/c
HOST_OUT := $(OUT)/host
AMIGA_OUT:= $(OUT)/amiga

EXAMPLES_SRC := $(filter-out examples/error_%.exl, $(wildcard examples/*.exl))
EXAMPLE_NAMES:= $(notdir $(EXAMPLES_SRC:.exl=))

HOST_BINS  := $(addprefix $(HOST_OUT)/,$(EXAMPLE_NAMES))
AMIGA_BINS := $(addprefix $(AMIGA_OUT)/,$(EXAMPLE_NAMES))

.PHONY: all build test clean toolchain toolchain-clean
.PHONY: host amiga examples
.PHONY: host-% amiga-% run-% run-host-% c-%

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

# `make host-NAME`  → build host binary for examples/NAME.exl
host-%: examples/%.exl build
	$(EXILE) --target host --c-out $(C_OUT)/$*.c -o $(HOST_OUT)/$* $<

# `make amiga-NAME` → build m68k Amiga binary
amiga-%: examples/%.exl build
	@if [ ! -x $(AMIGA_GCC) ]; then \
		echo "amiga-gcc missing — run 'make toolchain' first"; \
		exit 1; \
	fi
	$(EXILE) --target amiga --c-out $(C_OUT)/$*.c -o $(AMIGA_OUT)/$* $<

# `make c-NAME` → just emit C, no native binary
c-%: examples/%.exl build
	$(EXILE) --c-out $(C_OUT)/$*.c $<

# `make run-NAME` → build for Amiga and run under vamos.
# `make run-host-NAME` → build host and run natively.
run-%: amiga-%
	vamos $(AMIGA_OUT)/$*

run-host-%: host-%
	$(HOST_OUT)/$*

# Build everything for one target.
host:  $(EXAMPLE_NAMES:%=host-%)
amiga: $(EXAMPLE_NAMES:%=amiga-%)
examples: host

clean:
	dune clean
	rm -rf $(OUT)
	rm -f examples/*.c

# Note: `clean` does NOT touch the toolchain. Use `toolchain-clean` for that.
