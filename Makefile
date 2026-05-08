# `verify-*` targets use process substitution (<(...)) to feed
# expected/actual into diff without temp files; that's a bashism, so
# pin the recipe shell to bash explicitly.
SHELL   := /bin/bash

EXILE   := dune exec --no-print-directory exilc --
CC      := cc
CFLAGS  := -ansi -pedantic -Wall

# `?=` so a CI image (or anyone with the toolchain elsewhere) can
# point at /opt/amiga-gcc without editing the Makefile.
TOOLCHAIN_PREFIX ?= $(CURDIR)/_build/toolchain
AMIGA_GCC        ?= $(TOOLCHAIN_PREFIX)/bin/m68k-amigaos-gcc

# Out-of-tree build artefacts.  `_build/` is already gitignored (dune
# owns it), so generated C and per-target binaries live under it too.
OUT      := _build/out
C_OUT    := $(OUT)/c
HOST_OUT := $(OUT)/host
AMIGA_OUT:= $(OUT)/amiga

EXAMPLES_SRC := $(filter-out examples/error_%.exl, $(wildcard examples/*.exl))
EXAMPLE_NAMES:= $(notdir $(EXAMPLES_SRC:.exl=))

GHCR_OWNER ?= damroth
CI_IMAGE   ?= ghcr.io/$(GHCR_OWNER)/exile-lang-ci:latest

HOST_BINS  := $(addprefix $(HOST_OUT)/,$(EXAMPLE_NAMES))
AMIGA_BINS := $(addprefix $(AMIGA_OUT)/,$(EXAMPLE_NAMES))

.PHONY: all build test clean toolchain toolchain-clean
.PHONY: host amiga examples
.PHONY: host-% amiga-% run-% run-host-% c-%
.PHONY: verify verify-host verify-amiga verify-host-% verify-amiga-%
.PHONY: rebaseline-host rebaseline-host-%
.PHONY: build-image

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

# If `examples/NAME_stub.c` exists alongside `examples/NAME.exl`, link
# it in too — convention for FFI examples (extern fn declarations on
# the exile side, definitions in the C stub).
stub_for = $(wildcard examples/$(1)_stub.c)
link_args = $(if $(call stub_for,$(1)),--link $(call stub_for,$(1)))

# `make host-NAME`  → build host binary for examples/NAME.exl
host-%: examples/%.exl $(call stub_for,%) build
	$(EXILE) --target host --c-out $(C_OUT)/$*.c $(call link_args,$*) -o $(HOST_OUT)/$* $<

# `make amiga-NAME` → build m68k Amiga binary
amiga-%: examples/%.exl $(call stub_for,%) build
	@if [ ! -x $(AMIGA_GCC) ]; then \
		echo "amiga-gcc missing — run 'make toolchain' first"; \
		exit 1; \
	fi
	$(EXILE) --target amiga --c-out $(C_OUT)/$*.c $(call link_args,$*) -o $(AMIGA_OUT)/$* $<

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

# Compare a binary's stdout against examples/NAME.expected.  Two
# variants — host runs the locally-built ELF, amiga runs the m68k
# binary under vamos — diff against the same expected file, so any
# divergence between the two targets shows up as a failure on the
# amiga side.  `verify-host` / `verify-amiga` aggregate over every
# example.  Missing .expected file is a failure (run rebaseline-host
# first when adding a new example).
verify-host-%: host-%
	@if [ ! -f examples/$*.expected ]; then \
		echo "verify-host-$*: missing examples/$*.expected (run 'make rebaseline-host-$*')"; \
		exit 1; \
	fi
	@actual=$$($(HOST_OUT)/$*); \
	expected=$$(cat examples/$*.expected); \
	if [ "$$actual" = "$$expected" ]; then \
		echo "verify-host-$*: ok"; \
	else \
		echo "verify-host-$*: FAIL"; \
		diff <(echo "$$expected") <(echo "$$actual"); \
		exit 1; \
	fi

verify-amiga-%: amiga-%
	@if [ ! -f examples/$*.expected ]; then \
		echo "verify-amiga-$*: missing examples/$*.expected"; \
		exit 1; \
	fi
	@actual=$$(vamos $(AMIGA_OUT)/$* 2>/dev/null); \
	expected=$$(cat examples/$*.expected); \
	if [ "$$actual" = "$$expected" ]; then \
		echo "verify-amiga-$*: ok"; \
	else \
		echo "verify-amiga-$*: FAIL"; \
		diff <(echo "$$expected") <(echo "$$actual"); \
		exit 1; \
	fi

verify-host:  $(EXAMPLE_NAMES:%=verify-host-%)
verify-amiga: $(EXAMPLE_NAMES:%=verify-amiga-%)
verify: verify-host verify-amiga

# Capture current host stdout into examples/NAME.expected.  Use when
# adding a new example or after an *intentional* output change; CI
# verify will fail on accidental drift.
rebaseline-host-%: host-%
	@$(HOST_OUT)/$* > examples/$*.expected
	@echo "rebaselined examples/$*.expected ($$(wc -l < examples/$*.expected) lines)"

rebaseline-host: $(EXAMPLE_NAMES:%=rebaseline-host-%)

# Build CI image locally and push to GHCR.  Requires
# `docker login ghcr.io` (e.g. `gh auth token | docker login ghcr.io
# -u <user> --password-stdin`).  Override owner/tag via env, e.g.
# `GHCR_OWNER=foo make build-image` or `CI_IMAGE=... make build-image`.
build-image:
	docker buildx build -f Dockerfile.ci -t $(CI_IMAGE) --push .

clean:
	dune clean
	rm -rf $(OUT)
	rm -f examples/*.c

# Note: `clean` does NOT touch the toolchain or examples/*.expected.
# Use `toolchain-clean` for the cross-compiler.
