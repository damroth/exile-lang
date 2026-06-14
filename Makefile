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

# `amiga_*.exl` examples link against AmigaOS-only libraries
# (dos.library, intuition.library etc.) — they have no meaningful host
# build because the C declarations reference functions that don't
# exist outside of m68k-amigaos.  Filter them out of the host
# pipelines; the amiga pipelines still pick them up.
AMIGA_ONLY    := $(filter amiga_%, $(EXAMPLE_NAMES))
# `host_only_*.exl` examples lean on POSIX-style sys-seam fns whose
# Amiga backends are still stubs (e.g. sys_open returns -1 until
# BPTR<->fd bookkeeping lands).  They build cleanly on Amiga but
# diverge in output, so the verify-amiga compare would fail on
# every CI run — filter them from the amiga pipelines.  Host
# pipelines still pick them up.
HOST_ONLY     := $(filter host_only_%, $(EXAMPLE_NAMES))
# `multi_file` lives in a subdirectory, so the flat examples/*.exl glob
# above misses it; append it so host/verify aggregates pick it up.  It is
# host-only (no amiga build) — see the dedicated host-multi_file /
# host-selfhost rules for the two directory examples.
HOST_EXAMPLES := $(filter-out $(AMIGA_ONLY), $(EXAMPLE_NAMES)) multi_file selfhost
AMIGA_EXAMPLES:= $(filter-out $(HOST_ONLY), $(EXAMPLE_NAMES))

GHCR_OWNER ?= damroth
CI_IMAGE   ?= ghcr.io/$(GHCR_OWNER)/exile-lang-ci:latest

HOST_BINS  := $(addprefix $(HOST_OUT)/,$(EXAMPLE_NAMES))
AMIGA_BINS := $(addprefix $(AMIGA_OUT)/,$(AMIGA_EXAMPLES))

.PHONY: all build test clean toolchain toolchain-clean
.PHONY: host amiga examples
.PHONY: host-% amiga-% run-% run-host-% c-% host-multi_file host-selfhost
.PHONY: verify verify-host verify-amiga verify-host-% verify-amiga-%
.PHONY: verify-freestanding verify-freestanding-%
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

# DR-006 — per-target backends for the `sys::*` seam.  Linked into
# every build for the matching target; the linker discards them when
# no `sys_*` symbol is reached (cc only complains about unresolved
# references, not unused defs).
SYS_HOST := runtime/sys_host.c
SYS_AMIGA := runtime/sys_amiga.c

# `make host-NAME`  → build host binary for examples/NAME.exl
host-%: examples/%.exl $(call stub_for,%) $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/$*.c --link $(SYS_HOST) $(call link_args,$*) -o $(HOST_OUT)/$* $<

# `multi_file` is a directory example: its entry point is main.exl, which
# `use`s sibling file-modules.  The flat host-% rule can't express that,
# so it gets a dedicated rule; verify/rebaseline reuse the % patterns
# (expected output lives in examples/multi_file.expected).
host-multi_file: examples/multi_file/main.exl examples/multi_file/lib.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/multi_file.c --link $(SYS_HOST) -o $(HOST_OUT)/multi_file $<

# `selfhost` is the in-progress OCaml->Exile compiler port (Faza 0).  Its
# entry point main.exl `use`s the dump/ir/ast/pos module chain and emits a
# canonical AST dump + typed-IR dump for the bundled fixtures; verify diffs
# that against examples/selfhost.expected (the OCaml oracle).
host-selfhost: examples/selfhost/main.exl examples/selfhost/dump.exl examples/selfhost/ir.exl examples/selfhost/token.exl examples/selfhost/ast.exl examples/selfhost/pos.exl examples/selfhost/fixture.exl examples/selfhost/ir_fixture.exl examples/selfhost/token_fixture.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost $<

# `make amiga-NAME` → build m68k Amiga binary
amiga-%: examples/%.exl $(call stub_for,%) $(SYS_AMIGA) build
	@if [ ! -x $(AMIGA_GCC) ]; then \
		echo "amiga-gcc missing — run 'make toolchain' first"; \
		exit 1; \
	fi
	$(EXILE) --target amiga --c-out $(C_OUT)/$*.c --link $(SYS_AMIGA) $(call link_args,$*) -o $(AMIGA_OUT)/$* $<

# `make c-NAME` → just emit C, no native binary
c-%: examples/%.exl build
	$(EXILE) --c-out $(C_OUT)/$*.c $<

# `make run-NAME` → build for Amiga and run under vamos.
# `make run-host-NAME` → build host and run natively.
run-%: amiga-%
	vamos $(AMIGA_OUT)/$*

run-host-%: host-%
	$(HOST_OUT)/$*

# Build everything for one target.  `host` only builds examples that
# can plausibly link against libnix (skips amiga-only ones).
host:  $(HOST_EXAMPLES:%=host-%)
amiga: $(AMIGA_EXAMPLES:%=amiga-%)
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

verify-host:  $(HOST_EXAMPLES:%=verify-host-%)
verify-amiga: $(AMIGA_EXAMPLES:%=verify-amiga-%)
verify: verify-host verify-amiga

# ===== Freestanding codegen mode (--freestanding) =====
#
# `freestanding_*.exl` examples are gated two ways:
#   (1) nm-clean — the emitted C, compiled `-ffreestanding` with the
#       stack-protector/PIC build artifacts off, may reference ONLY the
#       __ex_* helpers + the sys_* seam.  Any libc symbol (printf/strlen/
#       memset/malloc/...) is a hard failure — that nm-clean object IS the
#       definition of done for the freestanding floor.
#   (2) functional — link the emitted C against runtime/freestanding.c +
#       sys_host.c (the host crt bootstraps `main`; sys_host is the host
#       backend), run, diff examples/NAME.expected.  The same .exl also
#       rides verify-host / verify-amiga via the libc-printf path with a
#       byte-identical .expected.
FS_OUT       := $(OUT)/freestanding
FS_RUNTIME   := runtime/freestanding.c
FREESTANDING_EXAMPLES := $(filter freestanding_%, $(EXAMPLE_NAMES))

verify-freestanding-%: build
	@mkdir -p $(C_OUT) $(FS_OUT)
	@$(EXILE) --target c --freestanding --c-out $(C_OUT)/$*.fs.c examples/$*.exl >/dev/null
	@$(CC) $(CFLAGS) -ffreestanding -fno-stack-protector -fno-pic -I runtime \
		-c $(C_OUT)/$*.fs.c -o $(FS_OUT)/$*.o
	@leak=$$(nm -u $(FS_OUT)/$*.o | awk '{print $$NF}' | grep -vE '^(__ex_|sys_)' || true); \
	if [ -n "$$leak" ]; then \
		echo "verify-freestanding-$*: LIBC LEAK -> $$leak"; exit 1; fi
	@$(CC) -I runtime $(C_OUT)/$*.fs.c $(FS_RUNTIME) $(SYS_HOST) -o $(FS_OUT)/$*
	@if [ ! -f examples/$*.expected ]; then \
		echo "verify-freestanding-$*: missing examples/$*.expected"; exit 1; fi
	@actual=$$($(FS_OUT)/$*); expected=$$(cat examples/$*.expected); \
	if [ "$$actual" = "$$expected" ]; then \
		echo "verify-freestanding-$*: ok (nm-clean + output)"; \
	else \
		echo "verify-freestanding-$*: FAIL"; \
		diff <(echo "$$expected") <(echo "$$actual"); exit 1; \
	fi

verify-freestanding: $(FREESTANDING_EXAMPLES:%=verify-freestanding-%)

# Capture current host stdout into examples/NAME.expected.  Use when
# adding a new example or after an *intentional* output change; CI
# verify will fail on accidental drift.
rebaseline-host-%: host-%
	@$(HOST_OUT)/$* > examples/$*.expected
	@echo "rebaselined examples/$*.expected ($$(wc -l < examples/$*.expected) lines)"

rebaseline-host: $(HOST_EXAMPLES:%=rebaseline-host-%)

# ===== Self-host bring-up Faza −1 — differential-harness golden corpus =====
#
# Three canonical dumps per example, committed under tests/golden/.
# The OCaml exilc is the oracle; the future exile port aims to
# produce byte-identical output (`make selfhost-diff` becomes the
# port's regression test).  Run `make selfhost-corpus` to regen
# after an intentional format change or when adding an example.
#
# Token and AST dumps work on every example out of the box.  IR
# dumps run the full typecheck so any example that wouldn't
# host-compile cleanly also wouldn't carry an IR golden — we keep
# the union view here and let `selfhost-diff-ir` surface the
# mismatch if any.  `multi_file` lives in a subdir and uses a
# bespoke entry path — out of the flat corpus for v1.

SELFHOST_GOLDEN := tests/golden
SELFHOST_TOKENS := $(SELFHOST_GOLDEN)/tokens
SELFHOST_AST    := $(SELFHOST_GOLDEN)/ast
SELFHOST_IR     := $(SELFHOST_GOLDEN)/ir

.PHONY: selfhost-corpus selfhost-corpus-tokens selfhost-corpus-ast selfhost-corpus-ir
.PHONY: selfhost-diff selfhost-diff-tokens selfhost-diff-ast selfhost-diff-ir
.PHONY: selfhost-corpus-% selfhost-diff-%

selfhost-corpus: selfhost-corpus-tokens selfhost-corpus-ast selfhost-corpus-ir

selfhost-corpus-tokens: build
	@mkdir -p $(SELFHOST_TOKENS)
	@for name in $(EXAMPLE_NAMES); do \
		$(EXILE) --emit-tokens -o $(SELFHOST_TOKENS)/$$name.tokens examples/$$name.exl; \
	done
	@echo "selfhost-corpus-tokens: wrote $(words $(EXAMPLE_NAMES)) dumps"

selfhost-corpus-ast: build
	@mkdir -p $(SELFHOST_AST)
	@for name in $(EXAMPLE_NAMES); do \
		$(EXILE) --emit-ast -o $(SELFHOST_AST)/$$name.ast examples/$$name.exl; \
	done
	@echo "selfhost-corpus-ast: wrote $(words $(EXAMPLE_NAMES)) dumps"

selfhost-corpus-ir: build
	@mkdir -p $(SELFHOST_IR)
	@for name in $(EXAMPLE_NAMES); do \
		$(EXILE) --emit-typed-ir --user-only -o $(SELFHOST_IR)/$$name.ir examples/$$name.exl; \
	done
	@echo "selfhost-corpus-ir: wrote $(words $(EXAMPLE_NAMES)) dumps"

# Per-example regen — useful when iterating on one example without
# rebuilding the whole 66-file corpus.
selfhost-corpus-%: examples/%.exl build
	@mkdir -p $(SELFHOST_TOKENS) $(SELFHOST_AST) $(SELFHOST_IR)
	$(EXILE) --emit-tokens               -o $(SELFHOST_TOKENS)/$*.tokens $<
	$(EXILE) --emit-ast                  -o $(SELFHOST_AST)/$*.ast       $<
	$(EXILE) --emit-typed-ir --user-only -o $(SELFHOST_IR)/$*.ir         $<

# Verify the OCaml emitter still produces the committed corpus
# byte-for-byte.  Any drift is either an intentional format change
# (rerun `selfhost-corpus`) or a regression in the OCaml emitter.
# Once the exile port lands, the port runs through these same
# targets — drift then signals a port-vs-oracle bug.
selfhost-diff: selfhost-diff-tokens selfhost-diff-ast selfhost-diff-ir

selfhost-diff-tokens: build
	@fail=0; \
	for name in $(EXAMPLE_NAMES); do \
		actual=$$(mktemp); \
		$(EXILE) --emit-tokens examples/$$name.exl > $$actual 2>/dev/null; \
		if [ ! -f $(SELFHOST_TOKENS)/$$name.tokens ]; then \
			echo "selfhost-diff-tokens: missing $(SELFHOST_TOKENS)/$$name.tokens"; \
			fail=1; \
		elif ! diff -q $(SELFHOST_TOKENS)/$$name.tokens $$actual >/dev/null; then \
			echo "selfhost-diff-tokens: drift $$name"; \
			diff $(SELFHOST_TOKENS)/$$name.tokens $$actual | head -10; \
			fail=1; \
		fi; \
		rm $$actual; \
	done; \
	if [ $$fail -eq 0 ]; then echo "selfhost-diff-tokens: clean"; else exit 1; fi

selfhost-diff-ast: build
	@fail=0; \
	for name in $(EXAMPLE_NAMES); do \
		actual=$$(mktemp); \
		$(EXILE) --emit-ast examples/$$name.exl > $$actual 2>/dev/null; \
		if [ ! -f $(SELFHOST_AST)/$$name.ast ]; then \
			echo "selfhost-diff-ast: missing $(SELFHOST_AST)/$$name.ast"; \
			fail=1; \
		elif ! diff -q $(SELFHOST_AST)/$$name.ast $$actual >/dev/null; then \
			echo "selfhost-diff-ast: drift $$name"; \
			diff $(SELFHOST_AST)/$$name.ast $$actual | head -10; \
			fail=1; \
		fi; \
		rm $$actual; \
	done; \
	if [ $$fail -eq 0 ]; then echo "selfhost-diff-ast: clean"; else exit 1; fi

selfhost-diff-ir: build
	@fail=0; \
	for name in $(EXAMPLE_NAMES); do \
		actual=$$(mktemp); \
		$(EXILE) --emit-typed-ir --user-only examples/$$name.exl > $$actual 2>/dev/null; \
		if [ ! -f $(SELFHOST_IR)/$$name.ir ]; then \
			echo "selfhost-diff-ir: missing $(SELFHOST_IR)/$$name.ir"; \
			fail=1; \
		elif ! diff -q $(SELFHOST_IR)/$$name.ir $$actual >/dev/null; then \
			echo "selfhost-diff-ir: drift $$name"; \
			diff $(SELFHOST_IR)/$$name.ir $$actual | head -10; \
			fail=1; \
		fi; \
		rm $$actual; \
	done; \
	if [ $$fail -eq 0 ]; then echo "selfhost-diff-ir: clean"; else exit 1; fi

# Per-example diff across all three forms.  Convenient for chasing
# down a single example's drift.
selfhost-diff-%: examples/%.exl build
	@actual=$$(mktemp); \
	$(EXILE) --emit-tokens $< > $$actual; \
	diff $(SELFHOST_TOKENS)/$*.tokens $$actual && \
	$(EXILE) --emit-ast $< > $$actual; \
	diff $(SELFHOST_AST)/$*.ast $$actual && \
	$(EXILE) --emit-typed-ir --user-only $< > $$actual; \
	diff $(SELFHOST_IR)/$*.ir $$actual && \
	rm $$actual && \
	echo "selfhost-diff-$*: clean"

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
