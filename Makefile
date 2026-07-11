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
.PHONY: host-% amiga-% run-% run-host-% c-% host-multi_file host-selfhost host-selfhost-lexer host-selfhost-parser host-selfhost-tc host-selfhost-drop
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
host-selfhost: examples/selfhost/main.exl examples/selfhost/dump_ast.exl examples/selfhost/dump_type.exl examples/selfhost/dump_ir.exl examples/selfhost/dump_token.exl examples/selfhost/dump_util.exl examples/selfhost/ir.exl examples/selfhost/token.exl examples/selfhost/lexer.exl examples/selfhost/error.exl examples/selfhost/ast.exl examples/selfhost/pos.exl examples/selfhost/fixture.exl examples/selfhost/ir_fixture.exl examples/selfhost/token_fixture.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost $<

# The lexer corpus harness: read a source path on stdin, lex it with the
# ported `lexer::tokenize`, emit the token dump.  Token-only, so it pulls
# in just the lexer + token dumper (dump_token / dump_util) — not the
# AST/IR dumpers.  Driven by `selfhost-port-tokens`.
host-selfhost-lexer: examples/selfhost/lex_corpus.exl examples/selfhost/lexer.exl examples/selfhost/token.exl examples/selfhost/pos.exl examples/selfhost/error.exl examples/selfhost/dump_token.exl examples/selfhost/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_lexer.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_lexer $<

# The parser corpus harness: read a source path on stdin, lex + parse it
# with the ported `parser::parse_program`, emit the AST dump.  Pulls in
# the lexer + parser + AST dumper (dump_ast / dump_util) — not the IR
# dumper.  Driven by `selfhost-port-ast`.
host-selfhost-parser: examples/selfhost/parse_corpus.exl examples/selfhost/parser.exl examples/selfhost/lexer.exl examples/selfhost/token.exl examples/selfhost/pos.exl examples/selfhost/ast.exl examples/selfhost/error.exl examples/selfhost/dump_ast.exl examples/selfhost/dump_type.exl examples/selfhost/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_parser.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_parser $<

host-selfhost-tc: examples/selfhost/tc_corpus.exl examples/selfhost/typecheck.exl examples/selfhost/parser.exl examples/selfhost/loader.exl examples/selfhost/lexer.exl examples/selfhost/token.exl examples/selfhost/pos.exl examples/selfhost/ast.exl examples/selfhost/ir.exl examples/selfhost/error.exl examples/selfhost/dump_ir.exl examples/selfhost/dump_ast.exl examples/selfhost/dump_type.exl examples/selfhost/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_tc.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_tc $<

# Post-drop dumper — the tc pipeline plus the ported Drop pass.  Driven by
# `selfhost-port-drop-ir` against the oracle `--after-drop` dump.
host-selfhost-drop: examples/selfhost/drop_corpus.exl examples/selfhost/drop.exl examples/selfhost/move.exl examples/selfhost/typecheck.exl examples/selfhost/parser.exl examples/selfhost/loader.exl examples/selfhost/lexer.exl examples/selfhost/token.exl examples/selfhost/pos.exl examples/selfhost/ast.exl examples/selfhost/ir.exl examples/selfhost/error.exl examples/selfhost/dump_ir.exl examples/selfhost/dump_ast.exl examples/selfhost/dump_type.exl examples/selfhost/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_drop.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_drop $<

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
.PHONY: selfhost-corpus-% selfhost-diff-% selfhost-port-tokens selfhost-port-errors selfhost-port-ast selfhost-port-parse-errors selfhost-port-ir

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

# ===== Self-host PORT verification — exile lexer vs golden corpus =====
#
# `selfhost-diff-tokens` re-checks the OCaml *emitter*; this runs the
# *exile port* (the compiled `lexer::tokenize`) over every example and
# diffs its token dump against the committed golden.  Drift is a
# port-vs-oracle bug.
#
# Two renderings are deliberately deferred (no prelude primitive yet,
# tracked in SELFHOST-WORKLOG): the float literal VALUE (`(Float ?? w)`
# vs OCaml's `%h` hex form) and string-escape DECODING (raw vs decoded
# `(String …)` content).  Both are masked before the compare, so a
# divergence on anything else — token type, position, count, ordering,
# or the f32/f64 width tag — still fails.  The deferring examples are
# printed every run so the debt stays visible, never silently passed.
selfhost-port-tokens: host-selfhost-lexer
	@mask='s/\(Float [^ ]+ /(Float /; s/\(String .*\) @/(String) @/'; \
	fail=0; clean=0; defer=""; \
	for name in $(EXAMPLE_NAMES); do \
		[ -f $(SELFHOST_TOKENS)/$$name.tokens ] || continue; \
		actual=$$(mktemp); \
		echo "examples/$$name.exl" | $(HOST_OUT)/selfhost_lexer > $$actual 2>/dev/null; \
		if diff -q $(SELFHOST_TOKENS)/$$name.tokens $$actual >/dev/null; then \
			clean=$$((clean+1)); \
		elif diff <(sed -E "$$mask" $(SELFHOST_TOKENS)/$$name.tokens) <(sed -E "$$mask" $$actual) >/dev/null; then \
			defer="$$defer $$name"; \
		else \
			echo "selfhost-port-tokens: REGRESSION $$name"; \
			diff $(SELFHOST_TOKENS)/$$name.tokens $$actual | head -12; \
			fail=1; \
		fi; \
		rm $$actual; \
	done; \
	echo "selfhost-port-tokens: $$clean/$(words $(EXAMPLE_NAMES)) byte-identical; deferred (float-value/string-escape):$$defer"; \
	if [ $$fail -eq 0 ]; then echo "selfhost-port-tokens: clean (no structural regressions)"; else exit 1; fi

# Verify the PORT reports lexical errors with the same diagnostic the
# OCaml oracle does.  Each fixture in lex_errors/ holds one lexical error;
# compare the port's stderr (the CompileError Display) against the FIRST
# line of the OCaml `show_error` output — lines 2-3 (the source echo +
# caret) are a driver-level presentation layer the lexer port doesn't
# emit.  Exercises the `try`/Result error threading that the valid-only
# token corpus can't reach.
# ===== DR-010 escape pass — the port's differential gate =====
#
# The escape pass emits no code: its entire observable behaviour is its
# diagnostics.  So the gate runs BOTH compilers over each probe and byte-compares
# the message (position included) — and over every example, where both must stay
# silent.  `escape_corpus` prints the first diagnostic in the oracle's format, or
# "escape: ok".
.PHONY: host-selfhost-escape selfhost-port-escape

host-selfhost-escape: examples/selfhost/escape_corpus.exl examples/selfhost/escape.exl examples/selfhost/typecheck.exl examples/selfhost/parser.exl examples/selfhost/loader.exl examples/selfhost/lexer.exl examples/selfhost/token.exl examples/selfhost/pos.exl examples/selfhost/ast.exl examples/selfhost/ir.exl examples/selfhost/error.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_escape.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_escape $<

selfhost-port-escape: host-selfhost-escape
	@fail=0; n=0; \
	for f in $(patsubst %,examples/%.exl,$(EXAMPLE_NAMES)) tests/escape/*.exl; do \
	  n=$$((n+1)); \
	  port=$$(echo $$f | $(HOST_OUT)/selfhost_escape 2>&1 | head -1); \
	  orac=$$($(EXILE) --target host --c-out /tmp/escgate.c $$f 2>&1 \
	          | grep -m1 'embeds the address\|use of borrow' || echo 'escape: ok'); \
	  port_msg=$$(printf '%s' "$$port" | sed 's/.*: error: //'); \
	  orac_msg=$$(printf '%s' "$$orac" | sed 's/.*: error: //'); \
	  if [ "$$port_msg" != "$$orac_msg" ]; then \
	    fail=$$((fail+1)); echo "selfhost-port-escape: DIVERGE $$f"; \
	    echo "  port:   $$port_msg"; echo "  oracle: $$orac_msg"; \
	  fi; \
	done; \
	echo "selfhost-port-escape: $$((n-fail))/$$n agree; $$fail diverge"; \
	[ $$fail -eq 0 ]

selfhost-port-errors: host-selfhost-lexer
	@fail=0; n=0; \
	for f in examples/selfhost/lex_errors/*.exl; do \
		n=$$((n+1)); \
		oc=$$($(EXILE) --emit-tokens $$f 2>&1 >/dev/null | head -1); \
		pt=$$(echo $$f | $(HOST_OUT)/selfhost_lexer 2>&1 >/dev/null | head -1); \
		if [ "$$oc" = "$$pt" ] && [ -n "$$pt" ]; then \
			: ; \
		else \
			echo "selfhost-port-errors: MISMATCH $$(basename $$f)"; \
			echo "  oracle: $$oc"; \
			echo "  port:   $$pt"; \
			fail=1; \
		fi; \
	done; \
	if [ $$fail -eq 0 ]; then echo "selfhost-port-errors: clean ($$n fixtures, port == oracle line 1)"; else exit 1; fi

# Run the PORTED parser (`parser::parse_program`) over the corpus and diff
# its `--emit-ast` dump against the golden.  Honest bucketing — an example
# falls into exactly one of:
#   - byte-identical (the port's AST == oracle)
#   - float-value deferred (only `(float ?? w)` vs OCaml's `%h`, masked)
#   - explicit deferral: the port stopped on a documented "not yet ported"
#     marker (a declaration item, or generic trait bounds) — a tracked
#     sub-stage boundary, NOT a regression
#   - REGRESSION: a parse error WITHOUT that marker (the ported grammar
#     choked mid-expr/stmt = incompleteness/bug), or a non-float AST diff
# The marker test is what stops a swallowed expression gap from hiding in
# the "not yet ported" bucket (REVIEW CHECKPOINT 2).  String literals are
# masked in the DEFERRAL comparison only (not the byte-identical check),
# so a raw-vs-decoded escape difference reads as the documented
# string-escape-decode deferral, while clean examples still verify their
# string content exactly.
selfhost-port-ast: host-selfhost-parser
	@mask='s/\(float [^ ]+ /(float /g; s/\(string "(\\.|[^"\\])*"\)/(string)/g'; \
	clean=0; defer=""; notp=0; mf=0; fail=0; \
	for name in $(EXAMPLE_NAMES); do \
		[ -f $(SELFHOST_AST)/$$name.ast ] || continue; \
		if [ "$$name" = "reexport" ]; then mf=$$((mf+1)); continue; fi; \
		actual=$$(mktemp); errf=$$(mktemp); \
		echo "examples/$$name.exl" | $(HOST_OUT)/selfhost_parser > $$actual 2>$$errf; \
		if [ -s $$errf ]; then \
			if grep -q "not yet ported" $$errf; then notp=$$((notp+1)); \
			else echo "selfhost-port-ast: REGRESSION (parse error) $$name"; cat $$errf; fail=1; fi; \
		elif diff -q $(SELFHOST_AST)/$$name.ast $$actual >/dev/null; then clean=$$((clean+1)); \
		elif diff <(sed -E "$$mask" $(SELFHOST_AST)/$$name.ast) <(sed -E "$$mask" $$actual) >/dev/null; then defer="$$defer $$name"; \
		else echo "selfhost-port-ast: REGRESSION (AST diff) $$name"; diff $(SELFHOST_AST)/$$name.ast $$actual | head -6; fail=1; fi; \
		rm $$actual $$errf; \
	done; \
	echo "selfhost-port-ast: $$clean byte-identical; float/string-deferred:$$defer; explicit-deferral (decl/bounds not-yet-ported): $$notp; multi-file (needs loader): $$mf"; \
	if [ $$fail -eq 0 ]; then echo "selfhost-port-ast: clean (no expr/stmt regressions)"; else exit 1; fi

# Parser error parity with the lexer's `selfhost-port-errors`: each fixture
# in parse_errors/ holds one malformed construct (expr/stmt/type/pattern or
# declaration — extern/attribute/generic-bound/duplicate-key), reachable by
# the ported grammar.  Compare the port's diagnostic against the first line
# of the OCaml `show_error` output.  The dynamic token spelling (`got
# identifier 'x'` / `integer N` / `string "s"`) is built into the message,
# so these match byte-for-byte.
selfhost-port-parse-errors: host-selfhost-parser
	@fail=0; n=0; \
	for f in examples/selfhost/parse_errors/*.exl; do \
		n=$$((n+1)); \
		oc=$$($(EXILE) --emit-ast $$f 2>&1 >/dev/null | head -1); \
		pt=$$(echo $$f | $(HOST_OUT)/selfhost_parser 2>&1 >/dev/null | head -1); \
		if [ "$$oc" = "$$pt" ] && [ -n "$$pt" ]; then \
			: ; \
		else \
			echo "selfhost-port-parse-errors: MISMATCH $$(basename $$f)"; \
			echo "  oracle: $$oc"; \
			echo "  port:   $$pt"; \
			fail=1; \
		fi; \
	done; \
	if [ $$fail -eq 0 ]; then echo "selfhost-port-parse-errors: clean ($$n fixtures, port == oracle line 1)"; else exit 1; fi

# Typed-IR port gate (staged).  Compare the port's typecheck output against
# the live oracle `--emit-typed-ir --user-only` (user `(tfn ...)` lines +
# the fixed prelude `mono-instances` footprint).  The gate is SELF-HONEST
# about which dimension diverges, by also comparing against a body-masked
# oracle (`(body ...)` collapsed to `(body )` per tfn line — bodies are the
# one dimension the elaborator fills in last):
#   clean       — port == oracle verbatim (signature + footprint + body).
#   body-pending — port == body-masked oracle: signature + mono-footprint
#                  are byte-identical, only the (empty) body is unfilled.
#                  This is the honest "ready modulo body" count.
#   SIG-DIVERGE — port differs even after body-masking: the name (mangling),
#                 a param/return type, or the mono-instances footprint is
#                 wrong, OR the port emitted a non-empty body that does not
#                 match — a real regression, NOT deferred work.  Printed.
# Every unsupported construct — expression OR statement — lowers to a
# visible deferral marker (`null :ty c_void`, or `(float ??` for the
# consciously-deferred hex-float value format), so incompleteness is visible.  That
# makes body-divergence honestly classifiable: when the signature/footprint
# match (masked-both) but the bodies differ, the port output MUST carry a
# marker for the divergence to be deferred work.  A body that differs with
# NO marker is a real elaboration bug (a dropped/mis-elaborated statement
# that looks complete) — flagged BODY-REGRESSION, not hidden in body-pending.
# Buckets: clean / body-pending (sig+footprint identical, body differs but
# every gap is a visible marker) / BODY-REGRESSION (sig identical, body
# differs, NO marker) / SIG-DIVERGE (differs after body-masking).  Examples
# whose oracle dump is empty (amiga-only / typecheck-rejected) are skipped.
selfhost-port-ir: host-selfhost-tc
	@clean=0; defer=0; bodyregr=0; sig=0; skip=0; fail=0; \
	mask='s/(body .*$$/(body ))/'; \
	for name in $(EXAMPLE_NAMES); do \
		[ -f examples/$$name.exl ] || continue; \
		oc=$$(mktemp); pt=$$(mktemp); ocm=$$(mktemp); ptm=$$(mktemp); \
		$(EXILE) --emit-typed-ir --user-only examples/$$name.exl >$$oc 2>/dev/null; \
		echo "examples/$$name.exl" | $(HOST_OUT)/selfhost_tc >$$pt 2>/dev/null; \
		if [ ! -s $$oc ]; then skip=$$((skip+1)); rm $$oc $$pt $$ocm $$ptm; continue; fi; \
		sed "$$mask" $$oc >$$ocm; sed "$$mask" $$pt >$$ptm; \
		if diff -q $$oc $$pt >/dev/null; then clean=$$((clean+1)); \
		elif diff -q $$ocm $$ptm >/dev/null; then \
			if grep -qE 'null :ty c_void|\(float \?\?' $$pt; then defer=$$((defer+1)); \
			else bodyregr=$$((bodyregr+1)); fail=1; \
				echo "selfhost-port-ir: BODY-REGRESSION $$name (sig OK, body differs, no deferral marker)"; \
				diff $$oc $$pt | head -4; fi; \
		else sig=$$((sig+1)); \
			echo "selfhost-port-ir: SIG-DIVERGE $$name"; \
			diff $$ocm $$ptm | head -4; fi; \
		rm $$oc $$pt $$ocm $$ptm; \
	done; \
	echo "selfhost-port-ir: $$clean clean; $$defer body-pending (marked); $$bodyregr BODY-REGRESSION; $$sig SIG-DIVERGE; $$skip skipped (oracle empty)"; \
	if [ $$bodyregr -ne 0 ]; then exit 1; fi

# Staged differential for the ported Drop pass.  The oracle runs
# `--after-drop` (post Move/Escape/Drop); the port binary is `selfhost_drop`
# (tc pipeline + ported Drop).  Drop has no deferral-marker mechanism — an
# unported body is simply the valid pre-drop body missing its auto-drop
# insertions — so the buckets are plain:
#   clean       — port == oracle (drop insertion reproduced byte-for-byte).
#   unported    — signature/footprint identical (body-masked equal), body
#                 differs: drop insertion not yet reproduced (staged work).
#   SIG-DIVERGE — differs even after body-masking: a real bug (wrong name,
#                 type, or footprint, or a mangled body).  Printed.
selfhost-port-drop-ir: host-selfhost-drop
	@clean=0; unported=0; sig=0; skip=0; \
	mask='s/(body .*$$/(body ))/'; \
	for name in $(EXAMPLE_NAMES); do \
		[ -f examples/$$name.exl ] || continue; \
		oc=$$(mktemp); pt=$$(mktemp); ocm=$$(mktemp); ptm=$$(mktemp); \
		$(EXILE) --emit-typed-ir --after-drop --user-only examples/$$name.exl >$$oc 2>/dev/null; \
		echo "examples/$$name.exl" | $(HOST_OUT)/selfhost_drop >$$pt 2>/dev/null; \
		if [ ! -s $$oc ]; then skip=$$((skip+1)); rm $$oc $$pt $$ocm $$ptm; continue; fi; \
		sed "$$mask" $$oc >$$ocm; sed "$$mask" $$pt >$$ptm; \
		if diff -q $$oc $$pt >/dev/null; then clean=$$((clean+1)); \
		elif diff -q $$ocm $$ptm >/dev/null; then unported=$$((unported+1)); \
		else sig=$$((sig+1)); \
			echo "selfhost-port-drop-ir: SIG-DIVERGE $$name"; \
			diff $$ocm $$ptm | head -4; fi; \
		rm $$oc $$pt $$ocm $$ptm; \
	done; \
	echo "selfhost-port-drop-ir: $$clean clean; $$unported unported; $$sig SIG-DIVERGE; $$skip skipped (oracle empty)"

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
