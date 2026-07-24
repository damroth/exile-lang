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

# Defined here, ahead of the gates, because five of them take the seed as a
# prerequisite and a prerequisite is expanded where the rule is READ.  What the
# snapshot is and when it is refreshed: see the seed bootstrap section below.
SEED_C := seed/exilc.c
# The same warning mask exilc itself passes to cc (exilc's own Lint already
# covers these), so a seed build is as quiet as a normal one.
CC_QUIET := -Wno-unused-variable -Wno-unused-but-set-variable -Wno-unused-function

# ===== The gates that need no OCaml =====
#
# `selfhost-verify` splits in two.  Twelve gates COMPARE the port against the
# reference, so they need opam by construction.  Five do not: `-port-tokens` and
# `-port-ast` diff against the committed goldens under tests/golden,
# `-port-module-roots` and `-no-fabrication` assert a property of the port alone,
# and `bootstrap-fixpoint` compares the port against itself.  In those five the
# oracle only ever BUILT the binary — so they build from the seed instead, and
# run on a machine that has never seen opam (`make selfhost-seed-gates`).
#
# The builder is stage B of the ladder — the compiler built from the CURRENT
# source by the seed-built compiler — never the seed binary itself.  The seed is
# a snapshot and is allowed to lag; a driver built from it would carry the lag,
# stage B never does.
#
# These binaries are deliberately SEPARATE from the oracle-built `selfhost_*`
# ones, even where the source root is identical.  An independent builder is the
# reason a codegen bug in the port cannot hide itself in the run that judges the
# port's semantics — the twelve comparing gates keep that property.
SEEDC_SRCS   := $(wildcard src/*.exl)
SEEDC_A      := $(HOST_OUT)/seedc_a
SEEDC_EXILC  := $(HOST_OUT)/seedc_b
SEEDC_LEXER  := $(HOST_OUT)/seedc_lexer
SEEDC_PARSER := $(HOST_OUT)/seedc_parser
SEEDC_TC     := $(HOST_OUT)/seedc_tc
SEEDC_CG     := $(HOST_OUT)/seedc_cg

# Stage A (the seed's own codegen) then stage B (current source).  Both floors
# are hard: an empty seed, and a seed that can no longer build the source, are
# the two ways this rots — each names itself here rather than surfacing later as
# a gate failing for an unrelated-looking reason.
$(SEEDC_EXILC): $(SEED_C) $(SEEDC_SRCS) $(SYS_HOST)
	@if [ ! -s $(SEED_C) ]; then echo "seedc: MISSING or EMPTY $(SEED_C)"; exit 1; fi
	@mkdir -p $(HOST_OUT) $(C_OUT)
	@rm -f $(SEEDC_A) $@ $(C_OUT)/seedc_b.c
	@$(CC) $(CFLAGS) $(CC_QUIET) -o $(SEEDC_A) $(SEED_C) $(SYS_HOST) 2>/dev/null \
	  || { echo "seedc: cc could not build $(SEED_C)"; exit 1; }
	@$(SEEDC_A) --target host --c-out $(C_OUT)/seedc_b.c --link $(SYS_HOST) \
	   -o $@ src/exilc.exl >/dev/null 2>&1 \
	  || { echo "seedc: the seed cannot build the current source — refresh it (make seed)"; exit 1; }
	@if [ ! -s $@ ]; then echo "seedc: stage B produced no binary"; exit 1; fi

seedc_build = rm -f $(1); \
	$(SEEDC_EXILC) --target host --c-out $(C_OUT)/$(notdir $(1)).c --link $(SYS_HOST) \
	   -o $(1) $(2) >/dev/null \
	  || { echo "seedc: stage B could not build $(2)"; exit 1; }; \
	if [ ! -s $(1) ]; then echo "seedc: no binary for $(2)"; exit 1; fi

# The four corpus drivers, same roots as the `host-selfhost-*` rules above, this
# time compiled by stage B.  Every driver depends on the whole of src/: the port
# modules are interconnected enough that a curated list is a maintenance trap,
# and over-rebuilding is the cheap direction to be wrong in.
$(SEEDC_LEXER):  src/lex_corpus.exl     $(SEEDC_EXILC) $(SEEDC_SRCS) $(SYS_HOST)
	@$(call seedc_build,$@,$<)
$(SEEDC_PARSER): src/parse_corpus.exl   $(SEEDC_EXILC) $(SEEDC_SRCS) $(SYS_HOST)
	@$(call seedc_build,$@,$<)
$(SEEDC_TC):     src/tc_corpus.exl      $(SEEDC_EXILC) $(SEEDC_SRCS) $(SYS_HOST)
	@$(call seedc_build,$@,$<)
$(SEEDC_CG):     src/codegen_corpus.exl $(SEEDC_EXILC) $(SEEDC_SRCS) $(SYS_HOST)
	@$(call seedc_build,$@,$<)

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
host-selfhost: src/main.exl src/dump_ast.exl src/dump_type.exl src/dump_ir.exl src/dump_token.exl src/dump_util.exl src/ir.exl src/token.exl src/lexer.exl src/error.exl src/ast.exl src/pos.exl src/fixture.exl src/ir_fixture.exl src/token_fixture.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost $<

# The lexer corpus harness: read a source path on stdin, lex it with the
# ported `lexer::tokenize`, emit the token dump.  Token-only, so it pulls
# in just the lexer + token dumper (dump_token / dump_util) — not the
# AST/IR dumpers.  Driven by `selfhost-port-tokens`.
host-selfhost-lexer: src/lex_corpus.exl src/lexer.exl src/token.exl src/pos.exl src/error.exl src/dump_token.exl src/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_lexer.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_lexer $<

# The parser corpus harness: read a source path on stdin, lex + parse it
# with the ported `parser::parse_program`, emit the AST dump.  Pulls in
# the lexer + parser + AST dumper (dump_ast / dump_util) — not the IR
# dumper.  Driven by `selfhost-port-ast`.
host-selfhost-parser: src/parse_corpus.exl src/parser.exl src/lexer.exl src/token.exl src/pos.exl src/ast.exl src/error.exl src/dump_ast.exl src/dump_type.exl src/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_parser.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_parser $<

host-selfhost-tc: src/tc_corpus.exl src/typecheck.exl src/parser.exl src/loader.exl src/lexer.exl src/token.exl src/pos.exl src/ast.exl src/ir.exl src/error.exl src/dump_ir.exl src/dump_ast.exl src/dump_type.exl src/dump_util.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_tc.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_tc $<

# Post-drop dumper — the tc pipeline plus the ported Drop pass.  Driven by
# `selfhost-port-drop-ir` against the oracle `--after-drop` dump.
host-selfhost-drop: src/drop_corpus.exl src/drop.exl src/move.exl src/typecheck.exl src/parser.exl src/loader.exl src/lexer.exl src/token.exl src/pos.exl src/ast.exl src/ir.exl src/error.exl src/dump_ir.exl src/dump_ast.exl src/dump_type.exl src/dump_util.exl $(SYS_HOST) build
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

# ===== selfhost-amiga — the port's --target amiga vs the oracle's =====
#
# Faza E: the port gained `--target amiga` (the DR-006 driver seam — m68k-
# amigaos-gcc, -noixemul, -lm, sys_amiga link).  The emitted C is target-
# INDEPENDENT (codegen branches on neither target nor profile), and its byte
# parity is already gated corpus-wide by `selfhost-verify`; this gate proves the
# DRIVER — that the port's gcc invocation produces the same running m68k binary
# the oracle's does.
#
# The set is tier-CLEAN examples (nothing above the amiga default profile
# `standard`), so parity holds in EVERY channel — emitted C, the cross-compiled
# binary, stdout, AND stderr.  Collection/closure examples (own_ptr, closures_a2)
# are deliberately excluded: under `--profile=standard` the oracle's linter emits
# tier warnings the full-only port does not, a stderr divergence that belongs to
# the DEFERRED profile-lint round, not the driver.  The chosen six cover the
# axes that DO reach the driver: amiga.lib linking (amiga_hello), soft-float -lm
# (floats), the freestanding source shape (freestanding_print), plain code
# (enums), monomorphisation (generics) and match lowering (pattern_guards).
#
# Requires the cross toolchain + vamos, so it is a STANDALONE target (like
# verify-amiga), NOT part of the toolchain-free `selfhost-verify`.
AMIGA_GATE := amiga_hello enums floats freestanding_print generics pattern_guards

.PHONY: selfhost-amiga
selfhost-amiga: $(EXILC_BIN)
	@if [ ! -x $(AMIGA_GCC) ]; then echo "selfhost-amiga: MISSING cross toolchain ($(AMIGA_GCC)) — run 'make toolchain'"; exit 1; fi
	@command -v vamos >/dev/null 2>&1 || { echo "selfhost-amiga: vamos not on PATH"; exit 1; }
	@mkdir -p $(C_OUT) $(AMIGA_OUT)
	@fail=0; n=0; \
	for name in $(AMIGA_GATE); do \
	  f=examples/$$name.exl; \
	  [ -f $$f ] || { echo "selfhost-amiga: MISSING $$f"; fail=1; continue; }; \
	  [ -f examples/$$name.expected ] || { echo "selfhost-amiga: MISSING examples/$$name.expected"; fail=1; continue; }; \
	  stub=""; [ -f examples/$${name}_stub.c ] && stub="--link examples/$${name}_stub.c"; \
	  rm -f $(C_OUT)/am_o.c $(C_OUT)/am_p.c $(AMIGA_OUT)/am_o $(AMIGA_OUT)/am_p \
	        $(C_OUT)/am_o.msg $(C_OUT)/am_p.msg $(C_OUT)/am_o.err $(C_OUT)/am_p.err; \
	  n=$$((n+1)); \
	  $(EXILE) --target amiga --c-out $(C_OUT)/am_o.c --link $(SYS_AMIGA) $$stub -o $(AMIGA_OUT)/am_o $$f >$(C_OUT)/am_o.msg 2>$(C_OUT)/am_o.err \
	    || { echo "selfhost-amiga: ORACLE build failed $$name"; cat $(C_OUT)/am_o.err; fail=1; continue; }; \
	  $(EXILC_BIN) --target amiga --c-out $(C_OUT)/am_p.c --link $(SYS_AMIGA) $$stub -o $(AMIGA_OUT)/am_p $$f >$(C_OUT)/am_p.msg 2>$(C_OUT)/am_p.err \
	    || { echo "selfhost-amiga: PORT build failed $$name"; cat $(C_OUT)/am_p.err; fail=1; continue; }; \
	  if [ ! -s $(C_OUT)/am_o.c ]; then echo "selfhost-amiga: EMPTY oracle C $$name (mutual-failure floor)"; fail=1; continue; fi; \
	  if ! cmp -s $(C_OUT)/am_o.c $(C_OUT)/am_p.c; then echo "selfhost-amiga: C DIVERGE $$name"; diff $(C_OUT)/am_o.c $(C_OUT)/am_p.c | head -6; fail=1; continue; fi; \
	  if [ ! -s $(AMIGA_OUT)/am_o ] || [ ! -s $(AMIGA_OUT)/am_p ]; then echo "selfhost-amiga: EMPTY binary $$name"; fail=1; continue; fi; \
	  if ! cmp -s $(AMIGA_OUT)/am_o $(AMIGA_OUT)/am_p; then echo "selfhost-amiga: BINARY DIVERGE $$name"; fail=1; continue; fi; \
	  o_msg=$$(sed 's|am_o|am_X|' $(C_OUT)/am_o.msg); p_msg=$$(sed 's|am_p|am_X|' $(C_OUT)/am_p.msg); \
	  if [ "$$o_msg" != "$$p_msg" ]; then echo "selfhost-amiga: MESSAGE $$name"; echo "  o: $$o_msg"; echo "  p: $$p_msg"; fail=1; continue; fi; \
	  case "$$p_msg" in *"[profile=standard, target=amiga]"*) ;; *) echo "selfhost-amiga: message lost the amiga profile tag $$name: $$p_msg"; fail=1; continue;; esac; \
	  if ! cmp -s $(C_OUT)/am_o.err $(C_OUT)/am_p.err; then echo "selfhost-amiga: STDERR DIVERGE $$name (tier-clean set must match on stderr too)"; diff $(C_OUT)/am_o.err $(C_OUT)/am_p.err | head; fail=1; continue; fi; \
	  out=$$(vamos $(AMIGA_OUT)/am_p 2>/dev/null); exp=$$(cat examples/$$name.expected); \
	  if [ "$$out" != "$$exp" ]; then echo "selfhost-amiga: VAMOS $$name (identical binaries must also RUN correctly)"; diff <(echo "$$exp") <(echo "$$out") | head; fail=1; continue; fi; \
	done; \
	rm -f $(C_OUT)/rune_am.c $(AMIGA_OUT)/rune_am; \
	if [ $$fail -eq 0 ]; then \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/rune_am.c tests/rune/ram_roundtrip.exl >/dev/null 2>&1 \
	    || { echo "selfhost-amiga: PORT rejected the rune-over-RAM witness"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(AMIGA_GCC) -noixemul -O2 $(CC_QUIET) -I src -o $(AMIGA_OUT)/rune_am $(C_OUT)/rune_am.c tests/rune/ram_roundtrip_stub.c $(SYS_AMIGA) -lm \
	    || { echo "selfhost-amiga: rune-over-RAM cross-compile failed"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  rout=$$(vamos $(AMIGA_OUT)/rune_am 2>/dev/null); rexp=$$(printf '11\n22\n0\n305419896'); \
	  if [ "$$rout" != "$$rexp" ]; then echo "selfhost-amiga: rune-over-RAM VAMOS round-trip WRONG (m68k volatile lowering):"; echo "$$rout"; fail=1; fi; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-amiga: clean ($$n examples port==oracle on C/m68k binary/stdout/stderr + vamos==expected; rune-over-RAM runs 11/22/0/305419896 on m68k under vamos)"; \
	else exit 1; fi

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
selfhost-port-tokens: $(SEEDC_LEXER)
	@mask='s/\(Float [^ ]+ /(Float /; s/\(String .*\) @/(String) @/'; \
	fail=0; clean=0; defer=""; \
	for name in $(EXAMPLE_NAMES); do \
		[ -f $(SELFHOST_TOKENS)/$$name.tokens ] || continue; \
		actual=$$(mktemp); \
		echo "examples/$$name.exl" | $(SEEDC_LEXER) > $$actual 2>/dev/null; \
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
# ===== The no-fabrication policy =====
#
# A fallback that emits something LOOKING like a result is worse than a crash: it
# turns a compiler bug into a confusing error from `cc`, or into code that runs.
# `<ctype?>` / `<mangle?>` / `/*stmt?*/` / `?concat?` / `<cvoid?>` were all dead on
# the whole valid corpus (measured), so they never helped a real program — they only
# ever hid a bug.  They are `ice()` now, and this gate keeps them from coming back.
#
# The signature is `?` in an emitted literal, and it is chosen because it is
# EXHAUSTIVE rather than conventional: `?` appears ZERO times in the C the reference
# emits for all 90 examples, so any `?` a codegen literal can put into the output is
# a marker by construction.  A first cut grepped for the marker SHAPE (`"<...?>"`)
# and let the historical `"<ctype?> "` back in on a trailing space — the shape is not
# the invariant, the character is.
#
# The one legitimate `?` is the ternary in `println(<bool>)`, which is spelled with
# an escaped quote (`) ? \"`); it is excluded by name, not by pattern, so a new
# exception has to be argued for rather than slipped in.
.PHONY: selfhost-no-fabrication

# Two halves, because the first one alone was scoped to the file where the
# family happened to be found.  A `?` shipped into the emitted C from
# typecheck.exl's mangler (`ex_apply2_?`), which the source scan never looked
# at: the PROPERTY was right, its SCOPE was one file.
#
# Half 1 scans every source that produces a NAME (codegen / typecheck / drop),
# with full-line comments stripped so prose about the rule cannot trip it.  The
# one legitimate marker is whitelisted by an intent TAG, not by its file: a
# latch-and-continue compiler must be able to name an unnameable type after a
# diagnostic is latched (proven by `vec_unbound_tparam`, where aborting instead
# replaced the user's error with a crash).
# Half 2 is the half with real teeth: it scans the ARTIFACT.  No emitted C may
# contain a `?` glued to an identifier, whatever file the literal lived in.
NAME_SOURCES := src/codegen.exl src/typecheck.exl src/drop.exl

selfhost-no-fabrication: $(SEEDC_EXILC)
	@hits=""; \
	for src in $(NAME_SOURCES); do \
	  h=$$(sed 's,^[[:space:]]*//.*,,' $$src \
	       | grep -nE '"[^"]*\?[^"]*"' \
	       | grep -vF ') ? \"' \
	       | grep -vF 'internal:' \
	       | grep -vF 'no-fabrication: sole marker' || true); \
	  if [ -n "$$h" ]; then hits="$$hits$$src:$$h\n"; fi; \
	done; \
	if [ -n "$$hits" ]; then \
	  echo "selfhost-no-fabrication: a name-producing literal would put '?' into the output —"; \
	  echo "  a fallback that fabricates a plausible token instead of calling ice():"; \
	  printf "  $$hits"; \
	  exit 1; \
	fi; \
	scanned=0; bad=0; \
	for f in $(patsubst %,examples/%.exl,$(EXAMPLE_NAMES)) tests/mono/*.exl tests/lint/*.exl tests/xprod/*.exl; do \
	  [ -f $$f ] || continue; \
	  scanned=$$((scanned+1)); \
	  rm -f $(C_OUT)/fabscan.c; \
	  $(SEEDC_EXILC) --target c --c-out $(C_OUT)/fabscan.c $$f >/dev/null 2>&1; \
	  if [ ! -s $(C_OUT)/fabscan.c ]; then \
	    echo "selfhost-no-fabrication: EMPTY C for $$f (scan floor)"; bad=$$((bad+1)); continue; \
	  fi; \
	  if grep -qE '[A-Za-z0-9_]\?' $(C_OUT)/fabscan.c; then \
	    echo "selfhost-no-fabrication: '?' glued to an identifier in the C for $$f"; \
	    grep -nE '[A-Za-z0-9_]\?' $(C_OUT)/fabscan.c | head -3 | sed 's/^/  /'; \
	    bad=$$((bad+1)); \
	  fi; \
	done; \
	if [ $$bad -ne 0 ]; then exit 1; fi; \
	echo "selfhost-no-fabrication: clean (no name literal, and no '?' in $$scanned emitted files)"

# ===== Faza A — typecheck diagnostics =====
#
# Same shape as selfhost-port-errors (lexer, 12) and -parse-errors (46): each
# fixture is a program the reference implementation REJECTS, and both compilers
# must produce the SAME first line — message and position.
#
# The corpus grows one error FAMILY at a time.  A family is done when its fixtures
# agree; a family not yet ported simply has no fixtures here yet, so the gate stays
# meaningful (green) instead of permanently red.
.PHONY: selfhost-port-tc-errors

selfhost-port-tc-errors: host-selfhost-tc
	@fail=0; n=0; \
	for f in src/tc_errors/*.exl; do \
	  n=$$((n+1)); \
	  oc=$$($(EXILE) --target c $$f 2>&1 >/dev/null | head -1); \
	  pt=$$(echo $$f | $(HOST_OUT)/selfhost_tc 2>&1 >/dev/null | head -1); \
	  if [ "$$oc" = "$$pt" ] && [ -n "$$pt" ]; then \
	    : ; \
	  else \
	    echo "selfhost-port-tc-errors: MISMATCH $$(basename $$f)"; \
	    echo "  oracle: $$oc"; \
	    echo "  port:   $$pt"; \
	    fail=1; \
	  fi; \
	done; \
	if [ $$n -lt 30 ]; then \
	  echo "selfhost-port-tc-errors: only $$n fixtures — the corpus is missing files."; \
	  echo "  A gate with nothing to check reads as clean.  Floor the count."; \
	  exit 1; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-port-tc-errors: clean ($$n fixtures, port == oracle line 1)"; \
	else exit 1; fi

# ===== The bootstrap fixpoint — the self-host proof, as a gate =====
#
# The port compiles the port, and the compiler THAT produces emits byte-identical
# C for the same source.  That is what "self-hosted" means here, and until now it
# was reproduced by hand in sessions — so the first regression would have gone
# unnoticed.  Chain:
#
#   gen-1 = the oracle compiles src/codegen_corpus.exl        (OCaml -> a port binary)
#   C1    = gen-1 emits C for src/codegen_corpus.exl          (port -> its own source)
#   gen-2 = cc C1 + runtime/sys_host.c                        (that C, built)
#   C2    = gen-2 emits C for src/codegen_corpus.exl          (the port built BY the port)
#   diff C1 C2 must be EMPTY.
#
# A non-empty diff means the port's output depends on which compiler built it —
# i.e. the port is not a fixpoint of itself.  Hard failure.
.PHONY: host-selfhost-cg bootstrap-fixpoint selfhost-verify selfhost-seed-gates selfhost-seed-parity selfhost-rune selfhost-ward

# The oracle-built codegen driver.  `bootstrap-fixpoint` used to be its only
# consumer and now builds from the seed; kept as the manual entry point, and its
# oracle codegen path stays measured by `selfhost-seed-parity`.
host-selfhost-cg: src/codegen_corpus.exl src/codegen.exl src/drop.exl src/move.exl src/typecheck.exl src/parser.exl src/loader.exl src/lexer.exl src/token.exl src/pos.exl src/ast.exl src/ir.exl src/error.exl $(SYS_HOST) build
	$(EXILE) --target host --c-out $(C_OUT)/selfhost_cg.c --link $(SYS_HOST) -o $(HOST_OUT)/selfhost_cg $<

bootstrap-fixpoint: $(SEEDC_CG)
	@mkdir -p $(C_OUT)/fixpoint
	@echo src/codegen_corpus.exl | $(SEEDC_CG) > $(C_OUT)/fixpoint/C1.c
	@if [ ! -s $(C_OUT)/fixpoint/C1.c ]; then \
	  echo "bootstrap-fixpoint: FAIL — gen-1 emitted nothing (crash?)"; exit 1; \
	fi
	@cc -ansi -pedantic -w -I src -o $(C_OUT)/fixpoint/gen2 \
	   $(C_OUT)/fixpoint/C1.c $(SYS_HOST)
	@echo src/codegen_corpus.exl | $(C_OUT)/fixpoint/gen2 > $(C_OUT)/fixpoint/C2.c
	@if cmp -s $(C_OUT)/fixpoint/C1.c $(C_OUT)/fixpoint/C2.c; then \
	  echo "bootstrap-fixpoint: ok — gen-2 reproduces gen-1's C byte for byte ($$(wc -l < $(C_OUT)/fixpoint/C1.c) lines)"; \
	else \
	  echo "bootstrap-fixpoint: FAIL — the port is not a fixpoint of itself"; \
	  diff $(C_OUT)/fixpoint/C1.c $(C_OUT)/fixpoint/C2.c | head -40; \
	  exit 1; \
	fi

# Everything that guards the self-host proof, in one target.  `make test` /
# `verify-host` / `selfhost-diff` check the ORACLE; these check the PORT.
# ===== exilc driver — the unified CLI vs the oracle =====
#
# Faza B stitches the six corpus drivers into one argv-driven `exilc`.  Its
# emit modes are the differential-gate contract, so it must reproduce the
# ORACLE byte-for-byte on every mode.  This gate builds exilc (oracle host
# build) and diffs its output against `dune exec exilc` — the reference — over
# a representative slice of the corpus.  Byte-drift is a driver bug.
EXILC_BIN := $(HOST_OUT)/exilc
EXILC_SAMPLE := enums traits generics closures_a2 let_else exhaustiveness \
                combinator_map pattern_guards modules reexport derive floats
$(EXILC_BIN): src/exilc.exl build
	@$(EXILE) --target host --c-out $(C_OUT)/exilc.c --link $(SYS_HOST) -o $(EXILC_BIN) src/exilc.exl >/dev/null

selfhost-exilc-driver: $(EXILC_BIN)
	@fail=0; \
	for name in $(EXILC_SAMPLE); do \
	  f=examples/$$name.exl; \
	  if [ ! -f $$f ]; then echo "selfhost-exilc-driver: MISSING sample $$f"; fail=1; continue; fi; \
	  for mode in "--emit-tokens" "--emit-ast" "--emit-typed-ir --user-only" "--emit-typed-ir --user-only --after-drop"; do \
	    $(EXILC_BIN) $$mode $$f > $(C_OUT)/exilc_e.out 2>/dev/null; \
	    $(EXILE) $$mode $$f -o $(C_OUT)/exilc_o.out 2>/dev/null; \
	    if [ ! -s $(C_OUT)/exilc_o.out ]; then \
	      echo "selfhost-exilc-driver: EMPTY reference $$name [$$mode] (mutual-failure floor)"; fail=1; \
	    elif ! diff -q $(C_OUT)/exilc_o.out $(C_OUT)/exilc_e.out >/dev/null; then \
	      echo "selfhost-exilc-driver: DRIFT $$name [$$mode]"; fail=1; \
	    fi; \
	  done; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/exilc_e.c $$f 2>/dev/null; \
	  $(EXILE) --target c --c-out $(C_OUT)/exilc_o.c $$f 2>/dev/null; \
	  if [ ! -s $(C_OUT)/exilc_o.c ]; then \
	    echo "selfhost-exilc-driver: EMPTY reference $$name [--target c] (mutual-failure floor)"; fail=1; \
	  elif ! diff -q $(C_OUT)/exilc_o.c $(C_OUT)/exilc_e.c >/dev/null; then \
	    echo "selfhost-exilc-driver: DRIFT $$name [--target c]"; fail=1; \
	  fi; \
	  $(EXILC_BIN) --target host --c-out $(C_OUT)/exilc_he.c -o $(HOST_OUT)/exilc_he --link $(SYS_HOST) $$f > $(C_OUT)/exilc_he.line 2>/dev/null; rce=$$?; \
	  $(EXILE)     --target host --c-out $(C_OUT)/exilc_ho.c -o $(HOST_OUT)/exilc_ho --link $(SYS_HOST) $$f > $(C_OUT)/exilc_ho.line 2>/dev/null; rco=$$?; \
	  sed 's#exilc_h[eo]#exilc_hX#' $(C_OUT)/exilc_ho.line > $(C_OUT)/exilc_ho.norm; \
	  sed 's#exilc_h[eo]#exilc_hX#' $(C_OUT)/exilc_he.line > $(C_OUT)/exilc_he.norm; \
	  if [ $$rco -ne 0 ]; then \
	    echo "selfhost-exilc-driver: reference build failed $$name [--target host] (mutual-failure floor)"; fail=1; \
	  elif [ $$rce -ne $$rco ]; then \
	    echo "selfhost-exilc-driver: DRIFT $$name [--target host: build-result exilc=$$rce oracle=$$rco]"; fail=1; \
	  elif ! diff -q $(C_OUT)/exilc_ho.norm $(C_OUT)/exilc_he.norm >/dev/null; then \
	    echo "selfhost-exilc-driver: DRIFT $$name [--target host: success line]"; fail=1; \
	  else \
	    $(HOST_OUT)/exilc_ho > $(C_OUT)/exilc_ho.run 2>&1; \
	    $(HOST_OUT)/exilc_he > $(C_OUT)/exilc_he.run 2>&1; \
	    if ! diff -q $(C_OUT)/exilc_ho.run $(C_OUT)/exilc_he.run >/dev/null; then \
	      echo "selfhost-exilc-driver: DRIFT $$name [--target host: run output]"; fail=1; \
	    fi; \
	  fi; \
	done; \
	for probe in "--emit-tokens" "--emit-ast" "--emit-typed-ir" "--target c --c-out $(C_OUT)/mi.c"; do \
	  rm -f $(C_OUT)/mi_o.err $(C_OUT)/mi_p.err $(C_OUT)/mi.c; \
	  $(EXILE) $$probe $(C_OUT)/no_such_input.exl >/dev/null 2>$(C_OUT)/mi_o.err; mo=$$?; \
	  $(EXILC_BIN) $$probe $(C_OUT)/no_such_input.exl >/dev/null 2>$(C_OUT)/mi_p.err; mp=$$?; \
	  if [ $$mo -eq 0 ]; then \
	    echo "selfhost-exilc-driver: reference ACCEPTED a missing input [$$probe] (floor)"; fail=1; \
	  elif [ ! -s $(C_OUT)/mi_o.err ]; then \
	    echo "selfhost-exilc-driver: EMPTY reference diagnostic for a missing input [$$probe] (floor)"; fail=1; \
	  elif [ $$mo -ne $$mp ]; then \
	    echo "selfhost-exilc-driver: MISSING-INPUT STATUS [$$probe] (oracle=$$mo exilc=$$mp)"; fail=1; \
	  elif grep -q "internal compiler error" $(C_OUT)/mi_o.err; then \
	    grep -q "cannot read file" $(C_OUT)/mi_p.err \
	      || { echo "selfhost-exilc-driver: MISSING-INPUT [$$probe] — reference ICEs here (register #2); the port must still give a clean diagnostic"; fail=1; }; \
	  elif ! diff -q <(head -1 $(C_OUT)/mi_o.err) <(head -1 $(C_OUT)/mi_p.err) >/dev/null; then \
	    echo "selfhost-exilc-driver: MISSING-INPUT TEXT [$$probe]"; \
	    echo "  oracle: `head -1 $(C_OUT)/mi_o.err`"; \
	    echo "  port:   `head -1 $(C_OUT)/mi_p.err`"; fail=1; \
	  fi; \
	done; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-exilc-driver: clean (exilc == oracle on tokens/ast/typed-ir/after-drop/c; host build+run parity; missing-input text+status)"; \
	else exit 1; fi

# ===== Move pass — text AND exit status =====
#
# The move contract is nine diagnostics that must match the reference byte for
# byte, and a rejected program must also EXIT non-zero: tc-errors compares only
# the message, which is how a compiler that printed an error and exited 0 stayed
# green for months.  This gate compares both from birth.
#
# tests/move/ pins one fixture per diagnostic plus the precedence shapes that
# fixtures written one-defect-at-a-time cannot reach (text + status).  examples/ is the
# FOREIGN set — the pass was derived from tests/move, so a false positive shows
# up there; those are compared on STATUS only, because the reference also emits
# Lint warnings the port does not have yet (Lint is the last unported pass), so
# its first stderr line is not always the error.
selfhost-port-move: $(EXILC_BIN)
	@fail=0; n=0; 	rm -f $(C_OUT)/mv_o.c $(C_OUT)/mv_p.c $(C_OUT)/mv_o.err $(C_OUT)/mv_p.err; 	ls tests/move/*.exl >/dev/null 2>&1 || { echo "selfhost-port-move: MISSING tests/move fixtures"; exit 1; }; 	for f in tests/move/*.exl; do 	  n=$$((n+1)); 	  rm -f $(C_OUT)/mv_o.c $(C_OUT)/mv_p.c $(C_OUT)/mv_o.err $(C_OUT)/mv_p.err; 	  $(EXILE) --target c --c-out $(C_OUT)/mv_o.c $$f >/dev/null 2>$(C_OUT)/mv_o.err; ro=$$?; 	  $(EXILC_BIN) --target c --c-out $(C_OUT)/mv_p.c $$f >/dev/null 2>$(C_OUT)/mv_p.err; rp=$$?; 	  if [ $$ro -eq 0 ]; then 	    echo "selfhost-port-move: reference ACCEPTED $$f — a move fixture must be rejected"; fail=1; 	  elif [ ! -s $(C_OUT)/mv_o.err ]; then 	    echo "selfhost-port-move: EMPTY reference diagnostic for $$f (mutual-failure floor)"; fail=1; 	  elif [ $$ro -ne $$rp ]; then 	    echo "selfhost-port-move: STATUS $$f (oracle=$$ro exilc=$$rp)"; fail=1; 	  elif ! diff -q <(head -1 $(C_OUT)/mv_o.err) <(head -1 $(C_OUT)/mv_p.err) >/dev/null; then 	    echo "selfhost-port-move: TEXT $$f"; 	    echo "  oracle: `head -1 $(C_OUT)/mv_o.err`"; 	    echo "  port:   `head -1 $(C_OUT)/mv_p.err`"; fail=1; 	  fi; 	done; 	for f in $(patsubst %,examples/%.exl,$(EXAMPLE_NAMES)); do 	  [ -f $$f ] || { echo "selfhost-port-move: MISSING sample $$f"; fail=1; continue; }; 	  n=$$((n+1)); 	  $(EXILC_BIN) --target c --c-out $(C_OUT)/mv_p.c $$f >/dev/null 2>&1 	    || { echo "selfhost-port-move: FALSE POSITIVE — exilc rejects valid $$f"; fail=1; }; 	done; 	if [ $$fail -eq 0 ]; then echo "selfhost-port-move: clean ($$n checked; move fixtures text+status, examples status-only)"; 	else exit 1; fi

# ===== Standalone module roots =====
#
# The CLI makes ANY file a compilation root.  Prelude/instance registration used
# to be lazy (populated at first method-use), so a module file compiled alone
# could drain a prelude body before its tables existed — four documented
# manifestations, all invisible to the corpus gates because those only exercise
# corpus roots.  Every library module must typecheck as its own root.
MODULE_ROOTS := pos token error lexer ast parser ir typecheck move drop escape codegen loader
selfhost-port-module-roots: $(SEEDC_TC)
	@fail=0; \
	for m in $(MODULE_ROOTS); do \
	  out=$$(echo src/$$m.exl | $(SEEDC_TC) 2>&1 >/dev/null | head -1); \
	  if [ -n "$$out" ]; then \
	    echo "selfhost-port-module-roots: FAIL $$m.exl: $$out"; fail=1; \
	  fi; \
	done; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-port-module-roots: clean (13 module files typecheck as standalone roots)"; \
	else exit 1; fi

selfhost-verify: bootstrap-fixpoint selfhost-port-tokens selfhost-port-errors \
	selfhost-port-module-roots selfhost-exilc-driver selfhost-exilc-fixpoint \
                 selfhost-port-ast selfhost-port-parse-errors selfhost-port-ir \
                 selfhost-port-drop-ir selfhost-port-escape selfhost-port-move selfhost-port-tc-errors \
                 selfhost-port-lint selfhost-mono-modules selfhost-xprod \
                 selfhost-no-fabrication selfhost-rune selfhost-ward
	@echo "selfhost-verify: all port gates green"

# The subset a fresh clone can run with nothing but `cc` — no dune, no opam.
# Same five recipes `selfhost-verify` runs; this target only names them as a
# group so the OCaml-free path is one command rather than a claim in a comment.
selfhost-seed-gates: bootstrap-fixpoint selfhost-port-tokens selfhost-port-ast \
                     selfhost-port-module-roots selfhost-no-fabrication
	@echo "selfhost-seed-gates: five gates green, built from the seed — no OCaml on the path"

# The transition floor, kept re-provable.  Moving these five off the oracle is
# sound only while the seed-built compiler emits, for every gate root, EXACTLY
# the C the oracle emits — otherwise the five would be judging a different
# program from the twelve.  Needs opam by construction, so it stays OUT of
# `selfhost-verify`; its moment is a seed refresh (`make seed`).
selfhost-seed-parity: $(SEEDC_EXILC) build
	@fail=0; n=0; \
	for root in exilc lex_corpus parse_corpus tc_corpus codegen_corpus; do \
	  n=$$((n+1)); \
	  rm -f $(C_OUT)/par_o.c $(C_OUT)/par_s.c; \
	  $(EXILE) --target c --c-out $(C_OUT)/par_o.c src/$$root.exl >/dev/null 2>&1 \
	    || { echo "selfhost-seed-parity: the oracle cannot build src/$$root.exl"; fail=1; continue; }; \
	  $(SEEDC_EXILC) --target c --c-out $(C_OUT)/par_s.c src/$$root.exl >/dev/null 2>&1 \
	    || { echo "selfhost-seed-parity: the seed-built compiler cannot build src/$$root.exl"; fail=1; continue; }; \
	  if [ ! -s $(C_OUT)/par_o.c ] || [ ! -s $(C_OUT)/par_s.c ]; then \
	    echo "selfhost-seed-parity: EMPTY C for src/$$root.exl (mutual-failure floor)"; fail=1; continue; \
	  fi; \
	  if ! cmp -s $(C_OUT)/par_o.c $(C_OUT)/par_s.c; then \
	    echo "selfhost-seed-parity: DIVERGENCE on src/$$root.exl"; \
	    diff $(C_OUT)/par_o.c $(C_OUT)/par_s.c | head -10; fail=1; \
	  fi; \
	done; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-seed-parity: clean ($$n roots — oracle and seed-built emit identical C)"; \
	else exit 1; fi

# ===== exilc whole-C fixpoint — the compiler as its own fixture =====
#
# The bootstrap fixpoint compares port-built-by-port against ITSELF, so it is
# blind to a port-vs-ORACLE divergence by construction; the corpus C comparison
# is byte-exact but too small to exercise the ordering and position paths a
# compiler-sized program hits.  This gate closes that axis: the port's own
# codegen must reproduce the ORACLE's C for the compiler's own source, byte for
# byte.  Nothing else measures it — and it found four classes every other gate
# missed (__lift order, aggregate typedef topo order, __drop_ret position,
# excess Slice instances).  It stays so the registration interleave cannot
# silently drift back.
# ===== Seed bootstrap — the OCaml-free entry =====
#
# `seed/exilc.c` is the compiler's own C, committed so a fresh clone can build
# exilc with nothing but `cc`.  It is a deliberate SNAPSHOT, refreshed only when
# `bootstrap-from-seed` says it can no longer build the current source — NOT on
# every commit.  Regenerating 66k lines per source change would drown the history
# to buy a property nobody needs: the seed's job is to let you in without OCaml,
# not to mirror HEAD.  It is generated by the PORT, not the oracle
# (selfhost-exilc-fixpoint proves the two emit identical C), so OCaml stays out
# of the refresh loop too.  `SEED_C` and `CC_QUIET` are defined near the top of
# this file, where the gates that consume them can see them.

seed: $(EXILC_BIN)
	@$(EXILC_BIN) --target c --c-out $(SEED_C) src/exilc.exl >/dev/null
	@echo "seed: regenerated $(SEED_C) (`wc -l < $(SEED_C)` lines) — commit it deliberately"

# The bootstrap ladder, using cc and the seed ONLY (no dune, no opam):
#   S  = compiler built from the seed        (seed's codegen — may be older)
#   Cb = S's C for the CURRENT source        (current codegen, emitted by old)
#   B  = compiler built from Cb
#   Cc = B's C for the CURRENT source        (current codegen, emitted by current)
# Cb == Cc is the fixpoint.  Cb differing from the seed's own text is EXPECTED —
# the seed is allowed to lag.  The gate goes red exactly when the seed can no
# longer compile the current source, which is precisely when it needs refreshing.
bootstrap-from-seed:
	@if [ ! -s $(SEED_C) ]; then echo "bootstrap-from-seed: MISSING or EMPTY $(SEED_C)"; exit 1; fi; \
	mkdir -p $(C_OUT) $(HOST_OUT); \
	rm -f $(C_OUT)/seed_b.c $(C_OUT)/seed_c.c $(C_OUT)/seed_ex.c $(C_OUT)/seed_enums.out \
	      $(HOST_OUT)/exilc_seed $(HOST_OUT)/exilc_seed_b $(HOST_OUT)/seed_enums; \
	cc -ansi -pedantic -Wall $(CC_QUIET) -o $(HOST_OUT)/exilc_seed $(SEED_C) $(SYS_HOST) 2>/dev/null \
	  || { echo "bootstrap-from-seed: cc could not build the seed"; exit 1; }; \
	$(HOST_OUT)/exilc_seed --target c --c-out $(C_OUT)/seed_b.c src/exilc.exl >/dev/null 2>&1 \
	  || { echo "bootstrap-from-seed: the seed compiler cannot build the current source — refresh it (make seed)"; exit 1; }; \
	if [ ! -s $(C_OUT)/seed_b.c ]; then echo "bootstrap-from-seed: EMPTY stage-B C (mutual-failure floor)"; exit 1; fi; \
	cc -ansi -pedantic -Wall $(CC_QUIET) -o $(HOST_OUT)/exilc_seed_b $(C_OUT)/seed_b.c $(SYS_HOST) 2>/dev/null \
	  || { echo "bootstrap-from-seed: cc could not build stage B"; exit 1; }; \
	$(HOST_OUT)/exilc_seed_b --target c --c-out $(C_OUT)/seed_c.c src/exilc.exl >/dev/null 2>&1 \
	  || { echo "bootstrap-from-seed: stage B cannot build the current source"; exit 1; }; \
	if [ ! -s $(C_OUT)/seed_c.c ]; then echo "bootstrap-from-seed: EMPTY stage-C C (mutual-failure floor)"; exit 1; fi; \
	if ! cmp -s $(C_OUT)/seed_b.c $(C_OUT)/seed_c.c; then \
	  echo "bootstrap-from-seed: DRIFT — stage C != stage B, the chain reaches no fixpoint"; \
	  diff $(C_OUT)/seed_b.c $(C_OUT)/seed_c.c | head -20; exit 1; fi; \
	$(HOST_OUT)/exilc_seed_b --target host --c-out $(C_OUT)/seed_ex.c --link $(SYS_HOST) \
	  -o $(HOST_OUT)/seed_enums examples/enums.exl >/dev/null 2>&1 \
	  || { echo "bootstrap-from-seed: the seed-built compiler cannot build an example"; exit 1; }; \
	$(HOST_OUT)/seed_enums > $(C_OUT)/seed_enums.out 2>&1; \
	if ! diff -q examples/enums.expected $(C_OUT)/seed_enums.out >/dev/null; then \
	  echo "bootstrap-from-seed: the seed-built compiler emits a WRONG binary"; \
	  diff examples/enums.expected $(C_OUT)/seed_enums.out | head; exit 1; fi; \
	echo "bootstrap-from-seed: clean (seed -> exilc -> itself; fixpoint at `wc -l < $(C_OUT)/seed_c.c` lines; no OCaml involved)"

selfhost-exilc-fixpoint: $(EXILC_BIN)
	@$(EXILE) --target c --c-out $(C_OUT)/xfx_oracle.c src/exilc.exl >/dev/null 2>&1; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/xfx_port.c src/exilc.exl >/dev/null 2>&1; \
	if [ ! -s $(C_OUT)/xfx_oracle.c ]; then \
	  echo "selfhost-exilc-fixpoint: EMPTY reference (mutual-failure floor)"; exit 1; fi; \
	if [ ! -s $(C_OUT)/xfx_port.c ]; then \
	  echo "selfhost-exilc-fixpoint: EMPTY port output (mutual-failure floor)"; exit 1; fi; \
	if cmp -s $(C_OUT)/xfx_oracle.c $(C_OUT)/xfx_port.c; then \
	  echo "selfhost-exilc-fixpoint: clean (port codegen == oracle on src/exilc.exl, `wc -l < $(C_OUT)/xfx_port.c` lines)"; \
	else \
	  echo "selfhost-exilc-fixpoint: DRIFT — port codegen != oracle on src/exilc.exl"; \
	  diff $(C_OUT)/xfx_oracle.c $(C_OUT)/xfx_port.c | head -20; exit 1; fi

# ===== rune — the first kernel-era feature's golden-C witness (RUNE-SPEC §7) =====
#
# rune is judged by RUNE-SPEC.md, not the frozen oracle (kernel-era features have
# no reference).  Increment 1 ships the SPINE — a standalone write-rune — and its
# witness is golden C: the port compiles the fixture and this gate asserts both
# directions of I-R1.  PRESENCE — the `volatile <T> *` binding and its UL-suffixed
# base address are emitted; MULTIPLICITY — the count of volatile stores EQUALS the
# count of source `.write`s (a count, not a grep: elision drops it below, a
# duplicated store lifts it above — two distinct betrayals).  The emitted C must
# also be valid C89 with zero warnings.  It is NOT run: the fixture points at a
# real custom-chip register, so a store moves no copper here — the runnable
# rune-over-RAM witness is Increment 2.  Port-only, so it lives outside the
# oracle-comparing gates; needs only cc, so it rides in `selfhost-verify`.
RUNE_FIXTURE := tests/rune/write_spine.exl
selfhost-rune: $(EXILC_BIN)
	@rm -f $(C_OUT)/rune_spine.c $(C_OUT)/rune_spine.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_spine.c $(RUNE_FIXTURE) >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the rune fixture"; exit 1; }; \
	if [ ! -s $(C_OUT)/rune_spine.c ]; then \
	  echo "selfhost-rune: EMPTY emitted C (mutual-failure floor)"; exit 1; fi; \
	grep -q 'volatile unsigned long \*cop1lc;' $(C_OUT)/rune_spine.c \
	  || { echo "selfhost-rune: MISSING volatile rune binding (I-R1 presence)"; exit 1; }; \
	grep -q '(volatile unsigned long \*)14676096UL;' $(C_OUT)/rune_spine.c \
	  || { echo "selfhost-rune: MISSING UL-suffixed base address (I-R1 address / no-cast-warning)"; exit 1; }; \
	writes=`sed 's|//.*||' $(RUNE_FIXTURE) | grep -c '\.write('`; \
	stores=`grep -c '\*cop1lc = ' $(C_OUT)/rune_spine.c`; \
	if [ "$$stores" -eq 0 ]; then \
	  echo "selfhost-rune: zero volatile stores (floor)"; exit 1; fi; \
	if [ "$$writes" != "$$stores" ]; then \
	  echo "selfhost-rune: I-R1 multiplicity — $$writes source writes but $$stores volatile stores (elision or duplication)"; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_spine.c -o $(C_OUT)/rune_spine.o \
	  || { echo "selfhost-rune: emitted C is not clean C89 (-ansi -pedantic -Wall -Werror)"; exit 1; }; \
	rm -f $(C_OUT)/rune_read.c $(C_OUT)/rune_read.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_read.c tests/rune/read_load.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the read fixture"; exit 1; }; \
	grep -q '\*out = \*status;' $(C_OUT)/rune_read.c \
	  || { echo "selfhost-rune: MISSING volatile load feeding store (I-R1 read: *out = *status)"; exit 1; }; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_read.c -o $(C_OUT)/rune_read.o \
	  || { echo "selfhost-rune: read_load C is not clean C89"; exit 1; }; \
	rm -f $(C_OUT)/r1.c $(C_OUT)/r1.err $(C_OUT)/r2.c $(C_OUT)/r2.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/r1.c tests/rune/reject_write_on_read.exl >/dev/null 2>$(C_OUT)/r1.err; then \
	  echo "selfhost-rune: R1 — port ACCEPTED a write on a read-only rune"; exit 1; fi; \
	if [ ! -s $(C_OUT)/r1.err ]; then echo "selfhost-rune: R1 empty diagnostic (floor)"; exit 1; fi; \
	grep -q 'cannot write a read-only rune' $(C_OUT)/r1.err \
	  || { echo "selfhost-rune: R1 wrong message: `head -1 $(C_OUT)/r1.err`"; exit 1; }; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/r2.c tests/rune/reject_read_on_write.exl >/dev/null 2>$(C_OUT)/r2.err; then \
	  echo "selfhost-rune: R2 — port ACCEPTED a read on a write-only rune"; exit 1; fi; \
	if [ ! -s $(C_OUT)/r2.err ]; then echo "selfhost-rune: R2 empty diagnostic (floor)"; exit 1; fi; \
	grep -q 'cannot read a write-only rune' $(C_OUT)/r2.err \
	  || { echo "selfhost-rune: R2 wrong message: `head -1 $(C_OUT)/r2.err`"; exit 1; }; \
	rm -f $(C_OUT)/rune_strobe.c $(C_OUT)/rune_strobe.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_strobe.c tests/rune/strobe_spine.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the strobe fixture"; exit 1; }; \
	strobes=`sed 's|//.*||' tests/rune/strobe_spine.exl | grep -c '\.strobe('`; \
	zstores=`grep -c '\*copjmp1 = 0;' $(C_OUT)/rune_strobe.c`; \
	if [ "$$zstores" -eq 0 ]; then echo "selfhost-rune: strobe emitted zero stores (floor)"; exit 1; fi; \
	if [ "$$strobes" != "$$zstores" ]; then \
	  echo "selfhost-rune: I-R1 strobe multiplicity — $$strobes strobes but $$zstores '= 0;' stores (a duplicated strobe is two copper starts)"; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_strobe.c -o $(C_OUT)/rune_strobe.o \
	  || { echo "selfhost-rune: strobe C is not clean C89"; exit 1; }; \
	rm -f $(C_OUT)/rune_rf.c $(C_OUT)/rune_rf.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_rf.c tests/rune/reg_file.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the register-file fixture"; exit 1; }; \
	grep -q 'volatile unsigned short \*color;' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING register-file volatile binding"; exit 1; }; \
	grep -q 'color\[0\] = 4095;' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING static indexed store color[0]=4095 (RUNE-SPEC §6)"; exit 1; }; \
	grep -q 'color\[i\] = 0;' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING runtime indexed store color[i] (I-R4 unchecked index)"; exit 1; }; \
	grep -q '= color\[0\];' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING register-file read load color[0]"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_rf.c -o $(C_OUT)/rune_rf.o \
	  || { echo "selfhost-rune: register-file C is not clean C89 at -O2"; exit 1; }; \
	rm -f $(C_OUT)/rune_r4.c $(C_OUT)/rune_r4.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/rune_r4.c tests/rune/reject_index_over_extent.exl >/dev/null 2>$(C_OUT)/rune_r4.err; then \
	  echo "selfhost-rune: R4 — port ACCEPTED a static index past the extent"; exit 1; fi; \
	if [ ! -s $(C_OUT)/rune_r4.err ]; then echo "selfhost-rune: R4 empty diagnostic (floor)"; exit 1; fi; \
	grep -q 'index 40 out of rune extent 32' $(C_OUT)/rune_r4.err \
	  || { echo "selfhost-rune: R4 wrong message: `head -1 $(C_OUT)/rune_r4.err`"; exit 1; }; \
	rm -f $(C_OUT)/rjA.c $(C_OUT)/rjA.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/rjA.c tests/rune/reject_amp_rune.exl >/dev/null 2>$(C_OUT)/rjA.err; then \
	  echo "selfhost-rune: R5 — port ACCEPTED &rune"; exit 1; fi; \
	grep -q 'a rune is not an ordinary pointer' $(C_OUT)/rjA.err \
	  || { echo "selfhost-rune: R5 wrong message: `head -1 $(C_OUT)/rjA.err`"; exit 1; }; \
	rm -f $(C_OUT)/rjB.c $(C_OUT)/rjB.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/rjB.c tests/rune/reject_free_rune.exl >/dev/null 2>$(C_OUT)/rjB.err; then \
	  echo "selfhost-rune: R6 — port ACCEPTED free(rune)"; exit 1; fi; \
	grep -q 'got rune' $(C_OUT)/rjB.err \
	  || { echo "selfhost-rune: R6 wrong message: `head -1 $(C_OUT)/rjB.err`"; exit 1; }; \
	rm -f $(C_OUT)/rjC.c $(C_OUT)/rjC.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/rjC.c tests/rune/reject_write_overflow.exl >/dev/null 2>$(C_OUT)/rjC.err; then \
	  echo "selfhost-rune: R7 — port ACCEPTED a too-wide rune write"; exit 1; fi; \
	grep -q "does not fit the rune's width" $(C_OUT)/rjC.err \
	  || { echo "selfhost-rune: R7 wrong message: `head -1 $(C_OUT)/rjC.err`"; exit 1; }; \
	rm -f $(C_OUT)/rjD.c $(C_OUT)/rjD.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/rjD.c tests/rune/reject_toplevel_ampglobal.exl >/dev/null 2>$(C_OUT)/rjD.err; then \
	  echo "selfhost-rune: R3b — port ACCEPTED a top-level &GLOBAL rune"; exit 1; fi; \
	if grep -q 'internal:' $(C_OUT)/rjD.err; then echo "selfhost-rune: R3b is ICE-enforced, not a clean diagnostic (tl3)"; exit 1; fi; \
	grep -q 'top-level rune base must be an integer' $(C_OUT)/rjD.err \
	  || { echo "selfhost-rune: R3b wrong message: `head -1 $(C_OUT)/rjD.err`"; exit 1; }; \
	rm -f $(C_OUT)/rune_tl.c $(C_OUT)/rune_tl.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_tl.c tests/rune/top_level.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the top-level fixture (item kind not yet ported?)"; exit 1; }; \
	tlg=`grep -c '\*const .* = (volatile' $(C_OUT)/rune_tl.c`; \
	if [ "$$tlg" != "3" ]; then echo "selfhost-rune: expected 3 top-level 'volatile T *const' globals, got $$tlg"; exit 1; fi; \
	grep -q 'volatile unsigned long \*const cop1lc = (volatile unsigned long \*)14676096UL;' $(C_OUT)/rune_tl.c \
	  || { echo "selfhost-rune: MISSING top-level *const global (cop1lc)"; exit 1; }; \
	grep -q '\*cop1lc = list_addr;' $(C_OUT)/rune_tl.c \
	  || { echo "selfhost-rune: MISSING top-level rune use across functions (*cop1lc = list_addr)"; exit 1; }; \
	grep -q 'color\[i\] = 0;' $(C_OUT)/rune_tl.c \
	  || { echo "selfhost-rune: MISSING top-level register-file use (color[i])"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_tl.c -o $(C_OUT)/rune_tl.o \
	  || { echo "selfhost-rune: top-level rune C is not clean C89 at -O2"; exit 1; }; \
	rm -f $(C_OUT)/rune_rr.c $(HOST_OUT)/rune_rr $(C_OUT)/rune_rr.out $(C_OUT)/rune_rr.expected; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_rr.c tests/rune/ram_roundtrip.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the rune-over-RAM witness"; exit 1; }; \
	grep -q '(volatile unsigned long \*)(&SCRATCH))' $(C_OUT)/rune_rr.c \
	  || { echo "selfhost-rune: MISSING &global rune base ((volatile T*)(&SCRATCH))"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/rune_rr $(C_OUT)/rune_rr.c tests/rune/ram_roundtrip_stub.c $(SYS_HOST) \
	  || { echo "selfhost-rune: rune-over-RAM C is not clean C89 at -O2"; exit 1; }; \
	$(HOST_OUT)/rune_rr > $(C_OUT)/rune_rr.out 2>&1; \
	printf '11\n22\n0\n305419896\n' > $(C_OUT)/rune_rr.expected; \
	if ! diff -q $(C_OUT)/rune_rr.expected $(C_OUT)/rune_rr.out >/dev/null; then \
	  echo "selfhost-rune: rune-over-RAM round-trip WRONG (volatile lowering broken at -O2):"; cat $(C_OUT)/rune_rr.out; exit 1; fi; \
	echo "selfhost-rune: clean (golden $$writes==$$stores + read + strobe $$strobes==$$zstores + register-file color[i] + top-level $$tlg *const globals + rejection table R1-R7 (R1/R2/R3b/R4/R5/R6/R7) + rune-over-RAM round-trip+width at -O2; cc -Wall -Werror)"

# ===== ward capability — the port's golden gate (WARD-SPEC, Phase 2) =====
#
# The ward era's differential gate.  Ward composes rune: a field access folds to
# a rune at (base + offset), the base and offset kept SEPARATE (§6).  Inc1 (the
# type+instance+field spine) asserts the fold shape, I-W1 zero-storage (the
# instance leaks no C variable), and clean C89.  Grows with the feature — offset
# overlap (W1), the runnable ward-over-RAM witness, and the full W table ride
# later increments.
selfhost-ward: $(EXILC_BIN)
	@rm -f $(C_OUT)/ward_spine.c $(C_OUT)/ward_spine.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ward_spine.c tests/ward/ward_spine.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ward: port rejected the ward spine"; exit 1; }; \
	if [ ! -s $(C_OUT)/ward_spine.c ]; then echo "selfhost-ward: EMPTY emitted C (floor)"; exit 1; fi; \
	grep -q '(volatile unsigned long \*)(14675968UL + 128UL)) = 74565;' $(C_OUT)/ward_spine.c \
	  || { echo "selfhost-ward: MISSING ward field write as base+offset (§6, I-W3)"; exit 1; }; \
	grep -q '(volatile unsigned short \*)(14675968UL + 136UL)) = 0;' $(C_OUT)/ward_spine.c \
	  || { echo "selfhost-ward: MISSING ward field strobe as base+offset"; exit 1; }; \
	if grep -q 'custom' $(C_OUT)/ward_spine.c; then \
	  echo "selfhost-ward: instance 'custom' leaked into C (I-W1 zero-storage violated)"; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/ward_spine.c -o $(C_OUT)/ward_spine.o \
	  || { echo "selfhost-ward: emitted C is not clean C89 (-ansi -pedantic -Wall -Werror)"; exit 1; }; \
	echo "selfhost-ward: clean (type+instance+field spine: base+offset fold, I-W1 zero-storage, I-W3 field=rune; cc -Wall -Werror)"

# ===== DR-010 escape pass — the port's differential gate =====
#
# The escape pass emits no code: its entire observable behaviour is its
# diagnostics.  So the gate runs BOTH compilers over each probe and byte-compares
# the message (position included) — and over every example, where both must stay
# silent.  `escape_corpus` prints the first diagnostic in the oracle's format, or
# "escape: ok".
.PHONY: host-selfhost-escape selfhost-port-escape

host-selfhost-escape: src/escape_corpus.exl src/escape.exl src/typecheck.exl src/parser.exl src/loader.exl src/lexer.exl src/token.exl src/pos.exl src/ast.exl src/ir.exl src/error.exl $(SYS_HOST) build
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

# ===== feature cross-product =====
#
# The self-host proof and the corpus are both correlated with the port's own
# blind spots: exilc's source declares no generic type, no generic fn and no
# trait of its own (it consumes the prelude's heavily), and the examples
# demonstrate features one at a time.  Every bug of the module-generics family
# lived in a CROSSING neither of them touched.  This gate is the foreign
# fixture set: one program per pair of features, checked for build status, run
# output and byte-exact C.
#
# A fixture needing a C companion (an allocator bridge, say) gets
# `tests/xprod/<name>_stub.c`, linked automatically — the same convention the
# examples use.
#
# Named, not globbed — and the list is deliberately short of the cells that do
# NOT agree yet; those are named in the worklog with their repro, not silently
# absent.  `c13_bare_lambda_to_bound` used to live here as a known divergence
# kept for the fabrication scan (the port wrongly accepted it, so it emitted C);
# bound conformance now rejects it, so it moved to src/tc_errors as a parity
# fixture.  c23 is the marker-trait acceptance twin of that closure — an empty
# `impl Marker for P {}` satisfying a `<T: Marker>` bound.
XPROD_FIXTURES := c01_trait_in_mod c02_trait_top_impl_in_mod \
                  c04_trait_impl_for_generic c05_generic_fn_bound \
                  c07_generic_in_generic c10_generic_ty_in_mod_with_impl \
                  c11_trait_generic_both_in_mod \
                  c12_closure_capture_annotated \
                  c14_enum_generic_in_mod_match_outside \
                  c16_generic_fn_over_generic_type c17_tuple_param_inference \
                  c18_relative_path_in_middle_module \
                  c19_callee_tparam_shadows_caller \
                  c20_own_param_in_generic_struct c21_generic_owner_nested_in_owner \
                  c22_capture_untyped_let c23_marker_bound_satisfied

selfhost-xprod: $(EXILC_BIN)
	@fail=0; n=0; \
	for name in $(XPROD_FIXTURES); do \
	  f=tests/xprod/$$name.exl; \
	  if [ ! -f $$f ]; then \
	    echo "selfhost-xprod: MISSING fixture $$f"; fail=$$((fail+1)); continue; \
	  fi; \
	  n=$$((n+1)); \
	  rm -f $(C_OUT)/xp_o.c $(C_OUT)/xp_p.c $(C_OUT)/xp_o_host.c $(C_OUT)/xp_p_host.c \
	        $(HOST_OUT)/xp_o $(HOST_OUT)/xp_p \
	        $(C_OUT)/xp_o.run $(C_OUT)/xp_p.run; \
	  $(EXILE) --target c --c-out $(C_OUT)/xp_o.c $$f >/dev/null 2>&1; oe=$$?; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/xp_p.c $$f >/dev/null 2>&1; pe=$$?; \
	  if [ ! -s $(C_OUT)/xp_o.c ]; then \
	    echo "selfhost-xprod: EMPTY reference C for $$f (fixture floor)"; fail=$$((fail+1)); continue; \
	  fi; \
	  if [ "$$oe" != "$$pe" ] || [ "$$oe" != "0" ]; then \
	    echo "selfhost-xprod: STATUS $$f oracle=$$oe port=$$pe"; fail=$$((fail+1)); continue; \
	  fi; \
	  if ! cmp -s $(C_OUT)/xp_o.c $(C_OUT)/xp_p.c; then \
	    echo "selfhost-xprod: C DIVERGE $$f"; \
	    diff $(C_OUT)/xp_o.c $(C_OUT)/xp_p.c | head -8; fail=$$((fail+1)); continue; \
	  fi; \
	  stub=""; \
	  if [ -f tests/xprod/$${name}_stub.c ]; then stub="--link tests/xprod/$${name}_stub.c"; fi; \
	  $(EXILE) --target host --c-out $(C_OUT)/xp_o_host.c $$stub -o $(HOST_OUT)/xp_o $$f >/dev/null 2>&1; \
	  $(EXILC_BIN) --target host --c-out $(C_OUT)/xp_p_host.c $$stub -o $(HOST_OUT)/xp_p $$f >/dev/null 2>&1; \
	  if [ ! -x $(HOST_OUT)/xp_o ] || [ ! -x $(HOST_OUT)/xp_p ]; then \
	    echo "selfhost-xprod: BUILD $$f"; fail=$$((fail+1)); continue; \
	  fi; \
	  $(HOST_OUT)/xp_o > $(C_OUT)/xp_o.run 2>&1; \
	  $(HOST_OUT)/xp_p > $(C_OUT)/xp_p.run 2>&1; \
	  if [ ! -s $(C_OUT)/xp_o.run ]; then \
	    echo "selfhost-xprod: SILENT reference run for $$f (fixture must observe something)"; \
	    fail=$$((fail+1)); continue; \
	  fi; \
	  if ! cmp -s $(C_OUT)/xp_o.run $(C_OUT)/xp_p.run; then \
	    echo "selfhost-xprod: RUN DIVERGE $$f"; \
	    diff $(C_OUT)/xp_o.run $(C_OUT)/xp_p.run | head -6; fail=$$((fail+1)); \
	  fi; \
	done; \
	echo "selfhost-xprod: $$((n-fail))/$$n agree; $$fail diverge"; \
	[ $$fail -eq 0 ]

# ===== mono instances of module-scoped generics =====
#
# A generic declared inside a `mod` must instantiate under the DECLARATION's
# path, so `Box<i32>` written inside `mod inner` is `inner::Box_i32`.  Under an
# unqualified instance path the failure has a loud face (a type that will not
# unify with itself, a pattern that will not match its own value) and a SILENT
# one: two modules' instances collapse into one C type emitted without its data
# union, dropping the payload on a clean exit-0 compile.
#
# The silent face is why this gate byte-compares the emitted C and not just the
# exit status: the evidence is the struct layout, and both compilers succeed.
# Run-output parity is checked too, so a divergence that survives the layout is
# still caught.  Fixtures are named, not globbed — a deleted one is MISSING.
MONO_FIXTURES := enum_two_modules struct_two_modules enum_mixed_qualification \
                 enum_silent_layout struct_nested_module \
                 impl_in_module impl_two_modules impl_wrong_body \
                 genfn_in_module genfn_two_modules genfn_bare_in_module \
                 genfn_nested_module

selfhost-mono-modules: $(EXILC_BIN)
	@fail=0; n=0; \
	for name in $(MONO_FIXTURES); do \
	  f=tests/mono/$$name.exl; \
	  if [ ! -f $$f ]; then \
	    echo "selfhost-mono-modules: MISSING fixture $$f"; fail=$$((fail+1)); continue; \
	  fi; \
	  n=$$((n+1)); \
	  rm -f $(C_OUT)/mono_o.c $(C_OUT)/mono_p.c $(C_OUT)/mono_o_host.c $(C_OUT)/mono_p_host.c \
	        $(HOST_OUT)/mono_o $(HOST_OUT)/mono_p \
	        $(C_OUT)/mono_o.run $(C_OUT)/mono_p.run; \
	  $(EXILE) --target c --c-out $(C_OUT)/mono_o.c $$f >/dev/null 2>&1; oe=$$?; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/mono_p.c $$f >/dev/null 2>&1; pe=$$?; \
	  if [ ! -s $(C_OUT)/mono_o.c ]; then \
	    echo "selfhost-mono-modules: EMPTY reference C for $$f (fixture floor)"; fail=$$((fail+1)); continue; \
	  fi; \
	  if [ "$$oe" != "$$pe" ]; then \
	    echo "selfhost-mono-modules: STATUS $$f oracle=$$oe port=$$pe"; fail=$$((fail+1)); continue; \
	  fi; \
	  if ! cmp -s $(C_OUT)/mono_o.c $(C_OUT)/mono_p.c; then \
	    echo "selfhost-mono-modules: C DIVERGE $$f"; \
	    diff $(C_OUT)/mono_o.c $(C_OUT)/mono_p.c | head -8; fail=$$((fail+1)); continue; \
	  fi; \
	  $(EXILE) --target host --c-out $(C_OUT)/mono_o_host.c -o $(HOST_OUT)/mono_o $$f >/dev/null 2>&1; \
	  $(EXILC_BIN) --target host --c-out $(C_OUT)/mono_p_host.c -o $(HOST_OUT)/mono_p $$f >/dev/null 2>&1; \
	  if [ ! -x $(HOST_OUT)/mono_o ] || [ ! -x $(HOST_OUT)/mono_p ]; then \
	    echo "selfhost-mono-modules: BUILD $$f (oracle or port produced no binary)"; fail=$$((fail+1)); continue; \
	  fi; \
	  $(HOST_OUT)/mono_o > $(C_OUT)/mono_o.run 2>&1; \
	  $(HOST_OUT)/mono_p > $(C_OUT)/mono_p.run 2>&1; \
	  if [ ! -s $(C_OUT)/mono_o.run ]; then \
	    echo "selfhost-mono-modules: SILENT reference run for $$f (fixture must observe its payload)"; \
	    fail=$$((fail+1)); continue; \
	  fi; \
	  if ! cmp -s $(C_OUT)/mono_o.run $(C_OUT)/mono_p.run; then \
	    echo "selfhost-mono-modules: RUN DIVERGE $$f"; \
	    diff $(C_OUT)/mono_o.run $(C_OUT)/mono_p.run | head -6; fail=$$((fail+1)); \
	  fi; \
	done; \
	echo "selfhost-mono-modules: $$((n-fail))/$$n agree; $$fail diverge"; \
	[ $$fail -eq 0 ]

# ===== lint — the WHOLE stderr, not its first line =====
#
# Unlike move / escape, lint emits every warning it finds rather than the first,
# so the ORDER the six category slots are concatenated in is as much a part of
# the contract as the texts.  This gate therefore byte-diffs the entire warning
# stream (only the `wrote ...` build line is dropped) and asserts exit-status
# parity — warnings must never fail a build.
#
# Two populations: the corpus proves no false positives on 95 real programs, the
# fixtures cover the categories the corpus never exercises.  Fixtures are listed
# by name, not globbed, so a deleted one is a hard MISSING rather than a silent
# skip, and each is required to produce a NON-EMPTY reference stream — a fixture
# that quietly stopped warning would otherwise agree with a port that also says
# nothing.
LINT_FIXTURES := must_use_prelude must_use_attr must_use_order must_use_modules slot_order

selfhost-port-lint: $(EXILC_BIN)
	@fail=0; n=0; \
	for name in $(LINT_FIXTURES); do \
	  if [ ! -f tests/lint/$$name.exl ]; then \
	    echo "selfhost-port-lint: MISSING fixture tests/lint/$$name.exl"; fail=$$((fail+1)); \
	  fi; \
	done; \
	for f in $(patsubst %,examples/%.exl,$(EXAMPLE_NAMES)) $(patsubst %,tests/lint/%.exl,$(LINT_FIXTURES)); do \
	  [ -f $$f ] || continue; \
	  n=$$((n+1)); \
	  rm -f $(C_OUT)/lint_o.c $(C_OUT)/lint_p.c $(C_OUT)/lint_o.raw $(C_OUT)/lint_p.raw \
	        $(C_OUT)/lint_o.err $(C_OUT)/lint_p.err; \
	  $(EXILE) --target c --c-out $(C_OUT)/lint_o.c $$f > $(C_OUT)/lint_o.raw 2>&1; oe=$$?; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/lint_p.c $$f > $(C_OUT)/lint_p.raw 2>&1; pe=$$?; \
	  grep -v '^wrote ' $(C_OUT)/lint_o.raw > $(C_OUT)/lint_o.err || true; \
	  grep -v '^wrote ' $(C_OUT)/lint_p.raw > $(C_OUT)/lint_p.err || true; \
	  case $$f in tests/lint/*) \
	    if [ ! -s $(C_OUT)/lint_o.err ]; then \
	      echo "selfhost-port-lint: EMPTY reference $$f (fixture floor)"; fail=$$((fail+1)); continue; \
	    fi;; \
	  esac; \
	  if [ "$$oe" != "$$pe" ] || [ "$$oe" != "0" ]; then \
	    echo "selfhost-port-lint: STATUS $$f oracle=$$oe port=$$pe (warnings must not fail a build)"; \
	    fail=$$((fail+1)); continue; \
	  fi; \
	  if ! cmp -s $(C_OUT)/lint_o.err $(C_OUT)/lint_p.err; then \
	    echo "selfhost-port-lint: DIVERGE $$f"; \
	    diff $(C_OUT)/lint_o.err $(C_OUT)/lint_p.err | head -8; fail=$$((fail+1)); \
	  fi; \
	done; \
	echo "selfhost-port-lint: $$((n-fail))/$$n agree; $$fail diverge"; \
	[ $$fail -eq 0 ]

selfhost-port-errors: host-selfhost-lexer
	@fail=0; n=0; \
	for f in src/lex_errors/*.exl; do \
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
selfhost-port-ast: $(SEEDC_PARSER)
	@mask='s/\(float [^ ]+ /(float /g; s/\(string "(\\.|[^"\\])*"\)/(string)/g'; \
	clean=0; defer=""; notp=0; mf=0; fail=0; \
	for name in $(EXAMPLE_NAMES); do \
		[ -f $(SELFHOST_AST)/$$name.ast ] || continue; \
		if [ "$$name" = "reexport" ]; then mf=$$((mf+1)); continue; fi; \
		actual=$$(mktemp); errf=$$(mktemp); \
		echo "examples/$$name.exl" | $(SEEDC_PARSER) > $$actual 2>$$errf; \
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
	for f in src/parse_errors/*.exl; do \
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
