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
AMIGA_NM         ?= $(TOOLCHAIN_PREFIX)/bin/m68k-amigaos-nm

# Out-of-tree build artefacts.  `_build/` is already gitignored (dune
# owns it), so generated C and per-target binaries live under it too.
OUT      := _build/out
C_OUT    := $(OUT)/c
HOST_OUT := $(OUT)/host
AMIGA_OUT:= $(OUT)/amiga

# The port compiler, built by the oracle (rule near EXILC_SAMPLE below).  It is
# defined HERE, not beside its rule, because `:=` expands where it is written:
# a gate naming it as a PREREQUISITE above the assignment gets an empty string
# and silently loses the dependency, while the same `$(EXILC_BIN)` inside the
# recipe expands at run time and points at a binary nobody built.
EXILC_BIN := $(HOST_OUT)/exilc

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

# Per-target backends for the `sys::*` seam.  Linked into
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

# `selfhost` is the in-progress OCaml->Exile compiler port.  Its
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
# The port gained `--target amiga` (the driver seam — m68k-
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
	  \
	  rm -f $(C_OUT)/ward_am.c $(AMIGA_OUT)/ward_am; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/ward_am.c tests/ward/ward_roundtrip.exl >/dev/null 2>&1 \
	    || { echo "selfhost-amiga: PORT rejected the ward-over-RAM witness"; fail=1; }; \
	  test -s $(C_OUT)/ward_am.c || { echo "selfhost-amiga: EMPTY ward-over-RAM emission (floor)"; fail=1; }; \
	  $(AMIGA_GCC) -noixemul -O2 $(CC_QUIET) -I src -o $(AMIGA_OUT)/ward_am $(C_OUT)/ward_am.c tests/ward/ward_roundtrip_stub.c $(SYS_AMIGA) -lm \
	    || { echo "selfhost-amiga: ward-over-RAM cross-compile failed"; fail=1; }; \
	  test -x $(AMIGA_OUT)/ward_am || { echo "selfhost-amiga: ward-over-RAM produced no m68k binary (floor)"; fail=1; }; \
	  wout=$$(vamos $(AMIGA_OUT)/ward_am 2>/dev/null); wexp=$$(printf '43981\n4660\n255'); \
	  if [ "$$wout" != "$$wexp" ]; then \
	    echo "selfhost-amiga: ward-over-RAM VAMOS round-trip WRONG - the field offsets do not mean on m68k what they mean on the host:"; \
	    echo "  got: $$wout"; fail=1; fi; \
	fi; \
	rm -f $(C_OUT)/seal_am.c $(AMIGA_OUT)/seal_am $(AMIGA_OUT)/seal_am0 $(C_OUT)/seal_am.o \
	      $(C_OUT)/seal_none.c $(AMIGA_OUT)/seal_none0 $(C_OUT)/seal_none.o; \
	if [ $$fail -eq 0 ]; then \
	  test -s tests/seal/amiga_callpath.expected || { echo "selfhost-amiga: MISSING/EMPTY tests/seal/amiga_callpath.expected"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(EXILC_BIN) --target amiga --c-out $(C_OUT)/seal_am.c --link $(SYS_AMIGA) -o $(AMIGA_OUT)/seal_am0 tests/seal/amiga_callpath.exl >/dev/null 2>&1 \
	    || { echo "selfhost-amiga: PORT rejected the seal call-path witness"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(AMIGA_GCC) -noixemul -O2 $(CC_QUIET) -I src -o $(AMIGA_OUT)/seal_am $(C_OUT)/seal_am.c $(SYS_AMIGA) -lm \
	    || { echo "selfhost-amiga: seal call-path cross-compile failed (Disable/Enable seam)"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  sout=$$(vamos $(AMIGA_OUT)/seal_am 2>/dev/null); sexp=$$(cat tests/seal/amiga_callpath.expected); \
	  if [ "$$sout" != "$$sexp" ]; then \
	    echo "selfhost-amiga: seal call-path VAMOS run WRONG (exec Disable/Enable pairing):"; \
	    diff <(echo "$$sexp") <(echo "$$sout") | head -6; fail=1; fi; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(AMIGA_GCC) -noixemul -O2 $(CC_QUIET) -I src -c -o $(C_OUT)/seal_am.o $(C_OUT)/seal_am.c \
	    || { echo "selfhost-amiga: seal object compile failed"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(EXILC_BIN) --target amiga --c-out $(C_OUT)/seal_none.c --link $(SYS_AMIGA) -o $(AMIGA_OUT)/seal_none0 examples/amiga_hello.exl >/dev/null 2>&1 \
	    && $(AMIGA_GCC) -noixemul -O2 $(CC_QUIET) -I src -c -o $(C_OUT)/seal_none.o $(C_OUT)/seal_none.c \
	    || { echo "selfhost-amiga: pay-for-use CONTROL build failed (examples/amiga_hello.exl)"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  used=`$(AMIGA_NM) $(C_OUT)/seal_am.o 2>/dev/null | grep -c '_sys_seal_'`; \
	  none=`$(AMIGA_NM) $(C_OUT)/seal_none.o 2>/dev/null | grep -c '_sys_seal_'`; \
	  if [ "$$used" != "2" ]; then \
	    echo "selfhost-amiga: the seal program's OBJECT names $$used seam symbols, expected 2"; fail=1; \
	  elif [ "$$none" != "0" ]; then \
	    echo "selfhost-amiga: PAY-FOR-USE broken — a seal-free program's object names $$none seam symbols"; fail=1; fi; \
	fi; \
	rm -f $(C_OUT)/seal_cram.c $(AMIGA_OUT)/seal_cram; \
	if [ $$fail -eq 0 ]; then \
	  test -s tests/seal/consumer_ram.amiga.expected \
	    || { echo "selfhost-amiga: MISSING/EMPTY tests/seal/consumer_ram.amiga.expected"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/seal_cram.c tests/seal/consumer_ram.exl >/dev/null 2>&1 \
	    || { echo "selfhost-amiga: PORT rejected the sealed-sequence witness"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  $(AMIGA_GCC) -noixemul -O2 -fno-strict-aliasing $(CC_QUIET) -I src -o $(AMIGA_OUT)/seal_cram \
	    $(C_OUT)/seal_cram.c tests/seal/consumer_ram_stub.c $(SYS_AMIGA) -lm \
	    || { echo "selfhost-amiga: sealed-sequence cross-compile failed"; fail=1; }; \
	fi; \
	if [ $$fail -eq 0 ]; then \
	  cout=$$(vamos $(AMIGA_OUT)/seal_cram 2>/dev/null); cexp=$$(cat tests/seal/consumer_ram.amiga.expected); \
	  if [ "$$cout" != "$$cexp" ]; then \
	    echo "selfhost-amiga: sealed SEQUENCE ran wrong on m68k (order, offset, or DMACON restore):"; \
	    diff <(echo "$$cexp") <(echo "$$cout") | head -8; fail=1; fi; \
	fi; \
	tcd=$(C_OUT)/tcprefix; rm -rf $$tcd; mkdir -p $$tcd/bin; \
	printf '#!/bin/sh\ntouch %s/called\nexit 0\n' "$$tcd" > $$tcd/bin/m68k-amigaos-gcc; \
	chmod +x $$tcd/bin/m68k-amigaos-gcc; \
	EXILE_TOOLCHAIN=$$tcd $(EXILC_BIN) --target amiga --c-out $(C_OUT)/tcenv.c \
	   -o $(AMIGA_OUT)/tcenv examples/amiga_hello.exl >/dev/null 2>&1; \
	if [ ! -f $$tcd/called ]; then \
	  echo "selfhost-amiga: \$$EXILE_TOOLCHAIN was NOT honoured — the override named a prefix and the driver went elsewhere (register #3 regressed)"; fail=1; fi; \
	rm -f $$tcd/called; \
	$(EXILC_BIN) --target amiga --c-out $(C_OUT)/tcenv.c \
	   -o $(AMIGA_OUT)/tcenv examples/amiga_hello.exl >/dev/null 2>&1; \
	if [ -f $$tcd/called ]; then \
	  echo "selfhost-amiga: the stub was invoked with NO override set — the fallback is not the fallback"; fail=1; fi; \
	rm -rf $$tcd; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-amiga: clean ($$n examples port==oracle on C/m68k binary/stdout/stderr + vamos==expected; \$$EXILE_TOOLCHAIN honoured (a stub prefix RECORDS the call) and ignored when unset; rune-over-RAM runs 11/22/0/305419896 and ward-over-RAM runs 43981/4660/255 on m68k under vamos; seal call-path runs nested on m68k through exec Disable/Enable — the SEAM, not the masking: vamos has nothing to race, interleaving stays registered for FS-UAE; pay-for-use measured on the OBJECT, 2 vs 0; the sealed blitter sequence RUNS on m68k 3/2544/64/32832 over RAM — the chipset itself is above vamos, so this proves the SEQUENCE and the seam, not the registers)"; \
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

# ===== Self-host bring-up — differential-harness golden corpus =====
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
# Two renderings are deliberately deferred (no prelude primitive yet):
# the float literal VALUE (`(Float ?? w)`
# vs OCaml's `%h` hex form) and string-escape DECODING (raw vs decoded
# `(String …)` content).  Both are masked before the compare, so a
# divergence on anything else — token type, position, count, ordering,
# or the f32/f64 width tag — still fails.  The deferring examples are
# printed every run so the debt stays visible, never silently passed.
selfhost-port-tokens: $(SEEDC_LEXER)
	@mask='s/\(Float [^ ]+ /(Float /; s/\(String .*\) @/(String) @/'; \
	fail=0; clean=0; defer=""; \
	for name in $(EXAMPLE_NAMES); do \
		if [ ! -f $(SELFHOST_TOKENS)/$$name.tokens ]; then \
			echo "selfhost-port-tokens: MISSING $(SELFHOST_TOKENS)/$$name.tokens"; \
			echo "  This used to skip, and a skipped example still counted as a checked"; \
			echo "  one. Run 'make selfhost-corpus-$$name'."; \
			fail=1; continue; \
		fi; \
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

# ===== Typecheck diagnostics =====
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
.PHONY: host-selfhost-cg bootstrap-fixpoint selfhost-verify selfhost-seed-gates selfhost-seed-parity selfhost-rune selfhost-ward selfhost-sigil selfhost-defer selfhost-seal selfhost-atomic selfhost-warning-free selfhost-freestanding selfhost-bare selfhost-ndk selfhost-parens selfhost-armreturn

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
# The driver stitches the six corpus drivers into one argv-driven `exilc`.  Its
# emit modes are the differential-gate contract, so it must reproduce the
# ORACLE byte-for-byte on every mode.  This gate builds exilc (oracle host
# build) and diffs its output against `dune exec exilc` — the reference — over
# a representative slice of the corpus.  Byte-drift is a driver bug.
EXILC_SAMPLE := enums traits generics closures_a2 let_else exhaustiveness \
                combinator_map pattern_guards modules reexport derive floats
# Every port module, not just the driver's own file: `src/exilc.exl` merely
# `use`s the rest, so a change to codegen / typecheck / drop left this binary
# stale. That made the plant in `fuzz-witness` land in the source and never reach
# the artifact under test - the witness was passing on a compiler built before
# the defect it was meant to rediscover.
$(EXILC_BIN): src/exilc.exl $(SEEDC_SRCS) build
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

selfhost-verify: selfhost-prelude-probe \
	bootstrap-fixpoint selfhost-port-tokens selfhost-port-errors \
	selfhost-port-module-roots selfhost-exilc-driver selfhost-exilc-fixpoint \
                 selfhost-port-ast selfhost-port-parse-errors selfhost-port-ir \
                 selfhost-port-drop-ir selfhost-port-drop-errors selfhost-port-escape selfhost-port-move selfhost-port-tc-errors \
                 selfhost-port-lint selfhost-mono-modules selfhost-xprod \
                 selfhost-no-fabrication selfhost-rune selfhost-ward selfhost-sigil selfhost-defer \
                 selfhost-seal selfhost-atomic selfhost-warning-free selfhost-freestanding selfhost-bare selfhost-ndk selfhost-parens selfhost-armreturn selfhost-noentry-externs docs-selfsufficient docs-capability-golden selfhost-own-tree selfhost-prelude-struct-lists
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

# The compiler, for someone who just wants to write exile.  `seed/exilc.c` is
# the self-hosted compiler's own C output, so this needs no OCaml, no opam and
# no dune: cc builds the seed, the seed compiles the current `src/*.exl`, and cc
# builds THAT into `./exilc`.  Two stages, because two is what a user needs —
# the third stage and the byte-equality check are the PROOF, and they live in
# `bootstrap-from-seed`, which is the gate.  This target must therefore never be
# cited as evidence of anything; it only hands you a compiler.
.PHONY: compiler
compiler:
	@if [ ! -s $(SEED_C) ]; then echo "compiler: MISSING or EMPTY $(SEED_C)"; exit 1; fi; \
	mkdir -p $(C_OUT) $(HOST_OUT); \
	rm -f $(C_OUT)/user_a.c $(C_OUT)/user_smoke.c $(HOST_OUT)/exilc_user_a ./exilc; \
	cc -ansi -pedantic -Wall $(CC_QUIET) -o $(HOST_OUT)/exilc_user_a $(SEED_C) $(SYS_HOST) 2>/dev/null \
	  || { echo "compiler: cc could not build the seed"; exit 1; }; \
	$(HOST_OUT)/exilc_user_a --target c --c-out $(C_OUT)/user_a.c src/exilc.exl >/dev/null 2>&1 \
	  || { echo "compiler: the seed cannot build the current source — refresh it (make seed)"; exit 1; }; \
	if [ ! -s $(C_OUT)/user_a.c ]; then echo "compiler: EMPTY C from the seed compiler"; exit 1; fi; \
	cc -ansi -pedantic -Wall $(CC_QUIET) -o ./exilc $(C_OUT)/user_a.c $(SYS_HOST) 2>/dev/null \
	  || { echo "compiler: cc could not build the compiler"; exit 1; }; \
	./exilc --target c --c-out $(C_OUT)/user_smoke.c examples/hello_world.exl >/dev/null 2>&1 \
	  || { echo "compiler: the built binary cannot compile hello_world"; exit 1; }; \
	if [ ! -s $(C_OUT)/user_smoke.c ]; then echo "compiler: the built binary emitted EMPTY C"; exit 1; fi; \
	echo "compiler: ./exilc ready (`wc -l < $(C_OUT)/user_a.c` lines of C, built by cc; no OCaml involved)"

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

# ===== rune — the first kernel-era feature's golden-C witness =====
#
# rune is judged by its own gate, not the frozen oracle (kernel-era features have
# no reference).  The SPINE is a standalone write-rune, and its
# witness is golden C: the port compiles the fixture and this gate asserts both
# directions of it.  PRESENCE — the `volatile <T> *` binding and its UL-suffixed
# base address are emitted; MULTIPLICITY — the count of volatile stores EQUALS the
# count of source `.write`s (a count, not a grep: elision drops it below, a
# duplicated store lifts it above — two distinct betrayals).  The emitted C must
# also be valid C89 with zero warnings.  It is NOT run: the fixture points at a
# real custom-chip register, so a store moves no copper here — the runnable
# rune-over-RAM witness is ram_roundtrip.exl.  Port-only, so it lives outside the
# oracle-comparing gates; needs only cc, so it rides in `selfhost-verify`.
RUNE_FIXTURE := tests/rune/write_spine.exl
selfhost-rune: $(EXILC_BIN)
	@rm -f $(C_OUT)/rune_spine.c $(C_OUT)/rune_spine.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_spine.c $(RUNE_FIXTURE) >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the rune fixture"; exit 1; }; \
	if [ ! -s $(C_OUT)/rune_spine.c ]; then \
	  echo "selfhost-rune: EMPTY emitted C (mutual-failure floor)"; exit 1; fi; \
	grep -q 'volatile unsigned long \*cop1lc;' $(C_OUT)/rune_spine.c \
	  || { echo "selfhost-rune: MISSING volatile rune binding (presence)"; exit 1; }; \
	grep -q '(volatile unsigned long \*)14676096UL;' $(C_OUT)/rune_spine.c \
	  || { echo "selfhost-rune: MISSING UL-suffixed base address (no-cast-warning)"; exit 1; }; \
	writes=`sed 's|//.*||' $(RUNE_FIXTURE) | grep -c '\.write('`; \
	stores=`grep -c '\*cop1lc = ' $(C_OUT)/rune_spine.c`; \
	if [ "$$stores" -eq 0 ]; then \
	  echo "selfhost-rune: zero volatile stores (floor)"; exit 1; fi; \
	if [ "$$writes" != "$$stores" ]; then \
	  echo "selfhost-rune: multiplicity — $$writes source writes but $$stores volatile stores (elision or duplication)"; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_spine.c -o $(C_OUT)/rune_spine.o \
	  || { echo "selfhost-rune: emitted C is not clean C89 (-ansi -pedantic -Wall -Werror)"; exit 1; }; \
	rm -f $(C_OUT)/rune_read.c $(C_OUT)/rune_read.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_read.c tests/rune/read_load.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the read fixture"; exit 1; }; \
	grep -q '\*out = \*status;' $(C_OUT)/rune_read.c \
	  || { echo "selfhost-rune: MISSING volatile load feeding store (read: *out = *status)"; exit 1; }; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_read.c -o $(C_OUT)/rune_read.o \
	  || { echo "selfhost-rune: read_load C is not clean C89"; exit 1; }; \
	rm -f $(C_OUT)/r1.c $(C_OUT)/r1.err $(C_OUT)/r2.c $(C_OUT)/r2.err; \
	rm -f $(C_OUT)/r8.c $(C_OUT)/r8.err $(C_OUT)/r9.c $(C_OUT)/r9.err $(C_OUT)/r10.c $(C_OUT)/r10.err; \
	for rr in r8:tests/rune/reject_read_args.exl r9:tests/rune/reject_reg_read_args.exl \
	          r10:tests/rune/reject_ward_field_read_args.exl; do \
	  tag=$${rr%%:*}; fx=$${rr##*:}; \
	  test -f $$fx || { echo "selfhost-rune: MISSING $$fx"; exit 1; }; \
	  if $(EXILC_BIN) --target c --c-out $(C_OUT)/$$tag.c $$fx >/dev/null 2>$(C_OUT)/$$tag.err; then \
	    echo "selfhost-rune: $$tag - port ACCEPTED extra arguments to a rune .read (they are DROPPED, not ignored)"; exit 1; fi; \
	  if [ ! -s $(C_OUT)/$$tag.err ]; then echo "selfhost-rune: $$tag empty diagnostic (floor)"; exit 1; fi; \
	  grep -q 'a rune `.read` takes no arguments' $(C_OUT)/$$tag.err \
	    || { echo "selfhost-rune: $$tag wrong message: `head -1 $(C_OUT)/$$tag.err`"; exit 1; }; \
	done; \
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
	  echo "selfhost-rune: strobe multiplicity — $$strobes strobes but $$zstores '= 0;' stores (a duplicated strobe is two copper starts)"; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_strobe.c -o $(C_OUT)/rune_strobe.o \
	  || { echo "selfhost-rune: strobe C is not clean C89"; exit 1; }; \
	rm -f $(C_OUT)/rune_rf.c $(C_OUT)/rune_rf.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_rf.c tests/rune/reg_file.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: port rejected the register-file fixture"; exit 1; }; \
	grep -q 'volatile unsigned short \*color;' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING register-file volatile binding"; exit 1; }; \
	grep -q 'color\[0\] = 4095;' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING static indexed store color[0]=4095"; exit 1; }; \
	grep -q 'color\[i\] = 0;' $(C_OUT)/rune_rf.c \
	  || { echo "selfhost-rune: MISSING runtime indexed store color[i] (unchecked index)"; exit 1; }; \
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
	grep -q 'got write rune<u32>' $(C_OUT)/rjB.err \
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
	rm -f $(C_OUT)/rune_sig.c $(C_OUT)/rune_sig.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_sig.c tests/rune/sig_annotation.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: P1 — port rejected the signature annotation"; exit 1; }; \
	grep -q 'volatile unsigned short \*r' $(C_OUT)/rune_sig.c \
	  || { echo "selfhost-rune: P1 param did not lower to a volatile T* (the annotation must be the same type, not a new one)"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/rune_sig.c -o $(C_OUT)/rune_sig.o \
	  || { echo "selfhost-rune: P1 signature C is not clean C89 at -O2"; exit 1; }; \
	rm -f $(C_OUT)/rune_att.c; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/rune_att.c tests/rune/accept_attenuation.exl >/dev/null 2>&1 \
	  || { echo "selfhost-rune: ACCEPT-probe — port REJECTED attenuation (readwrite lent as read/write is FEWER rights)"; exit 1; }; \
	for row in \
	  "P1-dir|reject_sig_direction|expected write rune<u16>, got read rune<u16>" \
	  "P1-widen|reject_sig_widening|expected readwrite rune<u16>, got write rune<u16>" \
	  "P1-bare|reject_bare_rune_ty|a rune type needs its direction" ; do \
	  id=`echo "$$row" | cut -d'|' -f1`; fx=`echo "$$row" | cut -d'|' -f2`; msg=`echo "$$row" | cut -d'|' -f3`; \
	  rm -f $(C_OUT)/prow.c $(C_OUT)/prow.err; \
	  if $(EXILC_BIN) --target c --c-out $(C_OUT)/prow.c tests/rune/$$fx.exl >/dev/null 2>$(C_OUT)/prow.err; then \
	    echo "selfhost-rune: $$id — port ACCEPTED tests/rune/$$fx.exl"; exit 1; fi; \
	  if grep -q 'internal:' $(C_OUT)/prow.err; then echo "selfhost-rune: $$id is ICE-enforced"; exit 1; fi; \
	  grep -qF "$$msg" $(C_OUT)/prow.err \
	    || { echo "selfhost-rune: $$id wrong message: `head -1 $(C_OUT)/prow.err`"; exit 1; }; \
	done; \
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
	echo "selfhost-rune: clean (golden $$writes==$$stores + read + strobe $$strobes==$$zstores + register-file color[i] + top-level $$tlg *const globals + rejection table R1-R10 (all three read paths refuse arguments) + P1 signatures (dir across the call boundary, widening, bare) + ACCEPT attenuation + rune-over-RAM round-trip+width at -O2; cc -Wall -Werror)"

# ===== ward capability — the port's golden gate =====
#
# The ward era's differential gate.  Ward composes rune: a field access folds to
# a rune at (base + offset), the base and offset kept SEPARATE.  The
# type+instance+field spine asserts the fold shape, zero-storage (the
# instance leaks no C variable), and clean C89.  Grows with the feature — offset
# overlap (W1), the runnable ward-over-RAM witness, and the full W table ride
# later increments.
selfhost-ward: $(EXILC_BIN)
	@rm -f $(C_OUT)/ward_spine.c $(C_OUT)/ward_spine.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ward_spine.c tests/ward/ward_spine.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ward: port rejected the ward spine"; exit 1; }; \
	if [ ! -s $(C_OUT)/ward_spine.c ]; then echo "selfhost-ward: EMPTY emitted C (floor)"; exit 1; fi; \
	grep -q '(volatile unsigned long \*)(14675968UL + 128UL)) = 74565;' $(C_OUT)/ward_spine.c \
	  || { echo "selfhost-ward: MISSING ward field write as base+offset"; exit 1; }; \
	grep -q '(volatile unsigned short \*)(14675968UL + 136UL)) = 0;' $(C_OUT)/ward_spine.c \
	  || { echo "selfhost-ward: MISSING ward field strobe as base+offset"; exit 1; }; \
	if grep -q 'custom' $(C_OUT)/ward_spine.c; then \
	  echo "selfhost-ward: instance 'custom' leaked into C (zero-storage violated)"; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/ward_spine.c -o $(C_OUT)/ward_spine.o \
	  || { echo "selfhost-ward: emitted C is not clean C89 (-ansi -pedantic -Wall -Werror)"; exit 1; }; \
	rm -f $(C_OUT)/ward_ov.c $(C_OUT)/ward_ov.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/ward_ov.c tests/ward/reject_overlap.exl >/dev/null 2>$(C_OUT)/ward_ov.err; then \
	  echo "selfhost-ward: W1 — port ACCEPTED overlapping ward fields"; exit 1; fi; \
	if grep -q 'internal:' $(C_OUT)/ward_ov.err; then echo "selfhost-ward: W1 is ICE-enforced, not a clean diagnostic"; exit 1; fi; \
	grep -q "fields 'a' \[0, 4) and 'b' \[2, 4) overlap" $(C_OUT)/ward_ov.err \
	  || { echo "selfhost-ward: W1 wrong message (must name both fields + ranges): `head -1 $(C_OUT)/ward_ov.err`"; exit 1; }; \
	rm -f $(C_OUT)/ward_rf.c $(C_OUT)/ward_rf.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ward_rf.c tests/ward/reg_field.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ward: port rejected the register-file field fixture"; exit 1; }; \
	grep -q '((volatile unsigned short \*)(14675968UL + 384UL))\[31\] = 0;' $(C_OUT)/ward_rf.c \
	  || { echo "selfhost-ward: MISSING register-file field extreme index color[31] at 0x180"; exit 1; }; \
	grep -q '((volatile unsigned short \*)(14675968UL + 384UL))\[0\] = 4095;' $(C_OUT)/ward_rf.c \
	  || { echo "selfhost-ward: MISSING register-file field color[0]"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/ward_rf.c -o $(C_OUT)/ward_rf.o \
	  || { echo "selfhost-ward: register-file field C is not clean C89 at -O2"; exit 1; }; \
	rm -f $(C_OUT)/ward_fo.c $(C_OUT)/ward_fo.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/ward_fo.c tests/ward/reject_file_overlap.exl >/dev/null 2>$(C_OUT)/ward_fo.err; then \
	  echo "selfhost-ward: W1(file) — port ACCEPTED a file overlapping a scalar"; exit 1; fi; \
	grep -q "fields 'bank' \[0, 16) and 'x' \[8, 10) overlap" $(C_OUT)/ward_fo.err \
	  || { echo "selfhost-ward: W1(file) wrong message (file range N·size): `head -1 $(C_OUT)/ward_fo.err`"; exit 1; }; \
	rm -f $(C_OUT)/ward_tl.c $(C_OUT)/ward_tl.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ward_tl.c tests/ward/top_level.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ward: port rejected the top-level ward fixture"; exit 1; }; \
	tla=`grep -c '(14675968UL + 128UL))' $(C_OUT)/ward_tl.c`; \
	if [ "$$tla" != "2" ]; then echo "selfhost-ward: top-level field fold not identical per use-site — expected 2 (chipa.cop1lc from 2 fns), got $$tla"; exit 1; fi; \
	grep -q '(14548992UL + 128UL))' $(C_OUT)/ward_tl.c \
	  || { echo "selfhost-ward: NDK second-instance fold missing (chipb at a different base)"; exit 1; }; \
	if grep -qE 'chipa|chipb' $(C_OUT)/ward_tl.c; then echo "selfhost-ward: top-level ward instance leaked into C (zero-storage violated)"; exit 1; fi; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/ward_tl.c -o $(C_OUT)/ward_tl.o \
	  || { echo "selfhost-ward: top-level ward C is not clean C89 at -O2"; exit 1; }; \
	rm -f $(C_OUT)/ward_tg.c $(C_OUT)/ward_tg.err; \
	if $(EXILC_BIN) --target c --c-out $(C_OUT)/ward_tg.c tests/ward/reject_toplevel_ampglobal.exl >/dev/null 2>$(C_OUT)/ward_tg.err; then \
	  echo "selfhost-ward: top-level &GLOBAL — port ACCEPTED"; exit 1; fi; \
	if grep -q 'internal:' $(C_OUT)/ward_tg.err; then echo "selfhost-ward: top-level &GLOBAL is ICE-enforced, not clean (R3b mirror)"; exit 1; fi; \
	grep -q 'top-level ward base must be an integer' $(C_OUT)/ward_tg.err \
	  || { echo "selfhost-ward: top-level &GLOBAL wrong message: `head -1 $(C_OUT)/ward_tg.err`"; exit 1; }; \
	for row in \
	  "W2|reject_nonconst_base|must be an integer (MMIO) or \`&GLOBAL\` (RAM)" \
	  "W3|reject_unknown_field|no field 'zzz' on ward 'C'" \
	  "W5|reject_ward_as_value|a ward is an overlay, not a value" \
	  "W4-R1|reject_field_r1|cannot write a read-only rune" \
	  "W4-R7|reject_field_r7|does not fit the rune's width" \
	  "W4-R5|reject_field_r5|a rune is not an ordinary pointer" ; do \
	  id=`echo "$$row" | cut -d'|' -f1`; fx=`echo "$$row" | cut -d'|' -f2`; msg=`echo "$$row" | cut -d'|' -f3`; \
	  rm -f $(C_OUT)/wrow.c $(C_OUT)/wrow.err; \
	  if $(EXILC_BIN) --target c --c-out $(C_OUT)/wrow.c tests/ward/$$fx.exl >/dev/null 2>$(C_OUT)/wrow.err; then \
	    echo "selfhost-ward: $$id — port ACCEPTED tests/ward/$$fx.exl"; exit 1; fi; \
	  if [ ! -s $(C_OUT)/wrow.err ]; then echo "selfhost-ward: $$id empty diagnostic (floor)"; exit 1; fi; \
	  if grep -q 'internal:' $(C_OUT)/wrow.err; then echo "selfhost-ward: $$id is ICE-enforced, not a clean diagnostic"; exit 1; fi; \
	  grep -qF "$$msg" $(C_OUT)/wrow.err \
	    || { echo "selfhost-ward: $$id wrong message: `head -1 $(C_OUT)/wrow.err`"; exit 1; }; \
	done; \
	rm -f $(C_OUT)/ward_ai.c $(C_OUT)/ward_ai.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ward_ai.c tests/ward/accept_runtime_index.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ward: ACCEPT-probe — port REJECTED a runtime register-file index (deliberately unchecked; the limit is a contract)"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/ward_ai.c -o $(C_OUT)/ward_ai.o \
	  || { echo "selfhost-ward: ACCEPT-probe C is not clean at -O2"; exit 1; }; \
	rm -f $(C_OUT)/ward_rr.c $(HOST_OUT)/ward_rr $(C_OUT)/ward_rr.out $(C_OUT)/ward_rr.expected; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ward_rr.c tests/ward/ward_roundtrip.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ward: port rejected the ward-over-RAM witness"; exit 1; }; \
	tn=`grep -o 'extern struct ex_[a-z0-9_]* SCRATCH' $(C_OUT)/ward_rr.c | sed 's/extern struct //;s/ SCRATCH//'`; \
	if [ -z "$$tn" ]; then echo "selfhost-ward: the ward-over-RAM emission no longer declares SCRATCH as a struct - the stub cannot define what it does not name"; exit 1; fi; \
	grep -q "struct $$tn {" tests/ward/ward_roundtrip_stub.c \
	  || { echo "selfhost-ward: the emission declares SCRATCH as 'struct $$tn' and the stub defines a different type - they would link to different storage"; exit 1; }; \
	grep -q '(char \*)&SCRATCH + 4UL' $(C_OUT)/ward_rr.c \
	  || { echo "selfhost-ward: MISSING &global field address ((char*)&SCRATCH + offset)"; exit 1; }; \
	cc -O2 -fno-strict-aliasing -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/ward_rr $(C_OUT)/ward_rr.c tests/ward/ward_roundtrip_stub.c $(SYS_HOST) \
	  || { echo "selfhost-ward: ward-over-RAM C is not clean at -O2 (-fno-strict-aliasing: MMIO overlays untyped memory)"; exit 1; }; \
	$(HOST_OUT)/ward_rr > $(C_OUT)/ward_rr.out 2>&1; \
	printf '43981\n4660\n255\n' > $(C_OUT)/ward_rr.expected; \
	if ! diff -q $(C_OUT)/ward_rr.expected $(C_OUT)/ward_rr.out >/dev/null; then \
	  echo "selfhost-ward: ward-over-RAM WRONG (offsets/disjointness broken at -O2):"; cat $(C_OUT)/ward_rr.out; exit 1; fi; \
	echo "selfhost-ward: clean (spine + register-file field color[31]@0x180 + top-level instances (fold ×2 + NDK 2-base + zero-storage) + rejection table W1-W5 (W1 scalar+file, W2, W3, W4→R1/R5/R7 through a FIELD, W5) + ACCEPT runtime-index limit + ward-over-RAM 43981/4660/255 at -O2; cc -Wall -Werror)"

# ===== defer x loop jumps (P3) — the exit guarantee, on EVERY path =====
#
# `defer` used to be swallowed by `break` / `continue`: a registered hole
# that seal turns from a wart into a hazard, since a seal left by a
# `break` would leave interrupts masked forever.  Port-only: the FROZEN oracle
# still has the old behaviour, so this is a deliberate, registered divergence —
# see SELFHOST-PLUMBING-REGISTER.md #5.  The corpus was measured neutral before
# the fix (no function anywhere pairs defer with a loop jump), which is why no
# comparing gate moves.
selfhost-defer: $(EXILC_BIN)
	@rm -f $(C_OUT)/defer_exits.c $(HOST_OUT)/defer_exits $(C_OUT)/defer_exits.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/defer_exits.c tests/defer/exits.exl >/dev/null 2>&1 \
	  || { echo "selfhost-defer: port rejected the exits fixture"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/defer_exits $(C_OUT)/defer_exits.c $(SYS_HOST) \
	  || { echo "selfhost-defer: emitted C is not clean C89 at -O2"; exit 1; }; \
	$(HOST_OUT)/defer_exits > $(C_OUT)/defer_exits.out 2>&1; \
	if ! diff -q tests/defer/exits.expected $(C_OUT)/defer_exits.out >/dev/null; then \
	  echo "selfhost-defer: WRONG defer trace (an exit path lost or gained a defer):"; \
	  diff tests/defer/exits.expected $(C_OUT)/defer_exits.out | head -8; exit 1; fi; \
	echo "selfhost-defer: clean (defer fires on normal exit, continue, break, return, and through nesting — inner jumps leave outer defers alone)"

# ===== differential fuzzing =====
#
# A6: `make fuzz` is a SEPARATE seeded target, deterministic per seed, outside
# `selfhost-verify` — the verification suite must stay deterministic.
FUZZ_SEED  ?= 1
FUZZ_N     ?= 150
# The mix is STEERED by the measured death-per-stage split, so the
# hand-set rate is no longer the default. `FUZZ_STEER=0` reproduces the
# earlier stream shape and is what `FUZZ_RATE` is for.
FUZZ_STEER ?= 1
FUZZ_RATE  ?= 0
FUZZ_FLAGS := $(if $(filter 0,$(FUZZ_STEER)),--no-steer --graft-rate $(FUZZ_RATE),)
.PHONY: fuzz fuzz-witness fuzz-filters fuzz-seed-hunt fuzz-budget-witness fuzz-limits fuzz-gates
fuzz-filters: $(EXILC_BIN)
	@python3 tools/fuzz/fuzz.py --seed 0 --selftest --cc

# The fuzzer's stated limits, executable. A limit nobody measures
# drifts into folklore, and one measured here goes red the day a future round
# closes it, so closing it has to be a decision rather than a side effect.
fuzz-limits: $(EXILC_BIN)
	@python3 tools/fuzz/limits.py

# The era's four gates as one command. They are deliberately outside
# `selfhost-verify`, which must stay deterministic input-for-input; each of these
# is deterministic per seed, which is a weaker property.
fuzz-gates: fuzz-filters fuzz-limits fuzz-witness fuzz-budget-witness
	@echo "fuzz-gates: four green - filters, limits, planted-defect witness, budget witness"

fuzz: $(EXILC_BIN)
	@python3 tools/fuzz/fuzz.py --seed $(FUZZ_SEED) -n $(FUZZ_N) $(FUZZ_FLAGS) --cc --shrink

# The witness, and the reason it is shaped this way: a run that
# finds nothing is indistinguishable from a run that did nothing, so the gate
# does NOT assert "no findings". It asserts that the fuzzer REDISCOVERS a defect
# deliberately planted in the port. A fuzzer that has never rediscovered a defect
# is not a fuzzer; it is a green light.
#
# E1: the seed is part of the gate. It selects a STREAM, so when the generator
# changes the seed must be RE-HUNTED and re-pinned here. A failed hunt — no seed
# within budget rediscovers the plant — is itself a finding ABOUT THE GENERATOR,
# never grounds for widening the budget or weakening the plant.
FUZZ_WITNESS_SEED ?= 1
FUZZ_HUNT_SEEDS   ?= 12

# E1 made operational: the hunt is a TARGET, not a session activity, so the seed
# a future generator change re-pins is chosen by a recipe anyone can re-run and
# whose budget is printed. It reports every seed it tried, hit or miss — a hunt
# that showed only its winner would hide how thin the margin was.
fuzz-seed-hunt: $(EXILC_BIN)
	@test -f tools/fuzz/plant.py || { echo "fuzz-seed-hunt: MISSING tools/fuzz/plant.py"; exit 1; }; \
	python3 tools/fuzz/plant.py plant || exit 1; \
	$(MAKE) -s $(EXILC_BIN) >/dev/null 2>&1; \
	hits=""; \
	for s in `seq 1 $(FUZZ_HUNT_SEEDS)`; do \
	  if python3 tools/fuzz/fuzz.py --seed $$s -n $(FUZZ_N) $(FUZZ_FLAGS) 2>&1 | grep -q 'F1:emitted-c'; then \
	    echo "  seed $$s: rediscovers the plant"; hits="$$hits $$s"; \
	  else echo "  seed $$s: misses"; fi; \
	done; \
	python3 tools/fuzz/plant.py restore; \
	$(MAKE) -s $(EXILC_BIN) >/dev/null 2>&1; \
	if [ -z "$$hits" ]; then \
	  echo "fuzz-seed-hunt: NO seed in 1..$(FUZZ_HUNT_SEEDS) at n=$(FUZZ_N) rediscovers the plant."; \
	  echo "  Per E1 that is a finding ABOUT THE GENERATOR — diagnose it, do not widen the budget."; \
	  exit 1; fi; \
	echo "fuzz-seed-hunt: budget $(FUZZ_HUNT_SEEDS) seeds x $(FUZZ_N) inputs; hits:$$hits"; \
	echo "  pin FUZZ_WITNESS_SEED to the first of these."

# The budgets, witnessed. A class that has never fired is a green
# light, and F4 had never fired: its two early-return paths still handed back
# 4-tuples after a later round added a fifth field, so the first input to trip a
# budget would have taken the fuzzer down with it. That sat undetected precisely
# because nothing exercised the class.
#
# Both directions, with the numbers stated: a time budget below any real compile
# must produce F4; a memory cap below the compiler's own floor must produce F4;
# and at the SHIPPED budgets neither may fire.
FUZZ_TINY_BUDGET ?= 0.002
FUZZ_TINY_RSS    ?= 2
fuzz-budget-witness: $(EXILC_BIN)
	@t=`python3 tools/fuzz/fuzz.py --seed 3 -n 20 --budget $(FUZZ_TINY_BUDGET) --quiet 2>&1 | grep -oE 'budget=[0-9]+' | tail -1`; \
	if [ "$$t" = "budget=0" ] || [ -z "$$t" ]; then \
	  echo "fuzz-budget-witness: the TIME budget did not fire at $(FUZZ_TINY_BUDGET)s — F4's timeout path is not reachable"; exit 1; fi; \
	m=`python3 tools/fuzz/fuzz.py --seed 3 -n 20 --rss $(FUZZ_TINY_RSS) --quiet 2>&1 | grep -oE 'budget=[0-9]+' | tail -1`; \
	if [ "$$m" = "budget=0" ] || [ -z "$$m" ]; then \
	  echo "fuzz-budget-witness: the MEMORY cap did not fire at $(FUZZ_TINY_RSS)MB — F4's rss path is not reachable"; exit 1; fi; \
	q=`python3 tools/fuzz/fuzz.py --seed 3 -n 40 --quiet 2>&1 | grep -oE 'budget=[0-9]+' | tail -1`; \
	if [ "$$q" != "budget=0" ]; then \
	  echo "fuzz-budget-witness: a budget fired at the SHIPPED limits ($$q) — the caps are too tight to distinguish a finding from the machine"; exit 1; fi; \
	echo "fuzz-budget-witness: clean (time $(FUZZ_TINY_BUDGET)s -> $$t, memory $(FUZZ_TINY_RSS)MB -> $$m, shipped limits -> $$q)"

fuzz-witness: $(EXILC_BIN)
	@test -f tools/fuzz/plant.py || { echo "fuzz-witness: MISSING tools/fuzz/plant.py"; exit 1; }; \
	base=`python3 tools/fuzz/fuzz.py --seed $(FUZZ_WITNESS_SEED) -n $(FUZZ_N) $(FUZZ_FLAGS) 2>&1 | grep -c 'F1:emitted-c'`; \
	python3 tools/fuzz/plant.py plant || exit 1; \
	$(MAKE) -s $(EXILC_BIN) >/dev/null 2>&1; \
	printf 'fn main() { let mut i = 0; while i < 2 { println(i); i = i + 1; } }\n' > $(C_OUT)/canary.exl; \
	rm -f $(C_OUT)/canary.c; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/canary.c $(C_OUT)/canary.exl >/dev/null 2>&1; \
	if ! grep -q 'while ( ' $(C_OUT)/canary.c 2>/dev/null; then \
	  python3 tools/fuzz/plant.py restore; $(MAKE) -s $(EXILC_BIN) >/dev/null 2>&1; \
	  echo "fuzz-witness: THE PLANT DID NOT REACH THE ARTIFACT - the compiler under test does not carry the defect,"; \
	  echo "  so anything the run finds is a real finding and the witness would be green for the wrong reason."; \
	  echo "  (This floor exists because exactly that happened: a later round added a TWhile arm ABOVE gen_while,"; \
	  echo "   the replace-first plant moved into the defer-body emitter, and the string-existence guard was happy.)"; \
	  exit 1; fi; \
	out=`python3 tools/fuzz/fuzz.py --seed $(FUZZ_WITNESS_SEED) -n $(FUZZ_N) $(FUZZ_FLAGS) 2>&1`; \
	python3 tools/fuzz/plant.py restore; \
	$(MAKE) -s $(EXILC_BIN) >/dev/null 2>&1; \
	got=`echo "$$out" | grep -c 'F1:emitted-c'`; \
	if [ "$$got" -le "$$base" ]; then \
	  echo "fuzz-witness: seed $(FUZZ_WITNESS_SEED) produced $$got emitted-c findings WITH the plant and $$base without it."; \
	  echo "  The signature alone proves nothing: this stream already carries a REAL emitted-c divergence, so"; \
	  echo "  \`grep -q\` would pass with no plant at all. The witness asks for the DIFFERENCE the plant makes."; \
	  echo "  Per E1 a shortfall is a finding ABOUT THE GENERATOR - re-hunt the seed, do not widen the budget."; \
	  exit 1; fi; \
	echo "fuzz-witness: clean (seed $(FUZZ_WITNESS_SEED): $$base emitted-c findings without the plant, $$got with it - the difference is the plant, and the canary proved it reached the artifact)"

# ===== register #12 - `return` inside a `match` arm (B2 divergence) =====
#
# The port lowers it; the FROZEN reference cannot, at two different barriers, and
# the codegen one names `defer` in a program that has none (`emit_simple_stmt`
# was written for defer bodies and the TMatch lowering routes arms through it).
# The seal era is built on this shape, so the port keeps it - which makes the
# gate mandatory rather than optional, per the same argument register #7 made: a
# divergence nothing measures is a divergence that rots.
#
# BOTH sides are pinned. The port must compile, emit a real `return` inside the
# switch, survive -Werror and RUN to the expected trace; the reference must still
# REFUSE it. If the frozen oracle ever accepted this, the register entry would be
# stale and this gate is what would say so.
selfhost-armreturn: $(EXILC_BIN)
	@test -f tests/armreturn/arm_returns.exl || { echo "selfhost-armreturn: MISSING tests/armreturn/arm_returns.exl"; exit 1; }; \
	test -s tests/armreturn/arm_returns.expected || { echo "selfhost-armreturn: MISSING/EMPTY expected"; exit 1; }; \
	if $(EXILE) --target c --c-out $(C_OUT)/armret_oracle.c tests/armreturn/arm_returns.exl >/dev/null 2>&1; then \
	  echo "selfhost-armreturn: the FROZEN reference now ACCEPTS this - register #12 is stale, re-measure it"; exit 1; fi; \
	rm -f $(C_OUT)/armret_oracle.c; \
	rm -f $(C_OUT)/armret.c $(HOST_OUT)/armret $(C_OUT)/armret.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/armret.c tests/armreturn/arm_returns.exl >/dev/null 2>&1 \
	  || { echo "selfhost-armreturn: the port REJECTED `return` in a match arm - the seal era rests on this shape"; exit 1; }; \
	if [ ! -s $(C_OUT)/armret.c ]; then echo "selfhost-armreturn: EMPTY emitted C (floor)"; exit 1; fi; \
	n=`sed 's|//.*||' $(C_OUT)/armret.c | grep -c 'return 10;\|return 20;\|return 30;'`; \
	if [ "$$n" != "3" ]; then \
	  echo "selfhost-armreturn: expected 3 arm returns lowered into the switch, found $$n"; exit 1; fi; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/armret $(C_OUT)/armret.c $(SYS_HOST) \
	  || { echo "selfhost-armreturn: the emission is not clean C89 at -O2"; exit 1; }; \
	$(HOST_OUT)/armret > $(C_OUT)/armret.out 2>&1; \
	if ! diff -q tests/armreturn/arm_returns.expected $(C_OUT)/armret.out >/dev/null; then \
	  echo "selfhost-armreturn: an arm left by the wrong path:"; \
	  diff tests/armreturn/arm_returns.expected $(C_OUT)/armret.out | head -6; exit 1; fi; \
	echo "selfhost-armreturn: clean (3 arm returns lowered into the switch, -Werror clean, RUNS 1/3/10/30/40/50; the frozen reference still refuses it)"

# ===== the tutorial's capability emission, pinned =====
#
# Section 22 shows the C that `rune`, `ward`, `sigil` and `seal` lower to. Shown
# C rots: the compiler moves, the prose does not, and a reader who trusts a
# fragment that no longer emits has been told something false by an artifact
# that looks authoritative.
#
# So each fragment is compiled from a checked-in program, and the gate asserts
# THREE things, of which the third is the one that pins the DOCUMENT rather than
# the fixture: the program still compiles, its emission still equals the golden,
# and every golden line still appears VERBATIM in the tutorial. Drop the third
# and the golden could track the compiler while the prose quietly drifts away
# from it.
#
# Compiled with the PORT. The frozen reference cannot parse the capability model
# at all - it only reserves the words - so no oracle-driven machinery takes part
# here, and that is why these programs live under tests/ rather than examples/,
# which is built by the reference end to end.
.PHONY: docs-capability-golden
docs-capability-golden: $(EXILC_BIN)
	@sig=""; \
	for f in cap_rune cap_ward; do \
	  test -f tests/docs/$$f.exl || { echo "docs-capability-golden: MISSING tests/docs/$$f.exl"; exit 1; }; \
	  test -s tests/docs/$$f.golden || { echo "docs-capability-golden: MISSING/EMPTY tests/docs/$$f.golden"; exit 1; }; \
	done; \
	test -s docs/exile-by-example.md || { echo "docs-capability-golden: MISSING/EMPTY docs/exile-by-example.md"; exit 1; }; \
	for e in tests/sigil/equality/gated.exl tests/sigil/equality/ungated.exl tests/seal/exits.exl; do \
	  test -f $$e || { echo "docs-capability-golden: MISSING $$e"; exit 1; }; \
	done; \
	rm -f $(C_OUT)/doc_cap_rune.c $(C_OUT)/doc_cap_ward.c $(C_OUT)/doc_gated.c $(C_OUT)/doc_ungated.c $(C_OUT)/doc_seal.c; \
	for f in cap_rune cap_ward; do \
	  w=`$(EXILC_BIN) --target c --c-out $(C_OUT)/doc_$$f.c tests/docs/$$f.exl 2>&1 >/dev/null`; \
	  test -s $(C_OUT)/doc_$$f.c || { echo "docs-capability-golden: the port emitted nothing for tests/docs/$$f.exl"; exit 1; }; \
	  if [ -n "$$w" ]; then \
	    echo "docs-capability-golden: tests/docs/$$f.exl is not warning-clean, and a tutorial snippet has to be:"; \
	    echo "$$w" | head -4; exit 1; fi; \
	done; \
	grep -E '^volatile' $(C_OUT)/doc_cap_rune.c > $(C_OUT)/doc_cap_rune.emit; \
	sed -e 's/^[[:space:]]*//' $(C_OUT)/doc_cap_ward.c | grep -E '^\*\(\(volatile' > $(C_OUT)/doc_cap_ward.emit; \
	for f in cap_rune cap_ward; do \
	  if ! diff -q tests/docs/$$f.golden $(C_OUT)/doc_$$f.emit >/dev/null; then \
	    echo "docs-capability-golden: the emission for $$f moved away from its golden:"; \
	    diff tests/docs/$$f.golden $(C_OUT)/doc_$$f.emit | head -8; exit 1; fi; \
	  n=0; \
	  while IFS= read -r line; do \
	    grep -Fq "$$line" docs/exile-by-example.md || { \
	      echo "docs-capability-golden: the tutorial no longer shows a line the compiler emits:"; \
	      echo "  $$line"; exit 1; }; \
	    n=`expr $$n + 1`; \
	  done < tests/docs/$$f.golden; \
	  sig="$$sig $$f=$$n-lines-pinned-in-prose"; \
	done; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/doc_gated.c tests/sigil/equality/gated.exl >/dev/null 2>&1; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/doc_ungated.c tests/sigil/equality/ungated.exl >/dev/null 2>&1; \
	test -s $(C_OUT)/doc_gated.c || { echo "docs-capability-golden: EMPTY gated emission (floor)"; exit 1; }; \
	if ! cmp -s $(C_OUT)/doc_gated.c $(C_OUT)/doc_ungated.c; then \
	  echo "docs-capability-golden: the tutorial says a claim costs nothing, and the two emissions now differ:"; \
	  diff $(C_OUT)/doc_gated.c $(C_OUT)/doc_ungated.c | head -6; exit 1; fi; \
	sig="$$sig sigil=gated==ungated"; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/doc_seal.c tests/seal/exits.exl >/dev/null 2>&1; \
	test -s $(C_OUT)/doc_seal.c || { echo "docs-capability-golden: EMPTY seal emission (floor)"; exit 1; }; \
	for line in '__seal0 = sys_seal_enter();' 'sys_seal_exit(__seal0);'; do \
	  grep -Fq "$$line" $(C_OUT)/doc_seal.c || { echo "docs-capability-golden: the seam shape the tutorial shows is not what a seal emits: $$line"; exit 1; }; \
	  grep -Fq "$$line" docs/exile-by-example.md || { echo "docs-capability-golden: the tutorial stopped showing the seam line: $$line"; exit 1; }; \
	done; \
	sig="$$sig seal=seam-pair-in-both"; \
	echo "docs-capability-golden: clean -$$sig"

# ===== the port compiles every file in its own tree =====
#
# `src/main.exl` was the one file the port could not build while the reference
# could - the single hole in the self-host claim, and one nothing measured. It
# is a consumer gate rather than a fixture on purpose: a reduced probe pins the
# mechanism that was found, and this pins the thing a user would actually notice.
#
# Both halves are asserted, because "it compiles" is not this project's standard:
# the port must accept the file AND emit what the reference emits, byte for byte.
.PHONY: selfhost-own-tree
selfhost-own-tree: $(EXILC_BIN)
	@test -f src/main.exl || { echo "selfhost-own-tree: MISSING src/main.exl - re-aim the gate at whatever replaced it"; exit 1; }; \
	rm -f $(C_OUT)/own_o.c $(C_OUT)/own_p.c; \
	$(EXILE) --target c --c-out $(C_OUT)/own_o.c src/main.exl >/dev/null 2>&1 \
	  || { echo "selfhost-own-tree: the REFERENCE rejected src/main.exl"; exit 1; }; \
	test -s $(C_OUT)/own_o.c || { echo "selfhost-own-tree: EMPTY reference emission (floor)"; exit 1; }; \
	if ! $(EXILC_BIN) --target c --c-out $(C_OUT)/own_p.c src/main.exl >/dev/null 2>$(C_OUT)/own_p.err; then \
	  echo "selfhost-own-tree: the PORT cannot compile a file in its own source tree:"; \
	  head -3 $(C_OUT)/own_p.err; exit 1; fi; \
	test -s $(C_OUT)/own_p.c || { echo "selfhost-own-tree: EMPTY port emission (floor)"; exit 1; }; \
	if ! diff -q $(C_OUT)/own_o.c $(C_OUT)/own_p.c >/dev/null; then \
	  echo "selfhost-own-tree: the port compiles src/main.exl but does not agree with the reference on it:"; \
	  diff $(C_OUT)/own_o.c $(C_OUT)/own_p.c | head -8; exit 1; fi; \
	echo "selfhost-own-tree: clean (port == reference on src/main.exl, `wc -l < $(C_OUT)/own_p.c` lines byte-identical)"

# ===== the two prelude-struct lists must name the same set =====
#
# `prelude_struct_index` seeds the emission with the prelude's struct names;
# `is_builtin_struct` answers "is this name a struct" during name resolution.
# They are two hand-written lists of one set, and they drifted: `Range` and
# `RangeInclusive` were seeded but not recognised, so a user ENUM of either name
# won a lookup the prelude's struct should have won - and both compilers still
# compiled the program, which is the face a user meets rather than reports.
#
# Pinned as SETS, not as text: order differs by design (the seed is ordered, the
# predicate is not), so comparing the sorted names is the property and comparing
# the lines would be the shape.
.PHONY: selfhost-prelude-struct-lists
selfhost-prelude-struct-lists:
	@test -f src/typecheck.exl || { echo "selfhost-prelude-struct-lists: MISSING src/typecheck.exl"; exit 1; }; \
	seed=`sed -n '/^fn prelude_struct_index(/,/^}/p' src/typecheck.exl | grep -o 'push_seed_struct(a, skip, &sigs, "[A-Za-z]*"' | sed 's/.*"\([A-Za-z]*\)"/\1/' | sort -u`; \
	pred=`sed -n '/^fn is_builtin_struct(/,/^}/p' src/typecheck.exl | grep -o 'str::eq(name, "[A-Za-z]*")' | sed -e 's/^str::eq(name, "//' -e 's/")$$//' | sort -u`; \
	if [ -z "$$seed" ]; then echo "selfhost-prelude-struct-lists: read NO names from the seed - the shape moved, re-aim the gate"; exit 1; fi; \
	if [ -z "$$pred" ]; then echo "selfhost-prelude-struct-lists: read NO names from the predicate - the shape moved, re-aim the gate"; exit 1; fi; \
	tps=`sed -n '/^fn prelude_struct_tps(/,/^}/p' src/typecheck.exl | grep -o 'push_prelude_stp(a, "[A-Za-z]*"' | sed 's/.*"\([A-Za-z]*\)"/\1/' | sort -u`; \
	if [ -z "$$tps" ]; then echo "selfhost-prelude-struct-lists: read NO names from the tparam table - the shape moved, re-aim the gate"; exit 1; fi; \
	missing=`comm -23 <(echo "$$seed") <(echo "$$pred")`; \
	if [ -n "$$missing" ]; then \
	  echo "selfhost-prelude-struct-lists: seeded as structs but not recognised as ones -"; \
	  echo "  a user enum of these names would win a lookup the prelude's struct should win:"; \
	  echo "$$missing" | sed 's/^/    /'; exit 1; fi; \
	notps=`comm -23 <(echo "$$pred") <(echo "$$tps")`; \
	if [ -n "$$notps" ]; then \
	  echo "selfhost-prelude-struct-lists: recognised as structs but carrying no parameter count -"; \
	  echo "  the arity check abstains on these, so a wrong application compiles:"; \
	  echo "$$notps" | sed 's/^/    /'; exit 1; fi; \
	echo "selfhost-prelude-struct-lists: clean (`echo "$$seed" | wc -l` seeded, `echo "$$pred" | wc -l` recognised, `echo "$$tps" | wc -l` with a parameter count - the three lists agree)"

# ===== the prelude is checked against the user's declarations =====
#
# The reference prepends its prelude to the program and type-checks the two
# together, so a user declaration taking a prelude name can break the prelude's
# own code - and that failure is the program's verdict, reported at the head of
# the prelude. The port synthesises its prelude on demand, so nothing asked, and
# it compiled a whole class of programs the reference refuses.
#
# The `tc-errors` corpus already compares first lines and therefore already
# compares positions. This gate exists for the half that a corpus comparison
# cannot state out loud: the position is a CONTRACT, `<prelude>:1:1`, and it is
# pinned here as a literal so that agreeing with the reference and agreeing with
# the contract are two separate assertions. A day when both sides move together
# is a day this gate is supposed to notice.
PRELUDE_PROBE_LINE := <prelude>:1:1: error: type 'Vec' expects 0 generic argument(s), got 1
PRELUDE_PROBE_FIXTURE := src/tc_errors/prelude_breaks_on_shadowed_arity.exl

.PHONY: selfhost-prelude-probe
selfhost-prelude-probe: $(EXILC_BIN)
	@test -f $(PRELUDE_PROBE_FIXTURE) || { echo "selfhost-prelude-probe: MISSING $(PRELUDE_PROBE_FIXTURE)"; exit 1; }; \
	rm -f $(C_OUT)/pp_o.err $(C_OUT)/pp_p.err $(C_OUT)/pp_o.c $(C_OUT)/pp_p.c; \
	$(EXILE) --target c --c-out $(C_OUT)/pp_o.c $(PRELUDE_PROBE_FIXTURE) >/dev/null 2>$(C_OUT)/pp_o.err; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/pp_p.c $(PRELUDE_PROBE_FIXTURE) >/dev/null 2>$(C_OUT)/pp_p.err; \
	oline=`grep -m1 'error:' $(C_OUT)/pp_o.err`; \
	pline=`grep -m1 'error:' $(C_OUT)/pp_p.err`; \
	if [ -z "$$oline" ]; then echo "selfhost-prelude-probe: the REFERENCE said nothing - the fixture stopped exercising the class"; exit 1; fi; \
	if [ -z "$$pline" ]; then echo "selfhost-prelude-probe: the port said nothing - the probe is not running"; exit 1; fi; \
	if [ "$$oline" != "$(PRELUDE_PROBE_LINE)" ]; then \
	  echo "selfhost-prelude-probe: the reference no longer says the pinned line"; \
	  echo "  pinned:    $(PRELUDE_PROBE_LINE)"; echo "  reference: $$oline"; exit 1; fi; \
	if [ "$$pline" != "$(PRELUDE_PROBE_LINE)" ]; then \
	  echo "selfhost-prelude-probe: the port does not say the pinned line"; \
	  echo "  pinned: $(PRELUDE_PROBE_LINE)"; echo "  port:   $$pline"; exit 1; fi; \
	echo "selfhost-prelude-probe: clean - both compilers say the pinned line, position included"

# ===== the NDK: exile over the silicon =====
#
# tests/kernel/ndk/ is a LIBRARY written in exile, not a binding: sigils naming
# who owns which byte range, wards naming the register layout, atomic groups
# naming which registers cannot be torn apart. It binds to the chip and never to
# an OS, so one file serves a program under AmigaOS and one on bare metal.
#
# Four things are asserted, and the first is the one that makes the rest worth
# having: the library EMITS NOTHING. A program that names it and touches nothing
# pays nothing, and a program that drives a register pays the same store it would
# have written by hand.
.PHONY: selfhost-ndk
selfhost-ndk: $(EXILC_BIN)
	@for f in tests/kernel/ndk/mod.exl tests/kernel/ndk_dma.exl tests/kernel/ndk_dma.expected \
	          tests/kernel/ndk_dma_stub.c tests/kernel/ndk_blit.exl tests/kernel/ndk_blit.golden \
	          tests/kernel/ndk_rows.txt; do \
	  test -s $$f || { echo "selfhost-ndk: MISSING/EMPTY $$f"; exit 1; }; \
	done; \
	rm -rf $(C_OUT)/ndk; mkdir -p $(C_OUT)/ndk; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ndk/lib.c tests/kernel/ndk/mod.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ndk: the library does not compile on its own"; exit 1; }; \
	test -s $(C_OUT)/ndk/lib.c || { echo "selfhost-ndk: EMPTY emission for the library (floor)"; exit 1; }; \
	leak=`grep -oE 'Blitter|Serial|DmaCtl|IntCtl|Control|Blit|dmacon|intena|bltsize|serdat' $(C_OUT)/ndk/lib.c | sort -u | tr '\n' ' '`; \
	test -z "$$leak" || { echo "selfhost-ndk: the library EMITTED something - [$$leak] reached the C, and a declaration must cost nothing"; exit 1; }; \
	n=`wc -l < $(C_OUT)/ndk/lib.c`; \
	test "$$n" -le 4 || { echo "selfhost-ndk: the library emitted $$n lines - it declares, it does not generate"; exit 1; }; \
	$(EXILC_BIN) --target host --link $(SYS_HOST) --link tests/kernel/ndk_dma_stub.c \
	   -o $(HOST_OUT)/ndk_dma tests/kernel/ndk_dma.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ndk: the DMA consumer does not build"; exit 1; }; \
	$(HOST_OUT)/ndk_dma > $(C_OUT)/ndk/dma.out 2>&1; \
	diff -q tests/kernel/ndk_dma.expected $(C_OUT)/ndk/dma.out >/dev/null \
	  || { echo "selfhost-ndk: the DMA read-modify-write RAN wrong:"; \
	       diff tests/kernel/ndk_dma.expected $(C_OUT)/ndk/dma.out | head -6; exit 1; }; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/ndk/blit.c tests/kernel/ndk_blit.exl >/dev/null 2>&1 \
	  || { echo "selfhost-ndk: the blitter consumer does not compile"; exit 1; }; \
	grep -E '^[[:space:]]+\*\(\(volatile' $(C_OUT)/ndk/blit.c | sed 's/^[[:space:]]*//' > $(C_OUT)/ndk/blit.emit; \
	test -s $(C_OUT)/ndk/blit.emit || { echo "selfhost-ndk: the blitter consumer emitted NO stores"; exit 1; }; \
	if ! diff -q tests/kernel/ndk_blit.golden $(C_OUT)/ndk/blit.emit >/dev/null; then \
	  echo "selfhost-ndk: the blitter SEQUENCE moved (an address, a width, or an order):"; \
	  diff tests/kernel/ndk_blit.golden $(C_OUT)/ndk/blit.emit | head -8; exit 1; fi; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/ndk/blit.c -o $(C_OUT)/ndk/blit.o \
	  || { echo "selfhost-ndk: the blitter C is not clean C89 at -O2"; exit 1; }; \
	rows=0; \
	while IFS=: read -r name want; do \
	  case "$$name" in ''|'#'*) continue;; esac; \
	  test -f tests/kernel/$$name.exl || { echo "selfhost-ndk: MISSING tests/kernel/$$name.exl"; exit 1; }; \
	  rows=`expr $$rows + 1`; \
	  if $(EXILC_BIN) --target c --c-out $(C_OUT)/ndk/$$name.c tests/kernel/$$name.exl >/dev/null 2>$(C_OUT)/ndk/$$name.err; then \
	    echo "selfhost-ndk: ACCEPTED $$name - the library's own declaration has no teeth"; exit 1; fi; \
	  got=`grep -m1 'error:' $(C_OUT)/ndk/$$name.err | sed 's/.*error: //'`; \
	  test "$$got" = "$$want" || { echo "selfhost-ndk: WORDING $$name"; echo "  want: $$want"; echo "  got:  $$got"; exit 1; }; \
	done < tests/kernel/ndk_rows.txt; \
	test $$rows -ge 2 || { echo "selfhost-ndk: only $$rows rejection rows ran - the table lost members"; exit 1; }; \
	echo "selfhost-ndk: clean (the library emits NOTHING; the DMA pair RUNS its read-modify-write in one region; the blitter emits its 7 pinned stores and compiles C89 at -O2; $$rows rows refuse on the library's own sigil and atomic group)"

# ===== the seam on bare metal =====
#
# `runtime/sys_amiga.c` answers the seam over AmigaOS; `runtime/sys_bare.c`
# answers the same five symbols over the silicon. The five were MEASURED with
# `nm -u` on an emitted program, not assumed: sys_write, sys_alloc, sys_free,
# sys_seal_enter, sys_seal_exit.
#
# The proof is a link, not a compile. tests/kernel/bare_seam.exl spends every
# part of the contract - it prints, allocates and seals a ward through an atomic
# group - and the gate links it `-nostdlib` against nothing but freestanding.c
# and the bare seam. If anything still reaches for libc, the linker says so by
# name.
#
# Two arms look at the ASSEMBLY, because "the seam masks interrupts" and "the
# seam writes to the serial port" are claims about instructions, and an
# implementation that compiled but touched neither would pass every other check.
BARE_SEAM := runtime/sys_bare.c
.PHONY: selfhost-bare
selfhost-bare: $(EXILC_BIN)
	@if [ ! -x $(AMIGA_GCC) ]; then echo "selfhost-bare: amiga-gcc missing - run 'make toolchain' first"; exit 1; fi; \
	for f in $(BARE_SEAM) runtime/freestanding.c tests/kernel/bare_seam.exl tests/kernel/bare_seam_stub.c; do \
	  test -s $$f || { echo "selfhost-bare: MISSING/EMPTY $$f"; exit 1; }; \
	done; \
	rm -rf $(C_OUT)/bare; mkdir -p $(C_OUT)/bare; \
	$(AMIGA_GCC) -noixemul -ffreestanding -fno-builtin -O2 -Wall -Werror -c $(BARE_SEAM) -o $(C_OUT)/bare/seam.o \
	  || { echo "selfhost-bare: the bare seam does not compile clean for m68k"; exit 1; }; \
	def=`$(AMIGA_NM) --defined-only -g $(C_OUT)/bare/seam.o | awk '{print $$3}' | sed 's/^_//' | sort | tr '\n' ' '`; \
	test "$$def" = "sys_alloc sys_free sys_seal_enter sys_seal_exit sys_write " \
	  || { echo "selfhost-bare: the seam defines [$$def] - the contract is exactly the five symbols an emitted program can reference"; exit 1; }; \
	need=`$(AMIGA_NM) -u $(C_OUT)/bare/seam.o | awk '{print $$NF}' | sort | tr '\n' ' '`; \
	test -z "$$need" || { echo "selfhost-bare: the seam itself needs [$$need] - a backend that depends on something else is not a backend"; exit 1; }; \
	$(AMIGA_GCC) -noixemul -ffreestanding -fno-builtin -O2 -S $(BARE_SEAM) -o $(C_OUT)/bare/seam.s; \
	grep -q 'move\.w %sr,' $(C_OUT)/bare/seam.s \
	  || { echo "selfhost-bare: the seal seam never READS sr - the token it returns is not a saved mask"; exit 1; }; \
	grep -q 'or\.w #0x0700,%sr' $(C_OUT)/bare/seam.s \
	  || { echo "selfhost-bare: the seal seam never MASKS (or.w #0x0700,sr) - the region would not be indivisible"; exit 1; }; \
	grep -q 'move\.w .*,%sr' $(C_OUT)/bare/seam.s \
	  || { echo "selfhost-bare: the seal seam never RESTORES sr - nesting could not hold"; exit 1; }; \
	for reg in 14676016 14675992; do \
	  grep -q "$$reg" $(C_OUT)/bare/seam.s \
	    || { echo "selfhost-bare: the write half never reaches custom register $$reg - it is not talking to the silicon"; exit 1; }; \
	done; \
	$(EXILC_BIN) --target c --freestanding --c-out $(C_OUT)/bare/prog.c tests/kernel/bare_seam.exl >/dev/null 2>&1 \
	  || { echo "selfhost-bare: the port cannot emit the seam witness"; exit 1; }; \
	test -s $(C_OUT)/bare/prog.c || { echo "selfhost-bare: EMPTY emission (floor)"; exit 1; }; \
	for u in sys_write sys_alloc sys_free sys_seal_enter sys_seal_exit; do \
	  grep -q "$$u" $(C_OUT)/bare/prog.c \
	    || { echo "selfhost-bare: the witness no longer reaches $$u - it stopped spending the whole contract"; exit 1; }; \
	done; \
	$(AMIGA_GCC) -noixemul -ffreestanding -fno-builtin -O2 -I runtime -c $(C_OUT)/bare/prog.c -o $(C_OUT)/bare/prog.o \
	  || { echo "selfhost-bare: the emitted C does not compile for m68k"; exit 1; }; \
	$(AMIGA_GCC) -noixemul -ffreestanding -fno-builtin -O2 -Wall -Werror -I runtime -c runtime/freestanding.c -o $(C_OUT)/bare/fs.o \
	  || { echo "selfhost-bare: runtime/freestanding.c does not compile clean for m68k"; exit 1; }; \
	$(AMIGA_GCC) -noixemul -ffreestanding -fno-builtin -O2 -c tests/kernel/bare_seam_stub.c -o $(C_OUT)/bare/stub.o \
	  || { echo "selfhost-bare: the overlay stub does not compile for m68k"; exit 1; }; \
	$(AMIGA_GCC) -noixemul -nostdlib -ffreestanding -fno-builtin -O2 -e _main -o $(C_OUT)/bare/witness \
	   $(C_OUT)/bare/prog.o $(C_OUT)/bare/fs.o $(C_OUT)/bare/seam.o $(C_OUT)/bare/stub.o 2>$(C_OUT)/bare/link.err \
	  || { echo "selfhost-bare: the -nostdlib link did NOT close - something still reaches for libc:"; \
	       grep -oE 'undefined reference to .[^\x27]*.' $(C_OUT)/bare/link.err | sort -u | head -6; exit 1; }; \
	test -s $(C_OUT)/bare/witness || { echo "selfhost-bare: the link produced nothing"; exit 1; }; \
	left=`$(AMIGA_NM) -u $(C_OUT)/bare/witness 2>/dev/null | awk '{print $$NF}' | sort | tr '\n' ' '`; \
	test -z "$$left" || { echo "selfhost-bare: the linked witness still owes [$$left]"; exit 1; }; \
	sz=`wc -c < $(C_OUT)/bare/witness`; \
	echo "selfhost-bare: clean (the seam defines exactly the five measured symbols and needs none; sr is read, masked and restored; the write half reaches the custom chip; and a ward+seal+atomic program links -nostdlib into $$sz bytes with no libc)"

# ===== `--freestanding` in the port: the mirror =====
#
# The kernel arc needs two halves that until now lived in different compilers:
# the capability model is port-only (the reference answers `'rune' is a reserved
# word`), and libc-free emission was reference-only (the port's driver answered
# `unknown flag`). This gate is the proof the second half crossed over.
#
# The comparison is the WHOLE example corpus in freestanding mode, not the two
# files named `freestanding_*`: a mode that changes print, strlen, memzero, the
# include block and every @debug fragment is not measured by one program. Each
# example either emits from both compilers byte for byte, or is refused by both
# with the same first diagnostic line - the project's contract for a diagnostic,
# the source-line echo being a driver presentation layer the port has never had.
.PHONY: selfhost-freestanding
selfhost-freestanding: $(EXILC_BIN)
	@test -f runtime/freestanding.c || { echo "selfhost-freestanding: MISSING runtime/freestanding.c"; exit 1; }; \
	n=`ls examples/*.exl 2>/dev/null | wc -l`; \
	test "$$n" -ge 90 || { echo "selfhost-freestanding: the corpus walk found only $$n examples - the walk is broken, not the corpus"; exit 1; }; \
	rm -rf $(C_OUT)/fsm; mkdir -p $(C_OUT)/fsm; \
	agree=0; rboth=0; bad=0; \
	for f in examples/*.exl; do \
	  b=`basename $$f .exl`; rm -f $(C_OUT)/fsm/o.c $(C_OUT)/fsm/p.c; \
	  oe=`$(EXILE) --target c --freestanding --c-out $(C_OUT)/fsm/o.c $$f 2>&1 >/dev/null`; orc=$$?; \
	  pe=`$(EXILC_BIN) --target c --freestanding --c-out $(C_OUT)/fsm/p.c $$f 2>&1 >/dev/null`; prc=$$?; \
	  if [ $$orc -ne 0 ] && [ $$prc -ne 0 ]; then \
	    ol=`echo "$$oe" | head -1`; pl=`echo "$$pe" | head -1`; \
	    if [ "$$ol" = "$$pl" ]; then rboth=`expr $$rboth + 1`; \
	    else echo "selfhost-freestanding: $$b refused by both, DIFFERENT first line"; \
	         echo "  oracle: $$ol"; echo "  port:   $$pl"; bad=`expr $$bad + 1`; fi; \
	  elif [ $$orc -ne 0 ] || [ $$prc -ne 0 ]; then \
	    echo "selfhost-freestanding: $$b - one compiler refused and the other did not (oracle=$$orc port=$$prc)"; \
	    echo "  oracle: `echo "$$oe" | head -1`"; echo "  port:   `echo "$$pe" | head -1`"; bad=`expr $$bad + 1`; \
	  elif cmp -s $(C_OUT)/fsm/o.c $(C_OUT)/fsm/p.c; then agree=`expr $$agree + 1`; \
	  else echo "selfhost-freestanding: $$b DIVERGED in freestanding emission"; \
	       diff $(C_OUT)/fsm/o.c $(C_OUT)/fsm/p.c | head -8; bad=`expr $$bad + 1`; fi; \
	done; \
	test $$bad -eq 0 || exit 1; \
	test $$agree -ge 90 || { echo "selfhost-freestanding: only $$agree emissions compared - the walk lost members"; exit 1; }; \
	test $$rboth -ge 1 || { echo "selfhost-freestanding: NO example was refused by both - the mode's one refusal (a float print) is not being exercised, so the limit is unmeasured"; exit 1; }; \
	rm -f $(C_OUT)/fsm/fp.c $(C_OUT)/fsm/fp.o $(C_OUT)/fsm/fp $(C_OUT)/fsm/fp.out; \
	$(EXILC_BIN) --target c --freestanding --c-out $(C_OUT)/fsm/fp.c examples/freestanding_print.exl >/dev/null 2>&1 \
	  || { echo "selfhost-freestanding: the port cannot emit the freestanding witness"; exit 1; }; \
	test -s $(C_OUT)/fsm/fp.c || { echo "selfhost-freestanding: EMPTY emission (floor)"; exit 1; }; \
	$(CC) $(CFLAGS) -ffreestanding -fno-stack-protector -fno-pic -I runtime -c $(C_OUT)/fsm/fp.c -o $(C_OUT)/fsm/fp.o \
	  || { echo "selfhost-freestanding: the port's freestanding C does not compile -ffreestanding"; exit 1; }; \
	leak=`nm -u $(C_OUT)/fsm/fp.o | awk '{print $$NF}' | grep -vE '^(__ex_|sys_)' || true`; \
	if [ -n "$$leak" ]; then echo "selfhost-freestanding: LIBC LEAK in the PORT's output -> $$leak"; exit 1; fi; \
	$(CC) -I runtime $(C_OUT)/fsm/fp.c runtime/freestanding.c $(SYS_HOST) -o $(C_OUT)/fsm/fp \
	  || { echo "selfhost-freestanding: the port's freestanding C does not link"; exit 1; }; \
	$(C_OUT)/fsm/fp > $(C_OUT)/fsm/fp.out 2>&1; \
	diff -q examples/freestanding_print.expected $(C_OUT)/fsm/fp.out >/dev/null \
	  || { echo "selfhost-freestanding: the port's freestanding binary RAN wrong:"; \
	       diff examples/freestanding_print.expected $(C_OUT)/fsm/fp.out | head -6; exit 1; }; \
	echo "selfhost-freestanding: clean ($$agree emissions byte-identical to the reference, $$rboth refused by both with the same first line, and the port's own output is nm-clean and RUNS)"

# ===== the compiler's own source compiles WARNING-FREE =====
#
# Thirty-three warnings accumulated here unnoticed, which is what an unwatched
# channel does. Two of them were not cosmetic: an unused `args` on both rune
# `.read` paths was the arity check nobody had written, and the argument was
# DROPPED rather than ignored.
#
# The measurement has to be taken through an OPEN channel. `bootstrap-from-seed`
# reports zero warnings for a reason that has nothing to do with the source - it
# redirects every compiler invocation to /dev/null - and a number read from a
# muted pipe is not a measurement.
.PHONY: selfhost-warning-free
selfhost-warning-free: $(EXILC_BIN)
	@test -f src/exilc.exl || { echo "selfhost-warning-free: MISSING src/exilc.exl"; exit 1; }; \
	rm -f $(C_OUT)/wf.c $(C_OUT)/wf.err; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/wf.c src/exilc.exl 2>$(C_OUT)/wf.err >/dev/null \
	  || { echo "selfhost-warning-free: the port cannot compile its own source"; head -3 $(C_OUT)/wf.err; exit 1; }; \
	test -s $(C_OUT)/wf.c \
	  || { echo "selfhost-warning-free: EMPTY emission - the run said nothing because it did nothing"; exit 1; }; \
	n=`grep -c 'warning:' $(C_OUT)/wf.err`; \
	if [ "$$n" != "0" ]; then \
	  echo "selfhost-warning-free: $$n warning(s) compiling the compiler's own source."; \
	  echo "  An unused parameter is worth reading before silencing - twice now one has been a missing check:"; \
	  grep 'warning:' $(C_OUT)/wf.err | head -10; exit 1; fi; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/wf.c -o $(C_OUT)/wf.o 2>$(C_OUT)/wf.cc \
	  || { echo "selfhost-warning-free: the compiler's OWN emission is not clean C89 at the standard every fixture is held to:"; \
	       head -6 $(C_OUT)/wf.cc; exit 1; }; \
	rm -f $(C_OUT)/wf.o; \
	echo "selfhost-warning-free: clean (src/exilc.exl compiles with 0 warnings off an open stderr, and its emission is clean C89 at -Wall -Werror)"

# ===== `atomic` groups: the rows, structural and regional =====
#
# The capability model is PORT-ONLY ground: the frozen reference answers
# `'ward' is a reserved word (capability model, future syntax)` and cannot parse
# any of these files. There is no differential to lean on, so the discipline is
# the one that stands in its place - every rejection row asserts its own WORDING
# and every accepting row RUNS, both read from data files so the signature comes
# from the arms actually walked, and the clause is proved to leave nothing behind
# by comparing a program to its twin with the clause deleted.
.PHONY: selfhost-atomic
selfhost-atomic: $(EXILC_BIN)
	@for t in atomic_rows.txt atomic_runs.txt; do \
	  test -s tests/ward/$$t || { echo "selfhost-atomic: MISSING/EMPTY tests/ward/$$t"; exit 1; }; \
	done; \
	for f in atomic_group_absent atomic_name_not_reserved; do \
	  test -f tests/ward/$$f.exl || { echo "selfhost-atomic: MISSING tests/ward/$$f.exl"; exit 1; }; \
	done; \
	rm -f $(C_OUT)/at_*.c $(C_OUT)/at_*.err $(C_OUT)/at_*.out $(HOST_OUT)/at_*; \
	fail=0; n=0; \
	while IFS=: read -r name want; do \
	  case "$$name" in ''|'#'*) continue;; esac; \
	  test -f tests/ward/$$name.exl || { echo "selfhost-atomic: MISSING tests/ward/$$name.exl"; exit 1; }; \
	  n=`expr $$n + 1`; \
	  if $(EXILC_BIN) --target c --c-out $(C_OUT)/at_$$name.c tests/ward/$$name.exl >/dev/null 2>$(C_OUT)/at_$$name.err; then \
	    echo "selfhost-atomic: ACCEPTED $$name - the row does not reject"; fail=`expr $$fail + 1`; continue; fi; \
	  got=`grep -m1 'error:' $(C_OUT)/at_$$name.err | sed 's/.*error: //'`; \
	  if [ -z "$$got" ]; then \
	    echo "selfhost-atomic: SILENT $$name - refused with no diagnostic"; fail=`expr $$fail + 1`; continue; fi; \
	  if [ "$$got" != "$$want" ]; then \
	    echo "selfhost-atomic: WORDING $$name"; echo "  want: $$want"; echo "  got:  $$got"; fail=`expr $$fail + 1`; fi; \
	done < tests/ward/atomic_rows.txt; \
	test $$n -ge 9 || { echo "selfhost-atomic: only $$n rejection rows ran - the table lost members"; exit 1; }; \
	test $$fail -eq 0 || exit 1; \
	m=0; \
	while IFS=: read -r name want; do \
	  case "$$name" in ''|'#'*) continue;; esac; \
	  test -f tests/ward/$$name.exl || { echo "selfhost-atomic: MISSING tests/ward/$$name.exl"; exit 1; }; \
	  test -f tests/ward/$${name}_stub.c || { echo "selfhost-atomic: MISSING the RAM backing tests/ward/$${name}_stub.c - an accepting row that cannot run is not an accepting row"; exit 1; }; \
	  m=`expr $$m + 1`; \
	  $(EXILC_BIN) --target host --link runtime/sys_host.c --link tests/ward/$${name}_stub.c \
	     -o $(HOST_OUT)/at_$$name tests/ward/$$name.exl >/dev/null 2>&1 \
	    || { echo "selfhost-atomic: the accepting row $$name does not build for the host"; exit 1; }; \
	  $(HOST_OUT)/at_$$name > $(C_OUT)/at_$$name.out 2>&1; \
	  test -s $(C_OUT)/at_$$name.out || { echo "selfhost-atomic: $$name printed NOTHING"; exit 1; }; \
	  got=`head -1 $(C_OUT)/at_$$name.out`; \
	  test "$$got" = "$$want" || { echo "selfhost-atomic: $$name RAN wrong - want $$want, got $$got"; cat $(C_OUT)/at_$$name.out; exit 1; }; \
	  grep -q 'seal-balance 0 misnest 0' $(C_OUT)/at_$$name.out \
	    || { echo "selfhost-atomic: $$name left its region UNBALANCED"; cat $(C_OUT)/at_$$name.out; exit 1; }; \
	done < tests/ward/atomic_runs.txt; \
	test $$m -ge 5 || { echo "selfhost-atomic: only $$m accepting rows ran - the table lost members"; exit 1; }; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/at_ok.c tests/ward/atomic_group_wellformed.exl >/dev/null 2>&1 \
	  || { echo "selfhost-atomic: the accepting row does not compile"; exit 1; }; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/at_bare.c tests/ward/atomic_group_absent.exl >/dev/null 2>&1 \
	  || { echo "selfhost-atomic: the clause-free twin does not compile"; exit 1; }; \
	test -s $(C_OUT)/at_ok.c || { echo "selfhost-atomic: the accepting row emitted an EMPTY translation unit"; exit 1; }; \
	cmp -s $(C_OUT)/at_ok.c $(C_OUT)/at_bare.c \
	  || { echo "selfhost-atomic: the clause CHANGED the emission - it must leave nothing behind"; \
	       diff $(C_OUT)/at_ok.c $(C_OUT)/at_bare.c | head -8; exit 1; }; \
	$(EXILC_BIN) --target host --link runtime/sys_host.c --link tests/ward/atomic_name_not_reserved_stub.c \
	   -o $(HOST_OUT)/at_name tests/ward/atomic_name_not_reserved.exl >/dev/null 2>&1 \
	  || { echo "selfhost-atomic: 'atomic' became RESERVED - a ward field, struct field, fn or local carrying that name no longer builds"; exit 1; }; \
	$(HOST_OUT)/at_name > $(C_OUT)/at_name.out 2>&1; \
	got=`tr '\n' ' ' < $(C_OUT)/at_name.out`; \
	test "$$got" = "42 42 " || { echo "selfhost-atomic: the name-collision row RAN wrong (a name bound to the wrong thing):"; \
	                             cat $(C_OUT)/at_name.out; exit 1; }; \
	echo "selfhost-atomic: clean ($$n rejections with their wording, $$m accepting rows RUN and balanced, 'atomic' still usable as a name, and the clause emits nothing)"

# ===== publicly-facing text must stand on its own =====
#
# Design records, phase names and step labels live in a directory that is not
# part of the repository, so a reader who meets a design-record number or a
# phase name in an example has nowhere to resolve it. The rule is not "do not
# mention the internals" - it is that a sentence must carry its own meaning,
# which for these labels means saying the thing instead of naming its record.
#
# Scoped to what a reader meets first: the feature catalogue, the tutorial, the
# changelog and the README. Compiler sources are a separate class with a
# separate clock and are deliberately NOT covered here.
DOCS_INTERNAL_RX := DR-0[0-9][0-9]|(FUZZ|SEAL|RUNE|WARD|SIGIL)-SPEC|WORKLOG|Faza|§[ ]*[0-9]|GATE-[0-9]|Site-[0-9]|Delta-[A-Z]\b|graft G[0-9]|\bS5[a-d]\b|Tier-[0-9]

# The compiler sources get the same rule with one term removed: `typecheck.exl`
# says "Phase 1:" / "Phase 2:" to describe a LOCAL two-phase algorithm in the
# same comment, and those sentences already carry their own meaning.
#
# R1-R7, W1-W5, S1-S5 are deliberately ABSENT from both patterns. They resolve
# inside this repository - each names a fixture whose header states the rule, and
# the rune/ward/sigil gate signatures cite them as a table. An identifier with a
# definition in the repo is not a reference that leaves it.
SRC_INTERNAL_RX := DR-0[0-9][0-9]|(FUZZ|SEAL|RUNE|WARD|SIGIL)-SPEC|WORKLOG|Faza|§[ ]*[0-9]|GATE-[0-9]|I-[RWST][0-9]|axis [0-9]|correction [A-Z]|Increment [0-9]

.PHONY: docs-selfsufficient
docs-selfsufficient:
	@for f in examples docs README.md CHANGELOG.md src; do \
	  test -e $$f || { echo "docs-selfsufficient: MISSING $$f - the scope moved, re-aim the gate"; exit 1; }; \
	done; \
	n=`grep -rInE '$(DOCS_INTERNAL_RX)' examples docs README.md CHANGELOG.md 2>/dev/null | wc -l`; \
	if [ "$$n" != "0" ]; then \
	  echo "docs-selfsufficient: $$n publicly-facing reference(s) a reader cannot resolve -"; \
	  echo "  the record they name is not in this repository. Say the thing, not its label:"; \
	  grep -rInE '$(DOCS_INTERNAL_RX)' examples docs README.md CHANGELOG.md 2>/dev/null | head -10; \
	  exit 1; fi; \
	m=`grep -rInE '$(SRC_INTERNAL_RX)' src --include=*.exl 2>/dev/null | wc -l`; \
	if [ "$$m" != "0" ]; then \
	  echo "docs-selfsufficient: $$m compiler-source comment(s) naming a record this repository does not ship:"; \
	  grep -rInE '$(SRC_INTERNAL_RX)' src --include=*.exl 2>/dev/null | head -10; \
	  exit 1; fi; \
	tf=`find tests -path tests/golden -prune -o -type f ! -name '*.expected' ! -name '*.golden' -print 2>/dev/null`; \
	test -n "$$tf" || { echo "docs-selfsufficient: the tests scan enumerated NOTHING - the walk is broken, not the corpus"; exit 1; }; \
	t=`echo "$$tf" | xargs grep -InE '$(SRC_INTERNAL_RX)' 2>/dev/null | wc -l`; \
	if [ "$$t" != "0" ]; then \
	  echo "docs-selfsufficient: $$t fixture comment(s) naming a record this repository does not ship."; \
	  echo "  The rule IDs a fixture header states - R1-R7, W1-W5, S1-S5, T1-T4 - are not this:"; \
	  echo "  each names a fixture or a table row in this repository, so they resolve here."; \
	  echo "  The scan is every authored file under tests/, not a list of extensions:"; \
	  echo "$$tf" | xargs grep -InE '$(SRC_INTERNAL_RX)' 2>/dev/null | head -10; \
	  exit 1; fi; \
	d=`grep -rInE '$(SRC_INTERNAL_RX)' tests --include=*.md 2>/dev/null | wc -l`; \
	d=`expr $$d + \`grep -rInE '$(SRC_INTERNAL_RX)' src --include=*.md 2>/dev/null | wc -l\``; \
	d=`expr $$d + \`grep -rInE '$(SRC_INTERNAL_RX)' runtime .github 2>/dev/null | wc -l\``; \
	d=`expr $$d + \`grep -rInE '$(SRC_INTERNAL_RX)' tools/fuzz 2>/dev/null | wc -l\``; \
	if [ "$$d" != "0" ]; then \
	  echo "docs-selfsufficient: $$d reference(s) in a directory README, a seam backend, the CI workflow or the fuzz tooling -"; \
	  echo "  the class does not stop at the file types a previous round happened to scan:"; \
	  grep -rInE '$(SRC_INTERNAL_RX)' tests src --include=*.md 2>/dev/null | head -4; \
	  grep -rInE '$(SRC_INTERNAL_RX)' runtime .github tools/fuzz 2>/dev/null | head -6; \
	  exit 1; fi; \
	k=`grep -nE '$(SRC_INTERNAL_RX)' Makefile | grep -v '_INTERNAL_RX' | wc -l`; \
	if [ "$$k" != "0" ]; then \
	  echo "docs-selfsufficient: $$k gate message(s) naming a record this repository does not ship -"; \
	  echo "  and a reader meets these at the moment a gate goes red, which is the worst moment to be handed a dead pointer:"; \
	  grep -nE '$(SRC_INTERNAL_RX)' Makefile | grep -v '_INTERNAL_RX' | head -10; \
	  exit 1; fi; \
	echo "docs-selfsufficient: clean (examples/ docs/ README CHANGELOG src/ tests/ runtime/ .github/ tools/fuzz/ and the gate messages - every tracked file we own except the frozen reference and its suite)"

# ===== register #13 - seam externs in a file with no entry point =====
#
# The reference walks reachability from `main`; with no entry point it has no
# root and declares the whole seam.  The port keys on what the emitted code
# names, so it declares exactly that - with an entry point or without one.
#
# THREE facts, because a divergence with only one side measured is one that can
# widen quietly.  The third is the one that carries the reading: "the two merely
# differ" would also pass on a port that emitted nothing, which is precisely the
# misreading this gate exists to make unrepresentable.
selfhost-noentry-externs: $(EXILC_BIN)
	@sig=""; pin=13; \
	for f in entry_uses_seam noentry_plain noentry_uses_seam; do \
	  test -f tests/noentry/$$f.exl || { echo "selfhost-noentry-externs: MISSING tests/noentry/$$f.exl"; exit 1; }; \
	done; \
	rm -f $(C_OUT)/ne_*.c; \
	for f in entry_uses_seam noentry_plain noentry_uses_seam; do \
	  $(EXILE) --target c --c-out $(C_OUT)/ne_$$f.o.c tests/noentry/$$f.exl >/dev/null 2>&1 \
	    || { echo "selfhost-noentry-externs: the REFERENCE rejected tests/noentry/$$f.exl"; exit 1; }; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/ne_$$f.p.c tests/noentry/$$f.exl >/dev/null 2>&1 \
	    || { echo "selfhost-noentry-externs: the PORT rejected tests/noentry/$$f.exl"; exit 1; }; \
	  test -s $(C_OUT)/ne_$$f.o.c || { echo "selfhost-noentry-externs: EMPTY reference emission for $$f (floor)"; exit 1; }; \
	  test -s $(C_OUT)/ne_$$f.p.c || { echo "selfhost-noentry-externs: EMPTY port emission for $$f (floor)"; exit 1; }; \
	  cc -O2 -ansi -pedantic -Wall -Werror -I src -c -o /dev/null $(C_OUT)/ne_$$f.o.c \
	    || { echo "selfhost-noentry-externs: the REFERENCE emission for $$f fails the project -Werror standard"; exit 1; }; \
	  cc -O2 -ansi -pedantic -Wall -Werror -I src -c -o /dev/null $(C_OUT)/ne_$$f.p.c \
	    || { echo "selfhost-noentry-externs: the PORT emission for $$f fails the project -Werror standard"; exit 1; }; \
	done; \
	if ! diff -q $(C_OUT)/ne_entry_uses_seam.o.c $(C_OUT)/ne_entry_uses_seam.p.c >/dev/null; then \
	  echo "selfhost-noentry-externs: WITH an entry point the two emissions must be byte-identical - the divergence has escaped its confinement:"; \
	  diff $(C_OUT)/ne_entry_uses_seam.o.c $(C_OUT)/ne_entry_uses_seam.p.c | head -8; exit 1; fi; \
	n=`grep -c '^extern .*sys_' $(C_OUT)/ne_entry_uses_seam.p.c`; \
	if [ "$$n" != "1" ]; then echo "selfhost-noentry-externs: with an entry point both sides should declare the ONE called seam fn, found $$n"; exit 1; fi; \
	sig="$$sig entry=byte-identical(1 extern)"; \
	o=`grep -c '^extern .*sys_' $(C_OUT)/ne_noentry_plain.o.c`; \
	p=`grep -c '^extern .*sys_' $(C_OUT)/ne_noentry_plain.p.c`; \
	if [ "$$o" != "$$pin" ]; then echo "selfhost-noentry-externs: the reference's no-root fallback emitted $$o seam externs, not the pinned $$pin - the divergence changed size"; exit 1; fi; \
	if [ "$$p" != "0" ]; then echo "selfhost-noentry-externs: the port declared $$p seam externs for a file that references none"; exit 1; fi; \
	grep -v '^extern .*sys_' $(C_OUT)/ne_noentry_plain.o.c > $(C_OUT)/ne_plain.stripped; \
	if ! diff -q $(C_OUT)/ne_plain.stripped $(C_OUT)/ne_noentry_plain.p.c >/dev/null; then \
	  echo "selfhost-noentry-externs: with no entry point the difference must be EXACTLY the seam-extern block, but something else moved too:"; \
	  diff $(C_OUT)/ne_plain.stripped $(C_OUT)/ne_noentry_plain.p.c | head -8; exit 1; fi; \
	sig="$$sig plain=$$pin-vs-0(nothing-else-differs)"; \
	o=`grep -c '^extern .*sys_' $(C_OUT)/ne_noentry_uses_seam.o.c`; \
	if [ "$$o" != "$$pin" ]; then echo "selfhost-noentry-externs: the reference emitted $$o seam externs for the one-call file, not the pinned $$pin"; exit 1; fi; \
	used=`grep -c '^extern int sys_close(int fd);' $(C_OUT)/ne_noentry_uses_seam.p.c`; \
	if [ "$$used" != "1" ]; then \
	  echo "selfhost-noentry-externs: the CALLED seam extern is missing from the port's emission - pay-for-use has become emit-nothing:"; \
	  grep -n '^extern ' $(C_OUT)/ne_noentry_uses_seam.p.c | head -4; exit 1; fi; \
	unused=`grep '^extern .*sys_' $(C_OUT)/ne_noentry_uses_seam.p.c | grep -vc 'sys_close'`; \
	if [ "$$unused" != "0" ]; then echo "selfhost-noentry-externs: the port declared $$unused of the $$pin seam fns the file never calls"; exit 1; fi; \
	sed '/^extern .*sys_/{ /sys_close/!d; }' $(C_OUT)/ne_noentry_uses_seam.o.c > $(C_OUT)/ne_used.stripped; \
	if ! diff -q $(C_OUT)/ne_used.stripped $(C_OUT)/ne_noentry_uses_seam.p.c >/dev/null; then \
	  echo "selfhost-noentry-externs: the port's emission is not the reference minus exactly the 12 UNCALLED externs:"; \
	  diff $(C_OUT)/ne_used.stripped $(C_OUT)/ne_noentry_uses_seam.p.c | head -8; exit 1; fi; \
	sig="$$sig used=1-kept/12-dropped(exact-referenced-set)"; \
	echo "selfhost-noentry-externs: clean -$$sig, both sides -Werror clean"

# ===== register #7 — the port's parenthesised emission (B2 divergence) =====
#
# The first deliberate byte-exact divergence in the LANGUAGE zone, so it is gated
# BEHAVIOURALLY rather than against the frozen oracle it cannot match: the port
# must parenthesise a mixed bitwise nesting, the emission must survive the
# project's own -Werror standard, and the program must still compute what it
# says. A byte gate here would pin the divergence to a reference that is wrong.
selfhost-parens: $(EXILC_BIN)
	@test -f tests/parens/mixed_bitwise.exl || { echo "selfhost-parens: MISSING tests/parens/mixed_bitwise.exl"; exit 1; }; \
	test -s tests/parens/mixed_bitwise.expected || { echo "selfhost-parens: MISSING/EMPTY expected"; exit 1; }; \
	rm -f $(C_OUT)/parens.c $(HOST_OUT)/parens $(C_OUT)/parens.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/parens.c tests/parens/mixed_bitwise.exl >/dev/null 2>&1 \
	  || { echo "selfhost-parens: port rejected the mixed-operator fixture"; exit 1; }; \
	if [ ! -s $(C_OUT)/parens.c ]; then echo "selfhost-parens: EMPTY emitted C (floor)"; exit 1; fi; \
	grep -q '32768 | (old & 64)' $(C_OUT)/parens.c \
	  || { echo "selfhost-parens: the mixed nesting was NOT parenthesised (register #7 regressed):"; \
	       grep -n 'return' $(C_OUT)/parens.c | head -3; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/parens $(C_OUT)/parens.c $(SYS_HOST) \
	  || { echo "selfhost-parens: the emission still fails -Werror — that IS the defect"; exit 1; }; \
	$(HOST_OUT)/parens > $(C_OUT)/parens.out 2>&1; \
	if ! diff -q tests/parens/mixed_bitwise.expected $(C_OUT)/parens.out >/dev/null; then \
	  echo "selfhost-parens: parenthesising CHANGED THE VALUE — the fix must not alter semantics:"; \
	  diff tests/parens/mixed_bitwise.expected $(C_OUT)/parens.out | head -6; exit 1; fi; \
	echo "selfhost-parens: clean (mixed bitwise nesting parenthesised, emission clean under -Werror, values unchanged 32832/21/3)"

# ===== seal capability — the port's gate =====
#
# Two levels, because either alone lies.  The GOLDEN level counts the seam
# calls in the emitted C: one enter, and one exit per exit path (the defer
# machinery must have placed them, not the author).  The RUNNABLE level then
# executes it, and the host stub reports the balance and mis-nesting it saw —
# a count that is right in the text but wrong at run time cannot pass both.
selfhost-seal: $(EXILC_BIN)
	@sig=""; \
	for f in exits nested blitter_setup consumer_ram accept_seam_namesake reject_expr \
	         reject_seam_enter reject_seam_exit reject_seam_extern reject_sealed_theft \
	         defer_in_seal seal_in_defer cross_nest arm_return_after_seal arm_sibling_return \
	         accept_arm_seal arm_nested_returns try_propagation accept_limit_forgotten \
	         accept_limit_blanket accept_limit_race accept_limit_latency \
	         accept_seal_returns ; do \
	  test -f tests/seal/$$f.exl || { echo "selfhost-seal: MISSING tests/seal/$$f.exl"; exit 1; }; \
	done; \
	for e in exits nested consumer_ram try_propagation defer_in_seal seal_in_defer cross_nest \
	         arm_return_after_seal arm_sibling_return accept_arm_seal arm_nested_returns \
	         accept_seal_returns ; do \
	  test -s tests/seal/$$e.expected || { echo "selfhost-seal: MISSING/EMPTY tests/seal/$$e.expected"; exit 1; }; \
	done; \
	test -s tests/seal/blitter_setup.golden || { echo "selfhost-seal: MISSING/EMPTY tests/seal/blitter_setup.golden"; exit 1; }; \
	test -s tests/seal/REJECT-TABLE || { echo "selfhost-seal: MISSING/EMPTY tests/seal/REJECT-TABLE"; exit 1; }; \
	rm -f $(C_OUT)/seal_exits.c $(HOST_OUT)/seal_exits $(C_OUT)/seal_exits.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_exits.c tests/seal/exits.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: port rejected the exits fixture"; exit 1; }; \
	if [ ! -s $(C_OUT)/seal_exits.c ]; then echo "selfhost-seal: EMPTY emitted C (floor)"; exit 1; fi; \
	ent=`sed 's|//.*||' $(C_OUT)/seal_exits.c | grep -c 'sys_seal_enter();'`; \
	if [ "$$ent" != "1" ]; then echo "selfhost-seal: expected 1 seam enter, found $$ent"; exit 1; fi; \
	ext=`sed 's|//.*||' $(C_OUT)/seal_exits.c | grep -c 'sys_seal_exit(__seal'`; \
	if [ "$$ext" != "4" ]; then \
	  echo "selfhost-seal: expected 4 seam exits (fallthrough, return, break, continue), found $$ext"; exit 1; fi; \
	grep -q 'extern unsigned long sys_seal_enter(void);' $(C_OUT)/seal_exits.c \
	  || { echo "selfhost-seal: the seam extern was not emitted (pay-for-use is not paying)"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/seal_exits $(C_OUT)/seal_exits.c $(SYS_HOST) \
	  || { echo "selfhost-seal: emitted C is not clean C89 at -O2"; exit 1; }; \
	$(HOST_OUT)/seal_exits > $(C_OUT)/seal_exits.out 2>&1; \
	if ! diff -q tests/seal/exits.expected $(C_OUT)/seal_exits.out >/dev/null; then \
	  echo "selfhost-seal: WRONG run (an exit path lost its seam call, or the region is left unbalanced):"; \
	  diff tests/seal/exits.expected $(C_OUT)/seal_exits.out | head -8; exit 1; fi; \
	sig="$$sig one enter/four exits from the defer machinery;"; \
	rm -f $(C_OUT)/seal_nest.c $(HOST_OUT)/seal_nest $(C_OUT)/seal_nest.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_nest.c tests/seal/nested.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: port REJECTED nesting — it is a guarantee, not a prohibition on depth"; exit 1; }; \
	nent=`sed 's|//.*||' $(C_OUT)/seal_nest.c | grep -c 'sys_seal_enter();'`; \
	if [ "$$nent" != "2" ]; then echo "selfhost-seal: nested region expected 2 enters, found $$nent"; exit 1; fi; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/seal_nest $(C_OUT)/seal_nest.c $(SYS_HOST) \
	  || { echo "selfhost-seal: nested C is not clean C89 at -O2"; exit 1; }; \
	$(HOST_OUT)/seal_nest > $(C_OUT)/seal_nest.out 2>&1; \
	if ! diff -q tests/seal/nested.expected $(C_OUT)/seal_nest.out >/dev/null; then \
	  echo "selfhost-seal: WRONG nested run (an inner exit crossed its outer, or a token was restored out of order):"; \
	  diff tests/seal/nested.expected $(C_OUT)/seal_nest.out | head -8; exit 1; fi; \
	sig="$$sig nesting legal and balanced through return+break+continue crossing both levels;"; \
	rows=0; \
	while IFS='|' read -r fx frag row; do \
	  case "$$fx" in ''|\#*) continue;; esac; \
	  rows=$$((rows+1)); \
	  test -f tests/seal/$$fx.exl || { echo "selfhost-seal: table row '$$row' has no fixture tests/seal/$$fx.exl"; exit 1; }; \
	  rm -f $(C_OUT)/seal_rej.err; \
	  $(EXILC_BIN) --target c --c-out /dev/null tests/seal/$$fx.exl > $(C_OUT)/seal_rej.err 2>&1; \
	  if [ $$? -eq 0 ]; then echo "selfhost-seal: $$row — tests/seal/$$fx.exl was ACCEPTED"; exit 1; fi; \
	  if ! grep -qF "$$frag" $(C_OUT)/seal_rej.err; then \
	    echo "selfhost-seal: $$row — rejected for the WRONG reason (wanted: $$frag)"; \
	    head -2 $(C_OUT)/seal_rej.err; exit 1; fi; \
	  if grep -q 'internal:' $(C_OUT)/seal_rej.err; then \
	    echo "selfhost-seal: $$row is an ICE, not a diagnostic"; exit 1; fi; \
	done < tests/seal/REJECT-TABLE; \
	if [ "$$rows" -lt 5 ]; then echo "selfhost-seal: the T table walked only $$rows rows (a row was lost)"; exit 1; fi; \
	for orphan in `ls tests/seal/reject_*.exl 2>/dev/null`; do \
	  b=`basename $$orphan .exl`; \
	  grep -q "^$$b|" tests/seal/REJECT-TABLE \
	    || { echo "selfhost-seal: $$orphan is a rejection fixture with NO row in the T table"; exit 1; }; \
	done; \
	sig="$$sig the T table walked from data over $$rows rows with no orphan fixtures;"; \
	rm -f $(C_OUT)/seal_namesake.c $(C_OUT)/seal_namesake.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_namesake.c tests/seal/accept_seam_namesake.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: ACCEPT — T4 widened from the emitted SYMBOL into a ban on a NAME"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/seal_namesake.c -o $(C_OUT)/seal_namesake.o \
	  || { echo "selfhost-seal: namesake C is not clean C89 at -O2"; exit 1; }; \
	sig="$$sig a same-NAME non-extern still accepted and still compiling;"; \
	rm -f $(C_OUT)/seal_bl.c $(C_OUT)/seal_bl.o $(C_OUT)/seal_bl.seq; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_bl.c tests/seal/blitter_setup.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: port rejected the composed consumer (sigil+ward+rune+seal)"; exit 1; }; \
	if [ ! -s $(C_OUT)/seal_bl.c ]; then echo "selfhost-seal: EMPTY consumer C (floor)"; exit 1; fi; \
	sed -n '/sys_seal_enter();/,/sys_seal_exit(/p' $(C_OUT)/seal_bl.c | sed 's/^[ \t]*//' > $(C_OUT)/seal_bl.seq; \
	if ! diff -q tests/seal/blitter_setup.golden $(C_OUT)/seal_bl.seq >/dev/null; then \
	  echo "selfhost-seal: the sealed BLITTER SEQUENCE changed (order, address or seam placement):"; \
	  diff tests/seal/blitter_setup.golden $(C_OUT)/seal_bl.seq | head -10; exit 1; fi; \
	grep -q '\*size = v;' $(C_OUT)/seal_bl.c \
	  || { echo "selfhost-seal: the borrowed rune's write did not emit (BLTSIZE crosses the call boundary)"; exit 1; }; \
	if grep -qE 'Blitter|DmaControl|Custom' $(C_OUT)/seal_bl.c; then \
	  echo "selfhost-seal: a sigil/ward name leaked into the consumer's C (zero-cost)"; exit 1; fi; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/seal_bl.c -o $(C_OUT)/seal_bl.o \
	  || { echo "selfhost-seal: consumer C is not clean C89 at -O2"; exit 1; }; \
	sig="$$sig the composed consumer emits its HRM sequence byte for byte;"; \
	rm -f $(C_OUT)/seal_cr.c $(HOST_OUT)/seal_cr $(C_OUT)/seal_cr.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_cr.c tests/seal/consumer_ram.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: port rejected the runnable consumer"; exit 1; }; \
	cc -O2 -fno-strict-aliasing -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/seal_cr \
	   $(C_OUT)/seal_cr.c tests/seal/consumer_ram_stub.c $(SYS_HOST) \
	  || { echo "selfhost-seal: runnable consumer C is not clean at -O2 (-fno-strict-aliasing: MMIO overlays untyped memory)"; exit 1; }; \
	$(HOST_OUT)/seal_cr > $(C_OUT)/seal_cr.out 2>&1; \
	if ! diff -q tests/seal/consumer_ram.expected $(C_OUT)/seal_cr.out >/dev/null; then \
	  echo "selfhost-seal: the sealed sequence RAN wrong (a store missed, or the DMACON restore miscomputed):"; \
	  diff tests/seal/consumer_ram.expected $(C_OUT)/seal_cr.out | head -8; exit 1; fi; \
	sig="$$sig and RUNS `tr '\n' '/' < tests/seal/consumer_ram.expected | sed 's|/$$||'`;"; \
	for lim in accept_limit_forgotten accept_limit_blanket accept_limit_race accept_limit_latency ; do \
	  rm -f $(C_OUT)/seal_$$lim.c $(C_OUT)/seal_$$lim.o; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/seal_$$lim.c tests/seal/$$lim.exl >/dev/null 2>&1 \
	    || { echo "selfhost-seal: LIMIT CONTRACT BROKEN — tests/seal/$$lim.exl was REJECTED; a limit is a boundary the language states, not one it enforces"; exit 1; }; \
	  cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/seal_$$lim.c -o $(C_OUT)/seal_$$lim.o \
	    || { echo "selfhost-seal: limit contract $$lim emits C that is not clean at -O2"; exit 1; }; \
	done; \
	rm -f $(C_OUT)/seal_inc.c $(C_OUT)/seal_inc.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_inc.c tests/seal/accept_seal_builtin_include.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: the sealed-builtin fixture was REJECTED"; exit 1; }; \
	grep -q '#include <string.h>' $(C_OUT)/seal_inc.c \
	  || { echo "selfhost-seal: a builtin used only INSIDE a region lost its libc include - the emitted C will not compile"; exit 1; }; \
	cc -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/seal_inc.c -o $(C_OUT)/seal_inc.o \
	  || { echo "selfhost-seal: the sealed-builtin C is not clean at the standard every fixture meets"; exit 1; }; \
	sig="$$sig a builtin used only inside a region keeps its include;"; \
	test ! -e tests/seal/accept_limit_wrong_region.exl \
	  || { echo "selfhost-seal: the fifth limit is back in the corpus as an ACCEPT fixture - it was closed, and one place must answer whether it is open"; exit 1; }; \
	sig="$$sig four limits pinned as CONTRACTS, the fifth CLOSED and its shape now rejected in the ward corpus;"; \
	rm -f $(C_OUT)/seal_ret.c $(HOST_OUT)/seal_ret $(C_OUT)/seal_ret.out; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/seal_ret.c tests/seal/accept_seal_returns.exl >/dev/null 2>&1 \
	  || { echo "selfhost-seal: a seal whose body RETURNS was rejected — the region runs inline and unconditionally, so it answers for the enclosing fn's exhaustive-return check (port-only, the oracle cannot parse seal at all)"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/seal_ret $(C_OUT)/seal_ret.c $(SYS_HOST) \
	  || { echo "selfhost-seal: accept_seal_returns C is not clean at -O2"; exit 1; }; \
	$(HOST_OUT)/seal_ret > $(C_OUT)/seal_ret.out 2>&1; \
	if ! diff -q tests/seal/accept_seal_returns.expected $(C_OUT)/seal_ret.out >/dev/null; then \
	  echo "selfhost-seal: a returning seal region ran to the wrong value:"; \
	  diff tests/seal/accept_seal_returns.expected $(C_OUT)/seal_ret.out | head -6; exit 1; fi; \
	sig="$$sig a seal that RETURNS answers for its function (bare and inside an if arm), compiled and RUN;"; \
	for m in arm_return_after_seal arm_sibling_return accept_arm_seal arm_nested_returns \
	         try_propagation ; do \
	  rm -f $(C_OUT)/seal_$$m.c $(HOST_OUT)/seal_$$m $(C_OUT)/seal_$$m.out; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/seal_$$m.c tests/seal/$$m.exl >/dev/null 2>&1 \
	    || { echo "selfhost-seal: port rejected tests/seal/$$m.exl (a region in a match arm)"; exit 1; }; \
	  cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/seal_$$m $(C_OUT)/seal_$$m.c $(SYS_HOST) \
	    || { echo "selfhost-seal: $$m C is not clean at -O2 (a spent or unassigned token reaches the seam)"; exit 1; }; \
	  $(HOST_OUT)/seal_$$m > $(C_OUT)/seal_$$m.out 2>&1; \
	  if ! diff -q tests/seal/$$m.expected $(C_OUT)/seal_$$m.out >/dev/null; then \
	    echo "selfhost-seal: $$m RAN unbalanced (the region's exit outlived the region):"; \
	    diff tests/seal/$$m.expected $(C_OUT)/seal_$$m.out | head -8; exit 1; fi; \
	done; \
	sig="$$sig a region in a diverging match arm does not leak its exit into the arm's return or into a sibling arm that never sealed, and try-propagation is gated rather than inherited;"; \
	for k in defer_in_seal seal_in_defer cross_nest ; do \
	  test -s tests/seal/$$k.golden || { echo "selfhost-seal: MISSING/EMPTY tests/seal/$$k.golden"; exit 1; }; \
	  rm -f $(C_OUT)/seal_$$k.c $(HOST_OUT)/seal_$$k $(C_OUT)/seal_$$k.out $(C_OUT)/seal_$$k.seq; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/seal_$$k.c tests/seal/$$k.exl >/dev/null 2>&1 \
	    || { echo "selfhost-seal: port rejected tests/seal/$$k.exl (defer x seal composition)"; exit 1; }; \
	  if [ ! -s $(C_OUT)/seal_$$k.c ]; then echo "selfhost-seal: EMPTY C for $$k (floor)"; exit 1; fi; \
	  sed -n '/sys_seal_enter();/,/^}/p' $(C_OUT)/seal_$$k.c | sed 's/^[ \t]*//' \
	    | grep -E 'sys_seal|printf' > $(C_OUT)/seal_$$k.seq; \
	  if ! diff -q tests/seal/$$k.golden $(C_OUT)/seal_$$k.seq >/dev/null; then \
	    echo "selfhost-seal: $$k ORDER changed (a defer body crossed the seam call, or a region moved):"; \
	    diff tests/seal/$$k.golden $(C_OUT)/seal_$$k.seq | head -10; exit 1; fi; \
	  cc -O2 -ansi -pedantic -Wall -Werror -I src -o $(HOST_OUT)/seal_$$k $(C_OUT)/seal_$$k.c $(SYS_HOST) \
	    || { echo "selfhost-seal: $$k C is not clean C89 at -O2"; exit 1; }; \
	  $(HOST_OUT)/seal_$$k > $(C_OUT)/seal_$$k.out 2>&1; \
	  if ! diff -q tests/seal/$$k.expected $(C_OUT)/seal_$$k.out >/dev/null; then \
	    echo "selfhost-seal: $$k RAN in the wrong order (or left a region unbalanced):"; \
	    diff tests/seal/$$k.expected $(C_OUT)/seal_$$k.out | head -8; exit 1; fi; \
	done; \
	sig="$$sig defer x seal both ways and the seal->defer->seal crossing hold their ORDER in artifact and in execution;"; \
	rm -f $(C_OUT)/seal_theft.err; \
	$(EXILC_BIN) --target c --c-out /dev/null tests/seal/reject_sealed_theft.exl > $(C_OUT)/seal_theft.err 2>&1; \
	if [ $$? -eq 0 ]; then \
	  echo "selfhost-seal: a NON-OWNER sealed its way to a covered address (seal shadowed the sigil claim)"; exit 1; fi; \
	grep -q "belongs to resource 'Blitter', claimed by 'gfx'" $(C_OUT)/seal_theft.err \
	  || { echo "selfhost-seal: the sealed theft was rejected, but not by S2:"; head -2 $(C_OUT)/seal_theft.err; exit 1; }; \
	sig="$$sig a non-owner cannot seal its way past S2;"; \
	for q in exits nested amiga_callpath accept_seam_namesake blitter_setup consumer_ram \
	         defer_in_seal seal_in_defer cross_nest \
	         arm_return_after_seal arm_sibling_return accept_arm_seal arm_nested_returns \
	         accept_limit_forgotten accept_limit_blanket accept_limit_race accept_limit_latency \
	         try_propagation ; do \
	  rm -f $(C_OUT)/seal_q.msg; \
	  $(EXILC_BIN) --target c --c-out /dev/null tests/seal/$$q.exl 2>&1 | grep -v '^wrote ' > $(C_OUT)/seal_q.msg; \
	  if [ -s $(C_OUT)/seal_q.msg ]; then \
	    echo "selfhost-seal: tests/seal/$$q.exl is not DIAGNOSTIC-FREE (a seal must not make the linter blind):"; \
	    head -3 $(C_OUT)/seal_q.msg; exit 1; fi; \
	done; \
	sig="$$sig every accepting fixture DIAGNOSTIC-FREE."; \
	echo "selfhost-seal: clean —$$sig"

selfhost-sigil: $(EXILC_BIN)
	@rm -f $(C_OUT)/sig_ok.c $(C_OUT)/sig_ok.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_ok.c tests/sigil/accept_owner.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: port rejected the owner's own materialisation"; exit 1; }; \
	if [ ! -s $(C_OUT)/sig_ok.c ]; then echo "selfhost-sigil: EMPTY emitted C (floor)"; exit 1; fi; \
	if grep -qE 'Blitter|Sprite0' $(C_OUT)/sig_ok.c; then \
	  echo "selfhost-sigil: a sigil/claim leaked into the emitted C (zero-cost violated)"; exit 1; fi; \
	grep -q '\*bltsize = 64;' $(C_OUT)/sig_ok.c \
	  || { echo "selfhost-sigil: the owner's covered access did not emit (gate must not break the owner)"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/sig_ok.c -o $(C_OUT)/sig_ok.o \
	  || { echo "selfhost-sigil: emitted C is not clean C89 at -O2"; exit 1; }; \
	rm -f $(C_OUT)/sig_ab.c; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_ab.c tests/sigil/accept_boundary.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: ACCEPT-probe — port REJECTED a boundary-adjacent materialisation (ranges are half-open)"; exit 1; }; \
	rm -f $(C_OUT)/sig_ow.c $(C_OUT)/sig_ow.o; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_ow.c tests/sigil/accept_owner_ward.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: the OWNER's ward-field access was rejected (the gate must not break the owner)"; exit 1; }; \
	grep -q '(14675968UL + 88UL)) = 64;' $(C_OUT)/sig_ow.c \
	  || { echo "selfhost-sigil: the owner's ward-field access did not emit its volatile store"; exit 1; }; \
	cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/sig_ow.c -o $(C_OUT)/sig_ow.o \
	  || { echo "selfhost-sigil: owner ward-field C is not clean C89 at -O2"; exit 1; }; \
	for ok in accept_ndk_shape accept_delegation accept_ndk_ward accept_owner_descendant \
	          accept_limit_rawcast accept_limit_standing_delegation accept_limit_redelegation \
	          accept_limit_mint_narrow accept_limit_intra_owner ; do \
	  rm -f $(C_OUT)/sig_$$ok.c $(C_OUT)/sig_$$ok.o; \
	  $(EXILC_BIN) --target c --c-out $(C_OUT)/sig_$$ok.c tests/sigil/$$ok.exl >/dev/null 2>&1 \
	    || { echo "selfhost-sigil: ACCEPT — port REJECTED tests/sigil/$$ok.exl"; exit 1; }; \
	  if grep -qE 'Blitter|Copper|Audio0' $(C_OUT)/sig_$$ok.c; then \
	    echo "selfhost-sigil: a sigil/claim leaked into the C of $$ok"; exit 1; fi; \
	  cc -O2 -ansi -pedantic -Wall -Werror -I src -c $(C_OUT)/sig_$$ok.c -o $(C_OUT)/sig_$$ok.o \
	    || { echo "selfhost-sigil: $$ok C is not clean C89 at -O2"; exit 1; }; \
	done; \
	grep -q 'volatile unsigned short \*gfx__lend_size(void)' $(C_OUT)/sig_accept_delegation.c \
	  || { echo "selfhost-sigil: the owner does not hand the handle out (delegation must cross the boundary as volatile T*)"; exit 1; }; \
	grep -q '\*r = 64;' $(C_OUT)/sig_accept_delegation.c \
	  || { echo "selfhost-sigil: the non-owner does not write THROUGH the borrowed handle (that is what delegation is)"; exit 1; }; \
	grep -q 'fx__burst(bltcon0);' $(C_OUT)/sig_accept_delegation.c \
	  || { echo "selfhost-sigil: attenuation at the call site did not emit (readwrite lent as write)"; exit 1; }; \
	grep -q '(14675968UL + 88UL)) = 64;' $(C_OUT)/sig_accept_ndk_ward.c \
	  || { echo "selfhost-sigil: the NDK ward field access did not emit at base+offset"; exit 1; }; \
	rm -f $(C_OUT)/sig_sa.c; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_sa.c tests/sigil/accept_sigil_adjacent.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: ACCEPT-probe — port REJECTED two TOUCHING resources (S3 ranges are half-open on both sides)"; exit 1; }; \
	rm -f $(C_OUT)/sig_fa.c; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_fa.c tests/sigil/accept_file_adjacent.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: ACCEPT-probe — port REJECTED a register file ending exactly at the resource's lower bound (half-open on the SPAN side)"; exit 1; }; \
	for row in \
	  "S2|reject_non_owner|address 0xDFF058 belongs to resource 'Blitter', claimed by 'gfx'" \
	  "S2-span|reject_span_below|address 0xDFF03E belongs to resource 'Blitter', claimed by 'gfx'" \
	  "claim|reject_unknown_claim|unknown resource 'Blittter'" \
	  "S2-ward|reject_ward_field|address 0xDFF058 belongs to resource 'Blitter', claimed by 'gfx'" \
	  "S2-wardfile|reject_ward_file_span|address 0xDFF038 belongs to resource 'Blitter', claimed by 'gfx'" \
	  "S2-file|reject_file_below|address 0xDFF038 belongs to resource 'Blitter', claimed by 'gfx'" \
	  "S1|reject_double_claim|resource 'Blitter' is already claimed by 'gfx'; 'sound' cannot claim it too" \
	  "S3|reject_sigil_overlap|resources 'Sprite0' [0xDFF140, 0xDFF148) and 'Other' [0xDFF144, 0xDFF150) overlap" \
	  "S5-empty|reject_empty_range|sigil 'Bad' has an EMPTY range [0xDFF080, 0xDFF080)" \
	  "S5-inv|reject_inverted_range|sigil 'Bad' has an INVERTED range [0xDFF05A, 0xDFF040)" \
	  "A-toplevel|reject_toplevel_rune|address 0xDFF058 belongs to resource 'Blitter', claimed by 'gfx'" \
	  "A-ndkward|reject_ndk_ward_outside|address 0xDFF058 belongs to resource 'Blitter', claimed by 'gfx'" ; do \
	  id=`echo "$$row" | cut -d'|' -f1`; fx=`echo "$$row" | cut -d'|' -f2`; msg=`echo "$$row" | cut -d'|' -f3`; \
	  rm -f $(C_OUT)/srow.c $(C_OUT)/srow.err; \
	  if $(EXILC_BIN) --target c --c-out $(C_OUT)/srow.c tests/sigil/$$fx.exl >/dev/null 2>$(C_OUT)/srow.err; then \
	    echo "selfhost-sigil: $$id — port ACCEPTED tests/sigil/$$fx.exl"; exit 1; fi; \
	  if [ ! -s $(C_OUT)/srow.err ]; then echo "selfhost-sigil: $$id empty diagnostic (floor)"; exit 1; fi; \
	  if grep -q 'internal:' $(C_OUT)/srow.err; then echo "selfhost-sigil: $$id is ICE-enforced, not a clean diagnostic"; exit 1; fi; \
	  grep -qF "$$msg" $(C_OUT)/srow.err \
	    || { echo "selfhost-sigil: $$id wrong message: `head -1 $(C_OUT)/srow.err`"; exit 1; }; \
	done; \
	rm -f $(C_OUT)/sig_eq_g.c $(C_OUT)/sig_eq_u.c; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_eq_g.c tests/sigil/equality/gated.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: equality witness — the GATED half did not compile"; exit 1; }; \
	$(EXILC_BIN) --target c --c-out $(C_OUT)/sig_eq_u.c tests/sigil/equality/ungated.exl >/dev/null 2>&1 \
	  || { echo "selfhost-sigil: equality witness — the UNGATED half did not compile"; exit 1; }; \
	if [ ! -s $(C_OUT)/sig_eq_g.c ]; then echo "selfhost-sigil: equality witness EMPTY C (floor)"; exit 1; fi; \
	grep -q '\*bltsize = 64;' $(C_OUT)/sig_eq_g.c \
	  || { echo "selfhost-sigil: equality witness does not exercise a COVERED access"; exit 1; }; \
	if ! cmp -s $(C_OUT)/sig_eq_g.c $(C_OUT)/sig_eq_u.c; then \
	  echo "selfhost-sigil: a claim changed the emitted C:"; diff $(C_OUT)/sig_eq_g.c $(C_OUT)/sig_eq_u.c | head -6; exit 1; fi; \
	echo "selfhost-sigil: clean (owner materialises + uses, bare rune AND ward field; rejection table S1/S2 x5/S3/S5 x2/owner-gated x2 + unknown-claim; ACCEPT x12 (4 boundary/owner probes + NDK shape, delegation+attenuation, NDK ward, descendant module + the five limits) + artifact equality (gated == ungated, byte for byte); zero emission for sigil/claim; cc -Wall -Werror)"

# ===== The escape pass — the port's differential gate =====
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
                  c22_capture_untyped_let c23_marker_bound_satisfied \
                  arm_generic_payload prelude_name_string \
                  prelude_name_collision prelude_name_seed_order \
                  enum_match_no_collision shadow_kind_silent \
                  trait_default_sig_assoc trait_default_body_assoc \
                  trait_default_assoc_in_app trait_default_assoc_ptr \
                  trait_default_assoc_app_called trait_default_assoc_app_uncalled \
                  trait_default_assoc_enum_called trait_default_assoc_enum_uncalled \
                  trait_default_generic_impl trait_default_generic_impl_assoc \
                  trait_default_concrete_impl \
                  prelude_intact_on_unused_name \
                  instance_field_pulls_allocator \
                  type_alias_generic type_alias_of_alias type_alias_plain \
                  nested_instance_order \
                  field_chain_pulls_allocator \
                  field_instance_names_nested \
                  prelude_named_field_dep prelude_named_and_built \
                  unused_enum_stays_dropped

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
		if [ ! -f $(SELFHOST_AST)/$$name.ast ]; then \
			echo "selfhost-port-ast: MISSING $(SELFHOST_AST)/$$name.ast"; \
			echo "  This used to skip. Run 'make selfhost-corpus-$$name'."; \
			fail=1; continue; \
		fi; \
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
# The drop pass's DIAGNOSTIC half, which the port had none of: fourteen
# `Error.failf` sites in the reference against zero here. These fixtures compare
# the first line through the drop-running drivers on both sides - `tc-errors`
# cannot host them, because its port driver stops before drop and would compare
# the oracle's rejection against the port's silence.
selfhost-port-drop-errors: host-selfhost-drop
	@fail=0; n=0; \
	for f in src/drop_errors/*.exl; do \
		n=$$((n+1)); \
		oc=$$($(EXILE) --emit-typed-ir --after-drop --user-only $$f 2>&1 >/dev/null | head -1); \
		pt=$$(echo $$f | $(HOST_OUT)/selfhost_drop 2>&1 >/dev/null | head -1); \
		if [ "$$oc" = "$$pt" ] && [ -n "$$pt" ]; then : ; else \
			echo "selfhost-port-drop-errors: MISMATCH $$(basename $$f)"; \
			echo "  oracle: $$oc"; echo "  port:   $$pt"; fail=1; fi; \
	done; \
	if [ $$n -lt 4 ]; then \
	  echo "selfhost-port-drop-errors: only $$n fixtures — the corpus is missing files."; exit 1; fi; \
	an=0; \
	for f in src/drop_accepts/*.exl; do \
		an=$$((an+1)); \
		$(EXILE) --emit-typed-ir --after-drop --user-only $$f >/dev/null 2>$(C_OUT)/dacc_o.err; ro=$$?; \
		echo $$f | $(HOST_OUT)/selfhost_drop >/dev/null 2>$(C_OUT)/dacc_p.err; rp=$$?; \
		if [ $$ro -ne 0 ]; then \
			echo "selfhost-port-drop-errors: the REFERENCE rejects $$(basename $$f) — an accept fixture must be accepted by both"; \
			head -1 $(C_OUT)/dacc_o.err; fail=1; \
		elif [ -s $(C_OUT)/dacc_p.err ]; then \
			echo "selfhost-port-drop-errors: NARROWING — the port rejects $$(basename $$f), which the reference accepts"; \
			head -1 $(C_OUT)/dacc_p.err; fail=1; \
		fi; \
	done; \
	if [ $$an -lt 4 ]; then \
	  echo "selfhost-port-drop-errors: only $$an accept fixtures — the rule's non-narrowing side is unmeasured."; exit 1; fi; \
	if [ $$fail -eq 0 ]; then \
	  echo "selfhost-port-drop-errors: clean ($$n rejections + $$an accepts, port == oracle line 1; the accepts pin that the rule does not narrow the language)"; \
	else exit 1; fi

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
