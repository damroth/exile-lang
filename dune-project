(lang dune 3.22)

(name exile-lang)

(generate_opam_files true)

(source
 (github damroth/exile-lang))

(authors "Damroth <damroth@df0.cc>")

(maintainers "Damroth <damroth@df0.cc>")

(license LICENSE)

(package
 (name exile-lang)
 (synopsis "A toy language that compiles to C, targeting Amiga m68k")
 (description
  "exile-lang is a work-in-progress programming language with a transpiler to C89, \
   designed to run on Amiga 500 (Kickstart 1.3). The transpiler is written in OCaml.")
 (depends ocaml dune)
 (tags
  (compiler language amiga m68k ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
