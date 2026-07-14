Fixtures for the typecheck-diagnostics gate (`make selfhost-port-tc-errors`).

Each file is a program the reference implementation REJECTS.  The gate runs both
compilers and byte-compares the first diagnostic line, position included — the same
shape as `src/lex_errors` (12) and `src/parse_errors` (46).

The corpus grows one ERROR FAMILY at a time, mirroring how the typecheck port itself
grew one construct at a time.  Family 1 — name resolution:

  undefined_variable            undefined variable 'x'
  no_such_field                 struct 'P' has no field 'y'
  no_such_field_through_ptr     ... reached through a `*const P`
  unknown_function              unknown function 'f'
  unknown_qualified_function    unknown function 'm::g'

Why this family first: it is the only one measured to FABRICATE a value.  Before
this, `p.y` on a struct without `y` elaborated to a NULL placeholder, and the port
emitted C that COMPILED and printed garbage.  The other families accept invalid
programs; this one produced invalid code.

Family 2 — mutability:

  assign_to_immutable           cannot assign to immutable 'x'
  assign_to_immutable_param     ... (or mark the parameter `mut`)
  mutate_field_of_immutable     cannot mutate field of immutable 'p'
  assign_into_immutable_array   cannot assign into immutable 'arr'
  assign_to_renamed_sibling     names `v`, not the minted `v__1`
  assign_to_foreach_element     names `x`, not the minted `__fv1`

The last two exist because this is the first family whose messages NAME A BINDING,
and the compiler mints names the user never wrote: disjoint siblings (`v` -> `v__1`),
the `for` counter (`i` -> `__fv1`), a scoped projection (`x` -> `x__with0`).  The
scope carries a map back to the source name, and these fixtures pin it — an error
naming a binding the user did not write is a bug in the compiler, not a hint.

Two rules this family had to mirror exactly, both discovered by measuring rather
than by reading:
  * Mutation THROUGH A POINTER is ungated (`p.x = 9` where `p: *P`) — it is pointee
    mutation, governed by the pointer's constness, not the binding's.  Without this
    the prelude itself fails to elaborate: `Vec::push(self: *Vec<T>)` writes
    `self.ptr[i]`, and `self` is not a `mut` param.
  * The counter of `for i in 0..n` IS mutable; the element of `for x in <iterator>`
    is NOT.  Two desugars, two answers.

Family 4 — the loan rule (DR-047/DR-052):

  loan_rvalue_let               `let q: *P = new(al) P { ... }`
  loan_rvalue_call_arg          a call argument
  loan_rvalue_method_receiver   a method receiver
  loan_rvalue_struct_field      a struct-literal field
  loan_rvalue_variant_arg       an enum-variant payload
  loan_rvalue_assign_field      `h.r = new(al) ...`
  loan_rvalue_assign_deref      `*ps = new(al) ...`

An `own *T` flowing into a borrow slot (`*T` / `*const T`) is a LOAN: the value is
restamped to the slot's type so the move pass keeps the OWNER live for its own
free/drop.  Only a PLACE can lend — the owner keeps living somewhere else, so the
loan has something to outlive.  A fresh owned RVALUE (`new(a) ...`, an own-returning
call) in a borrow slot leaves the allocation with no owner at all.

One fixture per slot kind, because the rule is one choke-point in the reference and
the port had it at THREE of the ten slots — with no rvalue rejection at any of them.
The failure mode was not a consistent leak: measured on the port before this,
`let q: *P = new(...)` came out ASan-clean (the drop pass rescued it by provenance),
adding an explicit `free` produced a DOUBLE FREE, and passing the rvalue as a call
argument LEAKED.  Unpredictable is worse than wrong.

Family 5 — the `null` literal:

  null_needs_annotation         `let n = null;` — nothing to infer a pointee from
  deref_null                    `*null`

`null` has a type of its own (TNullPtr) that is polymorphic over pointers and has no
C spelling.  Three separate relations shipped without a case for it — typ_eq (found
by the let-annotation check), is_ptr_ty (found by the loan rule), and the inference
path here — each invisible until something finally asked.  They now all ask through
`ir::is_nullish` / `ir::is_ptr_like` rather than matching TNullPtr inline, which is
what makes a fourth instance structurally harder rather than merely unlikely.

## Family 6+7 — calls: method, arity, argument type, return type

`no_such_method` was a FABRICATION, not a silent accept: `p.nie_ma()` fell into the
`defer_expr` placeholder and codegen emitted `printf("%d\n", ((void *)0))` — C that
compiles and prints garbage.  The reference funnels every call through one checker;
the port had none, so these went in as one choke-point (`check_call`).

The return rule is EQUALITY (`typ_eq`), not coercion.  A `*T` may be LENT into a
`*const T` slot, but a fn declaring `*T` must return a `*T`.  The bidirectional
`expected` seeds inner inference; it does not relax the contract.

## Family 8 — unknown type

The type-name resolver had no failure mode at all.  Every unresolved name became a
struct out of thin air, so `let x: Nieznany = 1;` typechecked and codegen emitted
`struct ex_Nieznany x;` — C that does not compile.

This is the base of the type system, not a leaf: every other check reads the types
this relation produces, and the relation could not say "no".  Adding the failure
mode immediately convicted three defects in the compiler's OWN signature tables,
each of which had been resolving annotations in the wrong scope and silently
inventing a second type: parameter types without the module path, parameter types
without the fn's type parameters, and a generic method's return type without its
own type parameters.  None was reachable by any gate before, because the wrong
type was DEAD — nothing ever compared it.  They are pinned now: revert any of the
three and `examples/generic_methods.exl` fails with `unknown type 'T'`.

Positions match the reference by construction: it reports the ENCLOSING construct
(the fn, the struct, the let), never the annotation, so the collectors thread the
item's own pos.  The port's prelude types are BUILT-IN where the reference's are
prelude SOURCE — so its struct table holds them and ours does not.  `is_builtin_struct`
names them; without it the check would reject every `StringBuilder`.
