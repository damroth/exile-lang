type int_width = W8 | W16 | W32

(* DR-floats: two IEEE float widths.  No arbitrary other widths —
   f32 is single, f64 is double, period.  Bare `3.14` defaults to f64. *)
type float_width = F32 | F64

type binop =
  | Add | Sub | Mul | Div | Mod
  | BitAnd | BitOr | BitXor | Shl | Shr  (* bitwise / shift; integer operands.
                                            Codegen emits the C operator with
                                            explicit parens so C's (looser)
                                            bitwise precedence never leaks. *)
  | And | Or                            (* short-circuiting logical operators
                                            on `bool` — same semantics as C
                                            `&&` / `||`. *)
  | Lt | Gt | LtEq | GtEq | EqEq | NotEq
  | Concat                              (* `++` — compile-time string concat;
                                           both operands must reduce to a
                                           string literal at typecheck time. *)

let binop_name = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | BitAnd -> "&" | BitOr -> "|" | BitXor -> "^" | Shl -> "<<" | Shr -> ">>"
  | And -> "&&" | Or -> "||"
  | Lt -> "<" | Gt -> ">" | LtEq -> "<=" | GtEq -> ">="
  | EqEq -> "==" | NotEq -> "!="
  | Concat -> "++"

type type_ann =
  | TyInt of { signed : bool; width : int_width }
  | TyCInt of { signed : bool }        (* c_int / c_uint — native C int *)
  | TyCShort of { signed : bool }      (* c_short / c_ushort *)
  | TyCLong of { signed : bool }       (* c_long / c_ulong *)
  | TyCChar                            (* c_char — implementation-defined *)
  | TyCSChar                           (* c_schar — explicitly signed *)
  | TyCUChar                           (* c_uchar *)
  | TyCVoid                            (* c_void — only legal under TyPtr *)
  | TyStr
  | TyBool
  | TyFloat of float_width             (* `f32` / `f64`.  Operators are
                                          built-in IEEE; `Eq` / `Ord` / `Hash`
                                          traits are deliberately NOT
                                          implemented (NaN ≠ NaN, no total
                                          equality / ordering — distinctively
                                          exile, not Rust's PartialEq/
                                          PartialOrd split). *)
  | TyTuple of type_ann list
  | TyStruct of { path : string list; args : type_ann list }
                                       (* qualified path + optional generic
                                          arguments.  `Foo` is `args=[]`,
                                          `Option<int>` is `path=["Option"]`,
                                          `args=[TyInt{signed=true;width=W32}]`.
                                          Generic param references inside a
                                          decl body (e.g. `T` in `Some(T)`)
                                          appear as `TyStruct { path=["T"];
                                          args=[] }` until typecheck binds
                                          them as type variables. *)
  | TyPtr of type_ann                  (* `*T` — mutable pointee *)
  | TyOwnPtr of type_ann               (* `own *T` — DR-030 Faza-1a Owner-
                                          sigil third-pointer type.  Marks
                                          unique ownership of the pointee
                                          (Allocator.alloc origin, free
                                          / move consumes, scope-exit on
                                          a Live owner injects drop).
                                          Codegen erases the sigil: emits
                                          `T *` exactly like TyPtr.  The
                                          OWN-D1 coercion (ir.ml's
                                          coercible_to) lets `own *T`
                                          decay to `*T`/`*const T` for
                                          borrowing, never the reverse —
                                          you can lend ownership, never
                                          fabricate it. *)
  | TyConstPtr of type_ann             (* `*const T` — pointee is read-only
                                          (maps to C `const T *`).  Writes
                                          through such a pointer (`*p = v`)
                                          and casts back to `*T` are
                                          rejected.  Coerces *in* from `*T`
                                          at assignment and call sites. *)
  | TyArray of { elem : type_ann; size : expr }
                                       (* `[T; N]` — fixed-size array.  N is
                                          a constant expression (literal or
                                          `const` reference), evaluated to a
                                          concrete size at typecheck. *)
  | TySelf                             (* placeholder for a bare `self` /
                                          `*self` method receiver — the
                                          parser substitutes the enclosing
                                          impl's target type before the AST
                                          leaves parse_impl_block, so it
                                          never reaches typecheck except as
                                          a misuse outside an impl. *)
  | TyFnPtr of { params : type_ann list; ret : type_ann option }
                                       (* `fn(T1, T2) -> R` as a type.
                                          C-side maps to a function
                                          pointer.  No variadic in this
                                          form yet. *)

and expr =
  | IntLit of int * Pos.t
  | FloatLit of float * float_width * Pos.t
  | BoolLit of bool * Pos.t
  | StringLit of string * Pos.t
  | Var of string * Pos.t
  | Neg of expr * Pos.t
  | BitNot of expr * Pos.t              (* `~e` — bitwise complement; integer
                                           operand.  Prefix unary alongside
                                           `-` / `&` / `*`. *)
  | Not of expr * Pos.t                 (* `!e` — logical negation; bool
                                           operand, bool result.  Maps to
                                           C `!`. *)
  | BinOp of binop * expr * expr * Pos.t
  | Call of { callee : string list; args : expr list; pos : Pos.t }
  | Cast of expr * type_ann * Pos.t
  | TupleLit of expr list * Pos.t
  | StructLit of { tname : string list; fields : (string * expr) list;
                   base : expr option; pos : Pos.t }
  | FieldAccess of expr * string * Pos.t
  | Ref of expr * Pos.t                (* `&expr` — take address *)
  | Deref of expr * Pos.t              (* `*expr` — load through pointer *)
  | NullLit of Pos.t                   (* `null` — typeless pointer literal *)
  | New of { tname : string list; fields : (string * expr) list;
             base : expr option; pos : Pos.t }
                                        (* `new T { f: e }` — heap-alloc + init *)
  | NewEnum of { tname : string list; args : expr list; pos : Pos.t }
                                        (* DR-031 `new Path::Variant(args)` —
                                           heap-allocate the enum value and
                                           return a `*Enum`.  Tuple-variant
                                           form only in v1; struct-variant
                                           heap-boxing defers.  Faithful
                                           OCaml→Exile enum-AST port wants
                                           this for recursive enums with
                                           pointer-typed payloads. *)
  | MethodCall of { receiver : expr; name : string;
                    args : expr list; pos : Pos.t }
                                        (* `recv.name(args)` — dot-form method
                                           call.  Elaboration resolves the
                                           method on receiver's struct and
                                           lowers it to an ordinary call. *)
  | EnumLit of { tname : string list; variant : string;
                 args : enum_lit_args; pos : Pos.t }
                                        (* `Foo::Variant` (unit), `Foo::V(e1)`
                                           (tuple), `Foo::V { f: e }` (struct).
                                           Parser emits EATuple for unit and
                                           tuple forms; struct-variant ctor
                                           parses as StructLit and is rewritten
                                           to EnumLit-EAStruct in elab. *)
  | Match of { scrutinee : expr; arms : match_arm list; pos : Pos.t }
                                        (* `match e { | pat => expr | ... }` —
                                           always an expression: usable as
                                           a let-RHS or return value, or
                                           as a stmt (its value is then
                                           dropped). *)
  | Orelse of expr * expr * Pos.t       (* `value orelse default` — yields
                                           the unwrapped payload of `value`
                                           (Result<T, _> or Option<T>) on
                                           Ok/Some, [default] on Err/None.
                                           Lowered to `match` in elab. *)
  | Try of expr * Pos.t                 (* `try value` — unwraps Ok/Some,
                                           early-returns Err/None from the
                                           enclosing fn.  Inner enum and
                                           outer ret type must agree
                                           (both Result with same E, or
                                           both Option). *)
  | SizeOf of type_ann * Pos.t          (* `size_of(T)` — yields the C
                                           `sizeof(T)` byte count as a
                                           c_uint.  Argument is a type
                                           annotation, not an expression;
                                           parser dispatches on the
                                           SizeOf keyword.  Resolves to
                                           a constant after monomorphization
                                           even when T is a tparam. *)
  | ArrayLit of expr list * Pos.t       (* `[e1, e2, e3]` — explicit array
                                           literal.  Element type and size
                                           inferred from the elements; empty
                                           `[]` is rejected (no inference). *)
  | ArrayRepeat of { value : expr; count : expr; pos : Pos.t }
                                        (* `[v; N]` — array of N copies of v.
                                           N is a constant expression. *)
  | Index of { base : expr; index : expr; pos : Pos.t }
                                        (* `a[i]` — element access (lvalue or
                                           rvalue).  No bounds check. *)
  | Lambda of { params : (string * type_ann) list;
                ret_ty : type_ann option;
                body : expr;
                captures : (string * bool) list;
                pos : Pos.t }
                                        (* `|p: T, q: U| body` — DR-008
                                           captureless decay (A1).  A
                                           pre-typecheck pass lifts each
                                           lambda to a top-level fn
                                           `__lambda_N` and replaces the
                                           expression with a
                                           `Var "__lambda_N"` that the
                                           ordinary lookup turns into a
                                           `TFnRef` (auto-decays to a C
                                           fn-pointer at the use site).
                                           Captureless is enforced by
                                           construction: the lifted body
                                           lives at the top level so any
                                           reference to an enclosing local
                                           is an "undefined variable"
                                           error.  v1: param types
                                           required, return type optional
                                           (inferred from body type). *)
  | Range of { lo : expr; hi : expr; inclusive : bool; pos : Pos.t }
                                        (* `a..b` (exclusive) / `a..=b`
                                           (inclusive) as an expression
                                           value.  Desugars to a literal of
                                           the prelude struct `Range<T>` or
                                           `RangeInclusive<T>` during elab;
                                           a `for v in <Range literal>` head
                                           still takes the direct fast path
                                           in walk_stmt (no struct alloc). *)
  | Block of stmt list * Pos.t          (* `{ stmts; trailing_expr }` — a
                                           statement block that produces a
                                           value (the trailing `Tail expr`).
                                           Today emitted only by
                                           `parse_arm_body` when a match
                                           arm body starts with `{`; other
                                           positions don't allow `{` to
                                           start an expression. *)
  | If of { cond : expr; then_blk : stmt list;
            else_blk : stmt list option; pos : Pos.t }
                                        (* `if c { ... } else { ... }`.  One
                                           node serves both roles: a void
                                           *statement* (guard clause / side
                                           effects, `else` optional) and an
                                           *expression* (value, `else`
                                           required, both branches one
                                           trailing expression of the same
                                           type).  Which role applies is
                                           decided by position during elab:
                                           value position requires `else` and
                                           single-expression branches; bare-
                                           block-expression branches are
                                           deferred (see WORKLOG). *)

and match_arm = { pat : pattern;
                   guard : expr option;   (* `pat if cond => body` —
                                             optional boolean guard
                                             evaluated *after* the pattern
                                             matches.  Guards don't
                                             contribute to exhaustiveness:
                                             a guarded arm doesn't prove
                                             coverage. *)
                   body : expr; arm_pos : Pos.t }

and enum_lit_args =
  | EATuple of expr list                (* unit ctor uses EATuple [] *)
  | EAStruct of (string * expr) list

and pattern =
  | PWildcard of Pos.t
  | PVar of string * Pos.t              (* binds the scrutinee to the name *)
  | PVariant of { tname : string list; variant : string;
                  binds : pat_binds; pos : Pos.t }
                                        (* tuple form: `Foo::V(p1, p2)`,
                                           struct form: `Foo::V { f: p }` or
                                           shorthand `Foo::V { f }`.  Unit
                                           variants use `PBTuple []`. *)
  | POr of pattern list * Pos.t         (* `pat1 | pat2 | ...` — pattern
                                           union; matches if any
                                           alternative matches.  MVP
                                           requires zero binds in every
                                           alternative (no variable can
                                           be referenced in the arm body
                                           that's only bound by some
                                           alternatives). *)

and pat_binds =
  | PBTuple of pattern list
  | PBStruct of (string * pattern) list  (* may include shorthand binds:
                                            shorthand `f` desugars at parse
                                            time to `("f", PVar "f")`. *)

and stmt =
  | Let of { name : string; value : expr; ty_ann : type_ann option;
             is_mut : bool; pos : Pos.t }
                                          (* `let x` is immutable; `let mut x`
                                             is reassignable.  Mutability is a
                                             compile-time-only property —
                                             codegen never emits `const`. *)
  | LetTuple of { names : string list; value : expr; is_mut : bool; pos : Pos.t }
  | LetElse of { pat : pattern; value : expr; else_body : stmt list;
                 pos : Pos.t }
                                          (* `let <refutable-pat> = expr
                                             else { divergent-block };`
                                             FP-2: bind-hoisting desugar.
                                             Pattern must be refutable
                                             (variant ctor) and the
                                             else-block must diverge
                                             (return / break / continue
                                             / never-returning fn).
                                             Binds escape to the
                                             enclosing block. *)
  | Assign of { path : string list; value : expr; pos : Pos.t }
                                          (* Single-segment path = local
                                             variable assignment; multi-
                                             segment = qualified ref to an
                                             `extern var` (e.g.
                                             `raw::DOSBase = ...`). *)
  | AssignField of { target : expr; field : string; value : expr; pos : Pos.t }
  | AssignIndex of { base : expr; index : expr; value : expr; pos : Pos.t }
                                          (* `a[i] = value` *)
  | AssignDeref of { target : expr; value : expr; pos : Pos.t }
  | Return of expr option * Pos.t       (* `return;` (None — void / main
                                           exit 0) or `return <expr>;` *)
  | ExprStmt of expr                    (* `e;` — value discarded *)
  | Tail of expr                        (* trailing block expression (`e`
                                           with no `;`, last in a block) —
                                           the block's value.  In a value
                                           function it becomes the return
                                           value; in void position it is a
                                           discarded / void statement. *)
  | While of { cond : expr; body : stmt list }
  | For of { var : string; range : expr;
             body : stmt list; pos : Pos.t }
                                          (* `for v in <range> { body }`.
                                             `range` is either an `Ast.Range`
                                             literal (fast path: direct
                                             counter loop, no struct alloc)
                                             or any expression of type
                                             `Range<T>` / `RangeInclusive<T>`
                                             (a binding, fn return, ...) —
                                             typecheck pulls `.lo` / `.hi` /
                                             inclusiveness off the value. *)
  | Defer of { body : stmt list; pos : Pos.t }
  | Break of Pos.t                        (* `break;` — exit the nearest
                                             enclosing loop *)
  | With of { target : expr; name : string; body : stmt list; pos : Pos.t }
                                          (* DR-012 scoped projection:
                                             `with <target> |<name>| { body }`.
                                             Binds `name` to `&<target>`
                                             (`*T` mutable pointer-honest)
                                             for the body block.  Desugars
                                             at typecheck time to a let-ref
                                             inside its own block, so the
                                             borrow is statically scoped —
                                             a name referenced outside the
                                             block is out-of-scope (the
                                             dangle is unutterable).
                                             Does not consume the target.
                                             The existing escape pass (DR-
                                             010) catches in-block escapes
                                             (return / store-into-non-local)
                                             without any extra plumbing. *)
  | Continue of Pos.t                     (* `continue;` — skip to the
                                             nearest loop's next iteration
                                             (runs the `for` step) *)

let expr_pos = function
  | IntLit (_, p) | FloatLit (_, _, p) | BoolLit (_, p) | StringLit (_, p)
  | Var (_, p) | Neg (_, p) | BitNot (_, p) | Not (_, p) | BinOp (_, _, _, p)
  | Orelse (_, _, p) | Try (_, p) | SizeOf (_, p)
  | Cast (_, _, p) | TupleLit (_, p)
  | FieldAccess (_, _, p) | Ref (_, p) | Deref (_, p)
  | NullLit p | Block (_, p) -> p
  | Call { pos; _ } | StructLit { pos; _ } | New { pos; _ }
  | NewEnum { pos; _ }
  | MethodCall { pos; _ } | EnumLit { pos; _ } | Match { pos; _ }
  | If { pos; _ } | ArrayRepeat { pos; _ } | Index { pos; _ }
  | Range { pos; _ } | ArrayLit (_, pos) | Lambda { pos; _ } -> pos

type param = {
  pname : string;
  pty : type_ann;
  is_mut : bool;                     (* `fn f(mut x: T)` — the parameter
                                        binding is reassignable / its owned
                                        value mutable.  Immutable by default
                                        (mirrors `let` vs `let mut`).  Pointee
                                        mutability through a `*T` param is a
                                        separate, deferred axis. *)
  preg : string option;              (* `@reg(d0)` AmigaOS register pin.
                                        Validated against m68k register
                                        names (d0..d7, a0..a6).  Codegen
                                        emits `__reg("X")` before the
                                        param C type.  Only legal on
                                        extern fn params. *)
}

type func = {
  name : string;
  c_name : string;                   (* C-side symbol name.  Equals [name]
                                        unless an `as` rename is present
                                        on an extern fn:
                                          `extern fn alloc_mem as AllocMem(...)`
                                        → name = "alloc_mem", c_name =
                                        "AllocMem".  typecheck uses this
                                        for the mangled C identifier of
                                        extern fns, so the linker pulls
                                        the right C symbol. *)
  tparams : string list;             (* generic type parameters: [] for mono *)
  tbounds : (string * string list * (string * type_ann) list) list;
                                     (* trait bounds: `<T: Area>` →
                                        [("T", ["Area"], [])]; `<T: A + B>` →
                                        two entries for "T".  Checked at
                                        instantiation: the type bound to
                                        the tparam must `impl` each trait.
                                        Third element carries the DR-021
                                        sugar's assoc-bindings — `<F:
                                        |int|->int>` lowers to ("F",
                                        ["Fn1"], [("Arg", TyInt …);
                                        ("Output", TyInt …)]).  Plain
                                        trait-path bounds get [].  The
                                        bindings shortcut the assoc-type
                                        projection: `F::Output` projects
                                        directly to the bound type
                                        without consulting the impl's
                                        assoc table.
                                        [] when no bounds. *)
  params : param list;
  ret_ty : type_ann option;
  body : stmt list;                  (* empty when is_extern = true *)
  is_pub : bool;
  is_extern : bool;                  (* `extern fn name(...) -> T;` —
                                        forward decl only, no body, no
                                        ex_/mod__ name mangling *)
  is_variadic : bool;                (* `extern fn printf(fmt: str, ...);` —
                                        accepts any number of extra args
                                        after [params]; arg types past
                                        [params] not type-checked.  Only
                                        legal on extern fns. *)
  amiga_lib : string option;         (* `@amiga_lib(SysBase)` — marks an
                                        extern fn as an AmigaOS ROM library
                                        call whose base register lives at
                                        the named global.  Metadata for
                                        bindings hygiene; the actual
                                        register glue comes from Bebbo's
                                        amiga.lib stubs (linker pulls them
                                        in given the matching prototype). *)
  tier_hint : string option;         (* `@tier(core|standard|full)` attribute
                                        on the decl, raw string at parse
                                        time; typecheck validates and feeds
                                        to the linter as a Tier.t override
                                        of the inferred default. *)
  must_use : bool;                   (* `@must_use` attribute — discarding
                                        the return value (call in statement
                                        position with nobody binding it) is
                                        flagged by the linter. *)
  escapes_hatch : bool;              (* `@escapes` — function-level forward-
                                        compat hatch (DR-010): opts out of
                                        the escape-analysis floor for fns
                                        that legitimately return borrows
                                        rooted in arena/region storage the
                                        analyser can't yet model.  Skeleton
                                        atrybut — strukturalny swap-point
                                        gdy Owner-sigil / ward dochodzi. *)
  pos : Pos.t;
}

type struct_decl = {
  sname : string;
  stparams : string list;            (* [] for mono structs *)
  sfields : (string * type_ann) list;
  spos : Pos.t;
  sis_pub : bool;
  stier_hint : string option;        (* `@tier(...)` raw attribute; see
                                        [func.tier_hint] for semantics *)
  sis_debug : bool;                  (* `@debug` — synthesize a one-line
                                        Rust-Debug-style printer for values
                                        of this struct.  MVP: legal only
                                        on non-generic struct decls. *)
  sderives : string list;            (* `@derive(Eq, Clone)` — trait names
                                        to auto-implement.  A pre-typecheck
                                        pass synthesizes a real
                                        `impl Trait for Foo`. *)
  sis_move : bool;                   (* `@move` — affine / use-at-most-once
                                        semantics.  Marker is transitional
                                        (a stop-gap for owning-vs-borrowing
                                        `*u8` until the capability model's
                                        `own *u8` lands).  Parser records
                                        it; the move-pass (DR-002) reads
                                        `ss_is_move` on the struct_sig. *)
}

(* `enum Foo { | A | B(int, str) | C { f: T } }` — three variant forms.
   Tuple and struct kinds carry their fields in `vkind`; unit variants
   use `VUnit` and have no payload. *)
type variant_kind =
  | VUnit
  | VTuple of type_ann list
  | VStruct of (string * type_ann) list

type enum_variant = {
  vname : string;
  vkind : variant_kind;
  vpos : Pos.t;
}

type enum_decl = {
  ename : string;
  etparams : string list;            (* [] for mono enums *)
  evariants : enum_variant list;
  epos : Pos.t;
  eis_pub : bool;
  etier_hint : string option;        (* `@tier(...)` raw attribute *)
  emust_use : bool;                  (* `@must_use` — discarding a value of
                                        this enum in statement position
                                        (e.g. via a call returning it) is
                                        flagged.  Carries the spirit of
                                        Rust's `#[must_use]` applied to
                                        `Result`/`Option`. *)
  eis_debug : bool;                  (* `@debug` — synthesize a one-line
                                        Rust-Debug-style printer for values
                                        of this enum, so `print(v)` works.
                                        MVP: legal only on non-generic
                                        enum decls. *)
  ederives : string list;            (* `@derive(Eq, Clone)` on an enum. *)
}

type extern_struct = {
  esname : string;
  esfields : (string * type_ann) list option;
                                     (* None = opaque (`extern struct
                                        Foo;`); only legal use is via
                                        `*Foo`.  Some fs = exposed with
                                        fields (`extern struct Foo { ...
                                        }`); user-supplied layout must
                                        match the C header pulled in
                                        via @c_include — exile trusts
                                        the declaration. *)
  espos : Pos.t;
}

(* `xt*` prefix (eXternType) to avoid collision with `et*` on
   enum_decl (etparams).  ec*/es* don't collide today but keep the
   "xt" mnemonic consistent if more extern-* records appear. *)
type extern_type = {
  xtname : string;
  xtpos : Pos.t;
}

type extern_const = {
  ecname : string;
  ecty : type_ann;
  ecpos : Pos.t;
}

(* `const NAME: T = <expr>;` — a compile-time constant.  Unlike
   `extern const` (a linker-resolved C symbol), this carries an
   initialiser that the compiler folds to a literal at typecheck time and
   emits as `#define`.  The type annotation is required.  Top-level or
   module-level only; values are int / bool scalars for now. *)
type const_decl = {
  kname : string;
  kty : type_ann;
  kvalue : expr;
  kis_pub : bool;
  kpos : Pos.t;
}

(* `extern var DOSBase: *Library;` — globalna zmienna nie-funkcyjna,
   ustawiana przez kod C-strony (np. AmigaOS `OpenLibrary` zapisuje do
   `DOSBase`).  Identyczna struktura jak [extern_const] — różnica
   wyłącznie w semantyce (var jest l-value: można przypisać; const
   nie) i w codegenie (brak `const` w C deklaracji). *)
type extern_var = {
  evname : string;
  evty : type_ann;
  evpos : Pos.t;
}

(* `type Name<T...> = Type;` — pure (transparent) alias resolved by
   substitution in `resolve_type_ann_raw`.  NOT a newtype: the alias
   name and its target are interchangeable in every type annotation
   position (let-bind, fn-arg, ret, field, `as`).  Generic aliases
   (`type ParseRes<T> = Result<T, ParseErr>`) bind `tatparams` →
   call-site args before substituting into `tatarget`.  Pre-self-host
   ergonomy per FP-1 design 2026-05-28. *)
type type_alias_decl = {
  taname : string;
  tatparams : string list;
  tatarget : type_ann;
  tais_pub : bool;
  tapos : Pos.t;
}

type item =
  | Function of func
  | Module of module_decl
  | Use of { path : string list; is_wildcard : bool;
             is_pub : bool; pos : Pos.t }
                                          (* `pub use foo::bar;` re-exports
                                             `bar` from this scope under
                                             the same name.  Resolution
                                             redirects single-segment
                                             lookups of `bar` to `foo::bar`. *)
  | Struct of struct_decl
  | ExternStruct of extern_struct
  | ExternType of extern_type           (* `extern type LONG;` — raw C
                                           type alias visible in exile.
                                           Top-level only. *)
  | TypeAlias of type_alias_decl        (* `type Name<T...> = Type;` —
                                           pure alias, NOT newtype.  The
                                           alias name and target are
                                           interchangeable in every type
                                           annotation position; ctors
                                           still go through the original
                                           type.  Top-level + module
                                           level.  Resolved in
                                           resolve_type_ann_raw with a
                                           cycle-guard. *)
  | ExternConst of extern_const         (* `extern const FOO: c_uint;`
                                           — global value resolved by
                                           the linker.  Top-level only. *)
  | Const of const_decl                 (* `const NAME: T = expr;` —
                                           compile-time constant folded to
                                           a literal and emitted as
                                           `#define`.  Top-level or
                                           module-level. *)
  | ExternVar of extern_var             (* `extern var DOSBase: *Library;`
                                           — global mutable variable
                                           resolved by the linker.  Like
                                           ExternConst but assignable. *)
  | Enum of enum_decl
  | Impl of impl_block
  | Trait of trait_decl
  | View of view_decl                   (* `view Name(p: T) -> A | B(U) {
                                            body }` — DR-009 active patterns.
                                            A pre-typecheck pass synthesises
                                            a nominal enum `Name { A | B(U) }`
                                            (choice-enum) and a function
                                            `Name(p: T) -> Name { body }`.
                                            Match arms that name `Name::Case`
                                            against a scrutinee of type `T`
                                            (not `Name`) get rewritten to
                                            `let __c = Name(scr); match __c
                                            { Name::Case => ... }` — view-call
                                            then ordinary tagged switch.
                                            Maranget exhaustiveness comes for
                                            free since the choice-enum is
                                            nominal. *)
  | CInclude of { path : string; pos : Pos.t }
                                        (* `@c_include("path/to/header.h")`
                                           — emitted as `#include "..."` in
                                           the generated C, on top.  Only
                                           legal at top level. *)
and module_decl = {
  mname : string;
  mitems : item list;
  mpos : Pos.t;
  mis_pub : bool;
}
and impl_block = {
  itparams : string list;    (* `<A, B>` after `impl` — generic impl over a
                                generic struct; [] for a mono-struct impl *)
  itbounds : (string * string list * (string * type_ann) list) list;
                             (* `impl<T: A + B>` — trait bounds on the
                                impl's tparams, one entry per bound (so
                                `T: A + B` yields two).  Same shape as
                                `func.tbounds`; at method-lift time these
                                are spliced into each method's `tbounds`
                                so the existing instantiation-time
                                `type_impls_trait` check covers them
                                without a separate enforcement path.
                                Third element is the DR-021 sugar's
                                assoc-bindings; plain trait-path bounds
                                get []. *)
  itrait : string list option;
                             (* `impl Trait for Foo` carries `Some
                                ["Trait"]`; a plain inherent `impl Foo`
                                carries `None`.  Trait impls additionally
                                check signature conformance against the
                                trait decl. *)
  iassoc : (string * type_ann) list;
                             (* associated-type bindings: `type Item = int;`
                                → [("Item", TyInt ...)].  Only meaningful
                                in a trait impl; the trait's `trassoc`
                                names must all be bound. *)
  itarget : string list;     (* path written by user, may be relative *)
  iitems : func list;
  ipos : Pos.t;
}
and trait_decl = {
  trname : string;
  trassoc : string list;     (* associated-type names: `type Item;` →
                                ["Item"].  Every `impl` must bind each.
                                Used in method sigs as `Self::Item`. *)
  trsupers : string list list;
                             (* supertraits: `trait B: A` → [["A"]];
                                `trait B: A + C` → [["A"]; ["C"]].  An
                                `impl B for T` requires `impl A for T`
                                (and `impl C for T`) to exist. *)
  trmethods : func list;     (* method signatures; `body` carries the
                                default-method body when the method has
                                one (see [trdefaults]).  `self` receiver
                                stays `TySelf` (= `Self`) until conformance
                                substitutes the impl target. *)
  trdefaults : string list;  (* names of methods that declared a default
                                body (`fn m(self) { ... }` rather than
                                `fn m(self);`).  A defaulted method may be
                                omitted by an `impl`; the default is then
                                synthesised for that type. *)
  trpos : Pos.t;
  tris_pub : bool;
}
and view_decl = {
  vname : string;
  vparam : param;            (* the single scrutinee param; v1 = one param *)
  vcases : view_case list;
  vbody : stmt list;
  vpos : Pos.t;
  vis_pub : bool;
}
and view_case = {
  vcname : string;
  vcfields : (string * type_ann) list;
                             (* per-case payload — empty for unit cases,
                                synthetic `_0` / `_1` / ... for tuple-style,
                                user-given names for struct-style.  Same
                                shape `enum_decl.evariants` uses, so the
                                pre-typecheck pass can emit a matching
                                `Ast.Enum` literally. *)
  vcis_struct : bool;
}

type program = item list
