type int_width = W8 | W16 | W32

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
  | TyPtr of type_ann                  (* `*T` *)
  | TyFnPtr of { params : type_ann list; ret : type_ann option }
                                       (* `fn(T1, T2) -> R` as a type.
                                          C-side maps to a function
                                          pointer.  No variadic in this
                                          form yet. *)

type binop =
  | Add | Sub | Mul | Div
  | Lt | Gt | LtEq | GtEq | EqEq | NotEq
  | Concat                              (* `++` — compile-time string concat;
                                           both operands must reduce to a
                                           string literal at typecheck time. *)

let binop_name = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
  | Lt -> "<" | Gt -> ">" | LtEq -> "<=" | GtEq -> ">="
  | EqEq -> "==" | NotEq -> "!="
  | Concat -> "++"

type expr =
  | IntLit of int * Pos.t
  | BoolLit of bool * Pos.t
  | StringLit of string * Pos.t
  | Var of string * Pos.t
  | Neg of expr * Pos.t
  | BinOp of binop * expr * expr * Pos.t
  | Call of string list * expr list * Pos.t
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

and match_arm = { pat : pattern; body : expr; arm_pos : Pos.t }

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

and pat_binds =
  | PBTuple of pattern list
  | PBStruct of (string * pattern) list  (* may include shorthand binds:
                                            shorthand `f` desugars at parse
                                            time to `("f", PVar "f")`. *)

let expr_pos = function
  | IntLit (_, p) | BoolLit (_, p) | StringLit (_, p)
  | Var (_, p) | Neg (_, p) | BinOp (_, _, _, p)
  | Orelse (_, _, p) | Try (_, p) | SizeOf (_, p)
  | Call (_, _, p) | Cast (_, _, p) | TupleLit (_, p)
  | FieldAccess (_, _, p) | Ref (_, p) | Deref (_, p)
  | NullLit p -> p
  | StructLit { pos; _ } | New { pos; _ }
  | MethodCall { pos; _ } | EnumLit { pos; _ } | Match { pos; _ } -> pos

type param = {
  pname : string;
  pty : type_ann;
  preg : string option;              (* `@reg(d0)` AmigaOS register pin.
                                        Validated against m68k register
                                        names (d0..d7, a0..a6).  Codegen
                                        emits `__reg("X")` before the
                                        param C type.  Only legal on
                                        extern fn params. *)
}

type stmt =
  | Let of { name : string; value : expr; ty_ann : type_ann option; pos : Pos.t }
  | LetTuple of { names : string list; value : expr; pos : Pos.t }
  | Assign of { path : string list; value : expr; pos : Pos.t }
                                          (* Single-segment path = local
                                             variable assignment; multi-
                                             segment = qualified ref to an
                                             `extern var` (e.g.
                                             `raw::DOSBase = ...`). *)
  | AssignField of { target : expr; field : string; value : expr; pos : Pos.t }
  | AssignDeref of { target : expr; value : expr; pos : Pos.t }
  | Return of expr option * Pos.t       (* `return;` (None — void / main
                                           exit 0) or `return <expr>;` *)
  | ExprStmt of expr
  | If of { cond : expr; then_body : stmt list; else_body : stmt list }
  | While of { cond : expr; body : stmt list }
  | Defer of { body : stmt list; pos : Pos.t }

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
  | ExternConst of extern_const         (* `extern const FOO: c_uint;`
                                           — global value resolved by
                                           the linker.  Top-level only. *)
  | ExternVar of extern_var             (* `extern var DOSBase: *Library;`
                                           — global mutable variable
                                           resolved by the linker.  Like
                                           ExternConst but assignable. *)
  | Enum of enum_decl
  | Impl of impl_block
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
  itarget : string list;     (* path written by user, may be relative *)
  iitems : func list;
  ipos : Pos.t;
}

type program = item list
