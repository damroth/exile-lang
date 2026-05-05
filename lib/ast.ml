type int_width = W8 | W16 | W32

type type_ann =
  | TyInt of { signed : bool; width : int_width }
  | TyStr
  | TyBool
  | TyTuple of type_ann list
  | TyStruct of string list           (* qualified path: e.g. ["foo"; "Point"] *)
  | TyPtr of type_ann                  (* `*T` *)

type binop =
  | Add | Sub | Mul | Div
  | Lt | Gt | LtEq | GtEq | EqEq | NotEq

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
                                           always an expression in the AST;
                                           Phase A only allows it in
                                           ExprStmt position (effective
                                           statement form), Phase C adds
                                           let-RHS / return lowering. *)

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
  | Call (_, _, p) | Cast (_, _, p) | TupleLit (_, p)
  | FieldAccess (_, _, p) | Ref (_, p) | Deref (_, p)
  | NullLit p -> p
  | StructLit { pos; _ } | New { pos; _ }
  | MethodCall { pos; _ } | EnumLit { pos; _ } | Match { pos; _ } -> pos

type param = { pname : string; pty : type_ann }

type stmt =
  | Let of { name : string; value : expr; ty_ann : type_ann option; pos : Pos.t }
  | LetTuple of { names : string list; value : expr; pos : Pos.t }
  | Assign of { name : string; value : expr; pos : Pos.t }
  | AssignField of { target : expr; field : string; value : expr; pos : Pos.t }
  | AssignDeref of { target : expr; value : expr; pos : Pos.t }
  | Return of expr * Pos.t
  | ExprStmt of expr
  | If of { cond : expr; then_body : stmt list; else_body : stmt list }
  | While of { cond : expr; body : stmt list }
  | Defer of { body : stmt list; pos : Pos.t }

type func = {
  name : string;
  params : param list;
  ret_ty : type_ann option;
  body : stmt list;
  is_pub : bool;
  pos : Pos.t;
}

type struct_decl = {
  sname : string;
  sfields : (string * type_ann) list;
  spos : Pos.t;
  sis_pub : bool;
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
  evariants : enum_variant list;
  epos : Pos.t;
  eis_pub : bool;
}

type item =
  | Function of func
  | Module of module_decl
  | Use of { path : string list; is_wildcard : bool; pos : Pos.t }
  | Struct of struct_decl
  | Enum of enum_decl
  | Impl of impl_block
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
