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

let expr_pos = function
  | IntLit (_, p) | BoolLit (_, p) | StringLit (_, p)
  | Var (_, p) | Neg (_, p) | BinOp (_, _, _, p)
  | Call (_, _, p) | Cast (_, _, p) | TupleLit (_, p)
  | FieldAccess (_, _, p) | Ref (_, p) | Deref (_, p)
  | NullLit p -> p
  | StructLit { pos; _ } | New { pos; _ } -> pos

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

type item =
  | Function of func
  | Module of module_decl
  | Use of { path : string list; is_wildcard : bool; pos : Pos.t }
  | Struct of struct_decl
and module_decl = {
  mname : string;
  mitems : item list;
  mpos : Pos.t;
  mis_pub : bool;
}

type program = item list
