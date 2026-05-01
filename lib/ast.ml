type int_width = W8 | W16 | W32

type type_ann =
  | TyInt of { signed : bool; width : int_width }
  | TyStr
  | TyBool
  | TyTuple of type_ann list

type binop =
  | Add | Sub | Mul | Div
  | Lt | Gt | LtEq | GtEq | EqEq | NotEq

type expr =
  | IntLit of int
  | BoolLit of bool
  | StringLit of string
  | Var of string * Pos.t
  | Neg of expr
  | BinOp of binop * expr * expr
  | Call of string list * expr list * Pos.t
  | Cast of expr * type_ann * Pos.t
  | TupleLit of expr list * Pos.t

type param = { pname : string; pty : type_ann }

type stmt =
  | Let of { name : string; value : expr; ty_ann : type_ann option; pos : Pos.t }
  | LetTuple of { names : string list; value : expr; pos : Pos.t }
  | Assign of { name : string; value : expr; pos : Pos.t }
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

type item =
  | Function of func
  | Module of module_decl
  | Use of { path : string list; is_wildcard : bool; pos : Pos.t }
and module_decl = {
  mname : string;
  mitems : item list;
  mpos : Pos.t;
  mis_pub : bool;
}

type program = item list
