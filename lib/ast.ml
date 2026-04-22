type type_ann = TyInt | TyStr | TyBool

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
  | Call of string * expr list * Pos.t

type param = { pname : string; pty : type_ann }

type stmt =
  | Let of { name : string; value : expr; ty_ann : type_ann option; pos : Pos.t }
  | Assign of { name : string; value : expr; pos : Pos.t }
  | Return of expr
  | ExprStmt of expr
  | If of { cond : expr; then_body : stmt list; else_body : stmt list }
  | While of { cond : expr; body : stmt list }

type func = {
  name : string;
  params : param list;
  ret_ty : type_ann option;
  body : stmt list;
}

type program = func list
