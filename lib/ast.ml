type type_ann = TyInt | TyStr

type binop =
  | Add | Sub | Mul | Div
  | Lt | Gt | LtEq | GtEq | EqEq | NotEq

type expr =
  | IntLit of int
  | StringLit of string
  | Var of string
  | Neg of expr
  | BinOp of binop * expr * expr
  | Call of string * expr list

type param = { pname : string; pty : type_ann }

type stmt =
  | Let of { name : string; value : expr }
  | Return of expr
  | ExprStmt of expr
  | If of { cond : expr; then_body : stmt list; else_body : stmt list }

type func = {
  name : string;
  params : param list;
  ret_ty : type_ann option;
  body : stmt list;
}

type item = Function of func

type program = item list
