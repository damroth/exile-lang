(* Typed intermediate representation.  Owns the data structures the rest
   of the pipeline produces (Typecheck) and consumes (Codegen): the type
   domain, function/struct signatures, and the typed AST that mirrors
   Ast but carries `.ty` on every expression node.

   This module is pure — no validation, no Error.failf in the normal
   flow.  Validators (Typecheck) build values of these types; Codegen
   pattern-matches and emits.  Keeping it free of validation logic
   means future passes (monomorphization for generics, defer lowering,
   alternate backends) can consume Ir without dragging in the
   typechecker. *)

type int_kind = { signed : bool; width : Ast.int_width }

type typ =
  | TInt of int_kind
  | TBool
  | TString
  | TTuple of typ list
  | TStruct of string list             (* absolute path: e.g. ["foo"; "Point"] *)
  | TPtr of typ                        (* `*T` *)
  | TNullPtr                           (* type of `null` literal — compatible
                                          with any TPtr; never reaches codegen
                                          as a declaration type *)

(* Default integer type — what `int` and bare integer literals reduce to. *)
let t_i32 = TInt { signed = true; width = Ast.W32 }

let int_width_bits = function Ast.W8 -> 8 | Ast.W16 -> 16 | Ast.W32 -> 32

(* Does literal value [n] fit into an integer type of given signedness/width?
   OCaml's int is 63-bit on a 64-bit host, so all our ranges fit safely. *)
let int_fits n typ =
  match typ with
  | TInt { signed = true; width = Ast.W8 } -> n >= -128 && n <= 127
  | TInt { signed = false; width = Ast.W8 } -> n >= 0 && n <= 255
  | TInt { signed = true; width = Ast.W16 } -> n >= -32768 && n <= 32767
  | TInt { signed = false; width = Ast.W16 } -> n >= 0 && n <= 65535
  | TInt { signed = true; width = Ast.W32 } ->
      n >= -2147483648 && n <= 2147483647
  | TInt { signed = false; width = Ast.W32 } -> n >= 0 && n <= 4294967295
  | _ -> false

type fn_sig = {
  param_tys : typ list;
  ret_ty : typ option;
  mangled : string;            (* C-level name (e.g. "ex_foo" or "foo__bar") *)
  fn_pub : bool;
}

(* Struct signatures share the same module-aware resolution as functions —
   the registered path is the struct's absolute location. *)
type struct_sig = {
  sname_path : string list;     (* full path including struct name, e.g. ["foo"; "Point"] *)
  sfields_ty : (string * typ) list;
  sis_pub : bool;
}

(* Mangle a function name with its module path.  Top-level (path = []) gets
   the `ex_` prefix so emitted symbols never collide with C stdlib builtins
   (gcc warns about builtin-declaration-mismatch even when our top-level
   fn is `static`).  Inside a module, names join with "__"; the module
   prefix already keeps mod-internal symbols away from stdlib names. *)
let mangle path name =
  match path with
  | [] -> "ex_" ^ name
  | _ -> String.concat "__" path ^ "__" ^ name

let rec type_of_ann = function
  | Ast.TyInt { signed; width } -> TInt { signed; width }
  | Ast.TyStr -> TString
  | Ast.TyBool -> TBool
  | Ast.TyTuple ts -> TTuple (List.map type_of_ann ts)
  | Ast.TyStruct path -> TStruct path
  | Ast.TyPtr t -> TPtr (type_of_ann t)

let int_typ_name signed width =
  let prefix = if signed then "i" else "u" in
  let bits = match width with Ast.W8 -> "8" | Ast.W16 -> "16" | Ast.W32 -> "32" in
  prefix ^ bits

let rec typ_name = function
  | TInt { signed; width } -> int_typ_name signed width
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts -> "(" ^ String.concat ", " (List.map typ_name ts) ^ ")"
  | TStruct path -> String.concat "::" path
  | TPtr t -> "*" ^ typ_name t
  | TNullPtr -> "*<null>"

(* Equality used for type-match decisions in let/return/arg/field/assign
   sites.  Plain `=` would reject `TNullPtr` against any concrete `TPtr T`;
   here we treat the null literal as polymorphic over pointer types. *)
let rec typ_eq a b =
  match a, b with
  | TNullPtr, TPtr _ | TPtr _, TNullPtr -> true
  | TNullPtr, TNullPtr -> true
  | TPtr a, TPtr b -> typ_eq a b
  | TTuple xs, TTuple ys ->
      List.length xs = List.length ys && List.for_all2 typ_eq xs ys
  | _ -> a = b

(* Mangle a type to a C-identifier-safe string used as a unique key for
   tuple-struct dedup and as the C type name for named structs.  Tuples are
   anonymous (`tup<n>_T1_T2`); `tuple_struct_name` adds the `ex_` prefix so
   the emitted C struct sits in the same namespace as user fns/structs. *)
let rec mangle_typ = function
  | TInt { signed; width } -> int_typ_name signed width
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts ->
      Printf.sprintf "tup%d_%s" (List.length ts)
        (String.concat "_" (List.map mangle_typ ts))
  | TStruct path ->
      (match List.rev path with
       | [] -> failwith "empty struct path"
       | n :: rest -> mangle (List.rev rest) n)
  | TPtr t -> "ptr_" ^ mangle_typ t
  | TNullPtr -> failwith "TNullPtr should never be mangled"

let tuple_struct_name ts = "ex_" ^ mangle_typ (TTuple ts)

(* Typed AST — mirrors Ast.expr/stmt but every node carries the type
   computed by elaboration in `.ty`.  Codegen reads the type directly,
   never re-runs `type_of`. *)
type texpr_node =
  | TIntLit of int
  | TBoolLit of bool
  | TNullLit
  | TStringLit of string
  | TVar of string
  | TNeg of texpr
  | TBinOp of Ast.binop * texpr * texpr
  | TCall of { mangled : string; args : texpr list }
  | TBuiltinCall of { name : string; args : texpr list }
  | TCast of texpr * Ast.type_ann
  | TTupleLit of texpr list
  | TStructLit of { sname_path : string list;
                    fields : (string * texpr) list;
                    base : texpr option }
  | TFieldAccess of { target : texpr; field : string }
  | TRef of texpr
  | TDeref of texpr
  | TNew of { sname_path : string list;
              fields : (string * texpr) list;
              base : texpr option }

and texpr = {
  e : texpr_node;
  ty : typ;
  pos : Pos.t;
}

type tstmt =
  | TLet of { name : string; value : texpr; pos : Pos.t }
  | TLetTuple of { names : string list; value : texpr; pos : Pos.t }
  | TAssign of { name : string; value : texpr; pos : Pos.t }
  | TAssignField of { target : texpr; field : string; value : texpr;
                      pos : Pos.t }
  | TAssignDeref of { target : texpr; value : texpr; pos : Pos.t }
  | TReturn of { value : texpr; pos : Pos.t }
  | TExprStmt of texpr
  | TIf of { cond : texpr; then_body : tstmt list; else_body : tstmt list }
  | TWhile of { cond : texpr; body : tstmt list }
  | TDefer of { body : tstmt list; pos : Pos.t }

(* Per-function payload that codegen consumes — original Ast.func for
   the user-side trivia (param names, fn name, pos), the resolved C
   name, parameter and return types pre-resolved against the surrounding
   scope (so mod-local struct names round-trip to absolute paths in C),
   the elaborated body, and the hoisted let-decl list. *)
type tfunc = {
  tf_path : string list;
  tf_func : Ast.func;
  tf_mangled : string;
  tf_param_tys : typ list;          (* one per tf_func.params, scope-resolved *)
  tf_ret_ty : typ option;           (* mirrors tf_func.ret_ty, scope-resolved *)
  tf_body : tstmt list;
  tf_lets : (string * typ) list;
}

(* Whole-program checked + elaborated view that codegen consumes. *)
type tprogram = {
  tp_funcs : tfunc list;
  tp_struct_decls : (string list * Ast.struct_decl) list;
  tp_struct_index : struct_sig list;
  tp_global : (string list * string * fn_sig) list;
  tp_modules : (string list * bool) list;
  tp_uses_heap : bool;
  tp_tuple_types : (string * typ) list;
}
