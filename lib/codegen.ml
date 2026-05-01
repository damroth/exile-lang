(* C89 emission.  Consumes the typed view exposed by Typecheck — every
   type, lookup, and validation lives there.  This module owns only the
   shape of the emitted text. *)

open Typecheck

let add_separated buf sep f xs =
  List.iteri
    (fun i x -> if i > 0 then Buffer.add_string buf sep; f x)
    xs

let escape_c s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\000' -> Buffer.add_string buf "\\0"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* Mangle a type to a C-identifier-safe string used both for the tuple
   struct name and as a unique key in the codegen-side dedup table. *)
let rec mangle_typ = function
  | TInt { signed; width } -> int_typ_name signed width
  | TBool -> "bool"
  | TString -> "str"
  | TTuple ts ->
      Printf.sprintf "tup%d_%s" (List.length ts)
        (String.concat "_" (List.map mangle_typ ts))

let tuple_struct_name ts = "ex_" ^ mangle_typ (TTuple ts)

let rec c_type_prefix = function
  | TInt { signed; width } ->
      let s = if signed then "" else "unsigned " in
      let core = match width with
        | Ast.W8 -> "char"
        | Ast.W16 -> "short"
        | Ast.W32 -> "int"
      in
      (* "signed char" is needed because C89 leaves plain `char` signedness
         implementation-defined; for i8 we must be explicit. *)
      let signed_core =
        if signed && width = Ast.W8 then "signed char " else core ^ " "
      in
      s ^ signed_core
  | TBool -> "int "
  | TString -> "const char *"
  | TTuple ts -> "struct " ^ tuple_struct_name ts ^ " "

let strip_trailing_space s =
  if String.length s > 0 && s.[String.length s - 1] = ' '
  then String.sub s 0 (String.length s - 1)
  else s

let c_decl t name = c_type_prefix t ^ name

let c_param p = c_decl (type_of_ann p.Ast.pty) p.Ast.pname

(* Builtin emitters keyed by name.  Typecheck owns the bcheck side; this
   table covers only emission.  Adding a new builtin means a typecheck-side
   `builtin_sig` plus an emitter here. *)
type builtin_emit =
  Buffer.t -> typ list -> Ast.expr list -> (Ast.expr -> unit) -> unit

let emit_print : builtin_emit =
  fun buf arg_tys args emit_arg ->
    (* %d for signed (and bool), %u for unsigned.  Varargs default-promote
       smaller widths up to (un)signed int, so a single specifier per
       signedness works for all widths. *)
    let fmt = match List.hd arg_tys with
      | TBool -> "\"%d\\n\""
      | TInt { signed = true; _ } -> "\"%d\\n\""
      | TInt { signed = false; _ } -> "\"%u\\n\""
      | TString -> "\"%s\\n\""
      | TTuple _ -> assert false  (* typecheck rejected this earlier *)
    in
    Buffer.add_string buf "printf(";
    Buffer.add_string buf fmt;
    Buffer.add_string buf ", ";
    emit_arg (List.hd args);
    Buffer.add_char buf ')'

let builtin_emitters : (string * builtin_emit) list = [
  ("print", emit_print);
]

let lookup_builtin_emit = function
  | [ name ] -> List.assoc_opt name builtin_emitters
  | _ -> None

let prec = function
  | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq | Ast.EqEq | Ast.NotEq -> 0
  | Ast.Add | Ast.Sub -> 1
  | Ast.Mul | Ast.Div -> 2

let rec gen_expr buf ctx env = function
  | Ast.IntLit n -> Buffer.add_string buf (string_of_int n)
  | Ast.BoolLit b -> Buffer.add_string buf (if b then "1" else "0")
  | Ast.StringLit s ->
      Buffer.add_char buf '"';
      Buffer.add_string buf (escape_c s);
      Buffer.add_char buf '"'
  | Ast.Var (name, _) -> Buffer.add_string buf name
  | Ast.Neg e ->
      Buffer.add_char buf '-';
      (match e with
       | Ast.IntLit _ | Ast.Var _ -> gen_expr buf ctx env e
       | _ ->
           Buffer.add_char buf '(';
           gen_expr buf ctx env e;
           Buffer.add_char buf ')')
  | Ast.Cast (e, ann, _) ->
      let trimmed = strip_trailing_space (c_type_prefix (type_of_ann ann)) in
      Buffer.add_string buf "((";
      Buffer.add_string buf trimmed;
      Buffer.add_string buf ")";
      gen_expr buf ctx env e;
      Buffer.add_char buf ')'
  | Ast.BinOp (op, l, r) ->
      let op_str =
        match op with
        | Ast.Add -> " + " | Ast.Sub -> " - "
        | Ast.Mul -> " * " | Ast.Div -> " / "
        | Ast.Lt -> " < " | Ast.Gt -> " > "
        | Ast.LtEq -> " <= " | Ast.GtEq -> " >= "
        | Ast.EqEq -> " == " | Ast.NotEq -> " != "
      in
      let p = prec op in
      (match l with
       | Ast.BinOp (lop, _, _) when prec lop < p ->
           Buffer.add_char buf '('; gen_expr buf ctx env l; Buffer.add_char buf ')'
       | _ -> gen_expr buf ctx env l);
      Buffer.add_string buf op_str;
      (match r with
       | Ast.BinOp (rop, _, _)
         when prec rop < p || (prec rop = p && (op = Ast.Sub || op = Ast.Div)) ->
           Buffer.add_char buf '('; gen_expr buf ctx env r; Buffer.add_char buf ')'
       | _ -> gen_expr buf ctx env r)
  | Ast.Call (path, args, pos) ->
      (match lookup_builtin_emit path with
       | Some emit ->
           let arg_tys = List.map (type_of ctx env) args in
           emit buf arg_tys args (fun e -> gen_expr buf ctx env e)
       | None ->
           let mangled =
             match lookup_fn ctx path with
             | Some (_, s) -> s.mangled
             | None ->
                 Error.failf pos "unknown function '%s'" (String.concat "::" path)
           in
           Buffer.add_string buf mangled;
           Buffer.add_char buf '(';
           add_separated buf ", " (gen_expr buf ctx env) args;
           Buffer.add_char buf ')')
  | Ast.TupleLit (_, pos) ->
      Error.failf pos
        "tuple literal can only appear in 'return (...)' or as the RHS of \
         'let (...) = ...'"

(* Initialise an already-declared temp from an expression.  Tuple literals
   become field-by-field assignments (`__t._0 = e0; __t._1 = e1; ...`); any
   other RHS uses a single struct- or scalar-assignment (`__t = expr;`).
   Both forms are strict C89 — brace initializers with non-constant
   elements are a C99 relaxation that `-ansi -pedantic` rejects. *)
and emit_value_into_temp buf ctx env indent temp_name value =
  match value with
  | Ast.TupleLit (es, _) ->
      List.iteri
        (fun i e ->
          Buffer.add_string buf indent;
          Buffer.add_string buf temp_name;
          Buffer.add_string buf (Printf.sprintf "._%d = " i);
          gen_expr buf ctx env e;
          Buffer.add_string buf ";\n")
        es
  | _ ->
      Buffer.add_string buf indent;
      Buffer.add_string buf temp_name;
      Buffer.add_string buf " = ";
      gen_expr buf ctx env value;
      Buffer.add_string buf ";\n"

(* Statement emission with `defer` support.  `outer_scopes` is the list of
   defer-stack snapshots for each block enclosing this one (innermost first);
   inside the emitted block we accumulate `my_defers` as we walk statements
   and on every exit point we emit cleanups in LIFO order across declarations
   and in source order within each defer body.

   On fall-through end of block: emit only this block's cleanups.
   On `return` from inside the block: emit this block's cleanups AND every
   outer scope's cleanups, then return the value.  When defers are active
   the return value is captured into a fresh `__exile_ret` temp inside a
   new C block so cleanups can run before the actual `return` instruction.

   A `defer` body is a leaf — it must not contain another `defer` or
   `return`; both are rejected by `emit_simple_stmt`. *)
let rec emit_simple_stmt buf ctx env indent stmt =
  match stmt with
  | Ast.Let { name; value; _ } | Ast.Assign { name; value; _ } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf (name ^ " = ");
      gen_expr buf ctx env value;
      Buffer.add_string buf ";\n"
  | Ast.LetTuple { names; value; _ } ->
      emit_let_tuple buf ctx env indent names value
  | Ast.ExprStmt e ->
      Buffer.add_string buf indent;
      gen_expr buf ctx env e;
      Buffer.add_string buf ";\n"
  | Ast.If { cond; then_body; else_body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "if (";
      gen_expr buf ctx env cond;
      Buffer.add_string buf ") {\n";
      List.iter (emit_simple_stmt buf ctx env (indent ^ "    ")) then_body;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}';
      (match else_body with
       | [] -> Buffer.add_char buf '\n'
       | _ ->
           Buffer.add_string buf " else {\n";
           List.iter (emit_simple_stmt buf ctx env (indent ^ "    ")) else_body;
           Buffer.add_string buf indent;
           Buffer.add_string buf "}\n")
  | Ast.While { cond; body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "while (";
      gen_expr buf ctx env cond;
      Buffer.add_string buf ") {\n";
      List.iter (emit_simple_stmt buf ctx env (indent ^ "    ")) body;
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"
  | Ast.Defer { pos; _ } ->
      Error.failf pos "'defer' inside a defer body is not supported"
  | Ast.Return (_, pos) ->
      Error.failf pos "'return' inside a defer body is not supported"

(* Destructuring binding: introduce an inner C block, declare a `__t` temp
   of the tuple struct type, fill it from the RHS, then assign each hoisted
   name from the temp's numbered field. *)
and emit_let_tuple buf ctx env indent names value =
  let t = type_of ctx env value in
  let trimmed = strip_trailing_space (c_type_prefix t) in
  Buffer.add_string buf indent;
  Buffer.add_string buf "{\n";
  Buffer.add_string buf (indent ^ "    ");
  Buffer.add_string buf trimmed;
  Buffer.add_string buf " __t;\n";
  emit_value_into_temp buf ctx env (indent ^ "    ") "__t" value;
  List.iteri
    (fun i name ->
      Buffer.add_string buf (indent ^ "    ");
      Buffer.add_string buf (Printf.sprintf "%s = __t._%d;\n" name i))
    names;
  Buffer.add_string buf indent;
  Buffer.add_string buf "}\n"

let emit_cleanups buf ctx env indent defers =
  List.iter
    (fun body ->
      List.iter (fun s -> emit_simple_stmt buf ctx env indent s) body)
    defers

let rec gen_if buf ctx env indent outer_scopes my_defers
    cond then_body else_body =
  Buffer.add_string buf "if (";
  gen_expr buf ctx env cond;
  Buffer.add_string buf ") {\n";
  gen_block buf ctx env (indent ^ "    ")
    (my_defers :: outer_scopes) then_body;
  Buffer.add_string buf indent;
  Buffer.add_char buf '}';
  (match else_body with
   | [] -> Buffer.add_char buf '\n'
   | [ Ast.If { cond = ec; then_body = et; else_body = ee } ] ->
       Buffer.add_string buf " else ";
       gen_if buf ctx env indent outer_scopes my_defers ec et ee
   | _ ->
       Buffer.add_string buf " else {\n";
       gen_block buf ctx env (indent ^ "    ")
         (my_defers :: outer_scopes) else_body;
       Buffer.add_string buf indent;
       Buffer.add_string buf "}\n")

and gen_block buf ctx env indent outer_scopes stmts =
  let rec loop my_defers = function
    | [] ->
        emit_cleanups buf ctx env indent my_defers
    | Ast.Defer { body; _ } :: rest ->
        loop (body :: my_defers) rest
    | Ast.Return (e, _) :: _ ->
        let all = List.flatten (my_defers :: outer_scopes) in
        let needs_block =
          all <> [] || (match e with Ast.TupleLit _ -> true | _ -> false)
        in
        if not needs_block then begin
          Buffer.add_string buf indent;
          Buffer.add_string buf "return ";
          gen_expr buf ctx env e;
          Buffer.add_string buf ";\n"
        end else begin
          let trimmed =
            strip_trailing_space (c_type_prefix (type_of ctx env e))
          in
          Buffer.add_string buf indent;
          Buffer.add_string buf "{\n";
          Buffer.add_string buf (indent ^ "    ");
          Buffer.add_string buf trimmed;
          Buffer.add_string buf " __exile_ret;\n";
          emit_value_into_temp buf ctx env (indent ^ "    ") "__exile_ret" e;
          emit_cleanups buf ctx env (indent ^ "    ") all;
          Buffer.add_string buf (indent ^ "    ");
          Buffer.add_string buf "return __exile_ret;\n";
          Buffer.add_string buf indent;
          Buffer.add_string buf "}\n"
        end
    | (Ast.Let _ | Ast.Assign _ | Ast.ExprStmt _) as s :: rest ->
        emit_simple_stmt buf ctx env indent s;
        loop my_defers rest
    | Ast.LetTuple { names; value; _ } :: rest ->
        emit_let_tuple buf ctx env indent names value;
        loop my_defers rest
    | Ast.If { cond; then_body; else_body } :: rest ->
        Buffer.add_string buf indent;
        gen_if buf ctx env indent outer_scopes my_defers
          cond then_body else_body;
        loop my_defers rest
    | Ast.While { cond; body } :: rest ->
        Buffer.add_string buf indent;
        Buffer.add_string buf "while (";
        gen_expr buf ctx env cond;
        Buffer.add_string buf ") {\n";
        gen_block buf ctx env (indent ^ "    ")
          (my_defers :: outer_scopes) body;
        Buffer.add_string buf indent;
        Buffer.add_string buf "}\n";
        loop my_defers rest
  in
  loop [] stmts

(* Emit a function signature using a mangled C-level name (or "main" for the
   entry point — main() is special and not mangled).  Non-pub functions get
   a "static" linkage prefix so they are invisible across translation units
   (and act as documentation that they are module-internal). *)
let emit_fn_sig buf (f : Ast.func) mangled =
  if f.name = "main" then
    Buffer.add_string buf "int main(void)"
  else begin
    if not f.is_pub then Buffer.add_string buf "static ";
    let ret =
      match f.ret_ty with
      | None -> "void "
      | Some ty -> c_type_prefix (type_of_ann ty)
    in
    Buffer.add_string buf ret;
    Buffer.add_string buf mangled;
    Buffer.add_char buf '(';
    (match f.params with
     | [] -> Buffer.add_string buf "void"
     | _ ->
         add_separated buf ", " (fun p -> Buffer.add_string buf (c_param p)) f.params);
    Buffer.add_char buf ')'
  end

let gen_function buf ctx (f : Ast.func) mangled =
  List.iter (fun p -> check_c_ident f.pos "parameter" p.Ast.pname) f.params;
  emit_fn_sig buf f mangled;
  Buffer.add_string buf " {\n";
  let param_env = List.map (fun p -> (p.Ast.pname, type_of_ann p.Ast.pty)) f.params in
  let lets = collect_lets ctx param_env f.body in
  let full_env = param_env @ lets in
  List.iter
    (fun (name, t) ->
      Buffer.add_string buf (Printf.sprintf "    %s;\n" (c_decl t name)))
    lets;
  gen_block buf ctx full_env "    " [] f.body;
  if f.name = "main" then Buffer.add_string buf "    return 0;\n";
  Buffer.add_string buf "}\n"

(* Walk every function body looking for tuple types in use.  We collect
   them deduplicated by mangled name and emit one C struct per unique tuple
   shape.  Sources of tuples: fn signatures (ret_ty, params) and any
   `TupleLit` expression in code (typed via `type_of`). *)
let collect_tuple_types flat global modules =
  let seen = ref [] in
  let add t =
    let name = mangle_typ t in
    if not (List.exists (fun (n, _) -> n = name) !seen) then
      seen := (name, t) :: !seen
  in
  let rec walk_typ t =
    match t with
    | TTuple ts -> add t; List.iter walk_typ ts
    | _ -> ()
  in
  let walk_typ_ann ann = walk_typ (type_of_ann ann) in
  let rec walk_expr ctx env e =
    (match e with
     | Ast.TupleLit (es, _) ->
         walk_typ (type_of ctx env e);
         List.iter (walk_expr ctx env) es
     | Ast.Cast (sub, _, _) -> walk_expr ctx env sub
     | Ast.Neg sub -> walk_expr ctx env sub
     | Ast.BinOp (_, l, r) -> walk_expr ctx env l; walk_expr ctx env r
     | Ast.Call (_, args, _) -> List.iter (walk_expr ctx env) args
     | _ -> ())
  in
  let rec walk_stmt ctx env = function
    | Ast.Let { value; _ } -> walk_expr ctx env value
    | Ast.LetTuple { value; _ } -> walk_expr ctx env value
    | Ast.Assign { value; _ } -> walk_expr ctx env value
    | Ast.Return (e, _) -> walk_expr ctx env e
    | Ast.ExprStmt e -> walk_expr ctx env e
    | Ast.If { cond; then_body; else_body } ->
        walk_expr ctx env cond;
        List.iter (walk_stmt ctx env) then_body;
        List.iter (walk_stmt ctx env) else_body
    | Ast.While { cond; body } ->
        walk_expr ctx env cond;
        List.iter (walk_stmt ctx env) body
    | Ast.Defer { body; _ } ->
        List.iter (walk_stmt ctx env) body
  in
  List.iter
    (fun (path, (f : Ast.func), _) ->
      Option.iter walk_typ_ann f.ret_ty;
      List.iter (fun (p : Ast.param) -> walk_typ_ann p.pty) f.params;
      let ctx = { global; modules; scope = path } in
      let param_env =
        List.map (fun (p : Ast.param) ->
          (p.pname, type_of_ann p.pty)) f.params
      in
      let lets = collect_lets ctx param_env f.body in
      let env = param_env @ lets in
      List.iter (walk_stmt ctx env) f.body)
    flat;
  List.rev !seen

let emit_tuple_struct buf (_, t) =
  match t with
  | TTuple ts ->
      Buffer.add_string buf (Printf.sprintf "struct %s {" (tuple_struct_name ts));
      List.iteri
        (fun i ty ->
          Buffer.add_char buf ' ';
          Buffer.add_string buf (c_decl ty (Printf.sprintf "_%d" i));
          Buffer.add_char buf ';')
        ts;
      Buffer.add_string buf " };\n"
  | _ -> ()

let gen_program program =
  let flat = flatten_funcs program in
  (* main() must be at top level, not inside a module. *)
  List.iter
    (fun (path, f, _) ->
      if f.Ast.name = "main" && path <> [] then
        Error.raise_ f.Ast.pos
          "'main' must be at top level, not inside a module")
    flat;
  (* Top-level function names also need the C-keyword check; module fns
     are protected by the `mod__` prefix from mangling. *)
  List.iter
    (fun (path, f, _) ->
      if path = [] then check_c_ident f.Ast.pos "function" f.Ast.name)
    flat;
  let global = build_global_index flat in
  let modules = flatten_modules program in
  let tuples = collect_tuple_types flat global modules in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n";
  if tuples <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_tuple_struct buf) tuples
  end;
  let non_main = List.filter (fun (_, f, _) -> f.Ast.name <> "main") flat in
  if non_main <> [] then begin
    Buffer.add_char buf '\n';
    List.iter
      (fun (_, f, mangled) ->
        emit_fn_sig buf f mangled;
        Buffer.add_string buf ";\n")
      non_main
  end;
  Buffer.add_char buf '\n';
  let last = List.length flat - 1 in
  List.iteri
    (fun i (path, f, mangled) ->
      let ctx = { global; modules; scope = path } in
      gen_function buf ctx f mangled;
      if i < last then Buffer.add_char buf '\n')
    flat;
  Buffer.contents buf
