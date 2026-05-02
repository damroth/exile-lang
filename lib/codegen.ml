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

(* Mangle a type to a C-identifier-safe string used as a unique key in the
   codegen-side dedup table.  For named structs the path is run through
   `mangle` (so a top-level `Point` becomes `ex_Point` and a `mod foo`
   struct becomes `foo__Point`); tuples are anonymous, so `mangle_typ`
   produces just `tup<n>_T1_T2`, and `tuple_struct_name` adds the `ex_`
   prefix so the emitted C struct sits in the same namespace. *)
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

let tuple_struct_name ts = "ex_" ^ mangle_typ (TTuple ts)

let strip_trailing_space s =
  if String.length s > 0 && s.[String.length s - 1] = ' '
  then String.sub s 0 (String.length s - 1)
  else s

let rec c_type_prefix = function
  | TInt { signed; width } ->
      let s = if signed then "" else "unsigned " in
      let core = match width with
        | Ast.W8 -> "char"
        | Ast.W16 -> "short"
        (* C89 only guarantees `int >= 16 bits`; some Amiga compilers
           (SAS/C default) actually use 16-bit int.  `long >= 32 bits` is
           guaranteed, so we map i32/u32 to long for cross-compiler width
           stability. *)
        | Ast.W32 -> "long"
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
  | TStruct _ as t -> "struct " ^ mangle_typ t ^ " "
  | TPtr inner ->
      (* Pointer types render as `<base> *` with no trailing space, so
         `c_decl t name` produces `<base> *name`. *)
      strip_trailing_space (c_type_prefix inner) ^ " *"

let c_decl t name = c_type_prefix t ^ name

let c_param p = c_decl (type_of_ann p.Ast.pty) p.Ast.pname

(* Builtin emitters keyed by name.  Typecheck owns the bcheck side; this
   table covers only emission.  Adding a new builtin means a typecheck-side
   `builtin_sig` plus an emitter here. *)
type builtin_emit =
  Buffer.t -> typ list -> Ast.expr list -> (Ast.expr -> unit) -> unit

let emit_print : builtin_emit =
  fun buf arg_tys args emit_arg ->
    (* Varargs promote i8/i16 to int, so `%d`/`%u` cover them with no cast.
       i32/u32 are emitted as `long`/`unsigned long` (for cross-C-compiler
       width stability) and need `%ld`/`%lu`.  Because `printf` is variadic
       it does not auto-promote `int` to `long` — an int literal like `0`
       passed under `%ld` is ill-formed under `-Wformat`.  We force the
       cast on the call site. *)
    let arg_ty = List.hd arg_tys in
    let fmt = match arg_ty with
      | TBool -> "\"%d\\n\""
      | TInt { signed = true; width = Ast.W32 } -> "\"%ld\\n\""
      | TInt { signed = false; width = Ast.W32 } -> "\"%lu\\n\""
      | TInt { signed = true; _ } -> "\"%d\\n\""
      | TInt { signed = false; _ } -> "\"%u\\n\""
      | TString -> "\"%s\\n\""
      | TTuple _ | TStruct _ | TPtr _ -> assert false  (* typecheck rejected this earlier *)
    in
    let cast =
      match arg_ty with
      | TInt { signed = true; width = Ast.W32 } -> Some "(long)"
      | TInt { signed = false; width = Ast.W32 } -> Some "(unsigned long)"
      | _ -> None
    in
    Buffer.add_string buf "printf(";
    Buffer.add_string buf fmt;
    Buffer.add_string buf ", ";
    (match cast with
     | Some c ->
         Buffer.add_string buf c;
         Buffer.add_char buf '(';
         emit_arg (List.hd args);
         Buffer.add_char buf ')'
     | None -> emit_arg (List.hd args));
    Buffer.add_char buf ')'

let emit_free : builtin_emit =
  fun buf _arg_tys args emit_arg ->
    Buffer.add_string buf "free(";
    emit_arg (List.hd args);
    Buffer.add_char buf ')'

let builtin_emitters : (string * builtin_emit) list = [
  ("print", emit_print);
  ("free", emit_free);
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
  | Ast.StructLit { pos; _ } ->
      Error.failf pos
        "struct literal can only appear in 'return ...', as the RHS of \
         'let x = ...', or in a field assignment"
  | Ast.New { pos; _ } ->
      Error.failf pos
        "'new ...' can only appear as the RHS of 'let x = ...' or \
         in 'return ...'"
  | Ast.FieldAccess (target, fname, _) ->
      (* Auto-deref pointer-to-struct via `->`; otherwise plain `.`. *)
      let sep =
        match type_of ctx env target with
        | TPtr _ -> "->"
        | _ -> "."
      in
      gen_expr buf ctx env target;
      Buffer.add_string buf sep;
      Buffer.add_string buf fname
  | Ast.Ref (e, _) ->
      Buffer.add_char buf '&';
      (match e with
       | Ast.Var _ | Ast.FieldAccess _ | Ast.Deref _ -> gen_expr buf ctx env e
       | _ ->
           Buffer.add_char buf '(';
           gen_expr buf ctx env e;
           Buffer.add_char buf ')')
  | Ast.Deref (e, _) ->
      Buffer.add_char buf '*';
      (match e with
       | Ast.Var _ | Ast.FieldAccess _ | Ast.Deref _ -> gen_expr buf ctx env e
       | _ ->
           Buffer.add_char buf '(';
           gen_expr buf ctx env e;
           Buffer.add_char buf ')')

(* Initialise an already-declared temp from an expression.  Tuple/struct
   literals become field-by-field assignments; any other RHS uses a single
   struct- or scalar-assignment (`__t = expr;`).  Brace initializers with
   non-constant elements are a C99 relaxation that `-ansi -pedantic`
   rejects, so we always go through declare-then-assign. *)
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
  | Ast.StructLit { fields; _ } ->
      List.iter
        (fun (fname, fe) ->
          Buffer.add_string buf indent;
          Buffer.add_string buf temp_name;
          Buffer.add_char buf '.';
          Buffer.add_string buf fname;
          Buffer.add_string buf " = ";
          gen_expr buf ctx env fe;
          Buffer.add_string buf ";\n")
        fields
  | Ast.New { tname; fields; pos } ->
      let s =
        match lookup_struct ctx tname with
        | Some s -> s
        | None ->
            Error.failf pos "unknown struct '%s'" (String.concat "::" tname)
      in
      let cname = "struct " ^ mangle_typ (TStruct s.sname_path) in
      Buffer.add_string buf indent;
      Buffer.add_string buf temp_name;
      Buffer.add_string buf (" = malloc(sizeof(" ^ cname ^ "));\n");
      List.iter
        (fun (fname, fe) ->
          Buffer.add_string buf indent;
          Buffer.add_string buf temp_name;
          Buffer.add_string buf "->";
          Buffer.add_string buf fname;
          Buffer.add_string buf " = ";
          gen_expr buf ctx env fe;
          Buffer.add_string buf ";\n")
        fields
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
      (* Reuse the temp-init pattern with `name` as the destination — this
         covers scalars (simple assignment), struct literals (field-by-field),
         and `new` expressions (malloc + field-by-field via `->`). *)
      emit_value_into_temp buf ctx env indent name value
  | Ast.LetTuple { names; value; _ } ->
      emit_let_tuple buf ctx env indent names value
  | Ast.AssignField { target; field; value; _ } ->
      let sep =
        match type_of ctx env target with
        | TPtr _ -> "->"
        | _ -> "."
      in
      Buffer.add_string buf indent;
      gen_expr buf ctx env target;
      Buffer.add_string buf sep;
      Buffer.add_string buf field;
      Buffer.add_string buf " = ";
      gen_expr buf ctx env value;
      Buffer.add_string buf ";\n"
  | Ast.AssignDeref { target; value; _ } ->
      Buffer.add_string buf indent;
      Buffer.add_char buf '*';
      (match target with
       | Ast.Var _ | Ast.FieldAccess _ -> gen_expr buf ctx env target
       | _ ->
           Buffer.add_char buf '(';
           gen_expr buf ctx env target;
           Buffer.add_char buf ')');
      Buffer.add_string buf " = ";
      gen_expr buf ctx env value;
      Buffer.add_string buf ";\n"
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
          all <> [] ||
          (match e with
           | Ast.TupleLit _ | Ast.StructLit _ | Ast.New _ -> true
           | _ -> false)
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
    | (Ast.Let _ | Ast.Assign _ | Ast.AssignField _ | Ast.AssignDeref _ | Ast.ExprStmt _) as s :: rest ->
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
let collect_tuple_types flat global structs modules =
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
     | Ast.StructLit { fields; _ } ->
         List.iter (fun (_, fe) -> walk_expr ctx env fe) fields
     | Ast.FieldAccess (target, _, _) -> walk_expr ctx env target
     | Ast.Ref (sub, _) | Ast.Deref (sub, _) -> walk_expr ctx env sub
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
    | Ast.AssignField { target; value; _ } ->
        walk_expr ctx env target; walk_expr ctx env value
    | Ast.AssignDeref { target; value; _ } ->
        walk_expr ctx env target; walk_expr ctx env value
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
      let ctx = { global; structs; modules; scope = path } in
      let param_env =
        List.map (fun (p : Ast.param) ->
          (p.pname, type_of_ann p.pty)) f.params
      in
      let lets = collect_lets ctx param_env f.body in
      let env = param_env @ lets in
      List.iter (walk_stmt ctx env) f.body)
    flat;
  List.rev !seen

(* Emit a `struct ex_Foo { int x; int y; };` for one user-declared struct.
   The C struct name is the mangled path-qualified form, identical to what
   `mangle_typ (TStruct path)` produces. *)
let emit_named_struct buf (path, (s : Ast.struct_decl)) =
  let cname = mangle (path : string list) s.sname in
  Buffer.add_string buf (Printf.sprintf "struct %s {" cname);
  List.iter
    (fun (fname, ann) ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf (c_decl (type_of_ann ann) fname);
      Buffer.add_char buf ';')
    s.sfields;
  Buffer.add_string buf " };\n"

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

(* Detect any heap usage in the program — `new ...` expressions or `free(p)`
   calls — so we can conditionally include `<stdlib.h>` (for `malloc`/`free`). *)
let uses_heap program flat =
  let rec walk_expr = function
    | Ast.New _ -> true
    | Ast.Call (["free"], _, _) -> true
    | Ast.Ref (e, _) | Ast.Deref (e, _) | Ast.Cast (e, _, _) | Ast.Neg e ->
        walk_expr e
    | Ast.BinOp (_, l, r) -> walk_expr l || walk_expr r
    | Ast.Call (_, args, _) -> List.exists walk_expr args
    | Ast.TupleLit (es, _) -> List.exists walk_expr es
    | Ast.StructLit { fields; _ } ->
        List.exists (fun (_, e) -> walk_expr e) fields
    | Ast.FieldAccess (target, _, _) -> walk_expr target
    | _ -> false
  in
  let rec walk_stmt = function
    | Ast.Let { value; _ } | Ast.LetTuple { value; _ }
    | Ast.Assign { value; _ } | Ast.Return (value, _)
    | Ast.ExprStmt value -> walk_expr value
    | Ast.AssignField { target; value; _ }
    | Ast.AssignDeref { target; value; _ } ->
        walk_expr target || walk_expr value
    | Ast.If { cond; then_body; else_body } ->
        walk_expr cond
        || List.exists walk_stmt then_body
        || List.exists walk_stmt else_body
    | Ast.While { cond; body } ->
        walk_expr cond || List.exists walk_stmt body
    | Ast.Defer { body; _ } -> List.exists walk_stmt body
  in
  let _ = program in  (* in case future passes need it *)
  List.exists
    (fun (_, (f : Ast.func), _) -> List.exists walk_stmt f.body)
    flat

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
  let struct_flat = flatten_structs program in
  let structs = build_struct_index struct_flat in
  let modules = flatten_modules program in
  let tuples = collect_tuple_types flat global structs modules in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n";
  if uses_heap program flat then
    Buffer.add_string buf "#include <stdlib.h>\n";
  (* Named structs first, in source order — typically their fields refer
     to types declared earlier.  Tuple structs after, so any tuple whose
     elements include a named struct type sees it complete. *)
  if struct_flat <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_named_struct buf) struct_flat
  end;
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
      let ctx = { global; structs; modules; scope = path } in
      gen_function buf ctx f mangled;
      if i < last then Buffer.add_char buf '\n')
    flat;
  Buffer.contents buf
