(* C89 emission.  Consumes the typed view exposed by Typecheck — every
   type, lookup, and validation lives there.  This module owns only the
   shape of the emitted text. *)

open Typecheck

(* When set, gen_program prepends `/* file:line:col */` markers above each
   emitted top-level statement and function signature.  Off by default;
   the CLI flips it via `--annotate` for debug builds where mapping the
   emitted C back to exile source matters (e.g. when defer cleanups or
   tuple-temp blocks make the layout less obvious). *)
let annotate_mode = ref false

let emit_ann buf indent (pos : Pos.t) =
  if !annotate_mode then begin
    Buffer.add_string buf indent;
    Buffer.add_string buf
      (Printf.sprintf "/* %s:%d:%d */\n" pos.file pos.line pos.col)
  end

let stmt_pos = function
  | Ast.Let { pos; _ } | Ast.LetTuple { pos; _ }
  | Ast.Assign { pos; _ } | Ast.AssignField { pos; _ }
  | Ast.AssignDeref { pos; _ } | Ast.Defer { pos; _ } -> pos
  | Ast.Return (_, pos) -> pos
  | Ast.ExprStmt e -> Ast.expr_pos e
  | Ast.If { cond; _ } | Ast.While { cond; _ } -> Ast.expr_pos cond

let emit_stmt_ann buf indent stmt = emit_ann buf indent (stmt_pos stmt)

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
  | TNullPtr ->
      (* TNullPtr never owns a declaration — it is the type of the literal
         `null`, always consumed under a concrete TPtr context. *)
      failwith "TNullPtr should never reach c_type_prefix"

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
      | TTuple _ | TStruct _ | TPtr _ | TNullPtr ->
          assert false  (* typecheck rejected this earlier *)
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

(* Forms that don't need parens after a unary `&`/`*` prefix because they
   already bind tighter than (or equal to) the prefix. *)
let lvalue_like = function
  | Ast.Var _ | Ast.FieldAccess _ | Ast.Deref _ -> true
  | _ -> false

(* `<indent><lhs> = <rhs>;\n` — captures the four-line pattern that
   peppers emit_value_into_temp and the assignment statements. *)
let emit_assign_line buf indent ~lhs ~emit_rhs =
  Buffer.add_string buf indent;
  Buffer.add_string buf lhs;
  Buffer.add_string buf " = ";
  emit_rhs ();
  Buffer.add_string buf ";\n"

let rec gen_expr buf ctx env = function
  | Ast.IntLit (n, _) -> Buffer.add_string buf (string_of_int n)
  | Ast.BoolLit (b, _) -> Buffer.add_string buf (if b then "1" else "0")
  | Ast.NullLit _ -> Buffer.add_string buf "((void *)0)"
  | Ast.StringLit (s, _) ->
      Buffer.add_char buf '"';
      Buffer.add_string buf (escape_c s);
      Buffer.add_char buf '"'
  | Ast.Var (name, _) -> Buffer.add_string buf name
  | Ast.Neg (e, _) ->
      emit_unary buf ctx env '-' e
        ~simple:(function Ast.IntLit _ | Ast.Var _ -> true | _ -> false)
  | Ast.Cast (e, ann, _) ->
      let trimmed = strip_trailing_space (c_type_prefix (type_of_ann ann)) in
      Buffer.add_string buf "((";
      Buffer.add_string buf trimmed;
      Buffer.add_string buf ")";
      gen_expr buf ctx env e;
      Buffer.add_char buf ')'
  | Ast.BinOp (op, l, r, _) ->
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
       | Ast.BinOp (lop, _, _, _) when prec lop < p ->
           Buffer.add_char buf '('; gen_expr buf ctx env l; Buffer.add_char buf ')'
       | _ -> gen_expr buf ctx env l);
      Buffer.add_string buf op_str;
      (match r with
       | Ast.BinOp (rop, _, _, _)
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
        "tuple literal cannot be used inline; bind it first with \
         'let t = (...)' (then pass t) or 'let (a, b) = (...)'"
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
  | Ast.Ref (e, _) -> emit_unary buf ctx env '&' e ~simple:lvalue_like
  | Ast.Deref (e, _) -> emit_unary buf ctx env '*' e ~simple:lvalue_like

and emit_unary buf ctx env prefix ~simple e =
  Buffer.add_char buf prefix;
  if simple e then gen_expr buf ctx env e
  else begin
    Buffer.add_char buf '(';
    gen_expr buf ctx env e;
    Buffer.add_char buf ')'
  end

(* Initialise an already-declared temp from an expression.  Tuple/struct
   literals become field-by-field assignments; any other RHS uses a single
   struct- or scalar-assignment (`__t = expr;`).  Brace initializers with
   non-constant elements are a C99 relaxation that `-ansi -pedantic`
   rejects, so we always go through declare-then-assign. *)
and emit_value_into_temp buf ctx env indent temp_name value =
  let assign ~lhs e =
    emit_assign_line buf indent ~lhs
      ~emit_rhs:(fun () -> gen_expr buf ctx env e)
  in
  match value with
  | Ast.TupleLit (es, _) ->
      List.iteri
        (fun i e -> assign ~lhs:(temp_name ^ "._" ^ string_of_int i) e)
        es
  | Ast.StructLit { fields; base; _ } ->
      (* `..base` (functional update): copy base via struct assignment
         first, then apply explicit field overrides.  C89 supports
         `temp = expr;` for struct-typed values. *)
      Option.iter (assign ~lhs:temp_name) base;
      List.iter (fun (fname, fe) -> assign ~lhs:(temp_name ^ "." ^ fname) fe)
        fields
  | Ast.New { tname; fields; base; pos } ->
      let s =
        match lookup_struct ctx tname with
        | Some s -> s
        | None ->
            Error.failf pos "unknown struct '%s'" (String.concat "::" tname)
      in
      let cname = "struct " ^ mangle_typ (TStruct s.sname_path) in
      emit_assign_line buf indent ~lhs:temp_name
        ~emit_rhs:(fun () ->
          Buffer.add_string buf "malloc(sizeof(";
          Buffer.add_string buf cname;
          Buffer.add_string buf "))");
      (* `..base` for heap allocation: deref-assign the whole struct from
         the value-typed base, then override individual fields through
         the `->` arrow. *)
      Option.iter (assign ~lhs:("*" ^ temp_name)) base;
      List.iter (fun (fname, fe) -> assign ~lhs:(temp_name ^ "->" ^ fname) fe)
        fields
  | _ -> assign ~lhs:temp_name value

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
      emit_unary buf ctx env '*' target
        ~simple:(function Ast.Var _ | Ast.FieldAccess _ -> true | _ -> false);
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
  let inner = indent ^ "    " in
  let trimmed = strip_trailing_space (c_type_prefix (type_of ctx env value)) in
  Buffer.add_string buf indent;
  Buffer.add_string buf "{\n";
  Buffer.add_string buf inner;
  Buffer.add_string buf trimmed;
  Buffer.add_string buf " __t;\n";
  emit_value_into_temp buf ctx env inner "__t" value;
  List.iteri
    (fun i name ->
      emit_assign_line buf inner ~lhs:name ~emit_rhs:(fun () ->
        Buffer.add_string buf "__t._";
        Buffer.add_string buf (string_of_int i)))
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
    | (Ast.Defer { body; _ } as s) :: rest ->
        emit_stmt_ann buf indent s;
        loop (body :: my_defers) rest
    | (Ast.Return (e, _) as s) :: _ ->
        emit_stmt_ann buf indent s;
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
        emit_stmt_ann buf indent s;
        emit_simple_stmt buf ctx env indent s;
        loop my_defers rest
    | (Ast.LetTuple { names; value; _ } as s) :: rest ->
        emit_stmt_ann buf indent s;
        emit_let_tuple buf ctx env indent names value;
        loop my_defers rest
    | (Ast.If { cond; then_body; else_body } as s) :: rest ->
        emit_stmt_ann buf indent s;
        Buffer.add_string buf indent;
        gen_if buf ctx env indent outer_scopes my_defers
          cond then_body else_body;
        loop my_defers rest
    | (Ast.While { cond; body } as s) :: rest ->
        emit_stmt_ann buf indent s;
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

(* Emit one already-validated function.  cf carries the resolved metadata
   (path, mangled name, hoisted lets) computed by Typecheck.check_program;
   ctx is built from the program-wide indexes. *)
let gen_function buf ctx (cf : checked_func) =
  let f = cf.cf_func in
  emit_ann buf "" f.pos;
  emit_fn_sig buf f cf.cf_mangled;
  Buffer.add_string buf " {\n";
  let param_env =
    List.map (fun p -> (p.Ast.pname, type_of_ann p.Ast.pty)) f.params
  in
  let full_env = param_env @ cf.cf_lets in
  List.iter
    (fun (name, t) ->
      Buffer.add_string buf (Printf.sprintf "    %s;\n" (c_decl t name)))
    cf.cf_lets;
  gen_block buf ctx full_env "    " [] f.body;
  if f.name = "main" then Buffer.add_string buf "    return 0;\n";
  Buffer.add_string buf "}\n"

(* Shared shape for both user-declared and tuple structs:
   `struct NAME { f1; f2; ... };\n`. *)
let emit_struct_decl buf cname fields =
  Buffer.add_string buf "struct ";
  Buffer.add_string buf cname;
  Buffer.add_string buf " {";
  List.iter
    (fun (ty, fname) ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf (c_decl ty fname);
      Buffer.add_char buf ';')
    fields;
  Buffer.add_string buf " };\n"

let emit_named_struct buf (path, (s : Ast.struct_decl)) =
  let cname = mangle path s.sname in
  let fields = List.map (fun (fname, ann) -> (type_of_ann ann, fname)) s.sfields in
  emit_struct_decl buf cname fields

let emit_tuple_struct buf (_, t) =
  match t with
  | TTuple ts ->
      let fields = List.mapi (fun i ty -> (ty, "_" ^ string_of_int i)) ts in
      emit_struct_decl buf (tuple_struct_name ts) fields
  | _ -> ()

let gen_program ?(annotate = false) (cp : checked_program) =
  annotate_mode := annotate;
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n";
  if cp.cp_uses_heap then
    Buffer.add_string buf "#include <stdlib.h>\n";
  (* Named structs first, in source order — typically their fields refer
     to types declared earlier.  Tuple structs after, so any tuple whose
     elements include a named struct type sees it complete. *)
  if cp.cp_struct_decls <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_named_struct buf) cp.cp_struct_decls
  end;
  if cp.cp_tuple_types <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_tuple_struct buf) cp.cp_tuple_types
  end;
  let non_main =
    List.filter (fun cf -> cf.cf_func.Ast.name <> "main") cp.cp_funcs
  in
  if non_main <> [] then begin
    Buffer.add_char buf '\n';
    List.iter
      (fun cf ->
        emit_fn_sig buf cf.cf_func cf.cf_mangled;
        Buffer.add_string buf ";\n")
      non_main
  end;
  Buffer.add_char buf '\n';
  let last = List.length cp.cp_funcs - 1 in
  List.iteri
    (fun i cf ->
      let ctx = {
        global = cp.cp_global;
        structs = cp.cp_struct_index;
        modules = cp.cp_modules;
        scope = cf.cf_path;
      } in
      gen_function buf ctx cf;
      if i < last then Buffer.add_char buf '\n')
    cp.cp_funcs;
  Buffer.contents buf
