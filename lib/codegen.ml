(* C89 emission.  Consumes the typed IR (`tprogram`) produced by Typecheck:
   every expression carries its computed type in `.ty`, so this module only
   pattern-matches on shape and emits — it does not reconstruct types. *)

open Ir

(* When set, gen_program prepends `/* file:line:col */` markers above each
   emitted top-level statement and function signature.  Off by default;
   the CLI flips it via `--annotate` for debug builds where mapping the
   emitted C back to exile source matters (e.g. when defer cleanups or
   tuple-temp blocks make the layout less obvious). *)
let annotate_mode = ref false

(* Match-arm bind decls need each variant's payload types to declare
   the right C type for each bound name.  The typed AST carries the
   variant tag and bind sub-patterns but not the payload types — we
   stash the program-level enum index here so `emit_match_stmt` can
   look them up.  Set by `gen_program` at the start of every run. *)
let enum_index_ref : enum_sig list ref = ref []

let emit_ann buf indent (pos : Pos.t) =
  if !annotate_mode then begin
    Buffer.add_string buf indent;
    Buffer.add_string buf
      (Printf.sprintf "/* %s:%d:%d */\n" pos.file pos.line pos.col)
  end

let tstmt_pos = function
  | TLet { pos; _ } | TLetTuple { pos; _ }
  | TAssign { pos; _ } | TAssignField { pos; _ }
  | TAssignDeref { pos; _ } | TDefer { pos; _ }
  | TReturn { pos; _ } -> pos
  | TExprStmt te -> te.pos
  | TIf { cond; _ } | TWhile { cond; _ } -> cond.pos

let emit_tstmt_ann buf indent stmt = emit_ann buf indent (tstmt_pos stmt)

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

(* The c_* family is intentionally width-variable: each maps to the
   target C compiler's native type (`int` / `short` / `long` / ...).
   On amiga-gcc m68k all of int/long are 32-bit; on a 64-bit Linux
   host `long` is 64-bit, `int` is 32-bit.  This is the whole point —
   `c_long` matches the platform's `long`, just like `long` in C
   means.  For fixed widths regardless of target use `i32`/`u32`
   etc. — those map to a guaranteed-≥32-bit type with consistent
   value semantics across targets. *)
let rec c_type_prefix = function
  | TCInt { signed = true } -> "int "
  | TCInt { signed = false } -> "unsigned int "
  | TCShort { signed = true } -> "short "
  | TCShort { signed = false } -> "unsigned short "
  | TCLong { signed = true } -> "long "
  | TCLong { signed = false } -> "unsigned long "
  | TCChar -> "char "
  | TCSChar -> "signed char "
  | TCUChar -> "unsigned char "
  | TCVoid ->
      failwith "internal: naked c_void reached c_type_prefix — only \
                *c_void is allowed; typecheck should have caught this"
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
  | TExtStruct n -> "struct " ^ n ^ " "
  | TExtAlias n -> n ^ " "
  | TEnum _ as t -> "struct " ^ mangle_typ t ^ " "
  | TPtr TCVoid -> "void *"
  | TPtr inner ->
      (* Pointer types render as `<base> *` with no trailing space, so
         `c_decl t name` produces `<base> *name`. *)
      strip_trailing_space (c_type_prefix inner) ^ " *"
  | TFnPtr _ as t ->
      (* Reference fn-ptr types by typedef alias (emitted up top by
         gen_program).  Avoids the awkward C "function returning
         function pointer" syntax at every use site. *)
      mangle_typ t ^ " "
  | TNullPtr ->
      (* TNullPtr never owns a declaration — it is the type of the literal
         `null`, always consumed under a concrete TPtr context. *)
      failwith "TNullPtr should never reach c_type_prefix"
  | TVar n ->
      failwith
        ("internal: TVar '" ^ n ^ "' reached c_type_prefix — \
          monomorphization missed an instantiation")

let c_decl t name = c_type_prefix t ^ name

(* Builtin emitters keyed by name.  Codegen-side companion to the typecheck
   `builtin_sig.bcheck` table.  Adding a new builtin needs an entry in both. *)
type builtin_emit =
  Buffer.t -> texpr list -> (texpr -> unit) -> unit

let emit_print : builtin_emit =
  fun buf args emit_arg ->
    (* Varargs promote i8/i16 to int, so `%d`/`%u` cover them with no cast.
       i32/u32 are emitted as `long`/`unsigned long` (for cross-C-compiler
       width stability) and need `%ld`/`%lu`.  Because `printf` is variadic
       it does not auto-promote `int` to `long` — an int literal like `0`
       passed under `%ld` is ill-formed under `-Wformat`.  We force the
       cast on the call site. *)
    let arg = List.hd args in
    let fmt = match arg.ty with
      | TBool -> "\"%d\\n\""
      | TInt { signed = true; width = Ast.W32 } -> "\"%ld\\n\""
      | TInt { signed = false; width = Ast.W32 } -> "\"%lu\\n\""
      | TInt { signed = true; _ } -> "\"%d\\n\""
      | TInt { signed = false; _ } -> "\"%u\\n\""
      | TCInt { signed = true } -> "\"%d\\n\""
      | TCInt { signed = false } -> "\"%u\\n\""
      | TCShort _ | TCLong _ | TCChar | TCSChar | TCUChar | TCVoid ->
          assert false  (* typecheck rejects print on these *)
      | TString -> "\"%s\\n\""
      | TTuple _ | TStruct _ | TExtStruct _ | TExtAlias _ | TEnum _
      | TPtr _ | TFnPtr _ | TNullPtr | TVar _ ->
          assert false  (* typecheck rejected this earlier *)
    in
    let cast =
      match arg.ty with
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
         emit_arg arg;
         Buffer.add_char buf ')'
     | None -> emit_arg arg);
    Buffer.add_char buf ')'

let emit_free : builtin_emit =
  fun buf args emit_arg ->
    Buffer.add_string buf "free(";
    emit_arg (List.hd args);
    Buffer.add_char buf ')'

let builtin_emitters : (string * builtin_emit) list = [
  ("print", emit_print);
  ("free", emit_free);
]

let lookup_builtin_emit name = List.assoc_opt name builtin_emitters

let prec = function
  | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq | Ast.EqEq | Ast.NotEq -> 0
  | Ast.Add | Ast.Sub -> 1
  | Ast.Mul | Ast.Div -> 2

(* Forms that don't need parens after a unary `&`/`*` prefix because they
   already bind tighter than (or equal to) the prefix. *)
let lvalue_like = function
  | TVar _ | TFieldAccess _ | TDeref _ -> true
  | _ -> false

(* `<indent><lhs> = <rhs>;\n` — captures the assignment-line pattern. *)
let emit_assign_line buf indent ~lhs ~emit_rhs =
  Buffer.add_string buf indent;
  Buffer.add_string buf lhs;
  Buffer.add_string buf " = ";
  emit_rhs ();
  Buffer.add_string buf ";\n"

let rec gen_expr buf (te : texpr) =
  match te.e with
  | TIntLit n -> Buffer.add_string buf (string_of_int n)
  | TBoolLit b -> Buffer.add_string buf (if b then "1" else "0")
  | TNullLit -> Buffer.add_string buf "((void *)0)"
  | TStringLit s ->
      Buffer.add_char buf '"';
      Buffer.add_string buf (escape_c s);
      Buffer.add_char buf '"'
  | TVar name -> Buffer.add_string buf name
  | TFnRef name ->
      (* Bare function name in expression context — C autoconverts
         to function pointer, no `&` needed. *)
      Buffer.add_string buf name
  | TNeg sub ->
      emit_unary buf '-' sub
        ~simple:(function TIntLit _ | TVar _ -> true | _ -> false)
  | TCast (sub, _ann) ->
      (* Cast result type is already in `te.ty` (elab ran resolve_type_ann
         on the annotation); reading it here keeps the C output independent
         of the raw Ast.type_ann the elaborator stashed. *)
      let trimmed = strip_trailing_space (c_type_prefix te.ty) in
      Buffer.add_string buf "((";
      Buffer.add_string buf trimmed;
      Buffer.add_string buf ")";
      gen_expr buf sub;
      Buffer.add_char buf ')'
  | TBinOp (op, l, r) ->
      let op_str =
        match op with
        | Ast.Add -> " + " | Ast.Sub -> " - "
        | Ast.Mul -> " * " | Ast.Div -> " / "
        | Ast.Lt -> " < " | Ast.Gt -> " > "
        | Ast.LtEq -> " <= " | Ast.GtEq -> " >= "
        | Ast.EqEq -> " == " | Ast.NotEq -> " != "
      in
      let p = prec op in
      (match l.e with
       | TBinOp (lop, _, _) when prec lop < p ->
           Buffer.add_char buf '('; gen_expr buf l; Buffer.add_char buf ')'
       | _ -> gen_expr buf l);
      Buffer.add_string buf op_str;
      (match r.e with
       | TBinOp (rop, _, _)
         when prec rop < p || (prec rop = p && (op = Ast.Sub || op = Ast.Div)) ->
           Buffer.add_char buf '('; gen_expr buf r; Buffer.add_char buf ')'
       | _ -> gen_expr buf r)
  | TBuiltinCall { name; args } ->
      let emit =
        match lookup_builtin_emit name with
        | Some emit -> emit
        | None -> assert false   (* typecheck dispatched a known builtin *)
      in
      emit buf args (fun te -> gen_expr buf te)
  | TCall { mangled; args } ->
      Buffer.add_string buf mangled;
      Buffer.add_char buf '(';
      add_separated buf ", " (gen_expr buf) args;
      Buffer.add_char buf ')'
  | TTupleLit _ | TStructLit _ | TNew _ ->
      (* The `lift_block_exprs` pass in typecheck rewrites every
         block-shaped expression (tuple/struct/new/enum lit, match)
         that appears in a sub-expression position into a `__lift_N`
         temp + preceding `TLet`, so by the time codegen runs no such
         node reaches `gen_expr`.  Top-level uses (let RHS, return,
         assign) are routed through `emit_value_into_temp`. *)
      assert false
  | TFieldAccess { target; field } ->
      (* Auto-deref pointer-to-struct via `->`; otherwise plain `.`. *)
      let sep = match target.ty with TPtr _ -> "->" | _ -> "." in
      gen_expr buf target;
      Buffer.add_string buf sep;
      Buffer.add_string buf field
  | TRef sub -> emit_unary buf '&' sub ~simple:(fun n -> lvalue_like n)
  | TDeref sub -> emit_unary buf '*' sub ~simple:(fun n -> lvalue_like n)
  | TEnumLit _ | TMatch _ ->
      (* Same as the block-shaped lit cases above — `lift_block_exprs`
         hoists these to `__lift_N` temps before codegen sees them. *)
      assert false

and emit_unary buf prefix ~simple (te : texpr) =
  Buffer.add_char buf prefix;
  if simple te.e then gen_expr buf te
  else begin
    Buffer.add_char buf '(';
    gen_expr buf te;
    Buffer.add_char buf ')'
  end

(* Initialise an already-declared temp from a typed expression.  Tuple/struct
   literals become field-by-field assignments; other RHS values use a single
   struct- or scalar-assignment.  Brace initializers with non-constant
   elements are a C99 relaxation that `-ansi -pedantic` rejects, so we
   always go through declare-then-assign. *)
let rec emit_value_into_temp buf indent temp_name (value : texpr) =
  let assign ~lhs (e : texpr) =
    emit_assign_line buf indent ~lhs
      ~emit_rhs:(fun () -> gen_expr buf e)
  in
  match value.e with
  | TTupleLit es ->
      List.iteri
        (fun i e -> assign ~lhs:(temp_name ^ "._" ^ string_of_int i) e)
        es
  | TStructLit { fields; base; _ } ->
      (* `..base` (functional update): copy base via struct assignment
         first, then apply explicit field overrides.  C89 supports
         `temp = expr;` for struct-typed values. *)
      Option.iter (assign ~lhs:temp_name) base;
      List.iter (fun (fname, fe) -> assign ~lhs:(temp_name ^ "." ^ fname) fe)
        fields
  | TNew { sname_path; fields; base } ->
      let cname = "struct " ^ mangle_typ (TStruct sname_path) in
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
  | TEnumLit { ename_path; variant; args; _ } ->
      let cname = mangle_typ (TEnum ename_path) in
      emit_assign_line buf indent ~lhs:(temp_name ^ ".tag")
        ~emit_rhs:(fun () ->
          Buffer.add_string buf cname;
          Buffer.add_char buf '_';
          Buffer.add_string buf variant);
      (* Each (field_name, value) pair writes through the per-variant
         union member.  Tuple variants carry synthetic `_0`/`_1`/...
         names; struct variants carry the user-given names — emission
         is uniform. *)
      List.iter
        (fun (fname, arg) ->
          assign
            ~lhs:(Printf.sprintf "%s.data.%s.%s" temp_name variant fname)
            arg)
        args
  | TMatch _ ->
      (* Match used as a value (let-RHS, return, ...) lowers to a
         switch whose every case assigns its arm result to the same
         temp.  See emit_match_stmt's `assign_to` mode. *)
      emit_match_stmt ~assign_to:temp_name buf indent value
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
and emit_simple_stmt buf indent stmt =
  match stmt with
  | TLet { name; value; _ } | TAssign { name; value; _ } ->
      emit_value_into_temp buf indent name value
  | TLetTuple { names; value; _ } ->
      emit_let_tuple buf indent names value
  | TAssignField { target; field; value; _ } ->
      let sep = match target.ty with TPtr _ -> "->" | _ -> "." in
      Buffer.add_string buf indent;
      gen_expr buf target;
      Buffer.add_string buf sep;
      Buffer.add_string buf field;
      Buffer.add_string buf " = ";
      gen_expr buf value;
      Buffer.add_string buf ";\n"
  | TAssignDeref { target; value; _ } ->
      Buffer.add_string buf indent;
      emit_unary buf '*' target
        ~simple:(function TVar _ | TFieldAccess _ -> true | _ -> false);
      Buffer.add_string buf " = ";
      gen_expr buf value;
      Buffer.add_string buf ";\n"
  | TExprStmt e ->
      (match e.e with
       | TMatch _ -> emit_match_stmt buf indent e
       | _ ->
           Buffer.add_string buf indent;
           gen_expr buf e;
           Buffer.add_string buf ";\n")
  | TIf { cond; then_body; else_body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "if (";
      gen_expr buf cond;
      Buffer.add_string buf ") {\n";
      List.iter (emit_simple_stmt buf (indent ^ "    ")) then_body;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}';
      (match else_body with
       | [] -> Buffer.add_char buf '\n'
       | _ ->
           Buffer.add_string buf " else {\n";
           List.iter (emit_simple_stmt buf (indent ^ "    ")) else_body;
           Buffer.add_string buf indent;
           Buffer.add_string buf "}\n")
  | TWhile { cond; body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "while (";
      gen_expr buf cond;
      Buffer.add_string buf ") {\n";
      List.iter (emit_simple_stmt buf (indent ^ "    ")) body;
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"
  | TDefer { pos; _ } ->
      Error.failf pos "'defer' inside a defer body is not supported"
  | TReturn { pos; _ } ->
      Error.failf pos "'return' inside a defer body is not supported"

(* Lower a TMatch.  Hoists the scrutinee into a fresh `__m` temp in a
   new C block and dispatches on its tag.  Each variant arm becomes a
   `case`; a wildcard or bare-bind pattern becomes `default:`; tuple
   and struct payloads bind via decls at the top of the case block.
   When `assign_to = None` each arm body is emitted as an
   expression-statement (the match's value is dropped); when
   `Some lhs` each arm body becomes `lhs = <body>;` so the surrounding
   context picks up the match's value.  Nested match in an arm body
   lowers recursively under the same `assign_to`.  An arm with
   `tdiverges = true` (produced by the `try` desugar) emits
   `return tbody;` instead of assign+break and ignores `assign_to` —
   the body is, by elab construction, a TEnumLit on the enclosing
   fn's return-type instance. *)
and emit_match_stmt ?assign_to buf indent (m_expr : texpr) =
  match m_expr.e with
  | TMatch { scrutinee; ename_path; arms } ->
      let inner = indent ^ "    " in
      let case_indent = inner ^ "    " in
      let body_indent = case_indent ^ "    " in
      let cname = mangle_typ (TEnum ename_path) in
      Buffer.add_string buf indent;
      Buffer.add_string buf "{\n";
      Buffer.add_string buf inner;
      Buffer.add_string buf (Printf.sprintf "struct %s __m;\n" cname);
      emit_value_into_temp buf inner "__m" scrutinee;
      Buffer.add_string buf inner;
      Buffer.add_string buf "switch (__m.tag) {\n";
      List.iter
        (fun (a : tmatch_arm) ->
          Buffer.add_string buf inner;
          (match a.tpat with
           | TPVariant { variant; _ } ->
               Buffer.add_string buf (Printf.sprintf "case %s_%s:\n"
                                        cname variant)
           | TPWildcard | TPVar _ ->
               Buffer.add_string buf "default:\n");
          (* Each arm body lives in its own `{}` block so bind decls
             stay scoped to the case (and so we don't leak C variable
             names across cases). *)
          Buffer.add_string buf case_indent;
          Buffer.add_string buf "{\n";
          (* Emit binds: PVar at top level binds the whole __m;
             PVariant binds extract from data.<variant>._<i>. *)
          (* C89 requires decls at the top of a block — emit each
             bind as a single decl-with-init line. *)
          (match a.tpat with
           | TPVar n ->
               Buffer.add_string buf body_indent;
               Buffer.add_string buf (c_decl m_expr.ty n);
               Buffer.add_string buf " = __m;\n"
           | TPVariant { variant; binds; _ } ->
               let v =
                 List.find (fun (vs : variant_sig) -> vs.vsname = variant)
                   (List.find
                      (fun (es : enum_sig) -> es.ename_path = ename_path)
                      !enum_index_ref).evariants
               in
               List.iter
                 (fun (fname, bp) ->
                   match bp with
                   | TPVar n ->
                       let ft = List.assoc fname v.vsfields in
                       Buffer.add_string buf body_indent;
                       Buffer.add_string buf (c_decl ft n);
                       Buffer.add_string buf
                         (Printf.sprintf " = __m.data.%s.%s;\n"
                            variant fname)
                   | _ -> ())
                 binds
           | TPWildcard -> ());
          if a.tdiverges then begin
            (* `try` desugar's Err/None arm: early-return the body
               from the enclosing fn.  By elab construction the body
               is a TEnumLit on the outer fn's ret-type instance — we
               assert that here so any future caller of `tdiverges`
               with a different shape (TMatch, TVar, etc) breaks
               loudly rather than emitting subtly broken C (mixing
               decls and stmts in the same case block, or skipping
               an `assign_to` assignment).
               Defer integration is intentionally absent: defers
               active above a `try` site will NOT run on the early-
               return path.  Programs that mix the two compile but
               leak whatever the defer would have cleaned up.  No
               check fires today; revisit when a real use case for
               `defer` + `try` together shows up. *)
            (match a.tbody.e with
             | TEnumLit _ -> ()
             | _ ->
                 failwith
                   "internal: tdiverges arm body must be a TEnumLit \
                    (only `try` desugar produces diverging arms today)");
            let trimmed = strip_trailing_space (c_type_prefix a.tbody.ty) in
            Buffer.add_string buf body_indent;
            Buffer.add_string buf trimmed;
            Buffer.add_string buf " __try_ret;\n";
            emit_value_into_temp buf body_indent "__try_ret" a.tbody;
            Buffer.add_string buf body_indent;
            Buffer.add_string buf "return __try_ret;\n"
          end else begin
            (match assign_to, a.tbody.e with
             | Some lhs, TMatch _ ->
                 emit_match_stmt ~assign_to:lhs buf body_indent a.tbody
             | Some lhs, _ ->
                 emit_assign_line buf body_indent ~lhs
                   ~emit_rhs:(fun () -> gen_expr buf a.tbody)
             | None, _ ->
                 Buffer.add_string buf body_indent;
                 gen_expr buf a.tbody;
                 Buffer.add_string buf ";\n");
            Buffer.add_string buf body_indent;
            Buffer.add_string buf "break;\n"
          end;
          Buffer.add_string buf case_indent;
          Buffer.add_string buf "}\n")
        arms;
      Buffer.add_string buf inner;
      Buffer.add_string buf "}\n";
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"
  | _ -> assert false

(* Destructuring binding: introduce an inner C block, declare a `__t` temp
   of the tuple struct type, fill it from the RHS, then assign each hoisted
   name from the temp's numbered field. *)
and emit_let_tuple buf indent names (value : texpr) =
  let inner = indent ^ "    " in
  let trimmed = strip_trailing_space (c_type_prefix value.ty) in
  Buffer.add_string buf indent;
  Buffer.add_string buf "{\n";
  Buffer.add_string buf inner;
  Buffer.add_string buf trimmed;
  Buffer.add_string buf " __t;\n";
  emit_value_into_temp buf inner "__t" value;
  List.iteri
    (fun i name ->
      emit_assign_line buf inner ~lhs:name ~emit_rhs:(fun () ->
        Buffer.add_string buf "__t._";
        Buffer.add_string buf (string_of_int i)))
    names;
  Buffer.add_string buf indent;
  Buffer.add_string buf "}\n"

let emit_cleanups buf indent defers =
  List.iter
    (fun body ->
      List.iter (fun s -> emit_simple_stmt buf indent s) body)
    defers

let rec gen_if buf indent outer_scopes my_defers
    cond then_body else_body =
  Buffer.add_string buf "if (";
  gen_expr buf cond;
  Buffer.add_string buf ") {\n";
  gen_block buf (indent ^ "    ")
    (my_defers :: outer_scopes) then_body;
  Buffer.add_string buf indent;
  Buffer.add_char buf '}';
  (match else_body with
   | [] -> Buffer.add_char buf '\n'
   | [ TIf { cond = ec; then_body = et; else_body = ee } ] ->
       Buffer.add_string buf " else ";
       gen_if buf indent outer_scopes my_defers ec et ee
   | _ ->
       Buffer.add_string buf " else {\n";
       gen_block buf (indent ^ "    ")
         (my_defers :: outer_scopes) else_body;
       Buffer.add_string buf indent;
       Buffer.add_string buf "}\n")

and gen_block buf indent outer_scopes stmts =
  let rec loop my_defers = function
    | [] ->
        emit_cleanups buf indent my_defers
    | (TDefer { body; _ } as s) :: rest ->
        emit_tstmt_ann buf indent s;
        loop (body :: my_defers) rest
    | (TReturn { value; _ } as s) :: _ ->
        emit_tstmt_ann buf indent s;
        let all = List.flatten (my_defers :: outer_scopes) in
        let needs_block =
          all <> [] ||
          (match value.e with
           | TTupleLit _ | TStructLit _ | TNew _ | TMatch _ | TEnumLit _ ->
               true
           | _ -> false)
        in
        if not needs_block then begin
          Buffer.add_string buf indent;
          Buffer.add_string buf "return ";
          gen_expr buf value;
          Buffer.add_string buf ";\n"
        end else begin
          let trimmed = strip_trailing_space (c_type_prefix value.ty) in
          Buffer.add_string buf indent;
          Buffer.add_string buf "{\n";
          Buffer.add_string buf (indent ^ "    ");
          Buffer.add_string buf trimmed;
          Buffer.add_string buf " __exile_ret;\n";
          emit_value_into_temp buf (indent ^ "    ") "__exile_ret" value;
          emit_cleanups buf (indent ^ "    ") all;
          Buffer.add_string buf (indent ^ "    ");
          Buffer.add_string buf "return __exile_ret;\n";
          Buffer.add_string buf indent;
          Buffer.add_string buf "}\n"
        end
    | (TLet _ | TAssign _ | TAssignField _ | TAssignDeref _ | TExprStmt _) as s :: rest ->
        emit_tstmt_ann buf indent s;
        emit_simple_stmt buf indent s;
        loop my_defers rest
    | (TLetTuple { names; value; _ } as s) :: rest ->
        emit_tstmt_ann buf indent s;
        emit_let_tuple buf indent names value;
        loop my_defers rest
    | (TIf { cond; then_body; else_body } as s) :: rest ->
        emit_tstmt_ann buf indent s;
        Buffer.add_string buf indent;
        gen_if buf indent outer_scopes my_defers
          cond then_body else_body;
        loop my_defers rest
    | (TWhile { cond; body } as s) :: rest ->
        emit_tstmt_ann buf indent s;
        Buffer.add_string buf indent;
        Buffer.add_string buf "while (";
        gen_expr buf cond;
        Buffer.add_string buf ") {\n";
        gen_block buf (indent ^ "    ")
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
let emit_fn_sig buf (tf : tfunc) =
  let f = tf.tf_func in
  if f.name = "main" then
    Buffer.add_string buf "int main(void)"
  else begin
    if f.is_extern then Buffer.add_string buf "extern "
    else if not f.is_pub then Buffer.add_string buf "static ";
    let ret =
      match tf.tf_ret_ty with
      | None -> "void "
      | Some ty -> c_type_prefix ty
    in
    Buffer.add_string buf ret;
    Buffer.add_string buf tf.tf_mangled;
    Buffer.add_char buf '(';
    (match f.params, tf.tf_param_tys with
     | [], _ -> Buffer.add_string buf "void"
     | params, tys ->
         let zipped = List.combine params tys in
         add_separated buf ", "
           (fun ((p : Ast.param), t) ->
             Buffer.add_string buf (c_decl t p.pname))
           zipped);
    if f.is_variadic then Buffer.add_string buf ", ...";
    Buffer.add_char buf ')'
  end

(* Emit one already-elaborated function.  tf carries the typed body, the
   resolved C name, and the hoisted let-decl list. *)
let gen_function buf (tf : tfunc) =
  let f = tf.tf_func in
  emit_ann buf "" f.pos;
  emit_fn_sig buf tf;
  Buffer.add_string buf " {\n";
  List.iter
    (fun (name, t) ->
      Buffer.add_string buf (Printf.sprintf "    %s;\n" (c_decl t name)))
    tf.tf_lets;
  gen_block buf "    " [] tf.tf_body;
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

let emit_named_struct buf (s : struct_sig) =
  (* Field types come pre-resolved from the typecheck pass — relative
     `TyStruct` paths in the source were rewritten to absolute, so the
     C name we synthesize here matches what `c_type_prefix` produces
     for values of the same struct elsewhere. *)
  let cname = mangle_typ (TStruct s.sname_path) in
  let fields = List.map (fun (fname, ty) -> (ty, fname)) s.sfields_ty in
  emit_struct_decl buf cname fields

let emit_tuple_struct buf (_, t) =
  match t with
  | TTuple ts ->
      let fields = List.mapi (fun i ty -> (ty, "_" ^ string_of_int i)) ts in
      emit_struct_decl buf (tuple_struct_name ts) fields
  | _ -> ()

(* Enum lowering.  Tag enum + a wrapper struct.  When at least one
   variant carries payload, the struct also gets a `union data` whose
   members are per-variant inner structs (`struct { T _0; T _1; } V`).
   Unit variants don't appear in the union — C89 forbids empty
   structs.  When all variants are unit, no `data` field is emitted. *)
let emit_named_enum buf (e : enum_sig) =
  let cname = mangle_typ (TEnum e.ename_path) in
  Buffer.add_string buf (Printf.sprintf "enum %s_tag {" cname);
  List.iteri
    (fun i (vs : variant_sig) ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf ' ';
      Buffer.add_string buf cname;
      Buffer.add_char buf '_';
      Buffer.add_string buf vs.vsname)
    e.evariants;
  Buffer.add_string buf " };\n";
  let has_payload =
    List.exists (fun (vs : variant_sig) -> vs.vsfields <> []) e.evariants
  in
  Buffer.add_string buf
    (Printf.sprintf "struct %s { enum %s_tag tag;" cname cname);
  if has_payload then begin
    Buffer.add_string buf " union {";
    List.iter
      (fun (vs : variant_sig) ->
        if vs.vsfields <> [] then begin
          Buffer.add_string buf " struct {";
          List.iter
            (fun (fname, ty) ->
              Buffer.add_char buf ' ';
              Buffer.add_string buf (c_decl ty fname);
              Buffer.add_char buf ';')
            vs.vsfields;
          Buffer.add_string buf " } ";
          Buffer.add_string buf vs.vsname;
          Buffer.add_char buf ';'
        end)
      e.evariants;
    Buffer.add_string buf " } data;"
  end;
  Buffer.add_string buf " };\n"

let gen_program ?(annotate = false) (tp : tprogram) =
  annotate_mode := annotate;
  enum_index_ref := tp.tp_enum_index;
  let buf = Buffer.create 256 in
  Buffer.add_string buf "#include <stdio.h>\n";
  if tp.tp_uses_heap then
    Buffer.add_string buf "#include <stdlib.h>\n";
  (* User-supplied `@c_include("...")` lines.  Quoted form so paths
     with `/` work (`exec/exec.h`, `proto/intuition.h`).  C accepts
     redeclaration of stdio symbols already declared via stdio.h, so
     order vs our forward decls doesn't matter as long as types match. *)
  List.iter (fun path ->
    Buffer.add_string buf "#include \"";
    Buffer.add_string buf path;
    Buffer.add_string buf "\"\n")
    tp.tp_c_includes;
  if tp.tp_ext_consts <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (fun (name, t) ->
      Buffer.add_string buf "extern const ";
      Buffer.add_string buf (c_decl t name);
      Buffer.add_string buf ";\n")
      tp.tp_ext_consts
  end;
  (* Named structs first, in source order — typically their fields refer
     to types declared earlier.  Tuple structs after, so any tuple whose
     elements include a named struct type sees it complete.
     Generic decls (those carrying TVar in any field) are skipped here:
     the monomorphizer is responsible for substituting concrete types
     into a fresh decl per use, and only those concrete instances are
     emitted. *)
  let concrete_structs =
    List.filter (fun s ->
      List.for_all (fun (_, ty) -> is_concrete ty) s.sfields_ty)
      tp.tp_struct_index
  in
  let concrete_enums =
    List.filter (fun e ->
      List.for_all (fun vs ->
        List.for_all (fun (_, ty) -> is_concrete ty) vs.vsfields)
        e.evariants)
      tp.tp_enum_index
  in
  if concrete_structs <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_named_struct buf) concrete_structs
  end;
  if concrete_enums <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_named_enum buf) concrete_enums
  end;
  if tp.tp_tuple_types <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_tuple_struct buf) tp.tp_tuple_types
  end;
  (* Typedef aliases for every fn-pointer type used in the program.
     Use sites then refer to the alias name, sidestepping C's
     awkward fn-ptr declaration syntax (especially nasty when
     fn-ptr is itself a return type). *)
  if tp.tp_fnptr_types <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (fun (name, t) ->
      match t with
      | TFnPtr { params; ret } ->
          let r = match ret with
            | None -> "void "
            | Some t -> c_type_prefix t
          in
          let ps = match params with
            | [] -> "void"
            | _ -> String.concat ", "
                     (List.map (fun t ->
                        strip_trailing_space (c_type_prefix t)) params)
          in
          Buffer.add_string buf
            (Printf.sprintf "typedef %s(*%s)(%s);\n" r name ps)
      | _ -> assert false)
      tp.tp_fnptr_types
  end;
  (* Generic fns (with TVar in their resolved signature) skip codegen
     for the same reason as generic struct/enum decls — the
     monomorphizer materialises concrete instantiations later. *)
  let concrete_funcs =
    List.filter (fun tf ->
      List.for_all is_concrete tf.tf_param_tys
      && (match tf.tf_ret_ty with
          | Some t -> is_concrete t
          | None -> true))
      tp.tp_funcs
  in
  let non_main =
    List.filter (fun tf -> tf.tf_func.Ast.name <> "main") concrete_funcs
  in
  if non_main <> [] then begin
    Buffer.add_char buf '\n';
    List.iter
      (fun tf ->
        emit_fn_sig buf tf;
        Buffer.add_string buf ";\n")
      non_main
  end;
  Buffer.add_char buf '\n';
  (* extern fn has no body — fwd-decl above is the entire emission. *)
  let definable = List.filter (fun tf -> not tf.tf_func.Ast.is_extern) concrete_funcs in
  let last = List.length definable - 1 in
  List.iteri
    (fun i tf ->
      gen_function buf tf;
      if i < last then Buffer.add_char buf '\n')
    definable;
  Buffer.contents buf
