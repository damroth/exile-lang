(* C89 emission.  Consumes the typed IR (`tprogram`) produced by Typecheck:
   every expression carries its computed type in `.ty`, so this module only
   pattern-matches on shape and emits — it does not reconstruct types. *)

open Ir

(* Per-run codegen context.  Threaded through every emit/gen function
   as the first argument; replaces what used to be a set of module-
   level `ref`s holding "annotate?", "enum index", "active defer
   chain" and "bloat accumulator".  Two scopes:

   - Immutable inputs: [annotate] (flips file:line:col markers above
     emitted stmts) and [enum_index] (variant payload types needed
     by match-arm bind decls).  Set once at gen_program entry.
   - Mutable in-flight state: [defer_chain] (innermost block first,
     then enclosing scopes — gen_block push/pops as it walks; the
     diverging arm of `try` reads it to flush cleanups before its
     early return) and [bloat] (per-function byte counts collected
     by gen_function).

   Per-call lifetime kills the cross-test state-leak the global refs
   had.  [last_bloat] still reads from a tiny module cache populated
   at gen_program exit, for API compat with the CLI's
   `--bloat-report`. *)
type gen_ctx = {
  annotate : bool;
  enum_index : enum_sig list;
  struct_index : struct_sig list;
  mutable defer_chain : tstmt list list list;
  mutable bloat : (string * int) list;
}

let new_gen_ctx ~annotate ~enum_index ~struct_index =
  { annotate; enum_index; struct_index;
    defer_chain = []; bloat = [] }

(* Bloat snapshot from the most recent gen_program run.  Read by
   bin/main.ml when `--bloat-report` is set, and by tests.  Persisted
   in a module ref purely because [gen_program] returns just the
   string; if the API ever changes to return a result record this
   cache can go. *)
let last_bloat_cache : (string * int) list ref = ref []

let last_bloat () = List.rev !last_bloat_cache

let emit_ann ctx buf indent (pos : Pos.t) =
  if ctx.annotate then begin
    Buffer.add_string buf indent;
    Buffer.add_string buf
      (Printf.sprintf "/* %s:%d:%d */\n" pos.file pos.line pos.col)
  end

let tstmt_pos = function
  | TLet { pos; _ } | TLetTuple { pos; _ }
  | TAssign { pos; _ } | TAssignField { pos; _ }
  | TAssignIndex { pos; _ }
  | TAssignDeref { pos; _ } | TDefer { pos; _ }
  | TReturn { pos; _ } | TFor { pos; _ } -> pos
  | TExprStmt te -> te.pos
  | TIf { cond; _ } | TWhile { cond; _ } -> cond.pos

let emit_tstmt_ann ctx buf indent stmt = emit_ann ctx buf indent (tstmt_pos stmt)

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

(* `strip_trailing_space`, `c_type_prefix`, `c_decl` live in Ir now so the
   const-expr evaluator can spell types (for `size_of(T)` folding). *)

(* Builtin emitters keyed by name.  Codegen-side companion to the typecheck
   `builtin_sig.bcheck` table.  Adding a new builtin needs an entry in both. *)
type builtin_emit =
  gen_ctx -> Buffer.t -> texpr list -> (texpr -> unit) -> unit

(* Shared printf encoding for the integer family.  Variadic printf
   promotes i8/i16 to int, so `%d`/`%u` cover them with no cast.  i32/u32
   are emitted as `long`/`unsigned long` for cross-C-compiler width
   stability — variadic printf does *not* auto-promote int→long, so
   `%ld` needs the explicit cast at the call site (otherwise `-Wformat`
   fires on literals like `0`).

   Returns (format-spec, optional cast type-name) for int-printable types,
   None otherwise.  Used by both `emit_print` (top-level print) and
   `emit_field_debug` (nested @debug fields). *)
let printf_int_spec = function
  | TInt { signed = true; width = Ast.W32 } -> Some ("%ld", Some "long")
  | TInt { signed = false; width = Ast.W32 } -> Some ("%lu", Some "unsigned long")
  | TInt { signed = true; _ } -> Some ("%d", None)
  | TInt { signed = false; _ } -> Some ("%u", None)
  | TCInt { signed = true } -> Some ("%d", None)
  | TCInt { signed = false } -> Some ("%u", None)
  | _ -> None

(* `print` (no newline) and `println` (trailing '\n') share emission;
   [newline] picks the variant.  For `@debug` aggregates the synthesized
   printer emits the value, and println appends a separate `printf("\n")`. *)
let emit_print_impl ~newline buf args emit_arg =
    let nl = if newline then "\\n" else "" in
    let arg = List.hd args in
    match arg.ty with
    | TStruct _ | TEnum _ ->
        (* `@debug` aggregate — call the synthesized printer.  Typecheck
           rejects non-debug aggregates so anything reaching here has
           had its printer emitted up top. *)
        let fn = mangle_typ arg.ty ^ "__debug" in
        Buffer.add_string buf fn;
        Buffer.add_char buf '(';
        emit_arg arg;
        Buffer.add_char buf ')';
        if newline then Buffer.add_string buf "; printf(\"\\n\")"
    | _ ->
      let (fmt, cast) = match arg.ty with
        | TBool -> ("%d", None)
        | TString -> ("%s", None)
        | _ ->
            (match printf_int_spec arg.ty with
             | Some spec -> spec
             | None -> assert false  (* typecheck rejected this earlier *))
      in
      Printf.bprintf buf "printf(\"%s%s\", " fmt nl;
      (match cast with
       | Some c ->
           Printf.bprintf buf "(%s)(" c;
           emit_arg arg;
           Buffer.add_char buf ')'
       | None -> emit_arg arg);
      Buffer.add_char buf ')'

let emit_print : builtin_emit =
  fun _ctx buf args emit_arg -> emit_print_impl ~newline:false buf args emit_arg

let emit_println : builtin_emit =
  fun _ctx buf args emit_arg -> emit_print_impl ~newline:true buf args emit_arg

let emit_free : builtin_emit =
  fun _ctx buf args emit_arg ->
    Buffer.add_string buf "free(";
    emit_arg (List.hd args);
    Buffer.add_char buf ')'

(* `type_name(expr)` lowers to a `const char *` string literal of the
   arg's user-facing type name (rendered by `Ir.render_typ_user_facing`).
   The arg's value is not consumed — `sizeof` references it without
   evaluating (no side effects in C89, no VLAs in our backend) so cc
   still counts every variable as used. *)
let emit_type_name : builtin_emit =
  fun ctx buf args emit_arg ->
    let arg = List.hd args in
    Buffer.add_string buf "((void)sizeof(";
    emit_arg arg;
    Buffer.add_string buf "), \"";
    Buffer.add_string buf (escape_c
      (render_typ_user_facing ~structs:ctx.struct_index
         ~enums:ctx.enum_index arg.ty));
    Buffer.add_string buf "\")"

let builtin_emitters : (string * builtin_emit) list = [
  ("print", emit_print);
  ("println", emit_println);
  ("free", emit_free);
  ("type_name", emit_type_name);
]

let lookup_builtin_emit name = List.assoc_opt name builtin_emitters

(* Precedence levels of the *C* target (higher binds tighter), NOT exile's.
   The emitted parens (added below when a child binds looser than its
   parent) must reproduce the exile AST under C's rules — and exile's
   bitwise precedence is Rust-order (`& ^ |` above comparisons) which
   differs from C.  Pinning [prec] to C's ladder makes the minimal-paren
   logic emit exactly the parens that force C to match the AST. *)
let prec = function
  | Ast.Mul | Ast.Div | Ast.Mod -> 10
  | Ast.Add | Ast.Sub -> 9
  | Ast.Shl | Ast.Shr -> 8
  | Ast.Lt | Ast.Gt | Ast.LtEq | Ast.GtEq -> 7
  | Ast.EqEq | Ast.NotEq -> 6
  | Ast.BitAnd -> 5
  | Ast.BitXor -> 4
  | Ast.BitOr -> 3
  | Ast.And -> 2
  | Ast.Or -> 1
  | Ast.Concat ->
      (* Folded to a TStringLit during typecheck — no TBinOp(Concat) ever
         reaches codegen. *)
      assert false

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

(* Fresh loop-counter names for `[v; N]` fill loops; bumped per emission so
   nested repeats don't collide. *)
let fill_counter = ref 0
let match_label_counter = ref 0

let rec gen_expr ctx buf (te : texpr) =
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
      emit_unary ctx buf '-' sub
        ~simple:(function TIntLit _ | TVar _ -> true | _ -> false)
  | TBitNot sub ->
      emit_unary ctx buf '~' sub
        ~simple:(function TIntLit _ | TVar _ -> true | _ -> false)
  | TCast (sub, _ann) ->
      (* Cast result type is already in `te.ty` (elab ran resolve_type_ann
         on the annotation); reading it here keeps the C output independent
         of the raw Ast.type_ann the elaborator stashed. *)
      let trimmed = strip_trailing_space (c_type_prefix te.ty) in
      Buffer.add_string buf "((";
      Buffer.add_string buf trimmed;
      Buffer.add_string buf ")";
      gen_expr ctx buf sub;
      Buffer.add_char buf ')'
  | TBinOp (op, l, r) ->
      let op_str =
        match op with
        | Ast.Add -> " + " | Ast.Sub -> " - "
        | Ast.Mul -> " * " | Ast.Div -> " / " | Ast.Mod -> " % "
        | Ast.BitAnd -> " & " | Ast.BitOr -> " | " | Ast.BitXor -> " ^ "
        | Ast.Shl -> " << " | Ast.Shr -> " >> "
        | Ast.And -> " && " | Ast.Or -> " || "
        | Ast.Lt -> " < " | Ast.Gt -> " > "
        | Ast.LtEq -> " <= " | Ast.GtEq -> " >= "
        | Ast.EqEq -> " == " | Ast.NotEq -> " != "
        | Ast.Concat -> assert false   (* folded at typecheck time *)
      in
      let p = prec op in
      (* Left-associative operators where a same-precedence right operand
         changes meaning without parens (`a - (b - c)`, `a / (b / c)`,
         `a << (b << c)`).  Commutative/associative ones (`& ^ |`, `+`,
         `*`) are exempt — no parens needed. *)
      let left_assoc = function
        | Ast.Sub | Ast.Div | Ast.Mod | Ast.Shl | Ast.Shr -> true
        | _ -> false
      in
      (* `&&` under `||` is legal C but gcc/clang emit `-Wparentheses` even
         though the precedence is unambiguous.  Force parens around `&&`
         operands of `||` to keep the output -Werror-clean. *)
      let nest_warn child =
        match op, child with Ast.Or, Ast.And -> true | _ -> false
      in
      (match l.e with
       | TBinOp (lop, _, _) when prec lop < p || nest_warn lop ->
           Buffer.add_char buf '('; gen_expr ctx buf l; Buffer.add_char buf ')'
       | _ -> gen_expr ctx buf l);
      Buffer.add_string buf op_str;
      (match r.e with
       | TBinOp (rop, _, _)
         when prec rop < p || (prec rop = p && left_assoc op) || nest_warn rop ->
           Buffer.add_char buf '('; gen_expr ctx buf r; Buffer.add_char buf ')'
       | _ -> gen_expr ctx buf r)
  | TBuiltinCall { name; args } ->
      let emit =
        match lookup_builtin_emit name with
        | Some emit -> emit
        | None -> assert false   (* typecheck dispatched a known builtin *)
      in
      emit ctx buf args (fun te -> gen_expr ctx buf te)
  | TCall { mangled; args } ->
      Buffer.add_string buf mangled;
      Buffer.add_char buf '(';
      add_separated buf ", " (gen_expr ctx buf) args;
      Buffer.add_char buf ')'
  | TIndirectCall { fn_expr; args } ->
      Buffer.add_char buf '(';
      gen_expr ctx buf fn_expr;
      Buffer.add_string buf ")(";
      add_separated buf ", " (gen_expr ctx buf) args;
      Buffer.add_char buf ')'
  | TTupleLit _ | TStructLit _ | TNew _ ->
      (* The `lift_block_exprs` pass in typecheck rewrites every
         block-shaped expression (tuple/struct/new/enum lit, match)
         that appears in a sub-expression position into a `__lift_N`
         temp + preceding `TLet`, so by the time codegen runs no such
         node reaches `gen_expr`.  Top-level uses (let RHS, return,
         assign) are routed through `emit_value_into_temp`. *)
      assert false
  | TIndex { base; index } ->
      (* `a[i]` -> `a.data[i]` (the wrapper struct's array member).  A base
         that isn't already a tight lvalue (a deref, say) needs parens so
         `.data` binds to the whole base — deref-then-index, not
         index-then-deref. *)
      (match base.e with
       | TVar _ | TFieldAccess _ | TIndex _ -> gen_expr ctx buf base
       | _ ->
           Buffer.add_char buf '('; gen_expr ctx buf base; Buffer.add_char buf ')');
      Buffer.add_string buf ".data[";
      gen_expr ctx buf index;
      Buffer.add_char buf ']'
  | TFieldAccess { target; field } ->
      (* Auto-deref pointer-to-struct via `->`; otherwise plain `.`. *)
      let sep = match target.ty with TPtr _ -> "->" | _ -> "." in
      gen_expr ctx buf target;
      Buffer.add_string buf sep;
      Buffer.add_string buf field
  | TRef sub -> emit_unary ctx buf '&' sub ~simple:(fun n -> lvalue_like n)
  | TDeref sub -> emit_unary ctx buf '*' sub ~simple:(fun n -> lvalue_like n)
  | TSizeOf t ->
      Buffer.add_string buf "sizeof(";
      Buffer.add_string buf (strip_trailing_space (c_type_prefix t));
      Buffer.add_char buf ')'
  | TEnumLit _ | TMatch _ | TIfExpr _ | TArrayLit _ | TArrayRepeat _ ->
      (* Same as the block-shaped lit cases above — `lift_block_exprs`
         hoists these to `__lift_N` temps before codegen sees them. *)
      assert false
  | TBlock _ ->
      (* `{ stmts; trailing }` lives only at the arm-tbody position;
         `emit_arm_result` handles it before gen_expr is reached. *)
      assert false

and emit_unary ctx buf prefix ~simple (te : texpr) =
  Buffer.add_char buf prefix;
  if simple te.e then gen_expr ctx buf te
  else begin
    Buffer.add_char buf '(';
    gen_expr ctx buf te;
    Buffer.add_char buf ')'
  end

(* Initialise an already-declared temp from a typed expression.  Tuple/struct
   literals become field-by-field assignments; other RHS values use a single
   struct- or scalar-assignment.  Brace initializers with non-constant
   elements are a C99 relaxation that `-ansi -pedantic` rejects, so we
   always go through declare-then-assign. *)
let rec emit_value_into_temp ctx buf indent temp_name (value : texpr) =
  let assign ~lhs (e : texpr) =
    emit_assign_line buf indent ~lhs
      ~emit_rhs:(fun () -> gen_expr ctx buf e)
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
      emit_match_stmt ctx ~assign_to:temp_name buf indent value
  | TIfExpr { cond; then_val; else_val } ->
      (* `if` used as a value: each branch assigns its result to the same
         temp.  Branch values may themselves be block-shaped (nested
         if/match), so recurse through emit_value_into_temp. *)
      Buffer.add_string buf indent;
      Buffer.add_string buf "if (";
      gen_expr ctx buf cond;
      Buffer.add_string buf ") {\n";
      emit_value_into_temp ctx buf (indent ^ "    ") temp_name then_val;
      Buffer.add_string buf indent;
      Buffer.add_string buf "} else {\n";
      emit_value_into_temp ctx buf (indent ^ "    ") temp_name else_val;
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"
  | TArrayLit es ->
      (* `[e1, e2, ...]` — element-by-element into the wrapper's `data`
         member.  C89 forbids non-constant compound literals, so we always
         declare-then-fill. *)
      List.iteri
        (fun i e -> assign ~lhs:(Printf.sprintf "%s.data[%d]" temp_name i) e)
        es
  | TArrayRepeat { value; count } ->
      (* `[v; N]` — fill loop.  v is re-evaluated each iteration (matches the
         "evaluated at execution" semantics of the rest of the language). *)
      let i = !fill_counter in
      incr fill_counter;
      let iv = Printf.sprintf "__af%d" i in
      Buffer.add_string buf indent;
      Buffer.add_string buf "{\n";
      let inner = indent ^ "    " in
      Buffer.add_string buf inner;
      Buffer.add_string buf (Printf.sprintf "int %s;\n" iv);
      Buffer.add_string buf inner;
      Buffer.add_string buf
        (Printf.sprintf "for (%s = 0; %s < %d; %s = %s + 1) {\n"
           iv iv count iv iv);
      emit_value_into_temp ctx buf (inner ^ "    ")
        (Printf.sprintf "%s.data[%s]" temp_name iv) value;
      Buffer.add_string buf inner;
      Buffer.add_string buf "}\n";
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"
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
and emit_simple_stmt ctx buf indent stmt =
  match stmt with
  | TLet { name; value; _ } ->
      emit_value_into_temp ctx buf indent name value
  | TAssign { path; value; _ } ->
      (* Single-segment path → bare local name; multi-segment qualifies
         an `extern var` whose C symbol is the LAST segment (extern uses
         raw c_name regardless of module path). *)
      let lhs = match List.rev path with
        | n :: _ -> n
        | [] -> assert false
      in
      emit_value_into_temp ctx buf indent lhs value
  | TLetTuple { names; value; _ } ->
      emit_let_tuple ctx buf indent names value
  | TAssignField { target; field; value; _ } ->
      let sep = match target.ty with TPtr _ -> "->" | _ -> "." in
      Buffer.add_string buf indent;
      gen_expr ctx buf target;
      Buffer.add_string buf sep;
      Buffer.add_string buf field;
      Buffer.add_string buf " = ";
      gen_expr ctx buf value;
      Buffer.add_string buf ";\n"
  | TAssignDeref { target; value; _ } ->
      Buffer.add_string buf indent;
      emit_unary ctx buf '*' target
        ~simple:(function TVar _ | TFieldAccess _ -> true | _ -> false);
      Buffer.add_string buf " = ";
      gen_expr ctx buf value;
      Buffer.add_string buf ";\n"
  | TAssignIndex { base; index; value; _ } ->
      (* `a[i] = value` -> `a.data[i] = value;`.  Route through
         emit_value_into_temp so a block-shaped element value (struct,
         nested array, ...) lowers field-by-field into the element lvalue. *)
      let lhs =
        let b = Buffer.create 32 in
        gen_expr ctx b base;
        Buffer.add_string b ".data[";
        gen_expr ctx b index;
        Buffer.add_char b ']';
        Buffer.contents b
      in
      emit_value_into_temp ctx buf indent lhs value
  | TExprStmt e ->
      (match e.e with
       | TMatch _ -> emit_match_stmt ctx buf indent e
       | _ ->
           Buffer.add_string buf indent;
           gen_expr ctx buf e;
           Buffer.add_string buf ";\n")
  | TIf { cond; then_body; else_body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "if (";
      gen_expr ctx buf cond;
      Buffer.add_string buf ") {\n";
      List.iter (emit_simple_stmt ctx buf (indent ^ "    ")) then_body;
      Buffer.add_string buf indent;
      Buffer.add_char buf '}';
      (match else_body with
       | [] -> Buffer.add_char buf '\n'
       | _ ->
           Buffer.add_string buf " else {\n";
           List.iter (emit_simple_stmt ctx buf (indent ^ "    ")) else_body;
           Buffer.add_string buf indent;
           Buffer.add_string buf "}\n")
  | TWhile { cond; body } ->
      Buffer.add_string buf indent;
      Buffer.add_string buf "while (";
      gen_expr ctx buf cond;
      Buffer.add_string buf ") {\n";
      List.iter (emit_simple_stmt ctx buf (indent ^ "    ")) body;
      Buffer.add_string buf indent;
      Buffer.add_string buf "}\n"
  | TDefer { pos; _ } ->
      Error.failf pos "'defer' inside a defer body is not supported"
  | TReturn { pos; _ } ->
      Error.failf pos "'return' inside a defer body is not supported"
  | TFor _ ->
      (* The lift pass always expands TFor into TLet+TLet+TWhile; reaching
         codegen means lifting was skipped (compiler bug, not user error). *)
      assert false

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
and emit_match_stmt ctx ?assign_to buf indent (m_expr : texpr) =
  match m_expr.e with
  | TMatch { scrutinee; ename_path; arms } ->
      (* Flat matches (binds are only var/wildcard) compile to a tag
         `switch`; once any arm nests a variant pattern OR carries a
         guard, a flat switch can't distinguish two arms sharing an outer
         variant (or evaluate the guard with binds in scope), so we fall
         back to an if-else decision chain. *)
      let needs_decision =
        List.exists (fun (a : tmatch_arm) ->
          tpat_nested a.tpat || a.tguard <> None) arms
      in
      if needs_decision then
        emit_match_decision ctx ?assign_to buf indent ename_path arms scrutinee
      else
      let inner = indent ^ "    " in
      let case_indent = inner ^ "    " in
      let body_indent = case_indent ^ "    " in
      let cname = mangle_typ (TEnum ename_path) in
      Buffer.add_string buf indent;
      Buffer.add_string buf "{\n";
      Buffer.add_string buf inner;
      Buffer.add_string buf (Printf.sprintf "struct %s __m;\n" cname);
      emit_value_into_temp ctx buf inner "__m" scrutinee;
      Buffer.add_string buf inner;
      Buffer.add_string buf "switch (__m.tag) {\n";
      List.iter
        (fun (a : tmatch_arm) ->
          Buffer.add_string buf inner;
          (* Or-pattern (`A | B | C`) emits multiple `case` labels falling
             through to the same body; a wildcard alt subsumes the rest
             into a single `default:`.  Non-or patterns use a single
             label as before. *)
          let alts =
            match a.tpat with TPOr xs -> xs | other -> [ other ]
          in
          let has_wild =
            List.exists (function TPWildcard | TPVar _ -> true | _ -> false)
              alts
          in
          if has_wild then
            Buffer.add_string buf "default:\n"
          else
            List.iteri (fun i alt ->
              if i > 0 then Buffer.add_string buf inner;
              match alt with
              | TPVariant { variant; _ } ->
                  Buffer.add_string buf
                    (Printf.sprintf "case %s_%s:\n" cname variant)
              | TPOr _ | TPWildcard | TPVar _ -> assert false)
              alts;
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
                      ctx.enum_index).evariants
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
           | TPWildcard | TPOr _ -> ());
                                   (* TPOr alts bind no variables by MVP
                                      restriction; multiple case labels
                                      share the same (empty) bind block *)
          emit_arm_result ctx assign_to buf body_indent a;
          if not a.tdiverges then begin
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

(* Does any sub-pattern nest another variant pattern?  Drives the
   switch-vs-decision-chain choice in [emit_match_stmt]. *)
and tpat_nested = function
  | TPVariant { binds; _ } ->
      List.exists (fun (_, p) ->
        match p with TPVariant _ -> true | _ -> tpat_nested p) binds
  | TPOr _ -> false                       (* MVP alts are unit-variant /
                                             wildcard only; flat switch
                                             with stacked case labels
                                             always works *)
  | _ -> false

(* Arm body emission shared by the switch and decision-chain paths
   (without the switch's trailing `break`).  A `tdiverges` arm
   early-returns (flushing defers); otherwise the body assigns to
   [assign_to] (or recurses for a nested match), or runs as a bare
   expression-statement when the match's value is discarded. *)
and emit_arm_result ctx assign_to buf indent (a : tmatch_arm) =
  if a.tdiverges then begin
    (match a.tbody.e with
     | TEnumLit _ -> ()
     | _ ->
         failwith
           "internal: tdiverges arm body must be a TEnumLit \
            (only `try` desugar produces diverging arms today)");
    let trimmed = strip_trailing_space (c_type_prefix a.tbody.ty) in
    Buffer.add_string buf indent;
    Buffer.add_string buf trimmed;
    Buffer.add_string buf " __try_ret;\n";
    emit_value_into_temp ctx buf indent "__try_ret" a.tbody;
    let cleanups = List.flatten ctx.defer_chain in
    emit_cleanups ctx buf indent cleanups;
    Buffer.add_string buf indent;
    Buffer.add_string buf "return __try_ret;\n"
  end else
    (match assign_to, a.tbody.e with
     | Some lhs, TMatch _ ->
         emit_match_stmt ctx ~assign_to:lhs buf indent a.tbody
     | _, TBlock { stmts; trailing } ->
         (* Multi-stmt arm body — emit each stmt in source order, then
            either assign the trailing value to `lhs` or emit it as a
            void expression-statement.  Stmts share the surrounding fn's
            decl namespace (registered during typecheck's walk_stmt
            recursion through `walk_stmts_hook`).  Trailing None ->
            stmt-position block (no value); skip the assign. *)
         List.iter (emit_simple_stmt ctx buf indent) stmts;
         (match trailing with
          | None -> ()                  (* allow_void block — stmts only *)
          | Some te ->
              let trailing_arm =
                { a with tbody = te; tdiverges = false }
              in
              emit_arm_result ctx assign_to buf indent trailing_arm)
     | Some lhs, _ ->
         emit_assign_line buf indent ~lhs
           ~emit_rhs:(fun () -> gen_expr ctx buf a.tbody)
     | None, _ ->
         Buffer.add_string buf indent;
         gen_expr ctx buf a.tbody;
         Buffer.add_string buf ";\n")

(* Compile a (possibly nested) pattern against C l-value [scrut] of type
   [ty] into a list of tag tests (joined with `&&` by the caller) and a
   list of (bind-name, C l-value, type) extractions.  Nested variant
   patterns descend into `<scrut>.data.<variant>.<field>`. *)
and compile_pat ctx ~scrut ~ty tpat =
  match tpat with
  | TPWildcard -> ([], [])
  | TPVar n -> ([], [ (n, scrut, ty) ])
  | TPVariant { variant; binds; _ } ->
      let cname = mangle_typ ty in
      let tag_test = Printf.sprintf "%s.tag == %s_%s" scrut cname variant in
      let vsig =
        List.find (fun (vs : variant_sig) -> vs.vsname = variant)
          (List.find (fun (es : enum_sig) ->
               match ty with TEnum p -> es.ename_path = p | _ -> false)
             ctx.enum_index).evariants
      in
      List.fold_left
        (fun (ts, bs) (fname, subpat) ->
          let ft = List.assoc fname vsig.vsfields in
          let sub_scrut = Printf.sprintf "%s.data.%s.%s" scrut variant fname in
          let (t2, b2) = compile_pat ctx ~scrut:sub_scrut ~ty:ft subpat in
          (ts @ t2, bs @ b2))
        ([ tag_test ], []) binds
  | TPOr alts ->
      (* Or-pattern in decision-chain context: one `||`-joined test that
         folds together each alternative's tests.  By MVP restriction
         alternatives bind nothing, so `bs` from each alt is empty.  An
         alt with zero tests (wildcard) collapses the whole disjunction
         to "always true" — emit no test. *)
      let alt_tests =
        List.map (fun alt ->
          let (ts, _bs) = compile_pat ctx ~scrut ~ty alt in ts) alts
      in
      if List.exists (fun ts -> ts = []) alt_tests then ([], [])
      else
        let joined =
          List.map (fun ts ->
            "(" ^ String.concat " && " ts ^ ")") alt_tests
        in
        ([ "(" ^ String.concat " || " joined ^ ")" ], [])

(* Decision-chain emission for matches with nested patterns.  Each arm
   becomes `if (<tests>) { <binds>; <body> }`; the last arm is emitted as
   a bare `else` (sound because typecheck proved exhaustiveness, and it
   keeps the assigned value definitely-initialised under -Wall). *)
and emit_match_decision ctx ?assign_to buf indent ename_path arms scrutinee =
  let inner = indent ^ "    " in
  let body_indent = inner ^ "    " in
  let cname = mangle_typ (TEnum ename_path) in
  Buffer.add_string buf indent;
  Buffer.add_string buf "{\n";
  Buffer.add_string buf inner;
  Buffer.add_string buf (Printf.sprintf "struct %s __m;\n" cname);
  emit_value_into_temp ctx buf inner "__m" scrutinee;
  let has_guard = List.exists (fun (a : tmatch_arm) -> a.tguard <> None) arms in
  if has_guard then
    emit_arms_goto ctx assign_to buf inner body_indent (TEnum ename_path) arms
  else
    emit_arms_if_else ctx assign_to buf inner body_indent
      (TEnum ename_path) arms;
  Buffer.add_string buf indent;
  Buffer.add_string buf "}\n"

(* Guard-free decision chain — preserves the long-standing
   "if/else if/.../else" layout so existing goldens don't churn.
   The last arm is a bare `else` (typecheck-proved exhaustiveness
   guarantees it covers what's left). *)
and emit_arms_if_else ctx assign_to buf inner body_indent ty arms =
  let n = List.length arms in
  List.iteri
    (fun i (a : tmatch_arm) ->
      let is_last = i = n - 1 in
      let (tests, binds) =
        compile_pat ctx ~scrut:"__m" ~ty a.tpat
      in
      Buffer.add_string buf inner;
      (if is_last && i > 0 then
         Buffer.add_string buf "else {\n"
       else if is_last then
         Buffer.add_string buf "{\n"
       else begin
         Buffer.add_string buf (if i = 0 then "if (" else "else if (");
         Buffer.add_string buf (String.concat " && " tests);
         Buffer.add_string buf ") {\n"
       end);
      List.iter (fun (name, lval, ty) ->
          Buffer.add_string buf body_indent;
          Buffer.add_string buf (c_decl ty name);
          Buffer.add_string buf " = ";
          Buffer.add_string buf lval;
          Buffer.add_string buf ";\n")
        binds;
      emit_arm_result ctx assign_to buf body_indent a;
      Buffer.add_string buf inner;
      Buffer.add_string buf "}\n")
    arms

(* Guard-aware emission: each arm is a self-contained `if (tag-tests)
   { binds; if (guard) { body; goto done; } }`.  Binds must be in scope
   to evaluate the guard, so they can't live in the if-test position —
   hence the nested form.  When the guard fails (or the tag doesn't
   match), control falls through to the next arm's `if`.  An exhaustive
   unguarded arm somewhere in the chain will always fire — typecheck
   uses only unguarded arms for exhaustiveness, so this is guaranteed.
   Non-diverging arms `goto __mdoneK` to short-circuit the chain;
   diverging arms (`try` desugar) `return` instead. *)
and emit_arms_goto ctx assign_to buf inner body_indent ty arms =
  let label =
    let i = !match_label_counter in
    incr match_label_counter;
    Printf.sprintf "__mdone%d" i
  in
  List.iter (fun (a : tmatch_arm) ->
    let (tests, binds) = compile_pat ctx ~scrut:"__m" ~ty a.tpat in
    (* Tag-test wrapper opens iff there's any test.  An unconditional arm
       (wildcard / var only) still needs an enclosing block to scope
       any binds it introduces. *)
    Buffer.add_string buf inner;
    (if tests = [] then
       Buffer.add_string buf "{\n"
     else begin
       Buffer.add_string buf "if (";
       Buffer.add_string buf (String.concat " && " tests);
       Buffer.add_string buf ") {\n"
     end);
    List.iter (fun (name, lval, ty) ->
        Buffer.add_string buf body_indent;
        Buffer.add_string buf (c_decl ty name);
        Buffer.add_string buf " = ";
        Buffer.add_string buf lval;
        Buffer.add_string buf ";\n")
      binds;
    let stmt_indent, close_guard =
      match a.tguard with
      | None -> (body_indent, false)
      | Some g ->
          Buffer.add_string buf body_indent;
          Buffer.add_string buf "if (";
          gen_expr ctx buf g;
          Buffer.add_string buf ") {\n";
          (body_indent ^ "    ", true)
    in
    emit_arm_result ctx assign_to buf stmt_indent a;
    if not a.tdiverges then begin
      Buffer.add_string buf stmt_indent;
      Buffer.add_string buf (Printf.sprintf "goto %s;\n" label)
    end;
    if close_guard then begin
      Buffer.add_string buf body_indent;
      Buffer.add_string buf "}\n"
    end;
    Buffer.add_string buf inner;
    Buffer.add_string buf "}\n")
    arms;
  Buffer.add_string buf inner;
  Buffer.add_string buf label;
  Buffer.add_string buf ": ;\n"

(* Destructuring binding: introduce an inner C block, declare a `__t` temp
   of the tuple struct type, fill it from the RHS, then assign each hoisted
   name from the temp's numbered field. *)
and emit_let_tuple ctx buf indent names (value : texpr) =
  let inner = indent ^ "    " in
  let trimmed = strip_trailing_space (c_type_prefix value.ty) in
  Buffer.add_string buf indent;
  Buffer.add_string buf "{\n";
  Buffer.add_string buf inner;
  Buffer.add_string buf trimmed;
  Buffer.add_string buf " __t;\n";
  emit_value_into_temp ctx buf inner "__t" value;
  List.iteri
    (fun i name ->
      emit_assign_line buf inner ~lhs:name ~emit_rhs:(fun () ->
        Buffer.add_string buf "__t._";
        Buffer.add_string buf (string_of_int i)))
    names;
  Buffer.add_string buf indent;
  Buffer.add_string buf "}\n"

and emit_cleanups ctx buf indent defers =
  List.iter
    (fun body ->
      List.iter (fun s -> emit_simple_stmt ctx buf indent s) body)
    defers

and gen_if ctx buf indent outer_scopes my_defers
    cond then_body else_body =
  Buffer.add_string buf "if (";
  gen_expr ctx buf cond;
  Buffer.add_string buf ") {\n";
  gen_block ctx buf (indent ^ "    ")
    (my_defers :: outer_scopes) then_body;
  Buffer.add_string buf indent;
  Buffer.add_char buf '}';
  (match else_body with
   | [] -> Buffer.add_char buf '\n'
   | [ TIf { cond = ec; then_body = et; else_body = ee } ] ->
       Buffer.add_string buf " else ";
       gen_if ctx buf indent outer_scopes my_defers ec et ee
   | _ ->
       Buffer.add_string buf " else {\n";
       gen_block ctx buf (indent ^ "    ")
         (my_defers :: outer_scopes) else_body;
       Buffer.add_string buf indent;
       Buffer.add_string buf "}\n")

and gen_block ctx buf indent outer_scopes stmts =
  let saved_chain = ctx.defer_chain in
  let update_chain my_defers =
    ctx.defer_chain <- my_defers :: outer_scopes
  in
  let rec loop my_defers = function
    | [] ->
        update_chain my_defers;
        emit_cleanups ctx buf indent my_defers
    | (TDefer { body; _ } as s) :: rest ->
        emit_tstmt_ann ctx buf indent s;
        loop (body :: my_defers) rest
    | (TReturn { value; _ } as s) :: _ ->
        update_chain my_defers;
        emit_tstmt_ann ctx buf indent s;
        let all = List.flatten (my_defers :: outer_scopes) in
        (match value with
         | None ->
             (* Bare `return;` from a void fn: flush active defers, then
                return with no value. *)
             emit_cleanups ctx buf indent all;
             Buffer.add_string buf indent;
             Buffer.add_string buf "return;\n"
         | Some value ->
             let needs_block =
               all <> [] ||
               (match value.e with
                | TTupleLit _ | TStructLit _ | TNew _ | TMatch _ | TEnumLit _
                | TIfExpr _ | TArrayLit _ | TArrayRepeat _ -> true
                | _ -> false)
             in
             if not needs_block then begin
               Buffer.add_string buf indent;
               Buffer.add_string buf "return ";
               gen_expr ctx buf value;
               Buffer.add_string buf ";\n"
             end else begin
               let trimmed = strip_trailing_space (c_type_prefix value.ty) in
               Buffer.add_string buf indent;
               Buffer.add_string buf "{\n";
               Buffer.add_string buf (indent ^ "    ");
               Buffer.add_string buf trimmed;
               Buffer.add_string buf " __exile_ret;\n";
               emit_value_into_temp ctx buf (indent ^ "    ") "__exile_ret" value;
               emit_cleanups ctx buf (indent ^ "    ") all;
               Buffer.add_string buf (indent ^ "    ");
               Buffer.add_string buf "return __exile_ret;\n";
               Buffer.add_string buf indent;
               Buffer.add_string buf "}\n"
             end)
    | (TLet _ | TAssign _ | TAssignField _ | TAssignIndex _ | TAssignDeref _
      | TExprStmt _) as s :: rest ->
        update_chain my_defers;
        emit_tstmt_ann ctx buf indent s;
        emit_simple_stmt ctx buf indent s;
        loop my_defers rest
    | (TLetTuple { names; value; _ } as s) :: rest ->
        update_chain my_defers;
        emit_tstmt_ann ctx buf indent s;
        emit_let_tuple ctx buf indent names value;
        loop my_defers rest
    | (TIf { cond; then_body; else_body } as s) :: rest ->
        update_chain my_defers;
        emit_tstmt_ann ctx buf indent s;
        Buffer.add_string buf indent;
        gen_if ctx buf indent outer_scopes my_defers
          cond then_body else_body;
        loop my_defers rest
    | (TWhile { cond; body } as s) :: rest ->
        update_chain my_defers;
        emit_tstmt_ann ctx buf indent s;
        Buffer.add_string buf indent;
        Buffer.add_string buf "while (";
        gen_expr ctx buf cond;
        Buffer.add_string buf ") {\n";
        gen_block ctx buf (indent ^ "    ")
          (my_defers :: outer_scopes) body;
        Buffer.add_string buf indent;
        Buffer.add_string buf "}\n";
        loop my_defers rest
    | TFor _ :: _ ->
        (* Lift always expands TFor into TLet+TLet+TWhile before codegen. *)
        assert false
  in
  loop [] stmts;
  ctx.defer_chain <- saved_chain

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
             (* `@reg(X)` and `@amiga_lib(...)` are accepted on the
                exile side as documentation of the AmigaOS calling
                convention, but the EMIT stays plain.  Bebbo's
                amiga.lib provides per-function stubs (`_OpenLibrary`
                etc.) that read args off the stack via the normal C
                calling convention and load them into the right
                registers before the ROM JSR.  Emitting
                `register T name __asm("X")` on the prototype WOULD
                bypass the stack convention and the stub would read
                garbage — exactly what bit us when we tried.  Leaving
                the C declaration plain keeps the amiga.lib stub path
                happy. *)
             Buffer.add_string buf (c_decl t p.pname))
           zipped);
    if f.is_variadic then Buffer.add_string buf ", ...";
    Buffer.add_char buf ')'
  end

(* Emit one already-elaborated function.  tf carries the typed body, the
   resolved C name, and the hoisted let-decl list. *)
let gen_function ctx buf (tf : tfunc) =
  let f = tf.tf_func in
  emit_ann ctx buf "" f.pos;
  emit_fn_sig buf tf;
  Buffer.add_string buf " {\n";
  List.iter
    (fun (name, t) ->
      Buffer.add_string buf (Printf.sprintf "    %s;\n" (c_decl t name)))
    tf.tf_lets;
  ctx.defer_chain <- [];
  gen_block ctx buf "    " [] tf.tf_body;
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

(* `[T; N]` wrapper: `struct ex_arrN_T { T data[N]; };`.  The `[N]` suffix
   makes this irregular vs emit_struct_decl, so it is emitted directly. *)
let emit_array_struct buf (_, t) =
  match t with
  | TArray { elem; size } ->
      Buffer.add_string buf "struct ";
      Buffer.add_string buf ("ex_" ^ mangle_typ t);
      Buffer.add_string buf " { ";
      Buffer.add_string buf (c_decl elem "data");
      Buffer.add_string buf (Printf.sprintf "[%d]; };\n" size)
  | _ -> ()

(* Unified aggregate typedef view for the dependency-ordered emission.
   Every C-level typedef the program needs (user structs / enums, tuple
   wrappers, fixed-size array wrappers) lives in this list; gen_program
   topo-sorts the list by by-value dependencies so each typedef precedes
   any other that embeds it by value. *)
type emit_agg =
  | AStruct of struct_sig
  | AEnum of enum_sig
  | ATuple of string * typ              (* (mangled name, the TTuple typ) *)
  | AArray of string * typ              (* (mangled name, the TArray typ) *)

(* The C struct name this aggregate emits — also its key in the topo sort. *)
let agg_cname = function
  | AStruct s -> mangle_typ (TStruct s.sname_path)
  | AEnum e -> mangle_typ (TEnum e.ename_path)
  | ATuple (_, t) -> "ex_" ^ mangle_typ t
  | AArray (_, t) -> "ex_" ^ mangle_typ t

(* C struct names this type embeds by value (stops at pointers and fn-ptrs).
   Used to compute one aggregate's dependencies on others.  Note: tuples
   and arrays appear *themselves* in the dep list when nested, because
   they too need their wrapper typedef defined first. *)
let rec value_agg_keys t =
  match t with
  | TStruct _ as st -> [ mangle_typ st ]
  | TEnum _ as en -> [ mangle_typ en ]
  | TTuple ts as tt ->
      ("ex_" ^ mangle_typ tt) :: List.concat_map value_agg_keys ts
  | TArray { elem; _ } as ar ->
      ("ex_" ^ mangle_typ ar) :: value_agg_keys elem
  | _ -> []

let agg_deps = function
  | AStruct s ->
      List.concat_map (fun (_, t) -> value_agg_keys t) s.sfields_ty
  | AEnum e ->
      List.concat_map (fun (vs : variant_sig) ->
        List.concat_map (fun (_, t) -> value_agg_keys t) vs.vsfields)
        e.evariants
  | ATuple (_, TTuple ts) -> List.concat_map value_agg_keys ts
  | AArray (_, TArray { elem; _ }) -> value_agg_keys elem
  | ATuple _ | AArray _ -> []

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

(* Emit a single printf-style fragment that renders [access] (a C l-value
   of type [ty]) Rust-Debug style, with no trailing newline.  Nested
   `@debug` aggregates call their own __debug printer (recursion via the
   forward decls emitted up top). *)
let rec emit_field_debug buf ty access =
  match ty with
  | TBool ->
      Printf.bprintf buf
        "printf(\"%%s\", (%s) ? \"true\" : \"false\")" access
  | TString ->
      (* Quoted output, no runtime escape — user knows the content; for
         strings with embedded quotes/newlines the rendering is lossy. *)
      Printf.bprintf buf "printf(\"\\\"%%s\\\"\", %s)" access
  | TPtr _ -> Printf.bprintf buf "printf(\"%%p\", (void*)(%s))" access
  | TStruct _ | TEnum _ ->
      let fn = mangle_typ ty ^ "__debug" in
      Printf.bprintf buf "%s(%s)" fn access
  | _ ->
      (match printf_int_spec ty with
       | Some (spec, Some c) ->
           Printf.bprintf buf "printf(\"%s\", (%s)(%s))" spec c access
       | Some (spec, None) ->
           Printf.bprintf buf "printf(\"%s\", %s)" spec access
       | None -> assert false  (* typecheck @debug-validation rejected these *))

let emit_agg buf = function
  | AStruct s -> emit_named_struct buf s
  | AEnum e -> emit_named_enum buf e
  | ATuple (name, t) -> emit_tuple_struct buf (name, t)
  | AArray (name, t) -> emit_array_struct buf (name, t)

(* Stable dependency-ordered emission: DFS on the input list in source
   order, recursing into each entry's deps first.  Aggregates without
   forced ordering keep their original positions; only deps shuffle. *)
let emit_aggs_topo buf aggs =
  let by_key = Hashtbl.create 32 in
  List.iter (fun a -> Hashtbl.replace by_key (agg_cname a) a) aggs;
  let done_set = Hashtbl.create 32 in
  let rec visit a =
    let key = agg_cname a in
    if not (Hashtbl.mem done_set key) then begin
      Hashtbl.add done_set key ();
      List.iter (fun dep_key ->
        match Hashtbl.find_opt by_key dep_key with
        | Some dep -> visit dep
        | None -> ())   (* dep is a primitive / extern / fn-ptr — skip *)
        (agg_deps a);
      emit_agg buf a
    end
  in
  List.iter visit aggs

(* Last segment of a path — the user-facing type name to embed in the
   rendered debug output (e.g., `Point`, `Result`). *)
let last_segment path = List.nth path (List.length path - 1)

let emit_struct_debug_fwddecl buf (s : struct_sig) =
  if s.sis_debug then
    let cname = mangle_typ (TStruct s.sname_path) in
    Printf.bprintf buf "static void %s__debug(struct %s self);\n" cname cname

let emit_enum_debug_fwddecl buf (e : enum_sig) =
  if e.eis_debug then
    let cname = mangle_typ (TEnum e.ename_path) in
    Printf.bprintf buf "static void %s__debug(struct %s self);\n" cname cname

let emit_struct_debug_def ~structs ~enums buf (s : struct_sig) =
  if s.sis_debug then begin
    let cname = mangle_typ (TStruct s.sname_path) in
    (* User-facing header name: `Point` for mono, `Pair<i32, bool>` for a
       generic instance — not the mangled `Pair_i32_bool`. *)
    let name = render_typ_user_facing ~structs ~enums (TStruct s.sname_path) in
    Printf.bprintf buf "static void %s__debug(struct %s self) {\n" cname cname;
    Printf.bprintf buf "    printf(\"%s { \");\n" name;
    List.iteri (fun i (fname, fty) ->
      if i > 0 then Buffer.add_string buf "    printf(\", \");\n";
      Printf.bprintf buf "    printf(\"%s: \");\n" fname;
      Buffer.add_string buf "    ";
      emit_field_debug buf fty ("self." ^ fname);
      Buffer.add_string buf ";\n")
      s.sfields_ty;
    Buffer.add_string buf "    printf(\" }\");\n";
    Buffer.add_string buf "}\n"
  end

let emit_enum_debug_def ~structs ~enums buf (e : enum_sig) =
  if e.eis_debug then begin
    let cname = mangle_typ (TEnum e.ename_path) in
    let name = render_typ_user_facing ~structs ~enums (TEnum e.ename_path) in
    Printf.bprintf buf "static void %s__debug(struct %s self) {\n" cname cname;
    Buffer.add_string buf "    switch (self.tag) {\n";
    List.iter (fun (vs : variant_sig) ->
      Printf.bprintf buf "    case %s_%s:\n" cname vs.vsname;
      if vs.vsfields = [] then
        Printf.bprintf buf "        printf(\"%s::%s\");\n" name vs.vsname
      else if vs.vsis_struct then begin
        Printf.bprintf buf "        printf(\"%s::%s { \");\n" name vs.vsname;
        List.iteri (fun i (fname, fty) ->
          if i > 0 then Buffer.add_string buf "        printf(\", \");\n";
          Printf.bprintf buf "        printf(\"%s: \");\n" fname;
          Buffer.add_string buf "        ";
          emit_field_debug buf fty
            (Printf.sprintf "self.data.%s.%s" vs.vsname fname);
          Buffer.add_string buf ";\n")
          vs.vsfields;
        Buffer.add_string buf "        printf(\" }\");\n"
      end else begin
        Printf.bprintf buf "        printf(\"%s::%s(\");\n" name vs.vsname;
        List.iteri (fun i (fname, fty) ->
          if i > 0 then Buffer.add_string buf "        printf(\", \");\n";
          Buffer.add_string buf "        ";
          emit_field_debug buf fty
            (Printf.sprintf "self.data.%s.%s" vs.vsname fname);
          Buffer.add_string buf ";\n")
          vs.vsfields;
        Buffer.add_string buf "        printf(\")\");\n"
      end;
      Buffer.add_string buf "        break;\n")
      e.evariants;
    Buffer.add_string buf "    }\n";
    Buffer.add_string buf "}\n"
  end

(* Skip empty sections so the emitted C doesn't accumulate stray
   blank lines; when the section is non-empty, prepend a `\n` so it
   visually separates from the previous block.  Used by every "list of
   declarations" group in gen_program (extern consts, fnptr typedefs,
   struct decls, fn forward decls, ...).  Adding a new declaration
   category needs only one call here, not three lines of guard. *)
let emit_section buf items ~emit =
  if items <> [] then begin
    Buffer.add_char buf '\n';
    List.iter emit items
  end

let gen_program ?(annotate = false) (tp : tprogram) =
  (* Reset per-program so `__afK` loop-counter names start at 0 each
     compile — keeps golden output deterministic across test runs. *)
  fill_counter := 0;
  match_label_counter := 0;
  let ctx = new_gen_ctx ~annotate
    ~enum_index:tp.tp_enum_index
    ~struct_index:tp.tp_struct_index in
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
  (* User `const`s fold to literals emitted as `#define`s — usable in
     array dimensions and constant expressions at the C level, and read as
     mangled identifiers at use sites. *)
  emit_section buf tp.tp_consts ~emit:(fun (name, value) ->
    Buffer.add_string buf "#define ";
    Buffer.add_string buf name;
    Buffer.add_char buf ' ';
    Buffer.add_string buf value;
    Buffer.add_char buf '\n');
  emit_section buf tp.tp_ext_consts ~emit:(fun (name, t) ->
    Buffer.add_string buf "extern const ";
    Buffer.add_string buf (c_decl t name);
    Buffer.add_string buf ";\n");
  emit_section buf tp.tp_ext_vars ~emit:(fun (name, t) ->
    Buffer.add_string buf "extern ";
    Buffer.add_string buf (c_decl t name);
    Buffer.add_string buf ";\n");
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
  (* Typedef aliases for every fn-pointer type used in the program.
     Emitted before struct decls so a struct field of fn-ptr type
     (e.g. `Allocator.alloc_fn`) sees the alias.  Use sites refer
     to the alias name, sidestepping C's awkward fn-ptr declaration
     syntax (especially nasty when fn-ptr is itself a return type). *)
  emit_section buf tp.tp_fnptr_types ~emit:(fun (name, t) ->
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
    | _ -> assert false);
  (* Topologically order all aggregate typedefs so each precedes any other
     that embeds it by value (struct field, tuple element, array element).
     Replaces the per-kind sections so a struct field of `[T; N]` or
     `(int, int)` works without arranging by hand. *)
  let aggs =
    List.map (fun s -> AStruct s) concrete_structs
    @ List.map (fun e -> AEnum e) concrete_enums
    @ List.map (fun (n, t) -> ATuple (n, t)) tp.tp_tuple_types
    @ List.map (fun (n, t) -> AArray (n, t)) tp.tp_array_types
  in
  if aggs <> [] then begin
    Buffer.add_char buf '\n';
    emit_aggs_topo buf aggs
  end;
  (* `@debug` printers — synthesized one per marked struct/enum.  Forward
     decls first so a printer body can call another printer regardless
     of source declaration order; bodies come right after. *)
  let debug_structs =
    List.filter (fun s -> s.sis_debug) concrete_structs in
  let debug_enums =
    List.filter (fun e -> e.eis_debug) concrete_enums in
  if debug_structs <> [] || debug_enums <> [] then begin
    Buffer.add_char buf '\n';
    List.iter (emit_struct_debug_fwddecl buf) debug_structs;
    List.iter (emit_enum_debug_fwddecl buf) debug_enums;
    Buffer.add_char buf '\n';
    let structs = tp.tp_struct_index and enums = tp.tp_enum_index in
    List.iter (fun s ->
      emit_struct_debug_def ~structs ~enums buf s; Buffer.add_char buf '\n')
      debug_structs;
    List.iter (fun e ->
      emit_enum_debug_def ~structs ~enums buf e; Buffer.add_char buf '\n')
      debug_enums
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
  emit_section buf non_main ~emit:(fun tf ->
    emit_fn_sig buf tf;
    Buffer.add_string buf ";\n");
  Buffer.add_char buf '\n';
  (* extern fn has no body — fwd-decl above is the entire emission. *)
  let definable = List.filter (fun tf -> not tf.tf_func.Ast.is_extern) concrete_funcs in
  let last = List.length definable - 1 in
  List.iteri
    (fun i tf ->
      let before = Buffer.length buf in
      gen_function ctx buf tf;
      let after = Buffer.length buf in
      ctx.bloat <- (tf.tf_mangled, after - before) :: ctx.bloat;
      if i < last then Buffer.add_char buf '\n')
    definable;
  last_bloat_cache := ctx.bloat;
  Buffer.contents buf
