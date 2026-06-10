(* Differential-harness dumps (self-host bring-up Faza −1, ratified
   2026-06-02).  Three canonical dumps the OCaml exilc emits as
   oracles for the future exile port:

     --emit-tokens     pinned positions (spans), one token per line
     --emit-ast        structural s-expr, positions elided
     --emit-typed-ir   post-lift IR, `:ty` mandatory on every texpr

   Naczelna zasada: kanoniczność BY CONSTRUCTION.  No post-hoc
   normalization — the port copies this module verbatim and the
   bytes match.  Three governing decisions:

   1. S-expression `(Kind child…)` per node/line.  Emission is
      trivial and decision-free (`"(" + kind + " " + children + ")"`),
      so the port reproduces byte-identically.  Parens also pin diff
      boundaries without relying on indentation.

   2. Positions ELIDED in AST / IR, pinned in tokens.  `--emit-tokens`
      always carries span info (lexer contract); AST / IR omit
      positions so a single source shift doesn't cascade into noise.

   3. Dump-point = codegen-input (post-lift).  TFor / TForEach /
      TStructApp / TVar transients are gone by the time the IR
      dump runs; if they show up, that's a bug to catch.

   Ordering rules: NEVER iteration order — declaration order for
   anything user-typed (fields, params, variants, stmt-list);
   SORTED for hash / set-backed accumulators (mono-instances,
   trait_assoc_table, dedup sets) so dumps are bit-stable across
   runs.  Floats render as hex (`%h`) for exact round-trip — this
   is dump format, not codegen. *)

open Ir

let buf_add buf s = Buffer.add_string buf s
let buf_char buf c = Buffer.add_char buf c

(* C-style string escape for literals embedded in dumps.  Mirrors the
   subset DR Faza minus 1 ratified: newline, tab, quote, backslash
   as named escapes; everything else < 0x20 or >= 0x7f goes through
   the byte-level xHH form.  Printable 7-bit ASCII passes through
   unchanged. *)
let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  buf_char buf '"';
  String.iter (fun c ->
    let code = Char.code c in
    match c with
    | '\n' -> buf_add buf "\\n"
    | '\t' -> buf_add buf "\\t"
    | '"'  -> buf_add buf "\\\""
    | '\\' -> buf_add buf "\\\\"
    | _ when code < 0x20 || code >= 0x7f ->
        Printf.bprintf buf "\\x%02x" code
    | _ -> buf_char buf c)
    s;
  buf_char buf '"';
  Buffer.contents buf

(* Render a type for canonical dump.  Distinct from `typ_name` (user-
   facing) and `mangle_typ` (C identifier).  Rules:
   - int / c-int variants always carry explicit width + signedness
   - paths absolute, `::` joined
   - structural constructors prefixed: ptr / const-ptr / array
   - generic applications resolve to flat instance paths post-mono;
     unmonomorphized skeletons surface as their TVar / TStructApp form *)
let rec render_typ_canonical = function
  | TInt { signed = true; width = Ast.W8 } -> "(int i8)"
  | TInt { signed = true; width = Ast.W16 } -> "(int i16)"
  | TInt { signed = true; width = Ast.W32 } -> "(int i32)"
  | TInt { signed = false; width = Ast.W8 } -> "(int u8)"
  | TInt { signed = false; width = Ast.W16 } -> "(int u16)"
  | TInt { signed = false; width = Ast.W32 } -> "(int u32)"
  | TCInt { signed = true } -> "(c_int signed)"
  | TCInt { signed = false } -> "(c_int unsigned)"
  | TCShort { signed = true } -> "(c_short signed)"
  | TCShort { signed = false } -> "(c_short unsigned)"
  | TCLong { signed = true } -> "(c_long signed)"
  | TCLong { signed = false } -> "(c_long unsigned)"
  | TCChar -> "c_char"
  | TCSChar -> "c_schar"
  | TCUChar -> "c_uchar"
  | TCVoid -> "c_void"
  | TBool -> "bool"
  | TFloat Ast.F32 -> "(float f32)"
  | TFloat Ast.F64 -> "(float f64)"
  | TString -> "str"
  | TNullPtr -> "null-ptr"
  | TStruct path ->
      Printf.sprintf "(struct %s)" (String.concat "::" path)
  | TEnum path ->
      Printf.sprintf "(enum %s)" (String.concat "::" path)
  | TExtStruct n -> Printf.sprintf "(ext-struct %s)" n
  | TExtAlias n -> Printf.sprintf "(ext-alias %s)" n
  | TPtr inner ->
      Printf.sprintf "(ptr %s)" (render_typ_canonical inner)
  | TOwnPtr inner ->
      Printf.sprintf "(own-ptr %s)" (render_typ_canonical inner)
  | TConstPtr inner ->
      Printf.sprintf "(const-ptr %s)" (render_typ_canonical inner)
  | TArray { elem; size } ->
      Printf.sprintf "(array %s %d)"
        (render_typ_canonical elem) size
  | TTuple ts ->
      Printf.sprintf "(tuple %s)"
        (String.concat " " (List.map render_typ_canonical ts))
  | TFnPtr { params; ret } ->
      let ps =
        if params = [] then "()"
        else
          Printf.sprintf "(%s)"
            (String.concat " " (List.map render_typ_canonical params))
      in
      let r = match ret with
        | None -> "void"
        | Some t -> render_typ_canonical t
      in
      Printf.sprintf "(fn-ptr %s %s)" ps r
  | TVar n -> Printf.sprintf "(tvar %s)" n
  | TStructApp { path; args } ->
      Printf.sprintf "(struct-app %s %s)"
        (String.concat "::" path)
        (String.concat " " (List.map render_typ_canonical args))
  | TEnumApp { path; args } ->
      Printf.sprintf "(enum-app %s %s)"
        (String.concat "::" path)
        (String.concat " " (List.map render_typ_canonical args))
  | TAssocProj { head; assoc } ->
      Printf.sprintf "(assoc-proj %s %s)"
        (render_typ_canonical head) assoc

let header buf stage file =
  Printf.bprintf buf ";; exile-%s-dump v1 %s\n" stage file

(* `--emit-tokens` — one token per line as `(Kind [payload]) @file:L:C`.
   Lexer in this codebase only carries the start position per token
   (no end-span tracking), so we emit a single anchor instead of a
   span.  Format-wise the port reproduces this verbatim; gaining
   end-spans is an enhancement that ports as a unit. *)
let render_token (t : Token.t) : string =
  match t with
  | Token.Ident s -> Printf.sprintf "(Ident %s)" s
  | Token.Int n -> Printf.sprintf "(Int %d)" n
  | Token.Float (f, is32) ->
      Printf.sprintf "(Float %h %s)" f (if is32 then "f32" else "f64")
  | Token.String s -> Printf.sprintf "(String %s)" (escape_string s)
  | Token.Fn -> "Fn"
  | Token.Let -> "Let"
  | Token.Mut -> "Mut"
  | Token.Return -> "Return"
  | Token.If -> "If"
  | Token.Else -> "Else"
  | Token.While -> "While"
  | Token.Loop -> "Loop"
  | Token.Break -> "Break"
  | Token.Continue -> "Continue"
  | Token.For -> "For"
  | Token.In -> "In"
  | Token.Mod -> "Mod"
  | Token.Pub -> "Pub"
  | Token.Use -> "Use"
  | Token.As -> "As"
  | Token.Defer -> "Defer"
  | Token.Struct -> "Struct"
  | Token.Impl -> "Impl"
  | Token.Trait -> "Trait"
  | Token.View -> "View"
  | Token.With -> "With"
  | Token.Enum -> "Enum"
  | Token.Match -> "Match"
  | Token.FatArrow -> "FatArrow"
  | Token.Pipe -> "Pipe"
  | Token.PipePipe -> "PipePipe"
  | Token.PipeGt -> "PipeGt"
  | Token.AmpAmp -> "AmpAmp"
  | Token.Dot -> "Dot"
  | Token.DotDot -> "DotDot"
  | Token.DotDotEq -> "DotDotEq"
  | Token.Amp -> "Amp"
  | Token.New -> "New"
  | Token.Own -> "Own"
  | Token.Null -> "Null"
  | Token.LParen -> "LParen"
  | Token.RParen -> "RParen"
  | Token.LBrace -> "LBrace"
  | Token.RBrace -> "RBrace"
  | Token.LBracket -> "LBracket"
  | Token.RBracket -> "RBracket"
  | Token.Semicolon -> "Semicolon"
  | Token.Comma -> "Comma"
  | Token.Colon -> "Colon"
  | Token.DoubleColon -> "DoubleColon"
  | Token.Arrow -> "Arrow"
  | Token.Eq -> "Eq"
  | Token.EqEq -> "EqEq"
  | Token.NotEq -> "NotEq"
  | Token.Bang -> "Bang"
  | Token.Lt -> "Lt"
  | Token.Gt -> "Gt"
  | Token.LtEq -> "LtEq"
  | Token.GtEq -> "GtEq"
  | Token.Plus -> "Plus"
  | Token.PlusPlus -> "PlusPlus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.Percent -> "Percent"
  | Token.Caret -> "Caret"
  | Token.Tilde -> "Tilde"
  | Token.Shl -> "Shl"
  | Token.Shr -> "Shr"
  | Token.True -> "True"
  | Token.False -> "False"
  | Token.Question -> "Question"
  | Token.Orelse -> "Orelse"
  | Token.Try -> "Try"
  | Token.Extern -> "Extern"
  | Token.Ellipsis -> "Ellipsis"
  | Token.At -> "At"
  | Token.Type -> "Type"
  | Token.Const -> "Const"
  | Token.Var -> "Var"
  | Token.SizeOf -> "SizeOf"
  | Token.Eof -> "Eof"

let dump_tokens ~file (toks : (Token.t * Pos.t) list) : string =
  let buf = Buffer.create 1024 in
  header buf "tokens" file;
  List.iter (fun (t, p) ->
    Printf.bprintf buf "%s @%s:%d:%d\n"
      (render_token t) p.Pos.file p.Pos.line p.Pos.col)
    toks;
  Buffer.contents buf

(* ===== AST dump ===== *)

(* Render an Ast.type_ann.  Source-level form (before Ir's typ resolution),
   so this is structurally different from `render_typ_canonical` and we
   keep both — the AST dump runs pre-typecheck, the IR dump post. *)
let rec render_type_ann (ann : Ast.type_ann) : string =
  match ann with
  | Ast.TyInt { signed; width } ->
      let suffix = match width with
        | Ast.W8 -> "8" | Ast.W16 -> "16" | Ast.W32 -> "32" in
      Printf.sprintf "(int %s%s)"
        (if signed then "i" else "u") suffix
  | Ast.TyCInt { signed } ->
      Printf.sprintf "(c_int %s)"
        (if signed then "signed" else "unsigned")
  | Ast.TyCShort { signed } ->
      Printf.sprintf "(c_short %s)"
        (if signed then "signed" else "unsigned")
  | Ast.TyCLong { signed } ->
      Printf.sprintf "(c_long %s)"
        (if signed then "signed" else "unsigned")
  | Ast.TyCChar -> "c_char"
  | Ast.TyCSChar -> "c_schar"
  | Ast.TyCUChar -> "c_uchar"
  | Ast.TyCVoid -> "c_void"
  | Ast.TyStr -> "str"
  | Ast.TyBool -> "bool"
  | Ast.TyFloat Ast.F32 -> "(float f32)"
  | Ast.TyFloat Ast.F64 -> "(float f64)"
  | Ast.TyTuple ts ->
      Printf.sprintf "(tuple %s)"
        (String.concat " " (List.map render_type_ann ts))
  | Ast.TyStruct { path; args = [] } ->
      Printf.sprintf "(struct %s)" (String.concat "::" path)
  | Ast.TyStruct { path; args } ->
      Printf.sprintf "(struct-app %s %s)"
        (String.concat "::" path)
        (String.concat " " (List.map render_type_ann args))
  | Ast.TyPtr t -> Printf.sprintf "(ptr %s)" (render_type_ann t)
  | Ast.TyOwnPtr t -> Printf.sprintf "(own-ptr %s)" (render_type_ann t)
  | Ast.TyConstPtr t ->
      Printf.sprintf "(const-ptr %s)" (render_type_ann t)
  | Ast.TyArray { elem; _ } ->
      (* Size is a const-expr; for dump purposes we just mark
         array-shape — the resolved IR dump carries the resolved
         numeric size, which is the canonical form for diff. *)
      Printf.sprintf "(array %s ?)" (render_type_ann elem)
  | Ast.TySelf -> "self"
  | Ast.TyFnPtr { params; ret } ->
      let ps =
        if params = [] then "()"
        else
          Printf.sprintf "(%s)"
            (String.concat " " (List.map render_type_ann params))
      in
      let r = match ret with
        | None -> "void"
        | Some t -> render_type_ann t
      in
      Printf.sprintf "(fn-ptr %s %s)" ps r

let render_binop = function
  | Ast.Add -> "+" | Ast.Sub -> "-" | Ast.Mul -> "*"
  | Ast.Div -> "/" | Ast.Mod -> "%"
  | Ast.BitAnd -> "&" | Ast.BitOr -> "|" | Ast.BitXor -> "^"
  | Ast.Shl -> "<<" | Ast.Shr -> ">>"
  | Ast.And -> "&&" | Ast.Or -> "||"
  | Ast.Lt -> "<" | Ast.Gt -> ">"
  | Ast.LtEq -> "<=" | Ast.GtEq -> ">="
  | Ast.EqEq -> "==" | Ast.NotEq -> "!="
  | Ast.Concat -> "++"

let rec render_expr (e : Ast.expr) : string =
  match e with
  | Ast.IntLit (n, _) -> Printf.sprintf "(int %d)" n
  | Ast.FloatLit (f, w, _) ->
      let tag = match w with Ast.F32 -> "f32" | Ast.F64 -> "f64" in
      Printf.sprintf "(float %h %s)" f tag
  | Ast.BoolLit (b, _) -> Printf.sprintf "(bool %b)" b
  | Ast.StringLit (s, _) ->
      Printf.sprintf "(string %s)" (escape_string s)
  | Ast.NullLit _ -> "null"
  | Ast.Var (n, _) -> Printf.sprintf "(var %s)" n
  | Ast.Neg (sub, _) -> Printf.sprintf "(neg %s)" (render_expr sub)
  | Ast.BitNot (sub, _) ->
      Printf.sprintf "(bit-not %s)" (render_expr sub)
  | Ast.Not (sub, _) -> Printf.sprintf "(not %s)" (render_expr sub)
  | Ast.BinOp (op, l, r, _) ->
      Printf.sprintf "(binop %s %s %s)"
        (render_binop op) (render_expr l) (render_expr r)
  | Ast.Orelse (a, b, _) ->
      Printf.sprintf "(orelse %s %s)" (render_expr a) (render_expr b)
  | Ast.Try (a, _) ->
      Printf.sprintf "(try %s)" (render_expr a)
  | Ast.Cast (sub, ann, _) ->
      Printf.sprintf "(cast %s %s)"
        (render_expr sub) (render_type_ann ann)
  | Ast.SizeOf (ann, _) ->
      Printf.sprintf "(size-of %s)" (render_type_ann ann)
  | Ast.TupleLit (es, _) ->
      Printf.sprintf "(tuple-lit %s)"
        (String.concat " " (List.map render_expr es))
  | Ast.Call { callee; args; _ } ->
      Printf.sprintf "(call %s %s)"
        (String.concat "::" callee)
        (String.concat " " (List.map render_expr args))
  | Ast.MethodCall { receiver; name; args; _ } ->
      Printf.sprintf "(method-call %s %s %s)"
        (render_expr receiver) name
        (String.concat " " (List.map render_expr args))
  | Ast.StructLit { tname; fields; base; _ } ->
      let fs = List.map (fun (n, e) ->
        Printf.sprintf "(field %s %s)" n (render_expr e)) fields in
      let base_s = match base with
        | None -> ""
        | Some b -> " (base " ^ render_expr b ^ ")"
      in
      Printf.sprintf "(struct-lit %s %s%s)"
        (String.concat "::" tname)
        (String.concat " " fs) base_s
  | Ast.FieldAccess (sub, n, _) ->
      Printf.sprintf "(field-access %s %s)" (render_expr sub) n
  | Ast.Ref (sub, _) -> Printf.sprintf "(ref %s)" (render_expr sub)
  | Ast.Deref (sub, _) ->
      Printf.sprintf "(deref %s)" (render_expr sub)
  | Ast.New { tname; fields; base; _ } ->
      let fs = List.map (fun (n, e) ->
        Printf.sprintf "(field %s %s)" n (render_expr e)) fields in
      let base_s = match base with
        | None -> ""
        | Some b -> " (base " ^ render_expr b ^ ")"
      in
      Printf.sprintf "(new %s %s%s)"
        (String.concat "::" tname)
        (String.concat " " fs) base_s
  | Ast.NewEnum { tname; args; _ } ->
      Printf.sprintf "(new-enum %s %s)"
        (String.concat "::" tname)
        (String.concat " " (List.map render_expr args))
  | Ast.EnumLit { tname; variant; args; _ } ->
      let args_s = match args with
        | Ast.EATuple [] -> ""
        | Ast.EATuple es ->
            " " ^ String.concat " " (List.map render_expr es)
        | Ast.EAStruct fs ->
            " " ^ String.concat " "
              (List.map (fun (n, e) ->
                Printf.sprintf "(field %s %s)" n (render_expr e)) fs)
      in
      Printf.sprintf "(enum-lit %s::%s%s)"
        (String.concat "::" tname) variant args_s
  | Ast.Match { scrutinee; arms; _ } ->
      let arms_s =
        List.map (fun (a : Ast.match_arm) ->
          let g = match a.guard with
            | None -> ""
            | Some g -> " (guard " ^ render_expr g ^ ")"
          in
          Printf.sprintf "(arm %s%s %s)"
            (render_pattern a.pat) g (render_expr a.body))
          arms
      in
      Printf.sprintf "(match %s %s)"
        (render_expr scrutinee) (String.concat " " arms_s)
  | Ast.If { cond; then_blk; else_blk; _ } ->
      let then_s =
        Printf.sprintf "(then %s)"
          (String.concat " " (List.map render_stmt then_blk))
      in
      let else_s = match else_blk with
        | None -> ""
        | Some b ->
            Printf.sprintf " (else %s)"
              (String.concat " " (List.map render_stmt b))
      in
      Printf.sprintf "(if %s %s%s)"
        (render_expr cond) then_s else_s
  | Ast.ArrayLit (es, _) ->
      Printf.sprintf "(array-lit %s)"
        (String.concat " " (List.map render_expr es))
  | Ast.ArrayRepeat { value; count; _ } ->
      Printf.sprintf "(array-repeat %s %s)"
        (render_expr value) (render_expr count)
  | Ast.Index { base; index; _ } ->
      Printf.sprintf "(index %s %s)"
        (render_expr base) (render_expr index)
  | Ast.Range { lo; hi; inclusive; _ } ->
      Printf.sprintf "(range %s %s %s)"
        (render_expr lo) (render_expr hi)
        (if inclusive then "inclusive" else "exclusive")
  | Ast.Block (stmts, _) ->
      Printf.sprintf "(block %s)"
        (String.concat " " (List.map render_stmt stmts))
  | Ast.Lambda { params; ret_ty; body; _ } ->
      let ps = List.map (fun (n, t) ->
        Printf.sprintf "(param %s %s)" n (render_type_ann t)) params in
      let r = match ret_ty with
        | None -> "void"
        | Some t -> render_type_ann t
      in
      Printf.sprintf "(lambda (%s) %s %s)"
        (String.concat " " ps) r (render_expr body)

and render_pattern (p : Ast.pattern) : string =
  match p with
  | Ast.PWildcard _ -> "_"
  | Ast.PVar (n, _) -> Printf.sprintf "(pat-var %s)" n
  | Ast.PLit (n, _) -> Printf.sprintf "(pat-lit %d)" n
  | Ast.PVariant { tname; variant; binds; _ } ->
      let binds_s = match binds with
        | Ast.PBTuple [] -> ""
        | Ast.PBTuple ps ->
            " " ^ String.concat " " (List.map render_pattern ps)
        | Ast.PBStruct fs ->
            " " ^ String.concat " "
              (List.map (fun (n, p) ->
                Printf.sprintf "(field %s %s)" n (render_pattern p)) fs)
      in
      Printf.sprintf "(pat-variant %s::%s%s)"
        (String.concat "::" tname) variant binds_s
  | Ast.POr (alts, _) ->
      Printf.sprintf "(pat-or %s)"
        (String.concat " " (List.map render_pattern alts))

and render_stmt (s : Ast.stmt) : string =
  match s with
  | Ast.Let { name; value; ty_ann; is_mut; _ } ->
      let mut_s = if is_mut then " mut" else "" in
      let ty_s = match ty_ann with
        | None -> ""
        | Some t -> " (ty " ^ render_type_ann t ^ ")"
      in
      Printf.sprintf "(let%s %s%s %s)"
        mut_s name ty_s (render_expr value)
  | Ast.LetTuple { names; value; is_mut; _ } ->
      let mut_s = if is_mut then " mut" else "" in
      Printf.sprintf "(let-tuple%s (%s) %s)"
        mut_s (String.concat " " names) (render_expr value)
  | Ast.LetElse { pat; value; else_body; _ } ->
      Printf.sprintf "(let-else %s %s (else %s))"
        (render_pattern pat) (render_expr value)
        (String.concat " " (List.map render_stmt else_body))
  | Ast.Assign { path; value; _ } ->
      Printf.sprintf "(assign %s %s)"
        (String.concat "::" path) (render_expr value)
  | Ast.AssignField { target; field; value; _ } ->
      Printf.sprintf "(assign-field %s %s %s)"
        (render_expr target) field (render_expr value)
  | Ast.AssignIndex { base; index; value; _ } ->
      Printf.sprintf "(assign-index %s %s %s)"
        (render_expr base) (render_expr index) (render_expr value)
  | Ast.AssignDeref { target; value; _ } ->
      Printf.sprintf "(assign-deref %s %s)"
        (render_expr target) (render_expr value)
  | Ast.Return (None, _) -> "(return)"
  | Ast.Return (Some e, _) ->
      Printf.sprintf "(return %s)" (render_expr e)
  | Ast.ExprStmt e -> Printf.sprintf "(expr-stmt %s)" (render_expr e)
  | Ast.Tail e -> Printf.sprintf "(tail %s)" (render_expr e)
  | Ast.While { cond; body } ->
      Printf.sprintf "(while %s %s)"
        (render_expr cond)
        (String.concat " " (List.map render_stmt body))
  | Ast.For { var; range; body; _ } ->
      Printf.sprintf "(for %s %s %s)"
        var (render_expr range)
        (String.concat " " (List.map render_stmt body))
  | Ast.Defer { body; _ } ->
      Printf.sprintf "(defer %s)"
        (String.concat " " (List.map render_stmt body))
  | Ast.With { target; name; body; _ } ->
      Printf.sprintf "(with %s %s %s)"
        name (render_expr target)
        (String.concat " " (List.map render_stmt body))
  | Ast.Break _ -> "(break)"
  | Ast.Continue _ -> "(continue)"

let render_param (p : Ast.param) : string =
  let mut_s = if p.is_mut then " mut" else "" in
  Printf.sprintf "(param%s %s %s)" mut_s p.pname
    (render_type_ann p.pty)

let render_func (f : Ast.func) : string =
  let header_parts = [
    f.name;
    if f.is_extern then " extern" else "";
    if f.is_pub then " pub" else "";
    if f.is_variadic then " variadic" else "";
    if f.must_use then " must-use" else "";
    if f.escapes_hatch then " escapes" else "";
  ] in
  let tparams_s =
    if f.tparams = [] then ""
    else
      Printf.sprintf " (tparams %s)"
        (String.concat " " f.tparams)
  in
  let params_s =
    Printf.sprintf "(params %s)"
      (String.concat " " (List.map render_param f.params))
  in
  let ret_s = match f.ret_ty with
    | None -> "void"
    | Some t -> render_type_ann t
  in
  let body_s =
    if f.is_extern then ""
    else
      " (body " ^
      String.concat " " (List.map render_stmt f.body) ^
      ")"
  in
  Printf.sprintf "(fn %s%s %s %s%s)"
    (String.concat "" header_parts) tparams_s params_s ret_s body_s

let render_struct_decl (s : Ast.struct_decl) : string =
  let tparams_s =
    if s.stparams = [] then ""
    else
      Printf.sprintf " (tparams %s)" (String.concat " " s.stparams)
  in
  let fields_s = String.concat " "
    (List.map (fun (n, t) ->
      Printf.sprintf "(field %s %s)" n (render_type_ann t)) s.sfields)
  in
  let pub_s = if s.sis_pub then " pub" else "" in
  let move_s = if s.sis_move then " move" else "" in
  Printf.sprintf "(struct %s%s%s%s (fields %s))"
    s.sname pub_s move_s tparams_s fields_s

let render_enum_decl (e : Ast.enum_decl) : string =
  let tparams_s =
    if e.etparams = [] then ""
    else
      Printf.sprintf " (tparams %s)" (String.concat " " e.etparams)
  in
  let variants_s = String.concat " "
    (List.map (fun (v : Ast.enum_variant) ->
      let kind = match v.vkind with
        | Ast.VUnit -> ""
        | Ast.VTuple ts ->
            " " ^ String.concat " " (List.map render_type_ann ts)
        | Ast.VStruct fs ->
            " " ^ String.concat " "
              (List.map (fun (n, t) ->
                Printf.sprintf "(field %s %s)" n (render_type_ann t))
                fs)
      in
      Printf.sprintf "(variant %s%s)" v.vname kind) e.evariants)
  in
  let pub_s = if e.eis_pub then " pub" else "" in
  Printf.sprintf "(enum %s%s%s (variants %s))"
    e.ename pub_s tparams_s variants_s

let rec render_item (it : Ast.item) : string =
  match it with
  | Ast.Function f -> render_func f
  | Ast.Struct s -> render_struct_decl s
  | Ast.Enum e -> render_enum_decl e
  | Ast.Const c ->
      Printf.sprintf "(const %s %s %s)"
        c.kname (render_type_ann c.kty) (render_expr c.kvalue)
  | Ast.TypeAlias ta ->
      let tparams_s =
        if ta.tatparams = [] then ""
        else
          Printf.sprintf " (tparams %s)"
            (String.concat " " ta.tatparams)
      in
      Printf.sprintf "(type-alias %s%s %s)"
        ta.taname tparams_s (render_type_ann ta.tatarget)
  | Ast.ExternStruct es ->
      Printf.sprintf "(extern-struct %s)" es.esname
  | Ast.ExternType et ->
      Printf.sprintf "(extern-type %s)" et.xtname
  | Ast.ExternConst ec ->
      Printf.sprintf "(extern-const %s %s)"
        ec.ecname (render_type_ann ec.ecty)
  | Ast.ExternVar ev ->
      Printf.sprintf "(extern-var %s %s)"
        ev.evname (render_type_ann ev.evty)
  | Ast.Impl ib ->
      let trait_s = match ib.itrait with
        | None -> ""
        | Some p -> Printf.sprintf " (trait %s)" (String.concat "::" p)
      in
      let methods_s =
        String.concat " " (List.map render_func ib.iitems)
      in
      Printf.sprintf "(impl %s%s %s)"
        (String.concat "::" ib.itarget) trait_s methods_s
  | Ast.Trait td ->
      let methods_s =
        String.concat " " (List.map render_func td.trmethods)
      in
      Printf.sprintf "(trait %s %s)" td.trname methods_s
  | Ast.View v ->
      let cases_s = String.concat " "
        (List.map (fun (c : Ast.view_case) ->
          let fields = match c.vcfields with
            | [] -> ""
            | fs ->
                " " ^ String.concat " "
                  (List.map (fun (n, t) ->
                    Printf.sprintf "(field %s %s)" n (render_type_ann t))
                    fs)
          in
          Printf.sprintf "(case %s%s)" c.vcname fields) v.vcases)
      in
      Printf.sprintf "(view %s %s %s (body %s))"
        v.vname (render_param v.vparam) cases_s
        (String.concat " " (List.map render_stmt v.vbody))
  | Ast.Module m ->
      let inner =
        String.concat "\n  " (List.map render_item m.mitems)
      in
      Printf.sprintf "(module %s\n  %s)" m.mname inner
  | Ast.Use { path; is_wildcard; is_pub; _ } ->
      let suffix = if is_wildcard then "::*" else "" in
      let pub_s = if is_pub then "pub " else "" in
      Printf.sprintf "(use %s%s%s)"
        pub_s (String.concat "::" path) suffix
  | Ast.CInclude { path; _ } ->
      Printf.sprintf "(c-include %s)" (escape_string path)

let dump_ast ~file (program : Ast.program) : string =
  let buf = Buffer.create 1024 in
  header buf "ast" file;
  List.iter (fun it ->
    buf_add buf (render_item it);
    buf_char buf '\n')
    program;
  Buffer.contents buf

(* ===== Typed IR dump ===== *)

(* IR dump runs post-typecheck and post-lift, so TFor / TForEach /
   TStructApp / TVar are all gone — their appearance in the dump is
   an internal-compiler-bug signal.  Every texpr carries an explicit
   `:ty <render_typ_canonical>` annotation so dispatch / inference /
   mono decisions are visible in the diff.  A sorted `(mono-instances
   ...)` section follows the fn list so hash-table iteration order
   never leaks into the dump. *)

let render_int_lit n = Printf.sprintf "%d" n

let rec render_tpattern (p : tpattern) : string =
  match p with
  | TPWildcard -> "_"
  | TPVar n -> Printf.sprintf "(pat-var %s)" n
  | TPLit n -> Printf.sprintf "(pat-lit %d)" n
  | TPVariant { variant; tag; binds } ->
      let binds_s =
        if binds = [] then ""
        else
          " " ^ String.concat " "
            (List.map (fun (n, p) ->
              Printf.sprintf "(field %s %s)" n (render_tpattern p))
              binds)
      in
      Printf.sprintf "(pat-variant %s tag=%d%s)" variant tag binds_s
  | TPOr alts ->
      Printf.sprintf "(pat-or %s)"
        (String.concat " " (List.map render_tpattern alts))

let rec render_texpr (te : texpr) : string =
  let ty = render_typ_canonical te.ty in
  let body = match te.e with
    | TIntLit n -> Printf.sprintf "(int %s)" (render_int_lit n)
    | TFloatLit (f, w) ->
        let tag = match w with Ast.F32 -> "f32" | Ast.F64 -> "f64" in
        Printf.sprintf "(float %h %s)" f tag
    | TBoolLit b -> Printf.sprintf "(bool %b)" b
    | TNullLit -> "null"
    | TStringLit s ->
        Printf.sprintf "(string %s)" (escape_string s)
    | TVar n -> Printf.sprintf "(var %s)" n
    | TFnRef n -> Printf.sprintf "(fn-ref %s)" n
    | TNeg sub -> Printf.sprintf "(neg %s)" (render_texpr sub)
    | TBitNot sub ->
        Printf.sprintf "(bit-not %s)" (render_texpr sub)
    | TNot sub -> Printf.sprintf "(not %s)" (render_texpr sub)
    | TBinOp (op, l, r) ->
        Printf.sprintf "(binop %s %s %s)"
          (render_binop op) (render_texpr l) (render_texpr r)
    | TCall { mangled; args } ->
        Printf.sprintf "(call %s%s)" mangled
          (if args = [] then ""
           else " " ^ String.concat " " (List.map render_texpr args))
    | TBuiltinCall { name; args } ->
        Printf.sprintf "(builtin-call %s%s)" name
          (if args = [] then ""
           else " " ^ String.concat " " (List.map render_texpr args))
    | TIndirectCall { fn_expr; args } ->
        Printf.sprintf "(indirect-call %s%s)" (render_texpr fn_expr)
          (if args = [] then ""
           else " " ^ String.concat " " (List.map render_texpr args))
    | TCast (sub, ann) ->
        Printf.sprintf "(cast %s %s)"
          (render_texpr sub) (render_type_ann ann)
    | TTupleLit es ->
        Printf.sprintf "(tuple-lit %s)"
          (String.concat " " (List.map render_texpr es))
    | TStructLit { sname_path; fields; base } ->
        let fs = List.map (fun (n, e) ->
          Printf.sprintf "(field %s %s)" n (render_texpr e)) fields in
        let base_s = match base with
          | None -> ""
          | Some b -> " (base " ^ render_texpr b ^ ")"
        in
        Printf.sprintf "(struct-lit %s %s%s)"
          (String.concat "::" sname_path)
          (String.concat " " fs) base_s
    | TFieldAccess { target; field } ->
        Printf.sprintf "(field-access %s %s)"
          (render_texpr target) field
    | TRef sub -> Printf.sprintf "(ref %s)" (render_texpr sub)
    | TDeref sub ->
        Printf.sprintf "(deref %s)" (render_texpr sub)
    | TNew { sname_path; fields; base } ->
        let fs = List.map (fun (n, e) ->
          Printf.sprintf "(field %s %s)" n (render_texpr e)) fields in
        let base_s = match base with
          | None -> ""
          | Some b -> " (base " ^ render_texpr b ^ ")"
        in
        Printf.sprintf "(new %s %s%s)"
          (String.concat "::" sname_path)
          (String.concat " " fs) base_s
    | TEnumLit { ename_path; variant; tag; args } ->
        let args_s =
          if args = [] then ""
          else
            " " ^ String.concat " "
              (List.map (fun (n, e) ->
                Printf.sprintf "(field %s %s)" n (render_texpr e))
                args)
        in
        Printf.sprintf "(enum-lit %s::%s tag=%d%s)"
          (String.concat "::" ename_path) variant tag args_s
    | TNewEnum { ename_path; variant; tag; args } ->
        let args_s =
          if args = [] then ""
          else
            " " ^ String.concat " "
              (List.map (fun (n, e) ->
                Printf.sprintf "(field %s %s)" n (render_texpr e))
                args)
        in
        Printf.sprintf "(new-enum %s::%s tag=%d%s)"
          (String.concat "::" ename_path) variant tag args_s
    | TMatch { scrutinee; ename_path; arms } ->
        let arms_s = List.map (fun (a : tmatch_arm) ->
          let g = match a.tguard with
            | None -> ""
            | Some g -> " (guard " ^ render_texpr g ^ ")"
          in
          let div = if a.tdiverges then " diverges" else "" in
          Printf.sprintf "(arm %s%s%s %s)"
            (render_tpattern a.tpat) g div (render_texpr a.tbody))
          arms
        in
        Printf.sprintf "(match %s :enum %s %s)"
          (render_texpr scrutinee)
          (String.concat "::" ename_path)
          (String.concat " " arms_s)
    | TArrayLit es ->
        Printf.sprintf "(array-lit %s)"
          (String.concat " " (List.map render_texpr es))
    | TArrayRepeat { value; count } ->
        Printf.sprintf "(array-repeat %s %d)"
          (render_texpr value) count
    | TIndex { base; index } ->
        Printf.sprintf "(index %s %s)"
          (render_texpr base) (render_texpr index)
    | TIfExpr { cond; then_val; else_val } ->
        Printf.sprintf "(if-expr %s %s %s)"
          (render_texpr cond)
          (render_texpr then_val)
          (render_texpr else_val)
    | TSizeOf t ->
        Printf.sprintf "(size-of %s)" (render_typ_canonical t)
    | TBlock { stmts; trailing } ->
        let trail_s = match trailing with
          | None -> ""
          | Some t -> " (trailing " ^ render_texpr t ^ ")"
        in
        Printf.sprintf "(block (stmts %s)%s)"
          (String.concat " " (List.map render_tstmt stmts))
          trail_s
  in
  Printf.sprintf "%s :ty %s" body ty

and render_tstmt (s : tstmt) : string =
  match s with
  | TLet { name; value; _ } ->
      Printf.sprintf "(let %s %s)" name (render_texpr value)
  | TLetTuple { names; value; _ } ->
      Printf.sprintf "(let-tuple (%s) %s)"
        (String.concat " " names) (render_texpr value)
  | TAssign { path; value; _ } ->
      Printf.sprintf "(assign %s %s)"
        (String.concat "::" path) (render_texpr value)
  | TAssignField { target; field; value; _ } ->
      Printf.sprintf "(assign-field %s %s %s)"
        (render_texpr target) field (render_texpr value)
  | TAssignIndex { base; index; value; _ } ->
      Printf.sprintf "(assign-index %s %s %s)"
        (render_texpr base) (render_texpr index) (render_texpr value)
  | TAssignDeref { target; value; _ } ->
      Printf.sprintf "(assign-deref %s %s)"
        (render_texpr target) (render_texpr value)
  | TReturn { value = None; _ } -> "(return)"
  | TReturn { value = Some v; _ } ->
      Printf.sprintf "(return %s)" (render_texpr v)
  | TExprStmt e -> Printf.sprintf "(expr-stmt %s)" (render_texpr e)
  | TIf { cond; then_body; else_body } ->
      Printf.sprintf "(if %s (then %s) (else %s))"
        (render_texpr cond)
        (String.concat " " (List.map render_tstmt then_body))
        (String.concat " " (List.map render_tstmt else_body))
  | TWhile { cond; body; post } ->
      let post_s =
        if post = [] then ""
        else " (post " ^
          String.concat " " (List.map render_tstmt post) ^ ")"
      in
      Printf.sprintf "(while %s (body %s)%s)"
        (render_texpr cond)
        (String.concat " " (List.map render_tstmt body))
        post_s
  | TFor _ ->
      "(t-for-transient!)"
      (* TFor should be lifted away; surfacing it is a compiler bug. *)
  | TForEach _ -> "(t-for-each-transient!)"
  | TDefer { body; _ } ->
      Printf.sprintf "(defer %s)"
        (String.concat " " (List.map render_tstmt body))
  | TBreak _ -> "(break)"
  | TContinue _ -> "(continue)"

let render_tfunc (tf : tfunc) : string =
  let f = tf.tf_func in
  let header_parts = [
    tf.tf_mangled;
    if f.is_extern then " extern" else "";
    if f.is_pub then " pub" else "";
    if f.is_variadic then " variadic" else "";
  ] in
  let params_s = String.concat " "
    (List.map2 (fun (p : Ast.param) ty ->
      let mut_s = if p.is_mut then " mut" else "" in
      Printf.sprintf "(param%s %s %s)" mut_s p.pname
        (render_typ_canonical ty))
      f.params tf.tf_param_tys)
  in
  let ret_s = match tf.tf_ret_ty with
    | None -> "void"
    | Some t -> render_typ_canonical t
  in
  let body_s =
    if f.is_extern then ""
    else
      " (body " ^
      String.concat " " (List.map render_tstmt tf.tf_body) ^
      ")"
  in
  Printf.sprintf "(tfn %s (params %s) %s%s)"
    (String.concat "" header_parts) params_s ret_s body_s

let dump_typed_ir ~file ?(user_only = false) (tp : tprogram) : string =
  let buf = Buffer.create 4096 in
  header buf "typed-ir" file;
  (* Functions in declaration order — tp_funcs is already source-
     ordered modulo the prelude prefix, so we just iterate. *)
  let funcs =
    if user_only then
      List.filter (fun tf ->
        tf.tf_func.Ast.pos.file <> "<prelude>")
        tp.tp_funcs
    else tp.tp_funcs
  in
  List.iter (fun tf ->
    buf_add buf (render_tfunc tf);
    buf_char buf '\n')
    funcs;
  (* Sorted mono instances — struct + enum.  Iteration over the IR
     index would leak insertion order; sort by canonical name so the
     output is stable across runs. *)
  let struct_names =
    List.map (fun (s : struct_sig) ->
      String.concat "::" s.sname_path) tp.tp_struct_index
    |> List.sort_uniq compare
  in
  let enum_names =
    List.map (fun (e : enum_sig) ->
      String.concat "::" e.ename_path) tp.tp_enum_index
    |> List.sort_uniq compare
  in
  Printf.bprintf buf "(mono-instances\n";
  List.iter (fun n -> Printf.bprintf buf "  (struct %s)\n" n)
    struct_names;
  List.iter (fun n -> Printf.bprintf buf "  (enum %s)\n" n)
    enum_names;
  Printf.bprintf buf ")\n";
  Buffer.contents buf

