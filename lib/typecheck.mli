(* Typecheck — the elaboration pass.  Walks an [Ast.program], resolves
   names, checks types, monomorphizes generics, and produces a fully
   typed [Ir.tprogram] ready for codegen.

   Everything else in this module is an implementation detail.  The
   ctx / fn_ctx records, the flatten / build_*_index passes, the
   prelude builder, the elaboration entry points — none of it should
   leak to callers.  This signature is the contract. *)

val check_program : Ast.program -> Ir.tprogram
