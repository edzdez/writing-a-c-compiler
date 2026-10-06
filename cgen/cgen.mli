open! Core

val cgen_expr : Ast.Expr.t -> Asm.Operand.t
val cgen_stmt : Ast.Stmt.t -> Asm.Instr.t Vec.t
val cgen_fun_def : Ast.Fun_def.t -> Asm.Fun_def.t
val cgen_prog : Ast.Prog.t -> Asm.Prog.t
