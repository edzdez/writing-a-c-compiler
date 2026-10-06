open! Core

val parse_unary_op : Lex.Token_stream.t -> Ast.Unary_op.t
val parse_expr : Lex.Token_stream.t -> Ast.Expr.t
val parse_stmt : Lex.Token_stream.t -> Ast.Stmt.t
val parse_fun_def : Lex.Token_stream.t -> Ast.Fun_def.t
val parse_prog : Lex.Token_stream.t -> Ast.Prog.t
