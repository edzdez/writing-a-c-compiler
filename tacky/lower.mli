(** Translate the source AST into TACKY IR. *)
open! Core

val program : Frontend.Ast.Prog.t -> symbols:Semantics.Symbol_table.t -> Ir.Prog.t
