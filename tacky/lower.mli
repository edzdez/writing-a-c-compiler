(** Translate the source AST into TACKY IR. *)
open! Core

val program : Frontend.Ast.Prog.t -> Ir.Prog.t
