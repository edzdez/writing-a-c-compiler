(** Label loops and validate the uses of their associated control flow. *)
open! Core

val program : Ast.Prog.t -> Ast.Prog.t
