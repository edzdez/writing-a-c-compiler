(** Rewrite instructions with invalid operand combinations. *)
open! Core

val program : Ir.Prog.t -> Ir.Prog.t
