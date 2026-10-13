(** Assign each pseudoregister a stack slot and allocate the stack frame. *)
open! Core

val program : Ir.Prog.t -> symbols:Semantics.Symbol_table.t -> Ir.Prog.t
