(** Assign each pseudoregister a stack slot and allocate the stack frame. *)
open! Core

val program : Ir.Prog.t -> Ir.Prog.t
