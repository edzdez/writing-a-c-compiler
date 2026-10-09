(** Assign IDs to switches and their cases, and reject invalid or duplicate cases. *)
open! Core

val program : Ast.Prog.t -> Ast.Prog.t
