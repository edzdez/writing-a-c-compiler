(** Resolve each [break] and [continue] to its enclosing control-flow construct. *)
open! Core

val program : Ast.Prog.t -> Ast.Prog.t
