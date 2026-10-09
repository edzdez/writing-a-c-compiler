(** Resolve local function labels and validate their uses. *)
open! Core

val program : Ast.Prog.t -> Ast.Prog.t
