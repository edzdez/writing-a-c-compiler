(** Lower TACKY to finalized x86-64 IR. *)
open! Core

val program : Tacky.Ir.Prog.t -> Ir.Prog.t
