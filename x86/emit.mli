(** Write finalized x86-64 IR. *)
open! Core

val program : Ir.Prog.t -> writer:Out_channel.t -> unit
