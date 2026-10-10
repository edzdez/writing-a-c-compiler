(** Write finalized x86-64 IR. *)
open! Core

val program
  :  Ir.Prog.t
  -> symbol_table:Semantics.Symbol_table.t
  -> writer:Out_channel.t
  -> unit
