(** Select x86-64 instructions from TACKY.

    The result may contain pseudoregisters and instructions that are not yet encodable. *)
open! Core

val program : Tacky.Ir.Prog.t -> Ir.Prog.t
