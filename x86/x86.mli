open! Core
module Ir = Ir

module Program : sig
  type selected
  type stack_allocated
  type legal
  type 'state t

  val ir : _ t -> Ir.Prog.t
  val symbols : _ t -> Semantics.Symbol_table.t
end

module Select : sig
  val program : Tacky.Ir.Prog.t Semantics.Compilation_unit.t -> Program.selected Program.t
end

module Stack_layout : sig
  val program : Program.selected Program.t -> Program.stack_allocated Program.t
end

module Fixup : sig
  val program : Program.stack_allocated Program.t -> Program.legal Program.t
end

module Lower : sig
  val program : Tacky.Ir.Prog.t Semantics.Compilation_unit.t -> Program.legal Program.t
end

module Emit : sig
  val program : Program.legal Program.t -> writer:Out_channel.t -> unit
end
