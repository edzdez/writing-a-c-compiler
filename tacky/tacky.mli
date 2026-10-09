open! Core
module Ir = Ir

module Lower : sig
  val program
    :  Frontend.Program.validated Frontend.Program.t
    -> Ir.Prog.t Semantics.Compilation_unit.t
end
