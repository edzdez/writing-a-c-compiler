open! Core
module Ir = Ir

module Lower : sig
  val program : Frontend.Program.validated Frontend.Program.t -> Ir.Prog.t
end
