open! Core
module Lower_pass = Lower
module Ir = Ir

module Lower = struct
  let program (program : Frontend.Program.validated Frontend.Program.t) =
    let symbols = Frontend.Program.symbols program in
    let program = program |> Frontend.Program.ast |> Lower_pass.program in
    Semantics.Compilation_unit.create ~program ~symbols
  ;;
end
