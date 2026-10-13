open! Core
module Lower_pass = Lower
module Ir = Ir

module Lower = struct
  let program (program : Frontend.Program.validated Frontend.Program.t) =
    let symbols = Frontend.Program.symbols program in
    let program = Lower_pass.program (Frontend.Program.ast program) ~symbols in
    Semantics.Compilation_unit.create ~program ~symbols
  ;;
end
