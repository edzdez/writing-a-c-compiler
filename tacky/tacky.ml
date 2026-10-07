open! Core
module Lower_pass = Lower
module Ir = Ir

module Lower = struct
  let program (program : Frontend.Program.validated Frontend.Program.t) =
    program |> Frontend.Program.ast |> Lower_pass.program
  ;;
end
