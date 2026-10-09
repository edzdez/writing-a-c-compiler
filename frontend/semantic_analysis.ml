open! Core

let analyze program =
  program
  |> Resolve_variables.program
  |> Resolve_labels.program
  |> Label_loops.program
  |> Label_switches.program
  |> Resolve_breaks_and_continues.program
;;
