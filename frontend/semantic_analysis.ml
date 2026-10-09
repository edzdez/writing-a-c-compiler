open! Core

let analyze program =
  program |> Resolve_variables.program |> Resolve_labels.program |> Label_loops.program
;;
