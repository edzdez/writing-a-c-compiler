open! Core

let analyze program =
  let program =
    program
    |> Resolve_idents.program
    |> Resolve_labels.program
    |> Label_loops.program
    |> Label_switches.program
    |> Resolve_breaks_and_continues.program
  in
  let symbols = Type_check.program program in
  Semantics.Compilation_unit.create ~program ~symbols
;;
