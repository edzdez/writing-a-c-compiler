open! Core

let analyze program =
  let program =
    program
    |> Resolve_idents.program
    |> Resolve_labels.program
    |> Resolve_control_flow.program
  in
  let symbols = Type_check.program program in
  Semantics.Compilation_unit.create ~program ~symbols
;;
