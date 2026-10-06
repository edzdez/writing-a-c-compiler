open! Core

let program tacky = tacky |> Select.program |> Stack_layout.program |> Fixup.program
