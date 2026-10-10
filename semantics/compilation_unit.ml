open! Core

type 'program t =
  { program : 'program
  ; symbols : Symbol_table.t
  }
[@@deriving fields ~getters]

let create ~program ~symbols = { program; symbols }
let map_program t ~f = { t with program = f t.program }
