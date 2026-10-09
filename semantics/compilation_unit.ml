open! Core

type 'program t =
  { program : 'program
  ; symbols : Symbol_table.t
  }

let create ~program ~symbols = { program; symbols }
let program t = t.program
let symbols t = t.symbols
let map_program t ~f = { t with program = f t.program }
