open! Core

type 'a t =
  { value : 'a
  ; location : Location.t
  }
[@@deriving fields ~getters, sexp_of]

let create value ~location = { value; location }
let map t ~f = { t with value = f t.value }
