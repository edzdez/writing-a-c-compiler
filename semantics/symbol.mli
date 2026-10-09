open! Core

module Attrs : sig
  type t =
    | Local
    | Fun of { defined : bool }
  [@@deriving sexp_of]
end

type t =
  { type_ : C_type.t
  ; attrs : Attrs.t
  }
[@@deriving sexp_of]
