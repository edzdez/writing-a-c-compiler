open! Core

type t =
  | Int
  | Fun of
      { params : t iarray
      ; result : t
      }
[@@deriving equal, sexp_of]
