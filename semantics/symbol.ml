open! Core

module Attrs = struct
  module Initial_value = struct
    type t =
      | Tentative
      | Initial of int
      | No_initializer
    [@@deriving sexp_of]
  end

  type t =
    | Local
    | Fun of
        { defined : bool
        ; global : bool
        }
    | Static of
        { init : Initial_value.t
        ; global : bool
        }
  [@@deriving sexp_of]
end

type t =
  { type_ : C_type.t
  ; attrs : Attrs.t
  }
[@@deriving sexp_of]
