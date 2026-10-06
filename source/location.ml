open! Core

type t =
  { start_position : Lexing.position
  ; end_position : Lexing.position
  }
[@@deriving fields ~getters]

let create ~start_position ~end_position = { start_position; end_position }

let between first last =
  { start_position = first.start_position; end_position = last.end_position }
;;

let sexp_of_position position =
  let col = position.pos_cnum - position.pos_bol + 1 in
  [%sexp
    { filename = (position.pos_fname : string)
    ; line = (position.pos_lnum : int)
    ; col : int
    }]
;;

let sexp_of_t { start_position; end_position } =
  Sexp.List
    [ Sexp.List [ Sexp.Atom "start_position"; sexp_of_position start_position ]
    ; Sexp.List [ Sexp.Atom "end_position"; sexp_of_position end_position ]
    ]
;;
