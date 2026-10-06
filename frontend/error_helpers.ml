open! Core

let raise_error msg ~position =
  let column = position.pos_cnum - position.pos_bol + 1 in
  let location =
    [%string "%{position.pos_fname}:%{position.pos_lnum#Int}:%{column#Int}"]
  in
  failwith [%string "%{location}: %{msg}"]
;;
