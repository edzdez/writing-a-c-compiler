open! Core

type t = { mutable next : int }

let create () = { next = 0 }

let fresh t =
  let fresh = t.next in
  t.next <- fresh + 1;
  fresh
;;
