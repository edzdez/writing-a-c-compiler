open! Core

type t = Symbol.t String.Table.t

let create () = String.Table.create ()
let find t name = Hashtbl.find_or_null t name
let add_exn t ~name ~symbol = Hashtbl.add_exn t ~key:name ~data:symbol
let set t ~name ~symbol = Hashtbl.set t ~key:name ~data:symbol

let iter t ~f =
  Hashtbl.iteri t ~f:(fun ~key:name ~data:symbol -> f ~name ~symbol)
;;
