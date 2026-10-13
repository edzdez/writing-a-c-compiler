open! Core

type t

val create : unit -> t
val find : t -> string -> Symbol.t or_null
val add_exn : t -> name:string -> symbol:Symbol.t -> unit
val set : t -> name:string -> symbol:Symbol.t -> unit
val iter : t -> f:(name:string -> symbol:Symbol.t -> unit) -> unit
