open! Core

type t

val create : unit -> t
val declare : t -> source_name:string -> [ `Declared of string | `Duplicate ]
val find : t -> string -> string or_null
val open_scope : t -> unit
val close_scope : t -> unit
