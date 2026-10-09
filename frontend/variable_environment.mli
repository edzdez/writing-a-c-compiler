open! Core

module Declaration : sig
  type t =
    | Declared of string
    | Duplicate
end

type t

val create : unit -> t
val declare : t -> source_name:string -> Declaration.t
val find : t -> string -> string or_null
