open! Core

module Linkage : sig
  type t =
    | No_linkage
    | External
  [@@deriving sexp_of]
end

type t

val create : unit -> t
val declare_var : t -> source_name:string -> [ `Declared of string | `Duplicate ]
val declare_fun : t -> source_name:string -> [ `Declared of string | `Duplicate ]
val find : t -> string -> string or_null
val open_scope : t -> unit
val close_scope : t -> unit
