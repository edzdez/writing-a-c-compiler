open! Core

type 'a t

val create : 'a -> location:Location.t -> 'a t
val value : 'a t -> 'a
val location : _ t -> Location.t
val map : 'a t -> f:('a -> 'b) -> 'b t
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
