open! Core

type t

val create : start_position:Lexing.position -> end_position:Lexing.position -> t
val start_position : t -> Lexing.position
val end_position : t -> Lexing.position
val between : t -> t -> t
val sexp_of_t : t -> Sexp.t
