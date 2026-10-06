open! Core

type t

val create : Lexing.lexbuf -> t
val peek : t -> distance:int -> Token.t Util.With_location.t
val pop : t -> Token.t Util.With_location.t
