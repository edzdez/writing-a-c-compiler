open! Core

type t

val create : Lexing.lexbuf -> t
val peek : t -> distance:int -> Token.t Source.Located.t
val pop : t -> Token.t Source.Located.t
