open! Core

val raise_error : string -> position:Lexing.position -> 'a
val raise_at : _ Source.Located.t -> string -> 'a
