open! Core

type 'program t

val create : program:'program -> symbols:Symbol_table.t -> 'program t
val program : 'program t -> 'program
val symbols : _ t -> Symbol_table.t
val map_program : 'a t -> f:('a -> 'b) -> 'b t
