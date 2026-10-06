open! Core

type t =
  | Lex
  | Parse
  | Codegen
  | Emit
  | Link
[@@deriving compare, equal, sexp]

val of_flags : lex:bool -> parse:bool -> codegen:bool -> emit:bool -> t Or_error.t
