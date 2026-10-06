open! Core

type t =
  | Lex
  | Parse
  | Tacky
  | Codegen
  | Emit
  | Link
[@@deriving compare, equal, sexp]

val of_flags
  :  lex:bool
  -> parse:bool
  -> tacky:bool
  -> codegen:bool
  -> emit:bool
  -> t Or_error.t
