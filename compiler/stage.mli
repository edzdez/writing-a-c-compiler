open! Core

type t =
  | Lex
  | Parse
  | Validate
  | Tacky
  | Codegen
  | Emit
  | Link
[@@deriving compare, equal, sexp]

val of_flags
  :  lex:bool
  -> parse:bool
  -> validate:bool
  -> tacky:bool
  -> codegen:bool
  -> emit:bool
  -> t Or_error.t
