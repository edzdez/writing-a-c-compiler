open! Core

type t =
  | Ident of string
  | Const of int
  | Int
  | Void
  | Return
  | Open_paren
  | Close_paren
  | Open_brace
  | Close_brace
  | Semicolon
  | Hyphen
  | Double_hyphen
  | Tilde
  | Eof
[@@deriving compare, equal, sexp]
