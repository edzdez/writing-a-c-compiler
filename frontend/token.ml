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
  | Plus
  | Star
  | Slash
  | Percent
  | Ampersand
  | Bar
  | Caret
  | Left_chevron
  | Right_chevron
  | Eof
[@@deriving compare, equal, sexp]
