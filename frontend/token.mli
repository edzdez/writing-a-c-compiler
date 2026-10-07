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
  | Hyphen_hyphen
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
  | Bang
  | Double_amp
  | Double_bar
  | Double_eq
  | Bang_eq
  | Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Plus_eq
  | Hyphen_eq
  | Star_eq
  | Slash_eq
  | Percent_eq
  | Amp_eq
  | Bar_eq
  | Caret_eq
  | Left_chevron_eq
  | Right_chevron_eq
  | Plus_plus
  | Eof
[@@deriving compare, equal, sexp]
