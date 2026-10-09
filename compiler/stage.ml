open! Core

type t =
  | Lex
  | Parse
  | Validate
  | Tacky
  | Codegen
  | Emit
  | Assemble
  | Link
[@@deriving compare, equal, sexp]

let of_flags ~lex ~parse ~validate ~tacky ~codegen ~emit ~assemble =
  match
    List.filter_opt
      [ Option.some_if lex Lex
      ; Option.some_if parse Parse
      ; Option.some_if validate Validate
      ; Option.some_if tacky Tacky
      ; Option.some_if codegen Codegen
      ; Option.some_if assemble Assemble
      ; Option.some_if emit Emit
      ]
  with
  | [] -> Ok Link
  | [ stop_after ] -> Ok stop_after
  | _ -> Or_error.error_s [%message "at most one stopping option may be specified"]
;;
