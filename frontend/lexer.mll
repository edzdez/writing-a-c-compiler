{
open! Core

let raise_error lexbuf =
  let lexeme = Lexing.lexeme lexbuf in
  let position = Lexing.lexeme_start_p lexbuf in
  Error_helpers.raise_error [%string "invalid token %{lexeme#String}"] ~position
;;

let ident_or_keyword lexbuf =
  match Lexing.lexeme lexbuf with
  | "int" -> Token.Int
  | "void" -> Void
  | "return" -> Return
  | ident -> Ident ident

let constant lexbuf =
  match Int.of_string_opt (Lexing.lexeme lexbuf) with
  | Some constant -> Token.Const constant
  | None -> raise_error lexbuf
;;
}

let digit = ['0'-'9']
let word = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let ident_start = ['a'-'z' 'A'-'Z' '_']
let ws = [' ' '\t' '\r' '\012']

rule next = parse
  | ws+                    { next lexbuf }
  | '\n'                   { Lexing.new_line lexbuf; next lexbuf }
  | ident_start word*      { ident_or_keyword lexbuf }
  | digit+ ident_start+    { raise_error lexbuf }
  | digit+                 { constant lexbuf }
  | '('                    { Token.Open_paren }
  | ')'                    { Token.Close_paren }
  | '{'                    { Token.Open_brace }
  | '}'                    { Token.Close_brace }
  | ';'                    { Token.Semicolon }
  | "--"                   { Token.Double_hyphen }
  | '-'                    { Token.Hyphen }
  | '~'                    { Token.Tilde }
  | '+'                    { Token.Plus }
  | '*'                    { Token.Star }
  | '/'                    { Token.Slash }
  | '%'                    { Token.Percent }
  | '&'                    { Token.Ampersand }
  | '|'                    { Token.Bar }
  | '^'                    { Token.Caret }
  | "<<"                   { Token.Left_chevron }
  | ">>"                   { Token.Right_chevron }
  | "&&"                   { Token.Double_amp }
  | "||"                   { Token.Double_bar }
  | "=="                   { Token.Double_eq }
  | "!="                   { Token.Bang_eq }
  | "<="                   { Token.Lte }
  | ">="                   { Token.Gte }
  | '!'                    { Token.Bang }
  | '<'                    { Token.Lt }
  | '>'                    { Token.Gt }
  | eof                    { Token.Eof }
  | _                      { raise_error lexbuf }
