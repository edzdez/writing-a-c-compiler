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
  | "if" -> If
  | "else" -> Else
  | "goto" -> Goto
  | "do" -> Do
  | "while" -> While
  | "for" -> For
  | "break" -> Break
  | "continue" -> Continue
  | "switch" -> Switch
  | "case" -> Case
  | "default" -> Default
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
  | "--"                   { Token.Hyphen_hyphen }
  | "-="                   { Token.Hyphen_eq }
  | '-'                    { Token.Hyphen }
  | '~'                    { Token.Tilde }
  | "++"                   { Token.Plus_plus }
  | "+="                   { Token.Plus_eq }
  | '+'                    { Token.Plus }
  | "*="                   { Token.Star_eq }
  | '*'                    { Token.Star }
  | "/="                   { Token.Slash_eq }
  | '/'                    { Token.Slash }
  | "%="                   { Token.Percent_eq }
  | '%'                    { Token.Percent }
  | "&="                   { Token.Amp_eq }
  | '&'                    { Token.Ampersand }
  | "|="                   { Token.Bar_eq }
  | '|'                    { Token.Bar }
  | "^="                   { Token.Caret_eq }
  | '^'                    { Token.Caret }
  | "<<="                  { Token.Left_chevron_eq }
  | "<<"                   { Token.Left_chevron }
  | ">>="                  { Token.Right_chevron_eq }
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
  | '='                    { Token.Eq }
  | '?'                    { Token.Question_mark }
  | ':'                    { Token.Colon }
  | eof                    { Token.Eof }
  | _                      { raise_error lexbuf }
