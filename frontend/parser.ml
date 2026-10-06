open! Core
open Ast
module Located = Source.Located

let raise_error actual ~expected =
  let position = actual |> Located.location |> Source.Location.start_position in
  let actual = actual |> Located.value |> Token.sexp_of_t |> Sexp.to_string_hum in
  Error_helpers.raise_error [%string "expected %{expected}, found %{actual}"] ~position
;;

let expect token_stream expected =
  let actual = Token_stream.pop token_stream in
  match Token.equal (Located.value actual) expected with
  | true -> actual
  | false ->
    raise_error actual ~expected:(expected |> Token.sexp_of_t |> Sexp.to_string_hum)
;;

let parse_unary_op token_stream =
  let token = Token_stream.pop token_stream in
  match Located.value token with
  | Hyphen -> Located.create Unary_op.Negate ~location:(Located.location token)
  | Tilde -> Located.create Unary_op.Complement ~location:(Located.location token)
  | _ -> raise_error token ~expected:"a unary operator"
;;

let precedence bin_op =
  match bin_op with
  | Token.Plus -> 45
  | Hyphen -> 45
  | Star -> 50
  | Slash -> 50
  | Percent -> 50
  | _ -> raise_s [%message "unknown precedence" (bin_op : Token.t)]
;;

let parse_binary_op token_stream =
  let token = Token_stream.pop token_stream in
  match Located.value token with
  | Plus -> Located.create Binary_op.Add ~location:(Located.location token)
  | Hyphen -> Located.create Binary_op.Sub ~location:(Located.location token)
  | Star -> Located.create Binary_op.Mul ~location:(Located.location token)
  | Slash -> Located.create Binary_op.Div ~location:(Located.location token)
  | Percent -> Located.create Binary_op.Rem ~location:(Located.location token)
  | _ -> raise_error token ~expected:"a binary operator"
;;

let rec parse_expr token_stream ~min_precedence =
  let mutable left = parse_factor token_stream in
  let mutable token = Located.value (Token_stream.peek token_stream ~distance:0) in
  while
    match token with
    | Plus | Hyphen | Star | Slash | Percent -> precedence token >= min_precedence
    | _ -> false
  do
    let op = parse_binary_op token_stream in
    let right = parse_expr token_stream ~min_precedence:(precedence token + 1) in
    let location =
      Source.Location.between (Located.location left) (Located.location right)
    in
    left <- Located.create (Expr.Binary { op; left; right }) ~location;
    token <- Located.value (Token_stream.peek token_stream ~distance:0)
  done;
  left

and parse_factor token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match Located.value token with
  | Const constant ->
    let _ = Token_stream.pop token_stream in
    Located.create (Expr.Const constant) ~location:(Located.location token)
  | Hyphen | Tilde ->
    let op = parse_unary_op token_stream in
    let expr = parse_factor token_stream in
    let location =
      Source.Location.between (Located.location op) (Located.location expr)
    in
    Located.create (Expr.Unary { op; expr }) ~location
  | Open_paren ->
    let open_paren = Token_stream.pop token_stream in
    let expr = parse_expr token_stream ~min_precedence:0 in
    let close_paren = expect token_stream Close_paren in
    let location =
      Source.Location.between (Located.location open_paren) (Located.location close_paren)
    in
    Located.create (Located.value expr) ~location
  | _ -> raise_error token ~expected:"an integer constant"
;;

let parse_stmt token_stream =
  let return = expect token_stream Return in
  let expr = parse_expr token_stream ~min_precedence:0 in
  let semicolon = expect token_stream Semicolon in
  let location =
    Source.Location.between (Located.location return) (Located.location semicolon)
  in
  Located.create (Stmt.Return expr) ~location
;;

let parse_fun_def token_stream =
  let int = expect token_stream Int in
  let name =
    let token = Token_stream.pop token_stream in
    match Located.value token with
    | Ident name -> name
    | _ -> raise_error token ~expected:"an identifier"
  in
  ignore (expect token_stream Open_paren);
  ignore (expect token_stream Void);
  ignore (expect token_stream Close_paren);
  ignore (expect token_stream Open_brace);
  let body = parse_stmt token_stream in
  let close_brace = expect token_stream Close_brace in
  let location =
    Source.Location.between (Located.location int) (Located.location close_brace)
  in
  Located.create { Fun_def.name; body } ~location
;;

let parse token_stream =
  let fun_def = parse_fun_def token_stream in
  ignore (expect token_stream Eof);
  Located.create fun_def ~location:(Located.location fun_def)
;;
