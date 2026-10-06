open! Core
open Ast
module Token = Lex.Token
module Token_stream = Lex.Token_stream
module With_location = Util.With_location

let raise_error actual ~expected =
  let position = actual |> With_location.location |> Util.Location.start_position in
  let column = position.pos_cnum - position.pos_bol + 1 in
  let actual = actual |> With_location.value |> Token.sexp_of_t |> Sexp.to_string_hum in
  failwith
    [%string
      "%{position.pos_fname}:%{position.pos_lnum#Int}:%{column#Int}: expected \
       %{expected}, found %{actual}"]
;;

let expect token_stream expected =
  let actual = Token_stream.pop token_stream in
  match Token.equal (With_location.value actual) expected with
  | true -> actual
  | false ->
    raise_error actual ~expected:(expected |> Token.sexp_of_t |> Sexp.to_string_hum)
;;

let parse_unary_op token_stream =
  let token = Token_stream.pop token_stream in
  match With_location.value token with
  | Hyphen ->
    With_location.create Unary_op.Negate ~location:(With_location.location token)
  | Tilde ->
    With_location.create Unary_op.Complement ~location:(With_location.location token)
  | _ -> raise_error token ~expected:"a unary operator"
;;

let rec parse_expr token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match With_location.value token with
  | Const constant ->
    let _ = Token_stream.pop token_stream in
    With_location.create (Expr.Const constant) ~location:(With_location.location token)
  | Hyphen | Tilde ->
    let op = parse_unary_op token_stream in
    let expr = parse_expr token_stream in
    With_location.create
      (Expr.Unary { op; expr })
      ~location:(With_location.location token)
  | Open_paren ->
    let _ = Token_stream.pop token_stream in
    let expr = parse_expr token_stream in
    let _ = expect token_stream Close_paren in
    expr
  | _ -> raise_error token ~expected:"an integer constant"
;;

let parse_stmt token_stream =
  let return = expect token_stream Return in
  let expr = parse_expr token_stream in
  let semicolon = expect token_stream Semicolon in
  let location =
    Util.Location.between
      (With_location.location return)
      (With_location.location semicolon)
  in
  With_location.create (Stmt.Return expr) ~location
;;

let parse_fun_def token_stream =
  let int = expect token_stream Int in
  let name =
    let token = Token_stream.pop token_stream in
    match With_location.value token with
    | Ident name -> name
    | _ -> raise_error token ~expected:"an identifier"
  in
  ignore (expect token_stream Open_paren : Token.t With_location.t);
  ignore (expect token_stream Void : Token.t With_location.t);
  ignore (expect token_stream Close_paren : Token.t With_location.t);
  ignore (expect token_stream Open_brace : Token.t With_location.t);
  let body = parse_stmt token_stream in
  let close_brace = expect token_stream Close_brace in
  let location =
    Util.Location.between
      (With_location.location int)
      (With_location.location close_brace)
  in
  With_location.create { Fun_def.name; body } ~location
;;

let parse_prog token_stream =
  let fun_def = parse_fun_def token_stream in
  ignore (expect token_stream Eof : Token.t With_location.t);
  With_location.create fun_def ~location:(With_location.location fun_def)
;;
