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

let parse_prefix_unary_op token_stream =
  let token = Token_stream.pop token_stream in
  match Located.value token with
  | Hyphen -> Located.create Unary_op.Negate ~location:(Located.location token)
  | Tilde -> Located.create Unary_op.Complement ~location:(Located.location token)
  | Bang -> Located.create Unary_op.Not ~location:(Located.location token)
  | Plus_plus -> Located.create Unary_op.Pre_incr ~location:(Located.location token)
  | Hyphen_hyphen -> Located.create Unary_op.Pre_decr ~location:(Located.location token)
  | _ -> raise_error token ~expected:"a prefix unary operator"
;;

let parse_postfix_unary_op token_stream =
  let token = Token_stream.pop token_stream in
  match Located.value token with
  | Plus_plus -> Located.create Unary_op.Post_incr ~location:(Located.location token)
  | Hyphen_hyphen -> Located.create Unary_op.Post_decr ~location:(Located.location token)
  | _ -> raise_error token ~expected:"a postfix unary operator"
;;

let precedence bin_op =
  match bin_op with
  | Token.Plus -> 45
  | Hyphen -> 45
  | Star -> 50
  | Slash -> 50
  | Percent -> 50
  | Ampersand -> 25
  | Bar -> 15
  | Caret -> 20
  | Left_chevron -> 40
  | Right_chevron -> 40
  | Double_amp -> 10
  | Double_bar -> 5
  | Double_eq -> 30
  | Bang_eq -> 30
  | Lt -> 35
  | Gt -> 35
  | Lte -> 35
  | Gte -> 35
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
  | Right_chevron_eq -> 1
  | _ -> -1
;;

let parse_binary_op token_stream =
  let token = Token_stream.pop token_stream in
  match Located.value token with
  | Plus -> Located.create Binary_op.Add ~location:(Located.location token)
  | Hyphen -> Located.create Binary_op.Sub ~location:(Located.location token)
  | Star -> Located.create Binary_op.Mul ~location:(Located.location token)
  | Slash -> Located.create Binary_op.Div ~location:(Located.location token)
  | Percent -> Located.create Binary_op.Rem ~location:(Located.location token)
  | Ampersand -> Located.create Binary_op.Bit_and ~location:(Located.location token)
  | Bar -> Located.create Binary_op.Bit_or ~location:(Located.location token)
  | Caret -> Located.create Binary_op.Xor ~location:(Located.location token)
  | Left_chevron -> Located.create Binary_op.L_shift ~location:(Located.location token)
  | Right_chevron -> Located.create Binary_op.R_shift ~location:(Located.location token)
  | Double_amp -> Located.create Binary_op.And ~location:(Located.location token)
  | Double_bar -> Located.create Binary_op.Or ~location:(Located.location token)
  | Double_eq -> Located.create Binary_op.Eq ~location:(Located.location token)
  | Bang_eq -> Located.create Binary_op.Neq ~location:(Located.location token)
  | Lt -> Located.create Binary_op.Lt ~location:(Located.location token)
  | Gt -> Located.create Binary_op.Gt ~location:(Located.location token)
  | Lte -> Located.create Binary_op.Lte ~location:(Located.location token)
  | Gte -> Located.create Binary_op.Gte ~location:(Located.location token)
  | _ -> raise_error token ~expected:"a binary operator"
;;

let compound_assignment_op token =
  let op =
    match Located.value token with
    | Token.Plus_eq -> Binary_op.Add
    | Hyphen_eq -> Sub
    | Star_eq -> Mul
    | Slash_eq -> Div
    | Percent_eq -> Rem
    | Amp_eq -> Bit_and
    | Bar_eq -> Bit_or
    | Caret_eq -> Xor
    | Left_chevron_eq -> L_shift
    | Right_chevron_eq -> R_shift
    | token -> raise_s [%message "not a compound assignment operator" (token : Token.t)]
  in
  Located.create op ~location:(Located.location token)
;;

let rec parse_expr token_stream ~min_precedence =
  let mutable left = parse_factor token_stream in
  let mutable token = Located.value (Token_stream.peek token_stream ~distance:0) in
  while precedence token >= min_precedence do
    match token with
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
    | Right_chevron_eq ->
      let assignment_op = Token_stream.pop token_stream in
      let right = parse_expr token_stream ~min_precedence:(precedence token) in
      let location =
        Source.Location.between (Located.location left) (Located.location right)
      in
      let assignment =
        match token with
        | Eq -> Expr.Assign { left; right }
        | _ ->
          Expr.Compound_assign { op = compound_assignment_op assignment_op; left; right }
      in
      left <- Located.create assignment ~location;
      token <- Located.value (Token_stream.peek token_stream ~distance:0)
    | _ ->
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
  | Plus_plus | Hyphen_hyphen | Hyphen | Tilde | Bang ->
    let op = parse_prefix_unary_op token_stream in
    let expr = parse_factor token_stream in
    let location =
      Source.Location.between (Located.location op) (Located.location expr)
    in
    Located.create (Expr.Unary { op; expr }) ~location
  | _ ->
    let mutable expr = parse_primary token_stream in
    while
      match Source.Located.value (Token_stream.peek token_stream ~distance:0) with
      | Plus_plus | Hyphen_hyphen -> true
      | _ -> false
    do
      let op = parse_postfix_unary_op token_stream in
      let location =
        Source.Location.between (Located.location expr) (Located.location op)
      in
      expr <- Located.create (Expr.Unary { op; expr }) ~location
    done;
    expr

and parse_primary token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match Located.value token with
  | Const constant ->
    let _ = Token_stream.pop token_stream in
    Located.create (Expr.Const constant) ~location:(Located.location token)
  | Ident ident ->
    let _ = Token_stream.pop token_stream in
    Located.create (Expr.Var ident) ~location:(Located.location token)
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

let parse_decl token_stream =
  let int = expect token_stream Int in
  let name =
    let token = Token_stream.pop token_stream in
    match Located.value token with
    | Ident name -> name
    | _ -> raise_error token ~expected:"an identifier"
  in
  let token = Token_stream.peek token_stream ~distance:0 in
  match Source.Located.value token with
  | Eq ->
    let _ = expect token_stream Eq in
    let init = parse_expr token_stream ~min_precedence:0 in
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location int) (Located.location semicolon)
    in
    Located.create { Decl.name; init = This init } ~location
  | Semicolon ->
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location int) (Located.location semicolon)
    in
    Located.create { Decl.name; init = Null } ~location
  | _ -> raise_error token ~expected:"Eq or Semicolon"
;;

let parse_stmt token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match Located.value token with
  | Return ->
    let return = expect token_stream Return in
    let expr = parse_expr token_stream ~min_precedence:0 in
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location return) (Located.location semicolon)
    in
    Located.create (Stmt.Return expr) ~location
  | Semicolon ->
    let semicolon = expect token_stream Semicolon in
    Located.create Stmt.Null ~location:(Located.location semicolon)
  | _ ->
    let expr = parse_expr token_stream ~min_precedence:0 in
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location expr) (Located.location semicolon)
    in
    Located.create (Stmt.Expr expr) ~location
;;

let parse_block_item token_stream =
  match Source.Located.value (Token_stream.peek token_stream ~distance:0) with
  | Int ->
    let decl = parse_decl token_stream in
    Located.create (Block_item.Decl decl) ~location:(Located.location decl)
  | _ ->
    let stmt = parse_stmt token_stream in
    Located.create (Block_item.Stmt stmt) ~location:(Located.location stmt)
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
  let body = Vec.create () in
  while
    match Source.Located.value (Token_stream.peek token_stream ~distance:0) with
    | Close_brace -> false
    | _ -> true
  do
    let block_item = parse_block_item token_stream in
    Vec.push_back body block_item
  done;
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
