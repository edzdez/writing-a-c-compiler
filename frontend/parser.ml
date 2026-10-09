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

module Prefix_operator = struct
  type t = Unary_op.node

  let of_token = function
    | Token.Hyphen -> This Unary_op.Negate
    | Tilde -> This Complement
    | Bang -> This Not
    | Plus_plus -> This Pre_incr
    | Hyphen_hyphen -> This Pre_decr
    | _ -> Null
  ;;
end

module Postfix_operator = struct
  type t = Unary_op.node

  let of_token = function
    | Token.Plus_plus -> This Unary_op.Post_incr
    | Hyphen_hyphen -> This Post_decr
    | _ -> Null
  ;;
end

module Infix_operator = struct
  module Associativity = struct
    type t =
      | Left
      | Right
  end

  module Kind = struct
    type t =
      | Binary of Binary_op.node
      | Assign
      | Compound_assign of Binary_op.node
      | Conditional
  end

  type t =
    { kind : Kind.t
    ; precedence : int
    ; associativity : Associativity.t
    }

  let of_token = function
    | Token.Plus ->
      This { kind = Binary Binary_op.Add; precedence = 45; associativity = Left }
    | Hyphen -> This { kind = Binary Sub; precedence = 45; associativity = Left }
    | Star -> This { kind = Binary Mul; precedence = 50; associativity = Left }
    | Slash -> This { kind = Binary Div; precedence = 50; associativity = Left }
    | Percent -> This { kind = Binary Rem; precedence = 50; associativity = Left }
    | Ampersand -> This { kind = Binary Bit_and; precedence = 25; associativity = Left }
    | Bar -> This { kind = Binary Bit_or; precedence = 15; associativity = Left }
    | Caret -> This { kind = Binary Xor; precedence = 20; associativity = Left }
    | Left_chevron ->
      This { kind = Binary L_shift; precedence = 40; associativity = Left }
    | Right_chevron ->
      This { kind = Binary R_shift; precedence = 40; associativity = Left }
    | Double_amp -> This { kind = Binary And; precedence = 10; associativity = Left }
    | Double_bar -> This { kind = Binary Or; precedence = 5; associativity = Left }
    | Double_eq -> This { kind = Binary Eq; precedence = 30; associativity = Left }
    | Bang_eq -> This { kind = Binary Neq; precedence = 30; associativity = Left }
    | Lt -> This { kind = Binary Lt; precedence = 35; associativity = Left }
    | Gt -> This { kind = Binary Gt; precedence = 35; associativity = Left }
    | Lte -> This { kind = Binary Lte; precedence = 35; associativity = Left }
    | Gte -> This { kind = Binary Gte; precedence = 35; associativity = Left }
    | Eq -> This { kind = Assign; precedence = 1; associativity = Right }
    | Plus_eq ->
      This { kind = Compound_assign Add; precedence = 1; associativity = Right }
    | Hyphen_eq ->
      This { kind = Compound_assign Sub; precedence = 1; associativity = Right }
    | Star_eq ->
      This { kind = Compound_assign Mul; precedence = 1; associativity = Right }
    | Slash_eq ->
      This { kind = Compound_assign Div; precedence = 1; associativity = Right }
    | Percent_eq ->
      This { kind = Compound_assign Rem; precedence = 1; associativity = Right }
    | Amp_eq ->
      This { kind = Compound_assign Bit_and; precedence = 1; associativity = Right }
    | Bar_eq ->
      This { kind = Compound_assign Bit_or; precedence = 1; associativity = Right }
    | Caret_eq ->
      This { kind = Compound_assign Xor; precedence = 1; associativity = Right }
    | Left_chevron_eq ->
      This { kind = Compound_assign L_shift; precedence = 1; associativity = Right }
    | Right_chevron_eq ->
      This { kind = Compound_assign R_shift; precedence = 1; associativity = Right }
    | Question_mark -> This { kind = Conditional; precedence = 3; associativity = Right }
    | _ -> Null
  ;;

  let precedence t =
    match t.associativity with
    | Left -> t.precedence + 1
    | Right -> t.precedence
  ;;
end

let rec parse_expr token_stream ~min_precedence =
  let mutable left = parse_factor token_stream in
  let mutable is_infix = true in
  while is_infix do
    let token = Token_stream.peek token_stream ~distance:0 in
    match Infix_operator.of_token (Located.value token) with
    | This operator when operator.precedence >= min_precedence ->
      left <- parse_infix_operator token_stream token operator ~left
    | This _ | Null -> is_infix <- false
  done;
  left

and parse_infix_operator token_stream token operator ~left =
  let precedence = Infix_operator.precedence operator in
  match operator.kind with
  | Binary op ->
    ignore (Token_stream.pop token_stream);
    let right = parse_expr token_stream ~min_precedence:precedence in
    let location =
      Source.Location.between (Located.location left) (Located.location right)
    in
    let op = Located.map token ~f:(stack_ fun _ -> op) [@nontail] in
    Located.create (Expr.Binary { op; left; right }) ~location
  | Assign ->
    ignore (Token_stream.pop token_stream);
    let right = parse_expr token_stream ~min_precedence:precedence in
    let location =
      Source.Location.between (Located.location left) (Located.location right)
    in
    Located.create (Expr.Assign { left; right }) ~location
  | Compound_assign op ->
    ignore (Token_stream.pop token_stream);
    let right = parse_expr token_stream ~min_precedence:precedence in
    let location =
      Source.Location.between (Located.location left) (Located.location right)
    in
    let op = Located.map token ~f:(stack_ fun _ -> op) [@nontail] in
    Located.create (Expr.Compound_assign { op; left; right }) ~location
  | Conditional ->
    let then_ = parse_conditional_then token_stream in
    let else_ = parse_expr token_stream ~min_precedence:precedence in
    let location =
      Source.Location.between (Located.location left) (Located.location else_)
    in
    Located.create (Expr.Conditional { cond = left; then_; else_ }) ~location

and parse_factor token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match Prefix_operator.of_token (Located.value token) with
  | This op ->
    ignore (Token_stream.pop token_stream);
    let op = Located.map token ~f:(stack_ fun _ -> op) [@nontail] in
    let expr = parse_factor token_stream in
    let location =
      Source.Location.between (Located.location op) (Located.location expr)
    in
    Located.create (Expr.Unary { op; expr }) ~location
  | Null ->
    let mutable expr = parse_primary token_stream in
    let mutable is_postfix = true in
    while is_postfix do
      let token = Token_stream.peek token_stream ~distance:0 in
      match Postfix_operator.of_token (Located.value token) with
      | This op ->
        ignore (Token_stream.pop token_stream);
        let op = Located.map token ~f:(stack_ fun _ -> op) [@nontail] in
        let location =
          Source.Location.between (Located.location expr) (Located.location op)
        in
        expr <- Located.create (Expr.Unary { op; expr }) ~location
      | Null -> is_postfix <- false
    done;
    expr

and parse_primary token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match Located.value token with
  | Const constant ->
    ignore (Token_stream.pop token_stream);
    Located.create (Expr.Const constant) ~location:(Located.location token)
  | Ident ident ->
    ignore (Token_stream.pop token_stream);
    Located.create (Expr.Var ident) ~location:(Located.location token)
  | Open_paren ->
    let open_paren = Token_stream.pop token_stream in
    let expr = parse_expr token_stream ~min_precedence:0 in
    let close_paren = expect token_stream Close_paren in
    let location =
      Source.Location.between (Located.location open_paren) (Located.location close_paren)
    in
    Located.create (Located.value expr) ~location
  | _ -> raise_error token ~expected:"a statement"

and parse_conditional_then token_stream =
  ignore (expect token_stream Question_mark);
  let expr = parse_expr token_stream ~min_precedence:0 in
  ignore (expect token_stream Colon);
  expr
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
  match Located.value token with
  | Eq ->
    ignore (Token_stream.pop token_stream);
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

let parse_for_init token_stream =
  let token = Token_stream.peek token_stream ~distance:0 in
  match Located.value token with
  | Int ->
    let decl = parse_decl token_stream in
    Located.map decl ~f:(stack_ fun _ -> For_init.Decl decl) [@nontail]
  | Semicolon ->
    ignore (Token_stream.pop token_stream);
    Located.map token ~f:(stack_ fun _ -> For_init.Expr Null) [@nontail]
  | _ ->
    let expr = parse_expr token_stream ~min_precedence:0 in
    ignore (expect token_stream Semicolon);
    Located.map expr ~f:(stack_ fun _ -> For_init.Expr (This expr)) [@nontail]
;;

let parse_for_control token_stream =
  ignore (expect token_stream Open_paren);
  let init = parse_for_init token_stream in
  let cond =
    match Located.value (Token_stream.peek token_stream ~distance:0) with
    | Semicolon -> Null
    | _ ->
      let expr = parse_expr token_stream ~min_precedence:0 in
      This expr
  in
  ignore (expect token_stream Semicolon);
  let post =
    match Located.value (Token_stream.peek token_stream ~distance:0) with
    | Close_paren -> Null
    | _ -> This (parse_expr token_stream ~min_precedence:0)
  in
  ignore (expect token_stream Close_paren);
  #(~init, ~cond, ~post)
;;

let rec parse_stmt token_stream =
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
  | If ->
    let if_ = expect token_stream If in
    ignore (expect token_stream Open_paren);
    let cond = parse_expr token_stream ~min_precedence:0 in
    ignore (expect token_stream Close_paren);
    let then_ = parse_stmt token_stream in
    let token = Token_stream.peek token_stream ~distance:0 in
    (match Located.value token with
     | Else ->
       ignore (Token_stream.pop token_stream);
       let else_ = parse_stmt token_stream in
       let location =
         Source.Location.between (Located.location if_) (Located.location else_)
       in
       Located.create (Stmt.If { cond; then_; else_ = This else_ }) ~location
     | _ ->
       let location =
         Source.Location.between (Located.location if_) (Located.location then_)
       in
       Located.create (Stmt.If { cond; then_; else_ = Null }) ~location)
  | Goto ->
    ignore (Token_stream.pop token_stream);
    let label =
      let token = Token_stream.pop token_stream in
      match Located.value token with
      | Ident name -> name
      | _ -> raise_error token ~expected:"an identifier"
    in
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location token) (Located.location semicolon)
    in
    Located.create (Stmt.Goto label) ~location
  | Ident lbl
    when Token.equal (Located.value (Token_stream.peek token_stream ~distance:1)) Colon ->
    ignore (Token_stream.pop token_stream);
    let colon = expect token_stream Colon in
    let stmt = parse_stmt token_stream in
    let location =
      Source.Location.between (Located.location token) (Located.location colon)
    in
    Located.create (Stmt.Label { lbl; stmt }) ~location
  | Open_brace ->
    let block = parse_block token_stream in
    Located.map block ~f:(stack_ fun _ -> Ast.Stmt.Compound block) [@nontail]
  | Break ->
    ignore (Token_stream.pop token_stream);
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location token) (Located.location semicolon)
    in
    Located.create (Stmt.Break "DUMMY") ~location
  | Continue ->
    ignore (Token_stream.pop token_stream);
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location token) (Located.location semicolon)
    in
    Located.create (Stmt.Continue "DUMMY") ~location
  | While ->
    ignore (Token_stream.pop token_stream);
    ignore (expect token_stream Open_paren);
    let cond = parse_expr token_stream ~min_precedence:0 in
    ignore (expect token_stream Close_paren);
    let body = parse_stmt token_stream in
    let location =
      Source.Location.between (Located.location token) (Located.location body)
    in
    Located.create (Stmt.While { cond; body; id = "DUMMY" }) ~location
  | Do ->
    ignore (Token_stream.pop token_stream);
    let body = parse_stmt token_stream in
    ignore (expect token_stream While);
    ignore (expect token_stream Open_paren);
    let cond = parse_expr token_stream ~min_precedence:0 in
    ignore (expect token_stream Close_paren);
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location token) (Located.location semicolon)
    in
    Located.create (Stmt.Do_while { body; cond; id = "DUMMY" }) ~location
  | For ->
    ignore (Token_stream.pop token_stream);
    let #(~init, ~cond, ~post) = parse_for_control token_stream in
    let body = parse_stmt token_stream in
    let location =
      Source.Location.between (Located.location token) (Located.location body)
    in
    Located.create (Stmt.For { init; cond; post; body; id = "DUMMY" }) ~location
  | _ ->
    let expr = parse_expr token_stream ~min_precedence:0 in
    let semicolon = expect token_stream Semicolon in
    let location =
      Source.Location.between (Located.location expr) (Located.location semicolon)
    in
    Located.create (Stmt.Expr expr) ~location

and parse_block_item token_stream =
  match Located.value (Token_stream.peek token_stream ~distance:0) with
  | Int ->
    let decl = parse_decl token_stream in
    Located.create (Block_item.Decl decl) ~location:(Located.location decl)
  | _ ->
    let stmt = parse_stmt token_stream in
    Located.create (Block_item.Stmt stmt) ~location:(Located.location stmt)

and parse_block token_stream =
  let open_brace = expect token_stream Open_brace in
  let block = Vec.create () in
  let mutable is_block = true in
  while is_block do
    let token = Token_stream.peek token_stream ~distance:0 in
    match Located.value token with
    | Close_brace -> is_block <- false
    | _ ->
      let block_item = parse_block_item token_stream in
      Vec.push_back block block_item
  done;
  let close_brace = expect token_stream Close_brace in
  let location =
    Source.Location.between (Located.location open_brace) (Located.location close_brace)
  in
  Located.create block ~location
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
  let body = parse_block token_stream in
  let location = Source.Location.between (Located.location int) (Located.location body) in
  Located.create { Fun_def.name; body } ~location
;;

let parse token_stream =
  let fun_def = parse_fun_def token_stream in
  ignore (expect token_stream Eof);
  Located.create fun_def ~location:(Located.location fun_def)
;;
