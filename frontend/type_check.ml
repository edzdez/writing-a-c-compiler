open! Core
module Located = Source.Located
module C_type = Semantics.C_type
module Symbol = Semantics.Symbol
module Symbol_table = Semantics.Symbol_table

let raise_type_error located ~expected ~actual =
  Error_helpers.raise_at
    located
    [%string
      "expected %{C_type.sexp_of_t expected#Sexp}, found %{C_type.sexp_of_t actual#Sexp}"]
;;

let expect_type located actual ~expected =
  match C_type.equal actual expected with
  | true -> ()
  | false -> raise_type_error located ~expected ~actual
;;

let find_symbol located name ~symbol_table =
  match Symbol_table.find symbol_table name with
  | This symbol -> symbol
  | Null -> Error_helpers.raise_at located [%string "unknown identifier %{name}"]
;;

let rec check_expr expr ~symbol_table =
  let check_int expr =
    let actual = check_expr expr ~symbol_table in
    expect_type expr actual ~expected:C_type.Int
  in
  match Located.value expr with
  | Ast.Expr.Const _ -> C_type.Int
  | Var name ->
    let symbol = find_symbol expr name ~symbol_table in
    (match symbol.type_ with
     | Int -> Int
     | Fun _ -> Error_helpers.raise_at expr [%string "%{name} is a function"])
  | Function_call { name; args } ->
    let symbol = find_symbol expr name ~symbol_table in
    (match symbol.type_ with
     | Int -> Error_helpers.raise_at expr [%string "%{name} is not a function"]
     | Fun { params; result } ->
       (match Iarray.zip args params with
        | None ->
          Error_helpers.raise_at
            expr
            [%string "%{name} called with the wrong number of arguments"]
        | Some args_and_params ->
          Iarray.iter args_and_params ~f:(stack_ fun (arg, expected) ->
            let actual = check_expr arg ~symbol_table in
            expect_type arg actual ~expected));
       result)
  | Unary { op = _; expr = operand } ->
    check_int operand;
    Int
  | Binary { op = _; left; right } | Compound_assign { op = _; left; right } ->
    check_int left;
    check_int right;
    Int
  | Assign { left; right } ->
    let left_type = check_expr left ~symbol_table in
    let right_type = check_expr right ~symbol_table in
    expect_type right right_type ~expected:left_type;
    left_type
  | Conditional { cond; then_; else_ } ->
    check_int cond;
    let then_type = check_expr then_ ~symbol_table in
    let else_type = check_expr else_ ~symbol_table in
    expect_type else_ else_type ~expected:then_type;
    then_type
;;

let check_var_decl var_decl ~symbol_table =
  let { Ast.Var_decl.name; init } = Located.value var_decl in
  Symbol_table.add_exn symbol_table ~name ~symbol:{ Symbol.type_ = Int; attrs = Local };
  Or_null.iter init ~f:(stack_ fun init ->
    let actual = check_expr init ~symbol_table in
    expect_type init actual ~expected:Int)
  [@nontail]
;;

let check_for_init for_init ~symbol_table =
  match Located.value for_init with
  | Ast.For_init.Decl var_decl -> check_var_decl var_decl ~symbol_table
  | Expr expr ->
    Or_null.iter expr ~f:(stack_ fun expr ->
      let actual = check_expr expr ~symbol_table in
      expect_type expr actual ~expected:Int)
    [@nontail]
;;

let rec check_stmt stmt ~symbol_table =
  let check_int expr =
    let actual = check_expr expr ~symbol_table in
    expect_type expr actual ~expected:Int
  in
  match Located.value stmt with
  | Ast.Stmt.Return expr | Expr expr -> check_int expr
  | If { cond; then_; else_ } ->
    check_int cond;
    check_stmt then_ ~symbol_table;
    Or_null.iter else_ ~f:(stack_ fun else_ -> check_stmt else_ ~symbol_table) [@nontail]
  | Label { lbl = _; body } -> check_stmt body ~symbol_table
  | Compound block -> check_block block ~symbol_table
  | While { cond; body; id = _ } ->
    check_int cond;
    check_stmt body ~symbol_table
  | Do_while { body; cond; id = _ } ->
    check_stmt body ~symbol_table;
    check_int cond
  | For { init; cond; post; body; id = _ } ->
    check_for_init init ~symbol_table;
    Or_null.iter cond ~f:(stack_ fun cond -> check_int cond);
    Or_null.iter post ~f:(stack_ fun post -> check_int post);
    check_stmt body ~symbol_table
  | Case { value; body; id = _ } ->
    check_int value;
    check_stmt body ~symbol_table
  | Default { body; id = _ } -> check_stmt body ~symbol_table
  | Switch { cases = _; cond; body; id = _ } ->
    check_int cond;
    check_stmt body ~symbol_table
  | Goto _ | Break _ | Continue _ | Null -> ()

and check_block_item block_item ~symbol_table =
  match Located.value block_item with
  | Ast.Block_item.Decl decl -> check_decl decl ~symbol_table
  | Stmt stmt -> check_stmt stmt ~symbol_table

and check_block block ~symbol_table =
  Iarray.iter (Located.value block) ~f:(stack_ fun block_item ->
    check_block_item block_item ~symbol_table)
  [@nontail]

and check_decl decl ~symbol_table =
  match Located.value decl with
  | Ast.Decl.Var_decl var_decl -> check_var_decl var_decl ~symbol_table
  | Fun_decl fun_decl -> check_fun_decl fun_decl ~symbol_table

and check_fun_decl fun_decl ~symbol_table =
  let { Ast.Fun_decl.name; params; body } = Located.value fun_decl in
  let type_ =
    C_type.Fun
      { params = Iarray.map params ~f:(stack_ fun _ -> C_type.Int); result = Int }
  in
  let defined = Or_null.is_this body in
  (match Symbol_table.find symbol_table name with
   | Null ->
     Symbol_table.add_exn
       symbol_table
       ~name
       ~symbol:{ Symbol.type_; attrs = Fun { defined } }
   | This prev ->
     (match C_type.equal prev.type_ type_ with
      | false ->
        Error_helpers.raise_at fun_decl [%string "incompatible declarations for %{name}"]
      | true ->
        (match prev.attrs with
         | Local ->
           Error_helpers.raise_at fun_decl [%string "%{name} was declared as a variable"]
         | Fun { defined = true } when defined ->
           Error_helpers.raise_at fun_decl [%string "%{name} defined more than once"]
         | Fun { defined = prev_defined } ->
           Symbol_table.set
             symbol_table
             ~name
             ~symbol:{ Symbol.type_; attrs = Fun { defined = prev_defined || defined } })));
  Or_null.iter body ~f:(stack_ fun body ->
    Iarray.iter params ~f:(stack_ fun name ->
      Symbol_table.add_exn
        symbol_table
        ~name
        ~symbol:{ Symbol.type_ = Int; attrs = Local });
    check_block body ~symbol_table)
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  let symbol_table = Symbol_table.create () in
  Iarray.iter (Located.value prog) ~f:(stack_ fun fun_decl ->
    check_fun_decl fun_decl ~symbol_table);
  symbol_table
;;
