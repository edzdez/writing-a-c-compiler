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

let find_compatible_symbol decl ~name ~type_ ~symbol_table =
  match Symbol_table.find symbol_table name with
  | Null -> Null
  | This previous ->
    if not (C_type.equal previous.type_ type_)
    then Error_helpers.raise_at decl [%string "incompatible declarations for %{name}"];
    This previous
;;

let global_of_storage storage =
  not (Or_null.equal Ast.Storage_class.equal storage (This Static))
;;

let rec infer_expr expr ~symbol_table =
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
            check_expr arg ~expected ~symbol_table));
       result)
  | Unary { op = _; expr = operand } ->
    check_expr operand ~expected:Int ~symbol_table;
    Int
  | Binary { op = _; left; right } | Compound_assign { op = _; left; right } ->
    check_expr left ~expected:Int ~symbol_table;
    check_expr right ~expected:Int ~symbol_table;
    Int
  | Assign { left; right } ->
    let left_type = infer_expr left ~symbol_table in
    check_expr right ~expected:left_type ~symbol_table;
    left_type
  | Conditional { cond; then_; else_ } ->
    check_expr cond ~expected:Int ~symbol_table;
    let then_type = infer_expr then_ ~symbol_table in
    check_expr else_ ~expected:then_type ~symbol_table;
    then_type

and check_expr expr ~expected ~symbol_table =
  let actual = infer_expr expr ~symbol_table in
  expect_type expr actual ~expected
;;

let check_local_var_decl var_decl ~symbol_table =
  let { Ast.Var_decl.name; init; type_; storage } = Located.value var_decl in
  match storage with
  | This Extern ->
    if Or_null.is_this init
    then Error_helpers.raise_at var_decl "extern variable cannot have an initializer";
    (match find_compatible_symbol var_decl ~name ~type_ ~symbol_table with
     | Null ->
       Symbol_table.add_exn
         symbol_table
         ~name
         ~symbol:{ Symbol.type_; attrs = Static { init = No_initializer; global = true } }
     | This _ -> ())
  | This Static ->
    let init =
      match init with
      | Null -> Symbol.Attrs.Initial_value.Initial 0
      | This expr ->
        (match Located.value expr with
         | Ast.Expr.Const n -> Initial n
         | _ -> Error_helpers.raise_at var_decl "non-constant initializer")
    in
    Symbol_table.add_exn
      symbol_table
      ~name
      ~symbol:{ Symbol.type_; attrs = Static { init; global = false } }
  | Null ->
    Symbol_table.add_exn symbol_table ~name ~symbol:{ Symbol.type_; attrs = Local };
    Or_null.iter init ~f:(stack_ fun init ->
      check_expr init ~expected:type_ ~symbol_table)
    [@nontail]
;;

let check_for_init for_init ~symbol_table =
  match for_init with
  | Ast.For_init.Decl var_decl ->
    let { Ast.Var_decl.storage; _ } = Located.value var_decl in
    if Or_null.is_this storage
    then Error_helpers.raise_at var_decl "[for] initializer cannot have a storage class";
    check_local_var_decl var_decl ~symbol_table
  | Expr expr ->
    Or_null.iter expr ~f:(stack_ fun expr -> check_expr expr ~expected:Int ~symbol_table)
    [@nontail]
;;

let fun_type ~params ~ret_type =
  C_type.Fun
    { params = Iarray.map params ~f:(stack_ fun _ -> C_type.Int); result = ret_type }
;;

let register_fun_decl fun_decl ~symbol_table =
  let { Ast.Fun_decl.name; params; ret_type; body; storage } = Located.value fun_decl in
  let type_ = fun_type ~params ~ret_type in
  let defined = Or_null.is_this body in
  let global = global_of_storage storage in
  match find_compatible_symbol fun_decl ~name ~type_ ~symbol_table with
  | Null ->
    Symbol_table.add_exn
      symbol_table
      ~name
      ~symbol:{ Symbol.type_; attrs = Fun { defined; global } }
  | This previous ->
    (match previous.attrs with
     | Local | Static _ ->
       Error_helpers.raise_at fun_decl [%string "%{name} was declared as a variable"]
     | Fun { defined = true; global = _ } when defined ->
       Error_helpers.raise_at fun_decl [%string "%{name} defined more than once"]
     | Fun { defined = _; global = true } when not global ->
       Error_helpers.raise_at
         fun_decl
         [%string "static function declaration follows non-static"]
     | Fun { defined = previous_defined; global } ->
       Symbol_table.set
         symbol_table
         ~name
         ~symbol:
           { Symbol.type_; attrs = Fun { defined = previous_defined || defined; global } })
;;

let rec check_stmt stmt ~symbol_table =
  match Located.value stmt with
  | Ast.Stmt.Return expr | Expr expr -> check_expr expr ~expected:Int ~symbol_table
  | If { cond; then_; else_ } ->
    check_expr cond ~expected:Int ~symbol_table;
    check_stmt then_ ~symbol_table;
    Or_null.iter else_ ~f:(stack_ fun else_ -> check_stmt else_ ~symbol_table) [@nontail]
  | Label { lbl = _; body } -> check_stmt body ~symbol_table
  | Compound block -> check_block block ~symbol_table
  | While { cond; body; id = _ } ->
    check_expr cond ~expected:Int ~symbol_table;
    check_stmt body ~symbol_table
  | Do_while { body; cond; id = _ } ->
    check_stmt body ~symbol_table;
    check_expr cond ~expected:Int ~symbol_table
  | For { init; cond; post; body; id = _ } ->
    check_for_init init ~symbol_table;
    Or_null.iter cond ~f:(stack_ fun cond -> check_expr cond ~expected:Int ~symbol_table);
    Or_null.iter post ~f:(stack_ fun post -> check_expr post ~expected:Int ~symbol_table);
    check_stmt body ~symbol_table
  | Case { value; body; id = _ } ->
    check_expr value ~expected:Int ~symbol_table;
    check_stmt body ~symbol_table
  | Default { body; id = _ } -> check_stmt body ~symbol_table
  | Switch { cases = _; cond; body; id = _ } ->
    check_expr cond ~expected:Int ~symbol_table;
    check_stmt body ~symbol_table
  | Goto _ | Break _ | Continue _ | Null -> ()

and check_block_item block_item ~symbol_table =
  match Located.value block_item with
  | Ast.Block_item.Decl decl -> check_block_scope_decl decl ~symbol_table
  | Stmt stmt -> check_stmt stmt ~symbol_table

and check_block block ~symbol_table =
  Iarray.iter (Located.value block) ~f:(stack_ fun block_item ->
    check_block_item block_item ~symbol_table)
  [@nontail]

and check_block_scope_decl decl ~symbol_table =
  match Located.value decl with
  | Ast.Decl.Var_decl var_decl -> check_local_var_decl var_decl ~symbol_table
  | Fun_decl fun_decl -> check_fun_decl fun_decl ~symbol_table

and check_fun_decl fun_decl ~symbol_table =
  register_fun_decl fun_decl ~symbol_table;
  let { Ast.Fun_decl.params; body; _ } = Located.value fun_decl in
  Or_null.iter body ~f:(stack_ fun body ->
    Iarray.iter params ~f:(stack_ fun name ->
      Symbol_table.add_exn
        symbol_table
        ~name
        ~symbol:{ Symbol.type_ = Int; attrs = Local });
    check_block body ~symbol_table)
  [@nontail]
;;

let file_scope_initial_value var_decl ~init ~storage =
  match init, storage with
  | This expr, _ ->
    (match Located.value expr with
     | Ast.Expr.Const n -> Symbol.Attrs.Initial_value.Initial n
     | _ -> Error_helpers.raise_at var_decl "non-constant initializer")
  | Null, This Ast.Storage_class.Extern -> No_initializer
  | Null, _ -> Tentative
;;

let merge_file_scope_linkage var_decl ~storage ~previous ~current =
  match storage with
  | This Ast.Storage_class.Extern -> previous
  | This Static | Null ->
    if Bool.(previous <> current)
    then Error_helpers.raise_at var_decl "conflicting variable linkage";
    current
;;

let merge_file_scope_initial_value var_decl ~name ~previous ~current =
  match previous, current with
  | Symbol.Attrs.Initial_value.Initial _, Symbol.Attrs.Initial_value.Initial _ ->
    Error_helpers.raise_at
      var_decl
      [%string "conflicting file-scope definitions for %{name}"]
  | Initial _, (Tentative | No_initializer) -> previous
  | Tentative, (Tentative | No_initializer) -> Tentative
  | (Tentative | No_initializer), Initial _ -> current
  | No_initializer, (Tentative | No_initializer) -> current
;;

let register_file_scope_var var_decl ~symbol_table =
  let { Ast.Var_decl.name; init; type_; storage } = Located.value var_decl in
  let init = file_scope_initial_value var_decl ~init ~storage in
  let global = global_of_storage storage in
  match find_compatible_symbol var_decl ~name ~type_ ~symbol_table with
  | Null ->
    Symbol_table.add_exn
      symbol_table
      ~name
      ~symbol:{ Symbol.type_; attrs = Static { init; global } }
  | This previous ->
    (match previous.attrs with
     | Local | Fun _ ->
       Error_helpers.raise_at var_decl [%string "incompatible declarations for %{name}"]
     | Static { init = prev_init; global = prev_global } ->
       let global =
         merge_file_scope_linkage var_decl ~storage ~previous:prev_global ~current:global
       in
       let init =
         merge_file_scope_initial_value var_decl ~name ~previous:prev_init ~current:init
       in
       Symbol_table.set
         symbol_table
         ~name
         ~symbol:{ Symbol.type_; attrs = Static { init; global } })
;;

let check_file_scope_decl decl ~symbol_table =
  match Located.value decl with
  | Ast.Decl.Var_decl var_decl -> register_file_scope_var var_decl ~symbol_table
  | Fun_decl fun_decl -> check_fun_decl fun_decl ~symbol_table
;;

let program (prog : Ast.Prog.t) =
  let symbol_table = Symbol_table.create () in
  Iarray.iter (Located.value prog) ~f:(stack_ fun decl ->
    check_file_scope_decl decl ~symbol_table);
  symbol_table
;;
