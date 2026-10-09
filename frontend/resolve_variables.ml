open! Core
module Located = Source.Located

let is_lvalue expr =
  match Located.value expr with
  | Ast.Expr.Var _ -> true
  | _ -> false
;;

let validate_lvalue expr =
  if not (is_lvalue expr)
  then
    Error_helpers.raise_at
      expr
      [%string "expected an lvalue, found %{Ast.Expr.describe expr}"]
;;

let validate_unary_lvalue op expr =
  match Located.value op with
  | (Ast.Unary_op.Pre_incr | Pre_decr | Post_incr | Post_decr) when not (is_lvalue expr)
    ->
    Error_helpers.raise_at
      expr
      [%string "expected an lvalue, found %{Ast.Expr.describe expr}"]
  | _ -> ()
;;

let rec resolve_expr expr ~env =
  match Located.value expr with
  | Ast.Expr.Assign { left; right } ->
    validate_lvalue left;
    Located.map expr ~f:(stack_ fun _ ->
      Ast.Expr.Assign { left = resolve_expr left ~env; right = resolve_expr right ~env })
    [@nontail]
  | Compound_assign { op; left; right } ->
    validate_lvalue left;
    Located.map expr ~f:(stack_ fun _ ->
      Ast.Expr.Compound_assign
        { op; left = resolve_expr left ~env; right = resolve_expr right ~env })
    [@nontail]
  | Conditional { cond; then_; else_ } ->
    Located.map expr ~f:(stack_ fun _ ->
      Ast.Expr.Conditional
        { cond = resolve_expr cond ~env
        ; then_ = resolve_expr then_ ~env
        ; else_ = resolve_expr else_ ~env
        })
    [@nontail]
  | Var source_name ->
    (match Variable_env.find env source_name with
     | Null -> Error_helpers.raise_at expr [%string "undeclared variable %{source_name}"]
     | This resolved_name ->
       Located.map expr ~f:(stack_ fun _ -> Ast.Expr.Var resolved_name) [@nontail])
  | Unary { op; expr = inner } ->
    validate_unary_lvalue op inner;
    Located.map expr ~f:(stack_ fun _ ->
      Ast.Expr.Unary { op; expr = resolve_expr inner ~env })
    [@nontail]
  | Binary { op; left; right } ->
    Located.map expr ~f:(stack_ fun _ ->
      Ast.Expr.Binary
        { op; left = resolve_expr left ~env; right = resolve_expr right ~env })
    [@nontail]
  | Const _ -> expr
;;

let resolve_decl decl ~env =
  let { Ast.Decl.name = source_name; init } = Located.value decl in
  match Variable_env.declare env ~source_name with
  | `Duplicate ->
    Error_helpers.raise_at decl [%string "duplicate variable declaration %{source_name}"]
  | `Declared resolved_name ->
    let init = Or_null.map init ~f:(resolve_expr ~env) in
    Located.map decl ~f:(stack_ fun _ -> { Ast.Decl.name = resolved_name; init })
    [@nontail]
;;

let resolve_for_init for_init ~env =
  Located.map for_init ~f:(stack_ function
    | Ast.For_init.Decl decl -> Ast.For_init.Decl (resolve_decl decl ~env)
    | Expr expr -> Expr (Or_null.map expr ~f:(stack_ fun expr -> resolve_expr expr ~env)))
    [@nontail]
;;

let rec resolve_stmt stmt ~env =
  match Located.value stmt with
  | Ast.Stmt.Return expr ->
    Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Return (resolve_expr expr ~env))
    [@nontail]
  | Expr expr ->
    Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Expr (resolve_expr expr ~env))
    [@nontail]
  | If { cond; then_; else_ } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.If
        { cond = resolve_expr cond ~env
        ; then_ = resolve_stmt then_ ~env
        ; else_ = Or_null.map else_ ~f:(resolve_stmt ~env)
        })
    [@nontail]
  | Label { lbl; body } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Label { lbl; body = resolve_stmt body ~env })
    [@nontail]
  | Compound block ->
    Located.map block ~f:(stack_ fun _ -> Ast.Stmt.Compound (resolve_block block ~env))
    [@nontail]
  | While { cond; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond = resolve_expr cond ~env; body = resolve_stmt body ~env; id })
    [@nontail]
  | Do_while { body; cond; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while
        { body = resolve_stmt body ~env; cond = resolve_expr cond ~env; id })
    [@nontail]
  | For { init; cond; post; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Variable_env.open_scope env;
      let init = resolve_for_init init ~env in
      let for_ =
        Ast.Stmt.For
          { init
          ; cond = Or_null.map cond ~f:(stack_ fun cond -> resolve_expr cond ~env)
          ; post = Or_null.map post ~f:(stack_ fun post -> resolve_expr post ~env)
          ; body = resolve_stmt body ~env
          ; id
          }
      in
      Variable_env.close_scope env;
      for_)
    [@nontail]
  | Case { value; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Case { value = resolve_expr value ~env; body = resolve_stmt body ~env; id })
    [@nontail]
  | Default { body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Default { body = resolve_stmt body ~env; id })
    [@nontail]
  | Switch { cases; cond; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Switch
        { cases; cond = resolve_expr cond ~env; body = resolve_stmt body ~env; id })
    [@nontail]
  | Break _ | Continue _ | Goto _ | Null -> stmt

and resolve_block_item block_item ~env =
  Located.map block_item ~f:(stack_ function
    | Ast.Block_item.Decl decl -> Ast.Block_item.Decl (resolve_decl decl ~env)
    | Stmt stmt -> Ast.Block_item.Stmt (resolve_stmt stmt ~env))
    [@nontail]

and resolve_block block ~env =
  Located.map block ~f:(stack_ fun body ->
    Variable_env.open_scope env;
    let resolved_body = Iarray.map body ~f:(fun item -> resolve_block_item item ~env) in
    Variable_env.close_scope env;
    resolved_body)
  [@nontail]
;;

let resolve_fun_def fun_def ~env =
  Located.map fun_def ~f:(stack_ fun { Ast.Fun_def.name; body } ->
    let body = resolve_block body ~env in
    { Ast.Fun_def.name; body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  let env = Variable_env.create () in
  Located.map prog ~f:(stack_ fun fun_def -> resolve_fun_def fun_def ~env) [@nontail]
;;
