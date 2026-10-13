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

let resolve_param param ~fun_decl ~env =
  match Ident_env.declare_local_var env ~source_name:param ~has_linkage:false with
  | `Duplicate ->
    Error_helpers.raise_at fun_decl [%string "duplicate parameter declaration %{param}"]
  | `Declared resolved_name -> resolved_name
;;

let resolve_fun_name name ~fun_decl ~env =
  match Ident_env.declare_fun env ~source_name:name with
  | `Duplicate ->
    Error_helpers.raise_at fun_decl [%string "duplicate function declaration %{name}"]
  | `Declared name -> name
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
  | Function_call { name; args } ->
    (match Ident_env.find env name with
     | Null -> Error_helpers.raise_at expr [%string "undeclared function %{name}"]
     | This name ->
       let args = Iarray.map args ~f:(stack_ fun arg -> resolve_expr arg ~env) in
       Located.map expr ~f:(stack_ fun _ -> Ast.Expr.Function_call { name; args })
       [@nontail])
  | Conditional { cond; then_; else_ } ->
    Located.map expr ~f:(stack_ fun _ ->
      Ast.Expr.Conditional
        { cond = resolve_expr cond ~env
        ; then_ = resolve_expr then_ ~env
        ; else_ = resolve_expr else_ ~env
        })
    [@nontail]
  | Var source_name ->
    (match Ident_env.find env source_name with
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

let resolve_local_var_decl decl ~env =
  let { Ast.Var_decl.name = source_name; init; type_; storage } = Located.value decl in
  let has_linkage =
    match storage with
    | This Extern -> true
    | This Static | Null -> false
  in
  match Ident_env.declare_local_var env ~source_name ~has_linkage with
  | `Duplicate ->
    Error_helpers.raise_at
      decl
      [%string "conflicting local declarations for %{source_name}"]
  | `Declared resolved_name ->
    let init = Or_null.map init ~f:(resolve_expr ~env) in
    Located.map decl ~f:(stack_ fun _ ->
      { Ast.Var_decl.name = resolved_name; init; type_; storage })
    [@nontail]
;;

let resolve_local_fun_decl fun_decl ~env =
  Located.map
    fun_decl
    ~f:(stack_ fun { Ast.Fun_decl.name; params; body; ret_type; storage } ->
      (match storage with
       | This Static ->
         Error_helpers.raise_at
           fun_decl
           "block-scope function declaration cannot be static"
       | This Extern | Null -> ());
      let name = resolve_fun_name name ~fun_decl ~env in
      let params =
        Ident_env.with_scope env ~f:(stack_ fun () ->
          Iarray.map params ~f:(stack_ fun param -> resolve_param param ~fun_decl ~env)
          [@nontail])
      in
      if Or_null.is_this body
      then Error_helpers.raise_at fun_decl "block-scope function declaration with body";
      { Ast.Fun_decl.name; params; body; ret_type; storage })
  [@nontail]
;;

let resolve_for_init for_init ~env =
  match for_init with
  | Ast.For_init.Decl decl -> Ast.For_init.Decl (resolve_local_var_decl decl ~env)
  | Expr expr -> Expr (Or_null.map expr ~f:(stack_ fun expr -> resolve_expr expr ~env))
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
    Located.map block ~f:(stack_ fun _ ->
      Ident_env.with_scope env ~f:(stack_ fun () ->
        Ast.Stmt.Compound (resolve_block block ~env))
      [@nontail])
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
      Ident_env.with_scope env ~f:(stack_ fun () ->
        let init = resolve_for_init init ~env in
        Ast.Stmt.For
          { init
          ; cond = Or_null.map cond ~f:(stack_ fun cond -> resolve_expr cond ~env)
          ; post = Or_null.map post ~f:(stack_ fun post -> resolve_expr post ~env)
          ; body = resolve_stmt body ~env
          ; id
          })
      [@nontail])
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
    | Ast.Block_item.Decl decl ->
      Ast.Block_item.Decl
        (match Located.value decl with
         | Ast.Decl.Var_decl var_decl ->
           Located.map decl ~f:(stack_ fun _ ->
             Ast.Decl.Var_decl (resolve_local_var_decl var_decl ~env))
           [@nontail]
         | Fun_decl fun_decl ->
           Located.map decl ~f:(stack_ fun _ ->
             Ast.Decl.Fun_decl (resolve_local_fun_decl fun_decl ~env)))
    | Stmt stmt -> Ast.Block_item.Stmt (resolve_stmt stmt ~env))
    [@nontail]

and resolve_block block ~env =
  Located.map block ~f:(stack_ fun body ->
    let resolved_body =
      Iarray.map body ~f:(stack_ fun item -> resolve_block_item item ~env)
    in
    resolved_body)
  [@nontail]
;;

let resolve_file_scope_var_decl decl ~env =
  let { Ast.Var_decl.name; init; type_; storage } = Located.value decl in
  Ident_env.declare_file_scope_var env ~source_name:name;
  Located.map decl ~f:(stack_ fun _ -> { Ast.Var_decl.name; init; type_; storage })
  [@nontail]
;;

let resolve_file_scope_fun_decl fun_decl ~env =
  Located.map
    fun_decl
    ~f:(stack_ fun { Ast.Fun_decl.name; params; body; ret_type; storage } ->
      let name = resolve_fun_name name ~fun_decl ~env in
      let params, body =
        Ident_env.with_scope env ~f:(stack_ fun () ->
          let params =
            Iarray.map params ~f:(stack_ fun param -> resolve_param param ~fun_decl ~env)
          in
          let body = Or_null.map body ~f:(stack_ fun body -> resolve_block body ~env) in
          params, body)
      in
      { Ast.Fun_decl.name; params; body; ret_type; storage })
  [@nontail]
;;

let resolve_decl decl ~env =
  Located.map decl ~f:(stack_ function
    | Ast.Decl.Var_decl decl -> Ast.Decl.Var_decl (resolve_file_scope_var_decl decl ~env)
    | Fun_decl decl -> Fun_decl (resolve_file_scope_fun_decl decl ~env))
    [@nontail]
;;

let program (prog : Ast.Prog.t) =
  let env = Ident_env.create () in
  Located.map prog ~f:(stack_ fun decls ->
    Iarray.map decls ~f:(stack_ fun decl -> resolve_decl decl ~env) [@nontail])
  [@nontail]
;;
