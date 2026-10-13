open! Core
module Located = Source.Located
module Control_id = Ast.Control_id

module Context = struct
  module Switch = struct
    type t =
      { id : Control_id.t
      ; mutable cases : Ast.Switch_cases.t
      }
  end

  type t =
    { fresh_ids : Common.Fresh_id.t
    ; break_target : Control_id.t or_null
    ; continue_target : Control_id.t or_null
    ; current_switch : Switch.t or_null
    }

  let create () =
    { fresh_ids = Common.Fresh_id.create ()
    ; break_target = Null
    ; continue_target = Null
    ; current_switch = Null
    }
  ;;

  let enter_loop t =
    let id = Control_id.fresh_loop t.fresh_ids in
    { t with break_target = This id; continue_target = This id }, id
  ;;

  let enter_switch t =
    let id = Control_id.fresh_switch t.fresh_ids in
    let switch = { Switch.id; cases = Ast.Switch_cases.empty } in
    { t with break_target = This id; current_switch = This switch }, switch
  ;;

  let break_target t = t.break_target
  let continue_target t = t.continue_target
  let current_switch t = t.current_switch
end

let validate_constexpr_value expr =
  match Located.value expr with
  | Ast.Expr.Const value -> value
  | _ ->
    Error_helpers.raise_at
      expr
      [%string "expected a constant expression, found %{Ast.Expr.describe expr}"]
;;

let rec resolve_stmt stmt ~context =
  match Located.value stmt with
  | Ast.Stmt.Break _ ->
    (match Context.break_target context with
     | Null -> Error_helpers.raise_at stmt "[break] has no enclosing loop or switch"
     | This id ->
       Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Break (This id)) [@nontail])
  | Continue _ ->
    (match Context.continue_target context with
     | Null -> Error_helpers.raise_at stmt "[continue] has no enclosing loop"
     | This id ->
       Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Continue (This id)) [@nontail])
  | While { cond; body; id = _ } ->
    let context, id = Context.enter_loop context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond; body = resolve_stmt body ~context; id = This id })
    [@nontail]
  | Do_while { body; cond; id = _ } ->
    let context, id = Context.enter_loop context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while { body = resolve_stmt body ~context; cond; id = This id })
    [@nontail]
  | For { init; cond; post; body; id = _ } ->
    let context, id = Context.enter_loop context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.For { init; cond; post; body = resolve_stmt body ~context; id = This id })
    [@nontail]
  | Switch { cases = _; cond; body; id = _ } ->
    let context, switch = Context.enter_switch context in
    let body = resolve_stmt body ~context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Switch { cases = switch.cases; cond; body; id = This switch.id })
    [@nontail]
  | Case { value; body; id = _ } ->
    (match Context.current_switch context with
     | Null -> Error_helpers.raise_at stmt "[case] has no enclosing [switch]"
     | This switch ->
       let case_value = validate_constexpr_value value in
       (match Ast.Switch_cases.add_case switch.cases case_value with
        | `Duplicate ->
          Error_helpers.raise_at stmt [%string "duplicate case value %{case_value#Int}"]
        | `Added cases ->
          switch.cases <- cases;
          Located.map stmt ~f:(stack_ fun _ ->
            Ast.Stmt.Case
              { value; body = resolve_stmt body ~context; id = This switch.id })
          [@nontail]))
  | Default { body; id = _ } ->
    (match Context.current_switch context with
     | Null -> Error_helpers.raise_at stmt "[default] has no enclosing [switch]"
     | This switch ->
       (match Ast.Switch_cases.add_default switch.cases with
        | `Duplicate -> Error_helpers.raise_at stmt "duplicate [default]"
        | `Added cases ->
          switch.cases <- cases;
          Located.map stmt ~f:(stack_ fun _ ->
            Ast.Stmt.Default { body = resolve_stmt body ~context; id = This switch.id })
          [@nontail]))
  | Label { lbl; body } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Label { lbl; body = resolve_stmt body ~context })
    [@nontail]
  | If { cond; then_; else_ } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.If
        { cond
        ; then_ = resolve_stmt then_ ~context
        ; else_ = Or_null.map else_ ~f:(stack_ fun else_ -> resolve_stmt else_ ~context)
        })
    [@nontail]
  | Compound block ->
    Located.map block ~f:(stack_ fun _ ->
      Ast.Stmt.Compound (resolve_block block ~context))
    [@nontail]
  | Goto _ | Return _ | Expr _ | Null -> stmt

and resolve_block_item block_item ~context =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    Located.map block_item ~f:(stack_ fun _ ->
      Ast.Block_item.Stmt (resolve_stmt stmt ~context))
    [@nontail]
  | Decl _ -> block_item

and resolve_block block ~context =
  Located.map block ~f:(stack_ fun block ->
    Iarray.map block ~f:(fun block_item -> resolve_block_item block_item ~context))
  [@nontail]
;;

let resolve_fun_decl fun_decl =
  Located.map
    fun_decl
    ~f:(stack_ fun { Ast.Fun_decl.name; params; body; ret_type; storage } ->
      let body =
        Or_null.map body ~f:(stack_ fun body ->
          resolve_block body ~context:(Context.create ()))
      in
      { Ast.Fun_decl.name; params; body; ret_type; storage })
  [@nontail]
;;

let resolve_decl decl =
  Located.map decl ~f:(stack_ fun decl ->
    match decl with
    | Ast.Decl.Var_decl _ -> decl
    | Fun_decl decl -> Ast.Decl.Fun_decl (resolve_fun_decl decl))
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun decls -> Iarray.map decls ~f:resolve_decl) [@nontail]
;;
