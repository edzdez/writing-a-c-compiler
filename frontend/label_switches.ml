open! Core
module Located = Source.Located

module Context = struct
  module Switch = struct
    type t =
      { id : Ast.Control_id.t
      ; mutable cases : Ast.Switch_cases.t
      }
  end

  type t =
    { fresh_ids : Common.Fresh_id.t
    ; current_switch : Switch.t or_null
    }

  let create () = { fresh_ids = Common.Fresh_id.create (); current_switch = Null }
  let current_switch t = t.current_switch

  let enter_switch t =
    let id = Ast.Control_id.fresh_switch t.fresh_ids in
    let switch = { Switch.id; cases = Ast.Switch_cases.empty } in
    { t with current_switch = This switch }, switch
  ;;
end

let validate_constexpr_value expr =
  match Located.value expr with
  | Ast.Expr.Const value -> value
  | _ ->
    Error_helpers.raise_at
      expr
      [%string "expected a constant expression, found %{Ast.Expr.describe expr}"]
;;

let rec label_stmt stmt ~context =
  match Located.value stmt with
  | Ast.Stmt.Case { value; body; id = _ } ->
    (match Context.current_switch context with
     | Null -> Error_helpers.raise_at stmt [%string "[case] has no enclosing [switch]"]
     | This switch ->
       let case_value = validate_constexpr_value value in
       (match Ast.Switch_cases.add_case switch.cases case_value with
        | `Duplicate ->
          Error_helpers.raise_at stmt [%string "duplicate case value %{case_value#Int}"]
        | `Added cases ->
          switch.cases <- cases;
          Located.map stmt ~f:(stack_ fun _ ->
            Ast.Stmt.Case { value; body = label_stmt body ~context; id = This switch.id })
          [@nontail]))
  | Default { body; id = _ } ->
    (match Context.current_switch context with
     | Null -> Error_helpers.raise_at stmt [%string "[default] has no enclosing [switch]"]
     | This switch ->
       (match Ast.Switch_cases.add_default switch.cases with
        | `Duplicate -> Error_helpers.raise_at stmt "duplicate [default]"
        | `Added cases ->
          switch.cases <- cases;
          Located.map stmt ~f:(stack_ fun _ ->
            Ast.Stmt.Default { body = label_stmt body ~context; id = This switch.id })
          [@nontail]))
  | Switch { cases = _; cond; body; id = _ } ->
    let context, switch = Context.enter_switch context in
    let body = label_stmt body ~context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Switch { cases = switch.cases; cond; body; id = This switch.id })
    [@nontail]
  | Label { lbl; body } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Label { lbl; body = label_stmt body ~context })
    [@nontail]
  | If { cond; then_; else_ } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.If
        { cond
        ; then_ = label_stmt then_ ~context
        ; else_ = Or_null.map else_ ~f:(stack_ fun else_ -> label_stmt else_ ~context)
        })
    [@nontail]
  | Compound block ->
    Located.map block ~f:(stack_ fun _ -> Ast.Stmt.Compound (label_block block ~context))
    [@nontail]
  | While { cond; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond; body = label_stmt body ~context; id })
    [@nontail]
  | Do_while { body; cond; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while { body = label_stmt body ~context; cond; id })
    [@nontail]
  | For { init; cond; post; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.For { init; cond; post; body = label_stmt body ~context; id })
    [@nontail]
  | Goto _ | Break _ | Continue _ | Return _ | Expr _ | Null -> stmt

and label_block_item block_item ~context =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    Located.map block_item ~f:(stack_ fun _ ->
      Ast.Block_item.Stmt (label_stmt stmt ~context))
    [@nontail]
  | Decl _ -> block_item

and label_block block ~context =
  Located.map block ~f:(stack_ fun block ->
    Iarray.map block ~f:(fun block_item -> label_block_item block_item ~context))
  [@nontail]
;;

let label_fun_def fun_def =
  Located.map fun_def ~f:(stack_ fun { Ast.Fun_def.name; body } ->
    let context = Context.create () in
    let body = label_block body ~context in
    { Ast.Fun_def.name; body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun fun_def -> label_fun_def fun_def) [@nontail]
;;
