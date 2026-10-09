open! Core
module Located = Source.Located

module Context = struct
  type t =
    { break_target : Ast.Control_id.t or_null
    ; continue_target : Ast.Control_id.t or_null
    }

  let empty = { break_target = Null; continue_target = Null }
  let enter_loop _ id = { break_target = This id; continue_target = This id }
  let enter_switch t id = { t with break_target = This id }
  let break_target t = t.break_target
  let continue_target t = t.continue_target
end

let resolve_id stmt kind = function
  | This id -> id
  | Null ->
    raise_s [%message "unresolved control-flow ID" (kind : string) (stmt : Ast.Stmt.t)]
;;

let rec resolve_stmt stmt ~context =
  match Located.value stmt with
  | Ast.Stmt.Break _ ->
    (match Context.break_target context with
     | Null ->
       Error_helpers.raise_at stmt [%string "[break] has no enclosing loop or switch"]
     | This id ->
       Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Break (This id)) [@nontail])
  | Continue _ ->
    (match Context.continue_target context with
     | Null -> Error_helpers.raise_at stmt [%string "[continue] has no enclosing loop"]
     | This id ->
       Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Continue (This id)) [@nontail])
  | While { cond; body; id } ->
    let id = resolve_id stmt "loop" id in
    let context = Context.enter_loop context id in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond; body = resolve_stmt body ~context; id = This id })
    [@nontail]
  | Do_while { body; cond; id } ->
    let id = resolve_id stmt "loop" id in
    let context = Context.enter_loop context id in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while { body = resolve_stmt body ~context; cond; id = This id })
    [@nontail]
  | For { init; cond; post; body; id } ->
    let id = resolve_id stmt "loop" id in
    let context = Context.enter_loop context id in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.For { init; cond; post; body = resolve_stmt body ~context; id = This id })
    [@nontail]
  | Switch { cases; cond; body; id } ->
    let id = resolve_id stmt "switch" id in
    let context = Context.enter_switch context id in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Switch { cases; cond; body = resolve_stmt body ~context; id = This id })
    [@nontail]
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
  | Case { value; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Case { value; body = resolve_stmt body ~context; id })
    [@nontail]
  | Default { body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Default { body = resolve_stmt body ~context; id })
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
  Located.map fun_decl ~f:(stack_ fun { Ast.Fun_decl.name; params; body } ->
    let body =
      Or_null.map body ~f:(stack_ fun body -> resolve_block body ~context:Context.empty)
    in
    { Ast.Fun_decl.name; params; body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun fun_decls -> Iarray.map fun_decls ~f:resolve_fun_decl)
  [@nontail]
;;
