open! Core
module Located = Source.Located

module Context = struct
  type t = { fresh_ids : Common.Fresh_id.t }

  let create () = { fresh_ids = Common.Fresh_id.create () }
  let fresh_id t = [%string "loop.%{Common.Fresh_id.fresh t.fresh_ids#Int}"]
end

let rec label_stmt stmt ~context =
  match Located.value stmt with
  | Ast.Stmt.While { cond; body; id = _ } ->
    let id = Context.fresh_id context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond; body = label_stmt body ~context; id = This id })
    [@nontail]
  | Do_while { body; cond; id = _ } ->
    let id = Context.fresh_id context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while { body = label_stmt body ~context; cond; id = This id })
    [@nontail]
  | For { init; cond; post; body; id = _ } ->
    let id = Context.fresh_id context in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.For { init; cond; post; body = label_stmt body ~context; id = This id })
    [@nontail]
  | Switch { cases; cond; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Switch { cases; cond; body = label_stmt body ~context; id })
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
  | Case { value; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Case { value; body = label_stmt body ~context; id })
    [@nontail]
  | Default { body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Default { body = label_stmt body ~context; id })
    [@nontail]
  | Break _ | Continue _ | Goto _ | Return _ | Expr _ | Null -> stmt

and label_block_item block_item ~context =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    Located.map block_item ~f:(stack_ fun _ ->
      Ast.Block_item.Stmt (label_stmt stmt ~context))
    [@nontail]
  | Decl _ -> block_item

and label_block block ~context =
  Located.map block ~f:(stack_ fun block ->
    let labeled_block = Vec.create () ~initial_capacity:(Vec.length block) in
    Vec.iter block ~f:(stack_ fun block_item ->
      Vec.push_back labeled_block (label_block_item block_item ~context));
    labeled_block)
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
