open! Core
module Located = Source.Located

module Context = struct
  type t = { fresh_ids : Common.Fresh_id.t }

  let create () = { fresh_ids = Common.Fresh_id.create () }
  let fresh_id t = Ast.Control_id.fresh_loop t.fresh_ids
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
    Iarray.map block ~f:(fun block_item -> label_block_item block_item ~context))
  [@nontail]
;;

let label_fun_decl fun_decl =
  Located.map fun_decl ~f:(stack_ fun { Ast.Fun_decl.name; params; body } ->
    let body =
      Or_null.map body ~f:(stack_ fun body ->
        label_block body ~context:(Context.create ()))
    in
    { Ast.Fun_decl.name; params; body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun fun_decls -> Iarray.map fun_decls ~f:label_fun_decl)
  [@nontail]
;;
