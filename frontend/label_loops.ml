open! Core
module Located = Source.Located

let fresh_loop_id fresh_ids = [%string "loop.%{Common.Fresh_id.fresh fresh_ids#Int}"]

let rec label_stmt stmt ~fresh_ids ~loop_id =
  match Located.value stmt with
  | Ast.Stmt.Break _ ->
    (match loop_id with
     | Null -> Error_helpers.raise_at stmt [%string "[break] has no enclosing loop"]
     | This loop_id ->
       Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Break loop_id) [@nontail])
  | Ast.Stmt.Continue _ ->
    (match loop_id with
     | Null -> Error_helpers.raise_at stmt [%string "[continue] has no enclosing loop"]
     | This loop_id ->
       Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Continue loop_id) [@nontail])
  | While { cond; body; id } ->
    let id = fresh_loop_id fresh_ids in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond; body = label_stmt body ~fresh_ids ~loop_id:(This id); id })
    [@nontail]
  | Do_while { body; cond; id } ->
    let id = fresh_loop_id fresh_ids in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while { body = label_stmt body ~fresh_ids ~loop_id:(This id); cond; id })
    [@nontail]
  | For { init; cond; post; body; id } ->
    let id = fresh_loop_id fresh_ids in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.For
        { init; cond; post; body = label_stmt body ~fresh_ids ~loop_id:(This id); id })
    [@nontail]
  | Label { lbl; stmt = inner } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Label { lbl; stmt = label_stmt inner ~fresh_ids ~loop_id })
    [@nontail]
  | Goto lbl -> Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Goto lbl) [@nontail]
  | If { cond; then_; else_ } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.If
        { cond
        ; then_ = label_stmt then_ ~fresh_ids ~loop_id
        ; else_ =
            Or_null.map else_ ~f:(stack_ fun else_ ->
              label_stmt else_ ~fresh_ids ~loop_id)
        })
    [@nontail]
  | Compound block ->
    Located.map block ~f:(stack_ fun _ ->
      Ast.Stmt.Compound (label_block block ~fresh_ids ~loop_id))
    [@nontail]
  | Return _ | Expr _ | Null -> stmt

and label_block_item block_item ~fresh_ids ~loop_id =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    Located.map block_item ~f:(stack_ fun _ ->
      Ast.Block_item.Stmt (label_stmt stmt ~fresh_ids ~loop_id))
    [@nontail]
  | Decl _ -> block_item

and label_block block ~fresh_ids ~loop_id =
  Located.map block ~f:(stack_ fun block ->
    let labeled_block = Vec.create () ~initial_capacity:(Vec.length block) in
    Vec.iter block ~f:(stack_ fun block_item ->
      Vec.push_back labeled_block (label_block_item block_item ~fresh_ids ~loop_id));
    labeled_block)
  [@nontail]
;;

let label_fun_def fun_def =
  Located.map fun_def ~f:(stack_ fun { Ast.Fun_def.name; body } ->
    let fresh_ids = Common.Fresh_id.create () in
    let body = label_block body ~fresh_ids ~loop_id:Null in
    { Ast.Fun_def.name; body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun fun_def -> label_fun_def fun_def) [@nontail]
;;
