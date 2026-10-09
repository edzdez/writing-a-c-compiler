open! Core
module Located = Source.Located

let fresh_label fresh_ids = [%string "lbl.%{Common.Fresh_id.fresh fresh_ids#Int}"]

let gather_labels body =
  let body = Located.value body in
  let fresh_ids = Common.Fresh_id.create () in
  let labels = String.Table.create () in
  let rec local_ gather_labels stmt =
    match Located.value stmt with
    | Ast.Stmt.Label { lbl; body } ->
      (match Hashtbl.mem labels lbl with
       | true -> Error_helpers.raise_at stmt [%string "duplicate label %{lbl}"]
       | false -> Hashtbl.add_exn labels ~key:lbl ~data:(fresh_label fresh_ids));
      gather_labels body
    | If { cond = _; then_; else_ } ->
      gather_labels then_;
      Or_null.iter else_ ~f:(stack_ fun else_ -> gather_labels else_) [@nontail]
    | Compound block ->
      let block = Located.value block in
      Vec.iter block ~f:(stack_ fun block_item ->
        match Located.value block_item with
        | Decl _ -> ()
        | Stmt stmt -> gather_labels stmt)
      [@nontail]
    | While { cond; body; id } -> gather_labels body
    | Do_while { body; cond; id } -> gather_labels body
    | For { init; cond; post; body; id } -> gather_labels body
    | Case { value = _; body; id = _ } -> gather_labels body
    | Default { body; id = _ } -> gather_labels body
    | Switch { cases = _; cond = _; body; id = _ } -> gather_labels body
    | Break _ | Continue _ | Return _ | Expr _ | Goto _ | Null -> ()
  in
  Vec.iter body ~f:(stack_ fun block_item ->
    match Located.value block_item with
    | Ast.Block_item.Stmt stmt -> gather_labels stmt
    | Decl _ -> ());
  labels
;;

let rec resolve_stmt stmt ~labels =
  match Located.value stmt with
  | Ast.Stmt.Label { lbl; body } ->
    let lbl = Hashtbl.find_exn labels lbl in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Label { lbl; body = resolve_stmt body ~labels })
    [@nontail]
  | Goto lbl ->
    (match Hashtbl.find_or_null labels lbl with
     | Null -> Error_helpers.raise_at stmt [%string "undeclared label %{lbl}"]
     | This lbl -> Located.map stmt ~f:(stack_ fun _ -> Ast.Stmt.Goto lbl) [@nontail])
  | If { cond; then_; else_ } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.If
        { cond
        ; then_ = resolve_stmt then_ ~labels
        ; else_ = Or_null.map else_ ~f:(stack_ fun else_ -> resolve_stmt else_ ~labels)
        })
    [@nontail]
  | Compound block ->
    Located.map block ~f:(stack_ fun _ -> Ast.Stmt.Compound (resolve_block block ~labels))
    [@nontail]
  | While { cond; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.While { cond; body = resolve_stmt body ~labels; id })
    [@nontail]
  | Do_while { body; cond; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Do_while { body = resolve_stmt body ~labels; cond; id })
    [@nontail]
  | For { init; cond; post; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.For { init; cond; post; body = resolve_stmt body ~labels; id })
    [@nontail]
  | Case { value; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Case { value; body = resolve_stmt body ~labels; id })
    [@nontail]
  | Default { body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Default { body = resolve_stmt body ~labels; id })
    [@nontail]
  | Switch { cases; cond; body; id } ->
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Switch { cases; cond; body = resolve_stmt body ~labels; id })
    [@nontail]
  | Break _ | Continue _ | Return _ | Expr _ | Null -> stmt

and resolve_block_item block_item ~labels =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    Located.map block_item ~f:(stack_ fun _ ->
      Ast.Block_item.Stmt (resolve_stmt stmt ~labels))
    [@nontail]
  | Decl _ -> block_item

and resolve_block block ~labels =
  Located.map block ~f:(stack_ fun block ->
    let resolved_block = Vec.create () ~initial_capacity:(Vec.length block) in
    Vec.iter block ~f:(stack_ fun block_item ->
      Vec.push_back resolved_block (resolve_block_item block_item ~labels));
    resolved_block)
  [@nontail]
;;

let resolve_fun_def fun_def =
  Located.map fun_def ~f:(stack_ fun { Ast.Fun_def.name; body } ->
    let labels = gather_labels body in
    let body = resolve_block body ~labels in
    { Ast.Fun_def.name; body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun fun_def -> resolve_fun_def fun_def) [@nontail]
;;
