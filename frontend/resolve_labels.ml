open! Core
module Located = Source.Located

let fresh_label fresh_ids = [%string "lbl.%{Common.Fresh_id.fresh fresh_ids#Int}"]

let gather_labels body =
  let fresh_ids = Common.Fresh_id.create () in
  let labels = String.Table.create () in
  let rec local_ gather_labels stmt =
    match Located.value stmt with
    | Ast.Stmt.Label { lbl; stmt = inner } ->
      (match Hashtbl.mem labels lbl with
       | true -> Error_helpers.raise_at stmt [%string "duplicate label %{lbl}"]
       | false -> Hashtbl.add_exn labels ~key:lbl ~data:(fresh_label fresh_ids));
      gather_labels inner
    | If { cond = _; then_; else_ } ->
      gather_labels then_;
      Or_null.iter else_ ~f:(stack_ fun else_ -> gather_labels else_) [@nontail]
    | Return _ | Expr _ | Goto _ | Null -> ()
  in
  Vec.iter body ~f:(stack_ fun block_item ->
    match Located.value block_item with
    | Ast.Block_item.Stmt stmt -> gather_labels stmt
    | Decl _ -> ());
  labels
;;

let rec resolve_stmt stmt ~labels =
  match Located.value stmt with
  | Ast.Stmt.Label { lbl; stmt = inner } ->
    let lbl = Hashtbl.find_exn labels lbl in
    Located.map stmt ~f:(stack_ fun _ ->
      Ast.Stmt.Label { lbl; stmt = resolve_stmt inner ~labels })
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
  | Return _ | Expr _ | Null -> stmt
;;

let resolve_block_item block_item ~labels =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    Located.map block_item ~f:(stack_ fun _ ->
      Ast.Block_item.Stmt (resolve_stmt stmt ~labels))
    [@nontail]
  | Decl _ -> block_item
;;

let resolve_fun_def fun_def =
  Located.map fun_def ~f:(stack_ fun { Ast.Fun_def.name; body } ->
    let labels = gather_labels body in
    let resolved_body = Vec.create () ~initial_capacity:(Vec.length body) in
    Vec.iter body ~f:(stack_ fun block_item ->
      Vec.push_back resolved_body (resolve_block_item block_item ~labels));
    { Ast.Fun_def.name; body = resolved_body })
  [@nontail]
;;

let program (prog : Ast.Prog.t) =
  Located.map prog ~f:(stack_ fun fun_def -> resolve_fun_def fun_def) [@nontail]
;;
