open! Core
module Located = Source.Located

module Temp = struct
  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh t =
    let x = t.next in
    t.next <- x + 1;
    x
  ;;
end

let gather_lbls body =
  let tmp = Temp.create () in
  let lbls = String.Table.create () in
  let rec local_ gather_lbls stmt =
    match Located.value stmt with
    | Ast.Stmt.Label { lbl; stmt = inner } ->
      (match Hashtbl.find_or_null lbls lbl with
       | This _ ->
         Error_helpers.raise_error
           [%string "duplicate label %{lbl}"]
           ~position:(Located.location stmt |> Source.Location.start_position)
       | Null -> Hashtbl.add_exn lbls ~key:lbl ~data:[%string "lbl.%{Temp.fresh tmp#Int}"]);
      gather_lbls inner
    | If { cond = _; then_; else_ } ->
      gather_lbls then_;
      Or_null.iter else_ ~f:(stack_ fun else_ -> gather_lbls else_) [@nontail]
    | Return _ | Expr _ | Goto _ | Null -> ()
  in
  Vec.iter body ~f:(stack_ fun block_item ->
    match Located.value block_item with
    | Ast.Block_item.Stmt stmt -> gather_lbls stmt
    | Decl _ -> ());
  lbls
;;

let rec resolve_stmt stmt ~lbls =
  match Located.value stmt with
  | Ast.Stmt.Label { lbl; stmt } ->
    let lbl = Hashtbl.find_exn lbls lbl in
    Located.create
      (Ast.Stmt.Label { lbl; stmt = resolve_stmt stmt ~lbls })
      ~location:(Located.location stmt)
  | Goto lbl ->
    (match Hashtbl.find_or_null lbls lbl with
     | Null ->
       Error_helpers.raise_error
         [%string "undeclared label %{lbl}"]
         ~position:(Located.location stmt |> Source.Location.start_position)
     | This lbl -> Located.create (Ast.Stmt.Goto lbl) ~location:(Located.location stmt))
  | If { cond; then_; else_ } ->
    Located.create
      (Ast.Stmt.If
         { cond
         ; then_ = resolve_stmt then_ ~lbls
         ; else_ = Or_null.map else_ ~f:(stack_ fun else_ -> resolve_stmt else_ ~lbls)
         })
      ~location:(Located.location stmt)
  | Return _ | Expr _ | Null -> stmt
;;

let resolve_block_item block_item ~lbls =
  match Located.value block_item with
  | Ast.Block_item.Stmt stmt ->
    let stmt = resolve_stmt stmt ~lbls in
    Located.create (Ast.Block_item.Stmt stmt) ~location:(Located.location block_item)
  | Decl _ -> block_item
;;

let resolve_fun_def fun_def =
  let { Ast.Fun_def.name; body } = Located.value fun_def in
  let lbls = gather_lbls body in
  let new_body = Vec.create () ~initial_capacity:(Vec.length body) in
  Vec.iter body ~f:(stack_ fun block_item ->
    let block_item = resolve_block_item block_item ~lbls in
    Vec.push_back new_body block_item);
  Located.create
    { Ast.Fun_def.name; body = new_body }
    ~location:(Located.location fun_def)
;;

let program (prog : Ast.Prog.t) =
  let fun_def = Located.value prog in
  let fun_def = resolve_fun_def fun_def in
  Located.create fun_def ~location:(Located.location prog)
;;
