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

let is_lvalue expr =
  match Located.value expr with
  | Ast.Expr.Var _ -> true
  | _ -> false
;;

let validate_lvalue expr =
  match is_lvalue expr with
  | true -> ()
  | false ->
    Error_helpers.raise_error
      [%string "expected an lvalue, found %{Ast.Expr.describe expr}"]
      ~position:(Located.location expr |> Source.Location.start_position)
;;

let rec resolve_expr expr ~vars =
  match Located.value expr with
  | Ast.Expr.Assign { left; right } ->
    validate_lvalue left;
    Located.create
      (Ast.Expr.Assign
         { left = resolve_expr left ~vars; right = resolve_expr right ~vars })
      ~location:(Located.location expr)
  | Compound_assign { op; left; right } ->
    validate_lvalue left;
    Located.create
      (Ast.Expr.Compound_assign
         { op; left = resolve_expr left ~vars; right = resolve_expr right ~vars })
      ~location:(Located.location expr)
  | Conditional { cond; then_; else_ } ->
    Located.create
      (Ast.Expr.Conditional
         { cond = resolve_expr cond ~vars
         ; then_ = resolve_expr then_ ~vars
         ; else_ = resolve_expr else_ ~vars
         })
      ~location:(Located.location expr)
  | Var v ->
    (match Hashtbl.find_or_null vars v with
     | Null ->
       Error_helpers.raise_error
         [%string "undeclared variable %{v}"]
         ~position:(Located.location expr |> Source.Location.start_position)
     | This v -> Located.create (Ast.Expr.Var v) ~location:(Located.location expr))
  | Unary { op; expr = inner } ->
    (match Located.value op with
     | (Pre_incr | Pre_decr | Post_incr | Post_decr) when not (is_lvalue inner) ->
       Error_helpers.raise_error
         [%string "expected an lvalue, found %{Ast.Expr.describe inner}"]
         ~position:(Located.location inner |> Source.Location.start_position)
     | _ ->
       Located.create
         (Ast.Expr.Unary { op; expr = resolve_expr inner ~vars })
         ~location:(Located.location expr))
  | Binary { op; left; right } ->
    Located.create
      (Ast.Expr.Binary
         { op; left = resolve_expr left ~vars; right = resolve_expr right ~vars })
      ~location:(Located.location expr)
  | Const _ -> expr
;;

let resolve_decl decl ~vars ~temp_vars =
  let { Ast.Decl.name; init } = Located.value decl in
  match Hashtbl.find_or_null vars name with
  | This _ ->
    Error_helpers.raise_error
      [%string "duplicate variable declaration %{name}"]
      ~position:(Located.location decl |> Source.Location.start_position)
  | Null ->
    let unique_name = [%string "var.%{name}.%{Temp.fresh temp_vars#Int}"] in
    Hashtbl.add_exn vars ~key:name ~data:unique_name;
    let init = Or_null.map init ~f:(resolve_expr ~vars) in
    Located.create { Ast.Decl.name = unique_name; init } ~location:(Located.location decl)
;;

let rec resolve_stmt stmt ~vars =
  match Located.value stmt with
  | Ast.Stmt.Return expr ->
    Located.create
      (Ast.Stmt.Return (resolve_expr expr ~vars))
      ~location:(Located.location stmt)
  | Expr expr ->
    Located.create
      (Ast.Stmt.Expr (resolve_expr expr ~vars))
      ~location:(Located.location stmt)
  | If { cond; then_; else_ } ->
    Located.create
      (Ast.Stmt.If
         { cond = resolve_expr cond ~vars
         ; then_ = resolve_stmt then_ ~vars
         ; else_ = Or_null.map else_ ~f:(resolve_stmt ~vars)
         })
      ~location:(Located.location stmt)
  | Null -> stmt
;;

let resolve_block_item block_item ~vars ~temp_vars =
  match Located.value block_item with
  | Ast.Block_item.Decl decl ->
    let decl = resolve_decl decl ~vars ~temp_vars in
    Located.create (Ast.Block_item.Decl decl) ~location:(Located.location block_item)
  | Stmt stmt ->
    let stmt = resolve_stmt stmt ~vars in
    Located.create (Ast.Block_item.Stmt stmt) ~location:(Located.location block_item)
;;

let resolve_fun_def fun_def ~vars ~temp_vars =
  let { Ast.Fun_def.name; body } = Located.value fun_def in
  let new_body = Vec.create () ~initial_capacity:(Vec.length body) in
  Vec.iter body ~f:(stack_ fun block_item ->
    let block_item = resolve_block_item block_item ~vars ~temp_vars in
    Vec.push_back new_body block_item);
  Located.create
    { Ast.Fun_def.name; body = new_body }
    ~location:(Located.location fun_def)
;;

let program (prog : Ast.Prog.t) =
  let fun_def = Located.value prog in
  let vars = String.Table.create () in
  let temp_vars = Temp.create () in
  let fun_def = resolve_fun_def fun_def ~vars ~temp_vars in
  Located.create fun_def ~location:(Located.location prog)
;;
