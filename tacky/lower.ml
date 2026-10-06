open! Core
module Ast = Frontend.Ast
module Located = Source.Located

module Temp_var = struct
  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh t =
    let x = t.next in
    t.next <- x + 1;
    x
  ;;
end

let lower_unary_op unary_op =
  match Located.value unary_op with
  | Ast.Unary_op.Complement -> Ir.Unary_op.Complement
  | Negate -> Negate
;;

let rec lower_expr expr ~temp_vars ~instrs =
  match Located.value expr with
  | Ast.Expr.Const constant -> Ir.Val.Constant constant
  | Unary { op; expr } ->
    let src = lower_expr expr ~temp_vars ~instrs in
    let dst = Ir.Val.Var [%string "tmp.%{Temp_var.fresh temp_vars#Int}"] in
    let op = lower_unary_op op in
    Vec.push_back instrs (Ir.Instr.Unary { op; src; dst });
    dst
;;

let lower_stmt stmt ~temp_vars ~instrs =
  match Located.value stmt with
  | Ast.Stmt.Return expr ->
    let val_ = lower_expr expr ~temp_vars ~instrs in
    Vec.push_back instrs (Ir.Instr.Return val_)
;;

let lower_fun_def fun_def ~temp_vars =
  let { Ast.Fun_def.name; body } = Located.value fun_def in
  let instrs = Vec.create () in
  lower_stmt body ~temp_vars ~instrs;
  { Ir.Fun_def.name; instrs }
;;

let program prog =
  let prog = Located.value prog in
  let temp_vars = Temp_var.create () in
  lower_fun_def prog ~temp_vars
;;
