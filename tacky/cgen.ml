open! Core
module With_location = Util.With_location

module Temp_var = struct
  let counter = ref 0

  let fresh () =
    let x = !counter in
    incr counter;
    x
  ;;
end

let cgen_unary_op (unary_op : Ast.Unary_op.t) : Tacky_ast.Unary_op.t =
  match With_location.value unary_op with
  | Complement -> Complement
  | Negate -> Negate
;;

let rec cgen_expr (expr : Ast.Expr.t) ~instrs : Tacky_ast.Val.t =
  match With_location.value expr with
  | Const constant -> Constant constant
  | Unary { op; expr } ->
    let src = cgen_expr expr ~instrs in
    let dst = Tacky_ast.Val.Var [%string "tmp.%{Temp_var.fresh ()#Int}"] in
    let op = cgen_unary_op op in
    Vec.push_back instrs (Tacky_ast.Instr.Unary { op; src; dst });
    dst
;;

let cgen_stmt (stmt : Ast.Stmt.t) ~instrs : unit =
  match With_location.value stmt with
  | Return expr ->
    let val_ = cgen_expr expr ~instrs in
    Vec.push_back instrs (Tacky_ast.Instr.Return val_)
;;

let cgen_fun_def (fun_def : Ast.Fun_def.t) : Tacky_ast.Fun_def.t =
  let { name; body } : Ast.Fun_def.node = With_location.value fun_def in
  let instrs = Vec.create () in
  cgen_stmt body ~instrs;
  { name; instrs }
;;

let cgen_prog (prog : Ast.Prog.t) : Tacky_ast.Prog.t =
  let prog = With_location.value prog in
  cgen_fun_def prog
;;
