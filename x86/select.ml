open! Core

let unary_op = function
  | Tacky.Ir.Unary_op.Negate -> Ir.Unary_op.Neg
  | Complement -> Ir.Unary_op.Not
;;

let operand = function
  | Tacky.Ir.Val.Constant constant -> Ir.Operand.Imm constant
  | Var var -> Ir.Operand.Pseudo var
;;

let instr instr ~instrs =
  match instr with
  | Tacky.Ir.Instr.Unary { op; src; dst } ->
    let src = operand src in
    let dst = operand dst in
    Vec.push_back instrs (Ir.Instr.Mov { src; dst });
    Vec.push_back instrs (Ir.Instr.Unary { op = unary_op op; dst })
  | Return val_ ->
    let val_ = operand val_ in
    Vec.push_back instrs (Ir.Instr.Mov { src = val_; dst = Ir.Operand.Reg `Ax });
    Vec.push_back instrs Ir.Instr.Ret
;;

let fun_def { Tacky.Ir.Fun_def.name; instrs = body } =
  let instrs = Vec.create () in
  Vec.iter body ~f:(instr ~instrs);
  { Ir.Fun_def.name; instrs }
;;

let program = fun_def
