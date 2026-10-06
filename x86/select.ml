open! Core

let unary_op = function
  | Tacky.Ir.Unary_op.Negate -> Ir.Unary_op.Neg
  | Complement -> Ir.Unary_op.Not
;;

let binary_op = function
  | Tacky.Ir.Binary_op.Add -> Ir.Binary_op.Add
  | Sub -> Sub
  | Mul -> Mult
  | op -> raise_s [%message "unsupported binary op" (op : Tacky.Ir.Binary_op.t)]
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
  | Binary { op; left; right; dst } ->
    let left = operand left in
    let dst = operand dst in
    let right = operand right in
    (match op with
     | Add | Sub | Mul ->
       Vec.push_back instrs (Ir.Instr.Mov { src = left; dst });
       Vec.push_back instrs (Ir.Instr.Binary { op = binary_op op; src = right; dst })
     | Div | Rem ->
       let aux_reg =
         match op with
         | Div -> `Ax
         | Rem -> `Dx
         | _ -> failwith "unreachable"
       in
       Vec.push_back instrs (Ir.Instr.Mov { src = left; dst = Reg `Ax });
       Vec.push_back instrs Ir.Instr.Cdq;
       Vec.push_back instrs (Ir.Instr.Idiv right);
       Vec.push_back instrs (Ir.Instr.Mov { src = Reg aux_reg; dst }))
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
