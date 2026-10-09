open! Core

let unary_op = function
  | Tacky.Ir.Unary_op.Negate -> Ir.Unary_op.Neg
  | Complement -> Ir.Unary_op.Not
  | Not as op -> raise_s [%message "nonstandard unary op" (op : Tacky.Ir.Unary_op.t)]
;;

let binary_op = function
  | Tacky.Ir.Binary_op.Add -> Ir.Binary_op.Add
  | Sub -> Sub
  | Mul -> Mult
  | Bit_and -> And
  | Bit_or -> Or
  | Xor -> Xor
  | L_shift -> Shl
  | R_shift -> Sar
  | op -> raise_s [%message "unsupported binary op" (op : Tacky.Ir.Binary_op.t)]
;;

let operand = function
  | Tacky.Ir.Val.Constant constant -> Ir.Operand.Imm constant
  | Var var -> Ir.Operand.Pseudo var
;;

let cc_of_binary_op = function
  | Tacky.Ir.Binary_op.Eq -> Ir.Cond_code.E
  | Neq -> Ne
  | Lt -> L
  | Gt -> G
  | Lte -> Le
  | Gte -> Ge
  | op ->
    raise_s [%message "no associated cond code for binary op" (op : Tacky.Ir.Binary_op.t)]
;;

let instr instr ~instrs =
  match instr with
  | Tacky.Ir.Instr.Unary { op; src; dst } ->
    let src = operand src in
    let dst = operand dst in
    (match op with
     | Negate | Complement ->
       Vec.push_back instrs (Ir.Instr.Mov { src; dst });
       Vec.push_back instrs (Ir.Instr.Unary { op = unary_op op; dst })
     | Not ->
       Vec.push_back instrs (Ir.Instr.Cmp { left = Imm 0; right = src });
       Vec.push_back instrs (Ir.Instr.Mov { src = Imm 0; dst });
       Vec.push_back instrs (Ir.Instr.Set_cc { cc = E; dst }))
  | Binary { op; left; right; dst } ->
    let left = operand left in
    let dst = operand dst in
    let right = operand right in
    (match op with
     | Add | Sub | Mul | Bit_and | Bit_or | Xor | L_shift | R_shift ->
       Vec.push_back instrs (Ir.Instr.Mov { src = left; dst });
       Vec.push_back instrs (Ir.Instr.Binary { op = binary_op op; src = right; dst })
     | Eq | Neq | Lt | Gt | Lte | Gte ->
       Vec.push_back instrs (Ir.Instr.Cmp { left = right; right = left });
       Vec.push_back instrs (Ir.Instr.Mov { src = Imm 0; dst });
       Vec.push_back instrs (Ir.Instr.Set_cc { cc = cc_of_binary_op op; dst })
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
  | Copy { src; dst } ->
    let src = operand src in
    let dst = operand dst in
    Vec.push_back instrs (Ir.Instr.Mov { src; dst })
  | Jmp tgt -> Vec.push_back instrs (Ir.Instr.Jmp tgt)
  | Jz { tst; tgt } ->
    let tst = operand tst in
    Vec.push_back instrs (Ir.Instr.Cmp { left = Imm 0; right = tst });
    Vec.push_back instrs (Ir.Instr.Jmp_cc { cc = E; tgt })
  | Jnz { tst; tgt } ->
    let tst = operand tst in
    Vec.push_back instrs (Ir.Instr.Cmp { left = Imm 0; right = tst });
    Vec.push_back instrs (Ir.Instr.Jmp_cc { cc = Ne; tgt })
  | Label lbl -> Vec.push_back instrs (Ir.Instr.Label lbl)
  | Return val_ ->
    let val_ = operand val_ in
    Vec.push_back instrs (Ir.Instr.Mov { src = val_; dst = Ir.Operand.Reg `Ax });
    Vec.push_back instrs Ir.Instr.Ret
;;

let fun_def { Tacky.Ir.Fun_def.name; instrs = body } =
  let instrs = Vec.create () in
  Iarray.iter body ~f:(instr ~instrs);
  { Ir.Fun_def.name; instrs = Vec.to_iarray instrs }
;;

let program = fun_def
