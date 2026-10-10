open! Core

module Builder = struct
  type t = { instructions : Ir.Instr.t Vec.t }

  let create () = { instructions = Vec.create () }
  let emit t instruction = Vec.push_back t.instructions instruction
  let finish t = Vec.to_iarray t.instructions
end

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

let arg_regs =
  Iarray.of_array [| Ir.Operand.Reg `Di; Reg `Si; Reg `Dx; Reg `Cx; Reg `R8; Reg `R9 |]
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

let fun_call ~name ~args ~dst ~builder =
  let reg_args, stack_args = Iarray.split_n args (Iarray.length arg_regs) in
  let stack_padding = Int.rem (Iarray.length stack_args) 2 * 8 in
  if stack_padding <> 0 then Builder.emit builder (Ir.Instr.Alloc_stack 8);
  for i = 0 to Iarray.length reg_args - 1 do
    let arg = operand reg_args.:(i) in
    Builder.emit builder (Ir.Instr.Mov { src = arg; dst = arg_regs.:(i) })
  done;
  Iarray.iter (Iarray.rev stack_args) ~f:(stack_ fun arg ->
    let arg = operand arg in
    match arg with
    | Ir.Operand.Imm _ | Reg _ -> Builder.emit builder (Ir.Instr.Push arg)
    | _ ->
      Builder.emit builder (Ir.Instr.Mov { src = arg; dst = Reg `Ax });
      Builder.emit builder (Ir.Instr.Push (Reg `Ax)));
  Builder.emit builder (Ir.Instr.Call name);
  let bytes_to_remove = (8 * Iarray.length stack_args) + stack_padding in
  if bytes_to_remove <> 0 then Builder.emit builder (Ir.Instr.Dealloc_stack bytes_to_remove);
  let dst = operand dst in
  Builder.emit builder (Ir.Instr.Mov { src = Reg `Ax; dst })
;;

let instr instr ~builder =
  match instr with
  | Tacky.Ir.Instr.Unary { op; src; dst } ->
    let src = operand src in
    let dst = operand dst in
    (match op with
     | Negate | Complement ->
       Builder.emit builder (Ir.Instr.Mov { src; dst });
       Builder.emit builder (Ir.Instr.Unary { op = unary_op op; dst })
     | Not ->
       Builder.emit builder (Ir.Instr.Cmp { left = Imm 0; right = src });
       Builder.emit builder (Ir.Instr.Mov { src = Imm 0; dst });
       Builder.emit builder (Ir.Instr.Set_cc { cc = E; dst }))
  | Binary { op; left; right; dst } ->
    let left = operand left in
    let dst = operand dst in
    let right = operand right in
    (match op with
     | Add | Sub | Mul | Bit_and | Bit_or | Xor | L_shift | R_shift ->
       Builder.emit builder (Ir.Instr.Mov { src = left; dst });
       Builder.emit builder (Ir.Instr.Binary { op = binary_op op; src = right; dst })
     | Eq | Neq | Lt | Gt | Lte | Gte ->
       Builder.emit builder (Ir.Instr.Cmp { left = right; right = left });
       Builder.emit builder (Ir.Instr.Mov { src = Imm 0; dst });
       Builder.emit builder (Ir.Instr.Set_cc { cc = cc_of_binary_op op; dst })
     | Div | Rem ->
       let aux_reg =
         match op with
         | Div -> `Ax
         | Rem -> `Dx
         | _ -> failwith "unreachable"
       in
       Builder.emit builder (Ir.Instr.Mov { src = left; dst = Reg `Ax });
       Builder.emit builder Ir.Instr.Cdq;
       Builder.emit builder (Ir.Instr.Idiv right);
       Builder.emit builder (Ir.Instr.Mov { src = Reg aux_reg; dst }))
  | Copy { src; dst } ->
    let src = operand src in
    let dst = operand dst in
    Builder.emit builder (Ir.Instr.Mov { src; dst })
  | Jmp tgt -> Builder.emit builder (Ir.Instr.Jmp tgt)
  | Jz { tst; tgt } ->
    let tst = operand tst in
    Builder.emit builder (Ir.Instr.Cmp { left = Imm 0; right = tst });
    Builder.emit builder (Ir.Instr.Jmp_cc { cc = E; tgt })
  | Jnz { tst; tgt } ->
    let tst = operand tst in
    Builder.emit builder (Ir.Instr.Cmp { left = Imm 0; right = tst });
    Builder.emit builder (Ir.Instr.Jmp_cc { cc = Ne; tgt })
  | Label lbl -> Builder.emit builder (Ir.Instr.Label lbl)
  | Fun_call { name; args; dst } -> fun_call ~name ~args ~dst ~builder
  | Return val_ ->
    let val_ = operand val_ in
    Builder.emit builder (Ir.Instr.Mov { src = val_; dst = Ir.Operand.Reg `Ax });
    Builder.emit builder Ir.Instr.Ret
;;

let fun_def { Tacky.Ir.Fun_def.name; params; instrs = body } =
  let builder = Builder.create () in
  Iarray.iteri params ~f:(stack_ fun i param ->
    let src =
      match i < Iarray.length arg_regs with
      | true -> arg_regs.:(i)
      | false -> Ir.Operand.Stack (-16 - (8 * (i - Iarray.length arg_regs)))
    in
    Builder.emit builder (Ir.Instr.Mov { src; dst = Pseudo param }));
  Iarray.iter body ~f:(instr ~builder);
  { Ir.Fun_def.name; instrs = Builder.finish builder }
;;

let program prog = Iarray.map prog ~f:fun_def
