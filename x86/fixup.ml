open! Core

let fun_def { Ir.Fun_def.name; instrs = input } =
  let instrs = Vec.create ~initial_capacity:(Iarray.length input) () in
  Iarray.iter input ~f:(stack_ fun instr ->
    match instr with
    | Ir.Instr.Mov { src = Stack _ as src; dst = Stack _ as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R10; dst })
    | Ir.Instr.Binary
        { op = (Add | Sub | And | Or | Xor) as op
        ; src = Stack _ as src
        ; dst = Stack _ as dst
        } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Binary { op; src = Ir.Operand.Reg `R10; dst })
    | Ir.Instr.Binary { op = Mult as op; src; dst = Stack _ as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = dst; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Binary { op; src; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R11; dst })
    | Ir.Instr.Binary { op = (Shl | Sar) as op; src = (Stack _ | Reg _) as src; dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `Cx });
      Vec.push_back instrs (Ir.Instr.Binary { op; src = Ir.Operand.Reg `Cx; dst })
    | Ir.Instr.Cmp { left; right = Imm imm as right } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = right; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Cmp { left; right = Ir.Operand.Reg `R11 })
    | Ir.Instr.Cmp { left = Stack _ as left; right = Stack _ as right } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = left; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Cmp { left = Ir.Operand.Reg `R10; right })
    | Idiv (Imm _ as op) ->
      Vec.push_back instrs (Ir.Instr.Mov { src = op; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Idiv (Ir.Operand.Reg `R10))
    | instr -> Vec.push_back instrs instr);
  { Ir.Fun_def.name; instrs = Vec.to_iarray instrs }
;;

let program program = Iarray.map program ~f:(stack_ fun f -> fun_def f) [@nontail]
