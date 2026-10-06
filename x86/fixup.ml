open! Core

let program { Ir.Fun_def.name; instrs = input } =
  let instrs = Vec.create ~initial_capacity:(Vec.length input) () in
  Vec.iter input ~f:(stack_ fun instr ->
    match instr with
    | Ir.Instr.Mov { src = Stack _ as src; dst = Stack _ as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R10; dst })
    | Ir.Instr.Binary
        { op = (Add | Sub) as op; src = Stack _ as src; dst = Stack _ as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Binary { op; src = Ir.Operand.Reg `R10; dst })
    | Ir.Instr.Binary { op = Mult as op; src; dst = Stack _ as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = dst; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Binary { op; src; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R11; dst })
    | Idiv (Imm _ as op) ->
      Vec.push_back instrs (Ir.Instr.Mov { src = op; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Idiv (Ir.Operand.Reg `R10))
    | instr -> Vec.push_back instrs instr);
  { Ir.Fun_def.name; instrs }
;;
