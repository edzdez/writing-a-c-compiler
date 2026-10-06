open! Core

let program { Ir.Fun_def.name; instrs = input } =
  let instrs = Vec.create ~initial_capacity:(Vec.length input) () in
  Vec.iter input ~f:(stack_ fun instr ->
    match instr with
    | Ir.Instr.Mov { src = Stack _ as src; dst = Stack _ as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R10; dst })
    | instr -> Vec.push_back instrs instr);
  { Ir.Fun_def.name; instrs }
;;
