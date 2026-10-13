open! Core

let fun_def { Ir.Fun_def.name; global; instrs = input } =
  let instrs = Vec.create ~initial_capacity:(Iarray.length input) () in
  Iarray.iter input ~f:(stack_ fun instr ->
    match instr with
    | Ir.Instr.Mov { src = (Stack _ | Data _) as src; dst = (Stack _ | Data _) as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R10; dst })
    | Ir.Instr.Binary
        { op = (Add | Sub | And | Or | Xor) as op
        ; src = (Stack _ | Data _) as src
        ; dst = (Stack _ | Data _) as dst
        } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Binary { op; src = Ir.Operand.Reg `R10; dst })
    | Ir.Instr.Binary { op = Mult as op; src; dst = (Stack _ | Data _) as dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = dst; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Binary { op; src; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Mov { src = Ir.Operand.Reg `R11; dst })
    | Ir.Instr.Binary
        { op = (Sal | Sar) as op; src = (Stack _ | Data _ | Reg _) as src; dst } ->
      Vec.push_back instrs (Ir.Instr.Mov { src; dst = Ir.Operand.Reg `Cx });
      Vec.push_back instrs (Ir.Instr.Binary { op; src = Ir.Operand.Reg `Cx; dst })
    | Ir.Instr.Cmp { left; right = Imm imm as right } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = right; dst = Ir.Operand.Reg `R11 });
      Vec.push_back instrs (Ir.Instr.Cmp { left; right = Ir.Operand.Reg `R11 })
    | Ir.Instr.Cmp
        { left = (Stack _ | Data _) as left; right = (Stack _ | Data _) as right } ->
      Vec.push_back instrs (Ir.Instr.Mov { src = left; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Cmp { left = Ir.Operand.Reg `R10; right })
    | Idiv (Imm _ as op) ->
      Vec.push_back instrs (Ir.Instr.Mov { src = op; dst = Ir.Operand.Reg `R10 });
      Vec.push_back instrs (Ir.Instr.Idiv (Ir.Operand.Reg `R10))
    | instr -> Vec.push_back instrs instr);
  { Ir.Fun_def.name; global; instrs = Vec.to_iarray instrs }
;;

let program program =
  Iarray.map program ~f:(stack_ function
    | Ir.Top_level.Fun f -> Ir.Top_level.Fun (fun_def f)
    | top_level -> top_level)
    [@nontail]
;;
