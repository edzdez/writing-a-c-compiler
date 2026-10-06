open! Core

let program { Ir.Fun_def.name; instrs = input } =
  let stack_slots = String.Table.create () in
  let local_ stack_offset pseudo =
    Hashtbl.find_or_add stack_slots pseudo ~default:(fun () ->
      (Hashtbl.length stack_slots + 1) * 4)
  in
  let local_ operand = function
    | Ir.Operand.Pseudo pseudo -> Ir.Operand.Stack (stack_offset pseudo)
    | operand -> operand
  in
  let instrs = Vec.create ~initial_capacity:(Vec.length input + 1) () in
  Vec.push_back instrs (Ir.Instr.Alloc_stack 0);
  Vec.iter input ~f:(stack_ fun instr ->
    let instr =
      match instr with
      | Ir.Instr.Mov { src; dst } -> Ir.Instr.Mov { src = operand src; dst = operand dst }
      | Unary { op; dst } -> Ir.Instr.Unary { op; dst = operand dst }
      | Alloc_stack _ | Ret -> instr
    in
    Vec.push_back instrs instr);
  Vec.unsafe_set instrs 0 (Ir.Instr.Alloc_stack (Hashtbl.length stack_slots * 4));
  { Ir.Fun_def.name; instrs }
;;
