open! Core

let fun_def { Ir.Fun_def.name; instrs = input } =
  let stack_slots = String.Table.create () in
  let local_ stack_offset pseudo =
    Hashtbl.find_or_add stack_slots pseudo ~default:(fun () ->
      (Hashtbl.length stack_slots + 1) * 4)
  in
  let local_ operand = function
    | Ir.Operand.Pseudo pseudo -> Ir.Operand.Stack (stack_offset pseudo)
    | operand -> operand
  in
  let instrs = Vec.create ~initial_capacity:(Iarray.length input + 1) () in
  Vec.push_back instrs (Ir.Instr.Alloc_stack 0);
  Iarray.iter input ~f:(stack_ fun instr ->
    let instr =
      match instr with
      | Ir.Instr.Mov { src; dst } -> Ir.Instr.Mov { src = operand src; dst = operand dst }
      | Unary { op; dst } -> Unary { op; dst = operand dst }
      | Binary { op; src; dst } -> Binary { op; src = operand src; dst = operand dst }
      | Cmp { left; right } -> Cmp { left = operand left; right = operand right }
      | Idiv op -> Idiv (operand op)
      | Set_cc { cc; dst } -> Set_cc { cc; dst = operand dst }
      | Push op -> Push (operand op)
      | Cdq | Jmp _ | Jmp_cc _ | Label _ | Alloc_stack _ | Dealloc_stack _ | Call _ | Ret
        -> instr
    in
    Vec.push_back instrs instr);
  let unaligned_stack_space = Hashtbl.length stack_slots * 4 in
  let stack_space = (unaligned_stack_space + 15) / 16 * 16 in
  Vec.unsafe_set instrs 0 (Ir.Instr.Alloc_stack stack_space);
  { Ir.Fun_def.name; instrs = Vec.to_iarray instrs }
;;

let program program = Iarray.map program ~f:(stack_ fun f -> fun_def f) [@nontail]
