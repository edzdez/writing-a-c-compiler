open! Core
module Symbol_table = Semantics.Symbol_table

let rewrite_pseudo pseudo ~symbols ~stack_slots =
  match Hashtbl.find_or_null stack_slots pseudo with
  | This slot -> Ir.Operand.Stack slot
  | Null ->
    (match Symbol_table.find symbols pseudo with
     | This { type_ = _; attrs = Static _ } -> Data pseudo
     | _ ->
       let slot = (Hashtbl.length stack_slots + 1) * 4 in
       Hashtbl.add_exn stack_slots ~key:pseudo ~data:slot;
       Stack slot)
;;

let fun_def { Ir.Fun_def.name; global; instrs = input } ~symbols =
  let stack_slots = String.Table.create () in
  let local_ operand = function
    | Ir.Operand.Pseudo pseudo -> rewrite_pseudo pseudo ~symbols ~stack_slots
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
  { Ir.Fun_def.name; global; instrs = Vec.to_iarray instrs }
;;

let program program ~symbols =
  Iarray.map program ~f:(stack_ function
    | Ir.Top_level.Fun f -> Ir.Top_level.Fun (fun_def f ~symbols)
    | top_level -> top_level)
    [@nontail]
;;
