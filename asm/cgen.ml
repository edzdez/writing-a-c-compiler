open! Core
module With_location = Util.With_location

let replace_pseudo_regs (instrs : Asm_ast.Instr.t Vec.t) =
  let mapping = String.Table.create () in
  let local_ find_or_add pseudo_reg =
    Hashtbl.find_or_add mapping pseudo_reg ~default:(fun () ->
      (Hashtbl.length mapping + 1) * 4)
  in
  let maybe_replace_reg (operand : Asm_ast.Operand.t) : Asm_ast.Operand.t =
    match operand with
    | Pseudo reg -> Stack (find_or_add reg)
    | _ -> operand
  in
  for i = 0 to Vec.length instrs - 1 do
    let new_instr : Asm_ast.Instr.t =
      match Vec.unsafe_get instrs i with
      | Mov { src; dst } ->
        Mov { src = maybe_replace_reg src; dst = maybe_replace_reg dst }
      | Unary { op; dst } -> Unary { op; dst = maybe_replace_reg dst }
      | instr -> instr
    in
    Vec.unsafe_set instrs i new_instr
  done;
  Hashtbl.length mapping * 4
;;

let fix_registers instrs =
  let new_instrs : Asm_ast.Instr.t Vec.t = Vec.create () in
  Vec.iter instrs ~f:(stack_ fun instr ->
    match (instr : Asm_ast.Instr.t) with
    | Mov { src = Stack _ as src; dst = Stack _ as dst } ->
      Vec.push_back new_instrs (Mov { src; dst = Reg `R10 });
      Vec.push_back new_instrs (Mov { src = Reg `R10; dst })
    | _ -> Vec.push_back new_instrs instr);
  new_instrs
;;

let cgen_unary_op (unary_op : Tacky.Unary_op.t) : Asm_ast.Unary_op.t =
  match unary_op with
  | Negate -> Neg
  | Complement -> Not
;;

let cgen_val (val_ : Tacky.Val.t) : Asm_ast.Operand.t =
  match val_ with
  | Constant constant -> Imm constant
  | Var var -> Pseudo var
;;

let cgen_instr (stmt : Tacky.Instr.t) ~(instrs : Asm_ast.Instr.t Vec.t) =
  match stmt with
  | Unary { op; src; dst } ->
    let src = cgen_val src in
    let dst = cgen_val dst in
    Vec.push_back instrs (Mov { src; dst });
    Vec.push_back instrs (Unary { op = cgen_unary_op op; dst })
  | Return val_ ->
    let val_ = cgen_val val_ in
    Vec.push_back instrs (Mov { src = val_; dst = Reg `Ax });
    Vec.push_back instrs Ret
;;

let cgen_fun_def ({ name; instrs = body } : Tacky.Fun_def.t) : Asm_ast.Fun_def.t =
  let instrs = Vec.of_array [| Asm_ast.Instr.Alloc_stack 0 |] in
  Vec.iter body ~f:(cgen_instr ~instrs);
  let offset = replace_pseudo_regs instrs in
  Vec.unsafe_set instrs 0 (Asm_ast.Instr.Alloc_stack offset);
  let instrs = fix_registers instrs in
  { name; instrs }
;;

let cgen_prog (prog : Tacky.Prog.t) : Asm_ast.Prog.t = cgen_fun_def prog
