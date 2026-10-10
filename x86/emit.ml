open! Core
module Symbol = Semantics.Symbol
module Symbol_table = Semantics.Symbol_table

let unary_op = function
  | Ir.Unary_op.Neg -> "negl"
  | Not -> "notl"
;;

let binary_op = function
  | Ir.Binary_op.Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"
  | And -> "andl"
  | Or -> "orl "
  | Xor -> "xorl"
  | Shl -> "shll"
  | Sar -> "sarl"
;;

let cond_code (cc : Ir.Cond_code.t) =
  match cc with
  | E -> "e"
  | Ne -> "ne"
  | G -> "g"
  | Ge -> "ge"
  | L -> "l"
  | Le -> "le"
;;

let operand ?(size = Ir.Operand.Size.Long) operand =
  match operand, size with
  | Ir.Operand.Imm imm, _ -> [%string "$%{imm#Int}"]
  | Reg `Ax, Quad -> "%rax"
  | Reg `Ax, Long -> "%eax"
  | Reg `Ax, Byte -> "%al"
  | Reg `Cx, Quad -> "%rcx"
  | Reg `Cx, Long -> "%ecx"
  | Reg `Cx, Byte -> "%cl"
  | Reg `Dx, Quad -> "%rdx"
  | Reg `Dx, Long -> "%edx"
  | Reg `Dx, Byte -> "%dl"
  | Reg `Di, Quad -> "%rdi"
  | Reg `Di, Long -> "%edi"
  | Reg `Di, Byte -> "%dil"
  | Reg `Si, Quad -> "%rsi"
  | Reg `Si, Long -> "%esi"
  | Reg `Si, Byte -> "%sil"
  | Reg `R8, Quad -> "%r8"
  | Reg `R8, Long -> "%r8d"
  | Reg `R8, Byte -> "%r8b"
  | Reg `R9, Quad -> "%r9"
  | Reg `R9, Long -> "%r9d"
  | Reg `R9, Byte -> "%r9b"
  | Reg `R10, Quad -> "%r10"
  | Reg `R10, Long -> "%r10d"
  | Reg `R10, Byte -> "%r10b"
  | Reg `R11, Quad -> "%r11"
  | Reg `R11, Long -> "%r11d"
  | Reg `R11, Byte -> "%r11b"
  | Stack offset, _ -> [%string "%{-offset#Int}(%rbp)"]
  | Pseudo _, _ -> failwith "pseudo registers should not be emitted."
;;

let call_target name ~symbol_table =
  match Symbol_table.find symbol_table name with
  | This { Symbol.attrs = Fun { defined = true }; _ } -> name
  | This { attrs = Fun { defined = false }; _ } -> [%string "%{name}@PLT"]
  | This symbol -> raise_s [%message "not a function" (name : string) (symbol : Symbol.t)]
  | Null -> raise_s [%message "unknown function" (name : string)]
;;

let local_label ~function_name label = [%string ".L%{function_name}.%{label}"]

let instr instr ~function_name ~symbol_table ~writer =
  match instr with
  | Ir.Instr.Mov { src; dst } ->
    Out_channel.output_line writer [%string "\tmovl\t%{operand src}, %{operand dst}"]
  | Unary { op; dst } ->
    Out_channel.output_line writer [%string "\t%{unary_op op}\t%{operand dst}"]
  | Binary { op = (Shl | Sar) as op; src; dst } ->
    Out_channel.output_line
      writer
      [%string "\t%{binary_op op}\t%{operand src ~size:Byte}, %{operand dst}"]
  | Binary { op; src; dst } ->
    Out_channel.output_line
      writer
      [%string "\t%{binary_op op}\t%{operand src}, %{operand dst}"]
  | Cmp { left; right } ->
    Out_channel.output_line writer [%string "\tcmpl\t%{operand left}, %{operand right}"]
  | Idiv dst -> Out_channel.output_line writer [%string "\tidivl\t%{operand dst}"]
  | Cdq -> Out_channel.output_line writer [%string "\tcdq"]
  | Jmp tgt ->
    Out_channel.output_line writer [%string "\tjmp \t%{local_label ~function_name tgt}"]
  | Jmp_cc { cc; tgt } ->
    Out_channel.output_line
      writer
      [%string "\tj%{cond_code cc}  \t%{local_label ~function_name tgt}"]
  | Set_cc { cc; dst } ->
    Out_channel.output_line writer [%string "\tset%{cond_code cc}\t%{operand dst}"]
  | Label lbl ->
    Out_channel.output_line writer [%string "%{local_label ~function_name lbl}:"]
  | Alloc_stack bytes ->
    Out_channel.output_line writer [%string "\tsubq\t$%{bytes#Int}, %rsp"]
  | Dealloc_stack bytes ->
    Out_channel.output_line writer [%string "\taddq\t$%{bytes#Int}, %rsp"]
  | Push op ->
    Out_channel.output_line writer [%string "\tpushq\t%{operand op ~size:Quad}"]
  | Call name ->
    let target = call_target name ~symbol_table in
    Out_channel.output_line writer [%string "\tcall\t%{target}"]
  | Ret ->
    Out_channel.output_line writer "\tmovq\t%rbp, %rsp";
    Out_channel.output_line writer "\tpopq\t%rbp";
    Out_channel.output_line writer "\tret"
;;

let fun_def { Ir.Fun_def.name; instrs } ~symbol_table ~writer =
  Out_channel.output_line writer [%string "\t.globl %{name}"];
  Out_channel.output_line writer [%string "%{name}:"];
  Out_channel.output_line writer "\tpushq\t%rbp";
  Out_channel.output_line writer "\tmovq\t%rsp, %rbp";
  Iarray.iter instrs ~f:(instr ~function_name:name ~symbol_table ~writer)
;;

let program program ~symbol_table ~writer =
  Iarray.iter program ~f:(stack_ fun f -> fun_def f ~symbol_table ~writer);
  Out_channel.output_line writer {|.section .note.GNU-stack,"",@progbits|}
;;
