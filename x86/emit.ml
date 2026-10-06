open! Core

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
  | Reg `Ax, Long -> "%eax"
  | Reg `Ax, Byte -> "%al"
  | Reg `Cx, Long -> "%ecx"
  | Reg `Cx, Byte -> "%cl"
  | Reg `Dx, Long -> "%edx"
  | Reg `Dx, Byte -> "%dl"
  | Reg `R10, Long -> "%r10d"
  | Reg `R10, Byte -> "%r10b"
  | Reg `R11, Long -> "%r11d"
  | Reg `R11, Byte -> "%r11b"
  | Stack offset, _ -> [%string "%{-offset#Int}(%rbp)"]
  | Pseudo _, _ -> failwith "pseudo registers should not be emitted."
;;

let instr instr ~writer =
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
  | Jmp tgt -> Out_channel.output_line writer [%string "\tjmp \t.L%{tgt}"]
  | Jmp_cc { cc; tgt } ->
    Out_channel.output_line writer [%string "\tj%{cond_code cc} \t.L%{tgt}"]
  | Set_cc { cc; dst } ->
    Out_channel.output_line writer [%string "\tset%{cond_code cc}\t%{operand dst}"]
  | Label lbl -> Out_channel.output_line writer [%string ".L%{lbl}:"]
  | Alloc_stack bytes ->
    Out_channel.output_line writer [%string "\tsubq\t$%{bytes#Int}, %rsp"]
  | Ret ->
    Out_channel.output_line writer "\tmovq\t%rbp, %rsp";
    Out_channel.output_line writer "\tpopq\t%rbp";
    Out_channel.output_line writer "\tret"
;;

let fun_def { Ir.Fun_def.name; instrs } ~writer =
  Out_channel.output_line writer [%string "\t.globl %{name}"];
  Out_channel.output_line writer [%string "%{name}:"];
  Out_channel.output_line writer "\tpushq\t%rbp";
  Out_channel.output_line writer "\tmovq\t%rsp, %rbp";
  Vec.iter instrs ~f:(instr ~writer)
;;

let program program ~writer =
  fun_def program ~writer;
  Out_channel.output_line writer {|.section .note.GNU-stack,"",@progbits|}
;;
