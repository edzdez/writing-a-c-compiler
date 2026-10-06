open! Core

let unary_op = function
  | Ir.Unary_op.Neg -> "negl"
  | Not -> "notl"
;;

let operand = function
  | Ir.Operand.Imm imm -> [%string "$%{imm#Int}"]
  | Reg `Ax -> "%eax"
  | Reg `R10 -> "%r10d"
  | Stack offset -> [%string "%{-offset#Int}(%rbp)"]
  | Pseudo _ -> failwith "pseudo registers should not be emitted."
;;

let instr instr ~writer =
  match instr with
  | Ir.Instr.Mov { src; dst } ->
    Out_channel.output_line writer [%string "\tmovl\t%{operand src}, %{operand dst}"]
  | Unary { op; dst } ->
    Out_channel.output_line writer [%string "\t%{unary_op op}\t%{operand dst}"]
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
