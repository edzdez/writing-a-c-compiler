open! Core

module Unary_op = struct
  type t =
    | Neg
    | Not
  [@@deriving sexp_of]

  let pp = function
    | Neg -> "negl"
    | Not -> "notl"
  ;;
end

module Operand = struct
  type t =
    | Imm of int
    | Reg of [ `Ax | `R10 ]
    | Pseudo of string
    | Stack of int
  [@@deriving sexp_of]

  let pp = function
    | Imm imm -> [%string "$%{imm#Int}"]
    | Reg `Ax -> "%eax"
    | Reg `R10 -> "%r10d"
    | Stack offset -> [%string "%{-offset#Int}(%rbp)"]
    | Pseudo _ -> failwith "pseudo registers should not be emitted."
  ;;
end

module Instr = struct
  type t =
    | Mov of
        { src : Operand.t
        ; dst : Operand.t
        }
    | Unary of
        { op : Unary_op.t
        ; dst : Operand.t
        }
    | Alloc_stack of int
    | Ret
  [@@deriving sexp_of]

  let pp t ~writer =
    match t with
    | Mov { src; dst } ->
      Out_channel.output_line
        writer
        [%string "\tmovl\t%{Operand.pp src}, %{Operand.pp dst}"]
    | Unary { op; dst } ->
      Out_channel.output_line writer [%string "\t%{Unary_op.pp op}\t%{Operand.pp dst}"]
    | Alloc_stack bytes ->
      Out_channel.output_line writer [%string "\tsubq\t$%{bytes#Int}, %rsp"]
    | Ret ->
      Out_channel.output_line writer "\tmovq\t%rbp, %rsp";
      Out_channel.output_line writer "\tpopq\t%rbp";
      Out_channel.output_line writer "\tret"
  ;;
end

module Fun_def = struct
  type t =
    { name : string
    ; instrs : Instr.t Vec.t
    }
  [@@deriving sexp_of]

  let pp { name; instrs } ~writer =
    Out_channel.output_line writer [%string "\t.globl %{name}"];
    Out_channel.output_line writer [%string "%{name}:"];
    Out_channel.output_line writer [%string "\tpushq\t%rbp"];
    Out_channel.output_line writer [%string "\tmovq\t%rsp, %rbp"];
    Vec.iter instrs ~f:(Instr.pp ~writer)
  ;;
end

module Prog = struct
  type t = Fun_def.t [@@deriving sexp_of]

  let pp t ~writer =
    Fun_def.pp t ~writer;
    Out_channel.output_line writer {|.section .note.GNU-stack,"",@progbits|}
  ;;
end
