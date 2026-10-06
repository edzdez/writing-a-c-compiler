open! Core

module Operand = struct
  type t =
    | Imm of int
    | Reg
  [@@deriving sexp_of]

  let pp = function
    | Imm imm -> [%string "$%{imm#Int}"]
    | Reg -> "%eax"
  ;;
end

module Instr = struct
  type t =
    | Mov of
        { src : Operand.t
        ; dst : Operand.t
        }
    | Ret
  [@@deriving sexp_of]

  let pp t =
    match t with
    | Mov { src; dst } -> [%string "movl\t%{Operand.pp src}, %{Operand.pp dst}"]
    | Ret -> "ret"
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
    Vec.iter instrs ~f:(stack_ fun instr ->
      Out_channel.output_line writer [%string "\t%{Instr.pp instr}"])
    [@nontail]
  ;;
end

module Prog = struct
  type t = Fun_def.t [@@deriving sexp_of]

  let pp t ~writer =
    Fun_def.pp t ~writer;
    Out_channel.output_line writer {|.section .note.GNU-stack,"",@progbits|}
  ;;
end
