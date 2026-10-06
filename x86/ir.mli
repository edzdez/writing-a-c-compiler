(** The x86-64 IR shared by the backend passes.

    Pseudoregisters are valid before stack layout. Emission expects stack layout and
    instruction fixup to have completed. *)
open! Core

module Unary_op : sig
  type t =
    | Neg
    | Not
  [@@deriving sexp_of]
end

module Binary_op : sig
  type t =
    | Add
    | Sub
    | Mult
  [@@deriving sexp_of]
end

module Operand : sig
  type t =
    | Imm of int
    | Reg of [ `Ax | `Dx | `R10 | `R11 ]
    | Pseudo of string
    | Stack of int
  [@@deriving sexp_of]
end

module Instr : sig
  type t =
    | Mov of
        { src : Operand.t
        ; dst : Operand.t
        }
    | Unary of
        { op : Unary_op.t
        ; dst : Operand.t
        }
    | Binary of
        { op : Binary_op.t
        ; src : Operand.t
        ; dst : Operand.t
        }
    | Idiv of Operand.t
    | Cdq
    | Alloc_stack of int
    | Ret
  [@@deriving sexp_of]
end

module Fun_def : sig
  type t =
    { name : string
    ; instrs : Instr.t Vec.t
    }
  [@@deriving sexp_of]
end

module Prog : sig
  type t = Fun_def.t [@@deriving sexp_of]
end
