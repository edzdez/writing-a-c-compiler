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
    | And
    | Or
    | Xor
    | Shl
    | Sar
  [@@deriving sexp_of]
end

module Cond_code : sig
  type t =
    | E
    | Ne
    | G
    | Ge
    | L
    | Le
  [@@deriving sexp_of]
end

module Operand : sig
  module Size : sig
    type t =
      | Byte
      | Long
    [@@deriving sexp_of]
  end

  type t =
    | Imm of int
    | Reg of [ `Ax | `Cx | `Dx | `R10 | `R11 ]
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
    | Cmp of
        { left : Operand.t
        ; right : Operand.t
        }
    | Idiv of Operand.t
    | Cdq
    | Jmp of string
    | Jmp_cc of
        { cc : Cond_code.t
        ; tgt : string
        }
    | Set_cc of
        { cc : Cond_code.t
        ; dst : Operand.t
        }
    | Label of string
    | Alloc_stack of int
    | Ret
  [@@deriving sexp_of]
end

module Fun_def : sig
  type t =
    { name : string
    ; instrs : Instr.t iarray
    }
  [@@deriving sexp_of]
end

module Prog : sig
  type t = Fun_def.t [@@deriving sexp_of]
end
