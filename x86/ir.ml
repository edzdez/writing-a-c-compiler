open! Core

module Unary_op = struct
  type t =
    | Neg
    | Not
  [@@deriving sexp_of]
end

module Binary_op = struct
  type t =
    | Add
    | Sub
    | Mult
  [@@deriving sexp_of]
end

module Operand = struct
  type t =
    | Imm of int
    | Reg of [ `Ax | `Dx | `R10 | `R11 ]
    | Pseudo of string
    | Stack of int
  [@@deriving sexp_of]
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

module Fun_def = struct
  type t =
    { name : string
    ; instrs : Instr.t Vec.t
    }
  [@@deriving sexp_of]
end

module Prog = struct
  type t = Fun_def.t [@@deriving sexp_of]
end
