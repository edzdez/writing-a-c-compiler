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
    | And
    | Or
    | Xor
    | Shl
    | Sar
  [@@deriving sexp_of]
end

module Cond_code = struct
  type t =
    | E
    | Ne
    | G
    | Ge
    | L
    | Le
  [@@deriving sexp_of]
end

module Operand = struct
  module Size = struct
    type t =
      | Byte
      | Long
      | Quad
    [@@deriving sexp_of]
  end

  type t =
    | Imm of int
    | Reg of [ `Ax | `Cx | `Dx | `Di | `Si | `R8 | `R9 | `R10 | `R11 ]
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
    | Dealloc_stack of int
    | Push of Operand.t
    | Call of string
    | Ret
  [@@deriving sexp_of]
end

module Fun_def = struct
  type t =
    { name : string
    ; instrs : Instr.t iarray
    }
  [@@deriving sexp_of]
end

module Prog = struct
  type t = Fun_def.t iarray [@@deriving sexp_of]
end
