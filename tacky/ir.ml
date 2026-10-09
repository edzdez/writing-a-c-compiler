open! Core

module Unary_op = struct
  type t =
    | Complement
    | Negate
    | Not
  [@@deriving sexp_of]
end

module Binary_op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Bit_and
    | Bit_or
    | Xor
    | L_shift
    | R_shift
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte
  [@@deriving sexp_of]
end

module Val = struct
  type t =
    | Constant of int
    | Var of string
  [@@deriving sexp_of]
end

module Instr = struct
  type t =
    | Unary of
        { op : Unary_op.t
        ; src : Val.t
        ; dst : Val.t
        }
    | Binary of
        { op : Binary_op.t
        ; left : Val.t
        ; right : Val.t
        ; dst : Val.t
        }
    | Copy of
        { src : Val.t
        ; dst : Val.t
        }
    | Jmp of string
    | Jz of
        { tst : Val.t
        ; tgt : string
        }
    | Jnz of
        { tst : Val.t
        ; tgt : string
        }
    | Label of string
    | Return of Val.t
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
  type t = Fun_def.t [@@deriving sexp_of]
end
