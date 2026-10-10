open! Core

module Unary_op : sig
  type t =
    | Complement
    | Negate
    | Not
  [@@deriving sexp_of]
end

module Binary_op : sig
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

module Val : sig
  type t =
    | Constant of int
    | Var of string
  [@@deriving sexp_of]
end

module Instr : sig
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
    | Fun_call of
        { name : string
        ; args : Val.t iarray
        ; dst : Val.t
        }
    | Return of Val.t
  [@@deriving sexp_of]
end

module Fun_def : sig
  type t =
    { name : string
    ; params : string iarray
    ; instrs : Instr.t iarray
    }
  [@@deriving sexp_of]
end

module Prog : sig
  type t = Fun_def.t iarray [@@deriving sexp_of]
end
