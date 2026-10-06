open! Core

module Unary_op : sig
  type t =
    | Complement
    | Negate
  [@@deriving sexp_of]
end

module Binary_op : sig
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
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
    | Return of Val.t
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
