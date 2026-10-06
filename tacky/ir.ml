open! Core

module Unary_op = struct
  type t =
    | Complement
    | Negate
  [@@deriving sexp_of]
end

module Binary_op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
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
    | Return of Val.t
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
