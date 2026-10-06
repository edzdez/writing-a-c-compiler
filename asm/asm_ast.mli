open! Core

module Unary_op : sig
  type t =
    | Neg
    | Not
  [@@deriving sexp_of]

  val pp : t -> string
end

module Operand : sig
  type t =
    | Imm of int
    | Reg of [ `Ax | `R10 ]
    | Pseudo of string
    | Stack of int
  [@@deriving sexp_of]

  val pp : t -> string
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
    | Alloc_stack of int
    | Ret
  [@@deriving sexp_of]

  val pp : t -> writer:Out_channel.t -> unit
end

module Fun_def : sig
  type t =
    { name : string
    ; instrs : Instr.t Vec.t
    }
  [@@deriving sexp_of]

  val pp : t -> writer:Out_channel.t -> unit
end

module Prog : sig
  type t = Fun_def.t [@@deriving sexp_of]

  val pp : t -> writer:Out_channel.t -> unit
end
