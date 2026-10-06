open! Core

module Unary_op : sig
  type t = node Source.Located.t

  and node =
    | Complement
    | Negate
  [@@deriving sexp_of]
end

module Binary_op : sig
  type t = node Source.Located.t

  and node =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
  [@@deriving sexp_of]
end

module Expr : sig
  type t = node Source.Located.t

  and node =
    | Const of int
    | Unary of
        { op : Unary_op.t
        ; expr : t
        }
    | Binary of
        { op : Binary_op.t
        ; left : t
        ; right : t
        }
  [@@deriving sexp_of]
end

module Stmt : sig
  type t = node Source.Located.t
  and node = Return of Expr.t [@@deriving sexp_of]
end

module Fun_def : sig
  type t = node Source.Located.t

  and node =
    { name : string
    ; body : Stmt.t
    }
  [@@deriving sexp_of]
end

module Prog : sig
  type t = node Source.Located.t
  and node = Fun_def.t [@@deriving sexp_of]
end
