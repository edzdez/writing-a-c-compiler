open! Core

module Unary_op : sig
  type t = node Source.Located.t

  and node =
    | Complement
    | Negate
    | Not
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
    | Bit_and
    | Bit_or
    | Xor
    | L_shift
    | R_shift
    | And
    | Or
    | Eq
    | Neq
    | Lt
    | Gt
    | Lte
    | Gte
  [@@deriving sexp_of]
end

module Expr : sig
  type t = node Source.Located.t

  and node =
    | Const of int
    | Var of string
    | Unary of
        { op : Unary_op.t
        ; expr : t
        }
    | Binary of
        { op : Binary_op.t
        ; left : t
        ; right : t
        }
    | Assign of
        { left : t
        ; right : t
        }
    | Compound_assign of
        { op : Binary_op.t
        ; left : t
        ; right : t
        }
  [@@deriving sexp_of]
end

module Decl : sig
  type t = node Source.Located.t

  and node =
    { name : string
    ; init : Expr.t or_null
    }
  [@@deriving sexp_of]
end

module Stmt : sig
  type t = node Source.Located.t

  and node =
    | Return of Expr.t
    | Expr of Expr.t
    | Null
  [@@deriving sexp_of]
end

module Block_item : sig
  type t = node Source.Located.t

  and node =
    | Decl of Decl.t
    | Stmt of Stmt.t
  [@@deriving sexp_of]
end

module Fun_def : sig
  type t = node Source.Located.t

  and node =
    { name : string
    ; body : Block_item.t Vec.t
    }
  [@@deriving sexp_of]
end

module Prog : sig
  type t = node Source.Located.t
  and node = Fun_def.t [@@deriving sexp_of]
end
