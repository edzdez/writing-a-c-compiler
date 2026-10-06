open! Core

module Unary_op = struct
  type t = node Util.With_location.t

  and node =
    | Complement
    | Negate
  [@@deriving sexp_of]
end

module Expr = struct
  type t = node Util.With_location.t

  and node =
    | Const of int
    | Unary of
        { op : Unary_op.t
        ; expr : t
        }
  [@@deriving sexp_of]
end

module Stmt = struct
  type t = node Util.With_location.t
  and node = Return of Expr.t [@@deriving sexp_of]
end

module Fun_def = struct
  type t = node Util.With_location.t

  and node =
    { name : string
    ; body : Stmt.t
    }
  [@@deriving sexp_of]
end

module Prog = struct
  type t = node Util.With_location.t
  and node = Fun_def.t [@@deriving sexp_of]
end
