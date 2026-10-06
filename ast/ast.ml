open! Core

module Expr = struct
  module Node = struct
    type t = Const of int [@@deriving sexp_of]
  end

  type t = Node.t Util.With_location.t [@@deriving sexp_of]
end

module Stmt = struct
  module Node = struct
    type t = Return of Expr.t [@@deriving sexp_of]
  end

  type t = Node.t Util.With_location.t [@@deriving sexp_of]
end

module Fun_def = struct
  module Node = struct
    type t =
      { name : string
      ; body : Stmt.t
      }
    [@@deriving sexp_of]
  end

  type t = Node.t Util.With_location.t [@@deriving sexp_of]
end

module Prog = struct
  module Node = struct
    type t = Fun_def.t [@@deriving sexp_of]
  end

  type t = Node.t Util.With_location.t [@@deriving sexp_of]
end
