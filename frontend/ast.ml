open! Core

module Unary_op = struct
  type t = node Source.Located.t

  and node =
    | Complement
    | Negate
    | Not
    | Pre_incr
    | Pre_decr
    | Post_incr
    | Post_decr
  [@@deriving sexp_of]
end

module Binary_op = struct
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

module Expr = struct
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

  let describe t =
    match Source.Located.value t with
    | Const _ -> "an integer constant"
    | Var _ -> "a variable"
    | Unary { op; _ } ->
      (match Source.Located.value op with
       | Complement | Negate | Not -> "a unary expression"
       | Pre_incr -> "a prefix increment expression"
       | Pre_decr -> "a prefix decrement expression"
       | Post_incr -> "a postfix increment expression"
       | Post_decr -> "a postfix decrement expression")
    | Binary _ -> "a binary expression"
    | Assign _ -> "an assignment expression"
    | Compound_assign _ -> "a compound assignment expression"
  ;;
end

module Decl = struct
  type t = node Source.Located.t

  and node =
    { name : string
    ; init : Expr.t or_null
    }
  [@@deriving sexp_of]
end

module Stmt = struct
  type t = node Source.Located.t

  and node =
    | Return of Expr.t
    | Expr of Expr.t
    | Null
  [@@deriving sexp_of]
end

module Block_item = struct
  type t = node Source.Located.t

  and node =
    | Decl of Decl.t
    | Stmt of Stmt.t
  [@@deriving sexp_of]
end

module Fun_def = struct
  type t = node Source.Located.t

  and node =
    { name : string
    ; body : Block_item.t Vec.t
    }
  [@@deriving sexp_of]
end

module Prog = struct
  type t = node Source.Located.t
  and node = Fun_def.t [@@deriving sexp_of]
end
