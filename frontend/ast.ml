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
    | Conditional of
        { cond : t
        ; then_ : t
        ; else_ : t
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
    | Conditional _ -> "a conditional expression"
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

module For_init = struct
  type t = node Source.Located.t

  and node =
    | Decl of Decl.t
    | Expr of Expr.t or_null
  [@@deriving sexp_of]
end

module rec Stmt : sig
  type t = node Source.Located.t

  and node =
    | Return of Expr.t
    | Expr of Expr.t
    | If of
        { cond : Expr.t
        ; then_ : t
        ; else_ : t or_null
        }
    | Label of
        { lbl : string
        ; stmt : t
        }
    | Goto of string
    | Compound of Block.t
    | Break of string
    | Continue of string
    | While of
        { cond : Expr.t
        ; body : t
        ; id : string
        }
    | Do_while of
        { body : t
        ; cond : Expr.t
        ; id : string
        }
    | For of
        { init : For_init.t
        ; cond : Expr.t or_null
        ; post : Expr.t or_null
        ; body : t
        ; id : string
        }
    | Null
  [@@deriving sexp_of]
end = struct
  type t = node Source.Located.t

  and node =
    | Return of Expr.t
    | Expr of Expr.t
    | If of
        { cond : Expr.t
        ; then_ : t
        ; else_ : t or_null
        }
    | Label of
        { lbl : string
        ; stmt : t
        }
    | Goto of string
    | Compound of Block.t
    | Break of string
    | Continue of string
    | While of
        { cond : Expr.t
        ; body : t
        ; id : string
        }
    | Do_while of
        { body : t
        ; cond : Expr.t
        ; id : string
        }
    | For of
        { init : For_init.t
        ; cond : Expr.t or_null
        ; post : Expr.t or_null
        ; body : t
        ; id : string
        }
    | Null
  [@@deriving sexp_of]
end

and Block_item : sig
  type t = node Source.Located.t

  and node =
    | Decl of Decl.t
    | Stmt of Stmt.t
  [@@deriving sexp_of]
end = struct
  type t = node Source.Located.t

  and node =
    | Decl of Decl.t
    | Stmt of Stmt.t
  [@@deriving sexp_of]
end

and Block : sig
  type t = node Source.Located.t
  and node = Block_item.t Vec.t [@@deriving sexp_of]
end = struct
  type t = node Source.Located.t
  and node = Block_item.t Vec.t [@@deriving sexp_of]
end

module Fun_def = struct
  type t = node Source.Located.t

  and node =
    { name : string
    ; body : Block.t
    }
  [@@deriving sexp_of]
end

module Prog = struct
  type t = node Source.Located.t
  and node = Fun_def.t [@@deriving sexp_of]
end
