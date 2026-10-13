open! Core

module Unary_op : sig
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
    | Conditional of
        { cond : t
        ; then_ : t
        ; else_ : t
        }
    | Function_call of
        { name : string
        ; args : t iarray
        }
  [@@deriving sexp_of]

  val describe : t -> string
end

module Storage_class : sig
  type t =
    | Static
    | Extern
  [@@deriving equal, sexp_of]
end

module Var_decl : sig
  type t = node Source.Located.t

  and node =
    { name : string
    ; init : Expr.t or_null
    ; type_ : Semantics.C_type.t
    ; storage : Storage_class.t or_null
    }
  [@@deriving sexp_of]
end

module For_init : sig
  type t =
    | Decl of Var_decl.t
    | Expr of Expr.t or_null
  [@@deriving sexp_of]
end

module Control_id : sig
  type t [@@deriving sexp_of]

  val fresh_loop : Common.Fresh_id.t -> t
  val fresh_switch : Common.Fresh_id.t -> t
  val to_string : t -> string
end

module Switch_cases : sig
  type t [@@deriving sexp_of]

  val empty : t
  val add_case : t -> int -> [ `Added of t | `Duplicate ]
  val add_default : t -> [ `Added of t | `Duplicate ]
  val iter : t -> f:(int -> unit) @ local -> unit
  val has_default : t -> bool
end

module rec Fun_decl : sig
  type t = node Source.Located.t

  and node =
    { name : string
    ; params : string iarray
    ; body : Block.t or_null
    ; ret_type : Semantics.C_type.t
    ; storage : Storage_class.t or_null
    }
  [@@deriving sexp_of]
end

and Decl : sig
  type t = node Source.Located.t

  and node =
    | Fun_decl of Fun_decl.t
    | Var_decl of Var_decl.t
  [@@deriving sexp_of]
end

and Stmt : sig
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
        ; body : t
        }
    | Goto of string
    | Compound of Block.t
    | Break of Control_id.t or_null
    | Continue of Control_id.t or_null
    | While of
        { cond : Expr.t
        ; body : t
        ; id : Control_id.t or_null
        }
    | Do_while of
        { body : t
        ; cond : Expr.t
        ; id : Control_id.t or_null
        }
    | For of
        { init : For_init.t
        ; cond : Expr.t or_null
        ; post : Expr.t or_null
        ; body : t
        ; id : Control_id.t or_null
        }
    | Case of
        { value : Expr.t
        ; body : t
        ; id : Control_id.t or_null
        }
    | Default of
        { body : t
        ; id : Control_id.t or_null
        }
    | Switch of
        { cases : Switch_cases.t
        ; cond : Expr.t
        ; body : t
        ; id : Control_id.t or_null
        }
    | Null
  [@@deriving sexp_of]
end

and Block : sig
  type t = node Source.Located.t
  and node = Block_item.t iarray [@@deriving sexp_of]
end

and Block_item : sig
  type t = node Source.Located.t

  and node =
    | Decl of Decl.t
    | Stmt of Stmt.t
  [@@deriving sexp_of]
end

module Prog : sig
  type t = node Source.Located.t
  and node = Decl.t iarray [@@deriving sexp_of]
end
