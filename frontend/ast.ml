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
    | Function_call of
        { name : string
        ; args : t iarray
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
    | Function_call _ -> "a function call"
  ;;
end

module Control_id = struct
  type t =
    | Loop of int
    | Switch of int
  [@@deriving sexp_of]

  let fresh_loop fresh_ids = Loop (Common.Fresh_id.fresh fresh_ids)
  let fresh_switch fresh_ids = Switch (Common.Fresh_id.fresh fresh_ids)

  let to_string = function
    | Loop id -> [%string "loop.%{id#Int}"]
    | Switch id -> [%string "switch.%{id#Int}"]
  ;;
end

module Switch_cases = struct
  type t =
    { values : Int.Set.t
    ; has_default : bool
    }
  [@@deriving sexp_of]

  let empty = { values = Int.Set.empty; has_default = false }

  let add_case t value =
    match Set.mem t.values value with
    | true -> `Duplicate
    | false -> `Added { t with values = Set.add t.values value }
  ;;

  let add_default t =
    match t.has_default with
    | true -> `Duplicate
    | false -> `Added { t with has_default = true }
  ;;

  let iter t ~f = Set.iter t.values ~f
  let has_default t = t.has_default
end

module Var_decl = struct
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
    | Decl of Var_decl.t
    | Expr of Expr.t or_null
  [@@deriving sexp_of]
end

module rec Fun_decl : sig
  type t = node Source.Located.t

  and node =
    { name : string
    ; params : string iarray
    ; body : Block.t or_null
    }
  [@@deriving sexp_of]
end = struct
  type t = node Source.Located.t

  and node =
    { name : string
    ; params : string iarray
    ; body : Block.t or_null
    }
  [@@deriving sexp_of]
end

and Decl : sig
  type t = node Source.Located.t

  and node =
    | Fun_decl of Fun_decl.t
    | Var_decl of Var_decl.t
  [@@deriving sexp_of]
end = struct
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
  and node = Block_item.t iarray [@@deriving sexp_of]
end = struct
  type t = node Source.Located.t
  and node = Block_item.t iarray [@@deriving sexp_of]
end

module Prog = struct
  type t = node Source.Located.t
  and node = Fun_decl.t iarray [@@deriving sexp_of]
end
