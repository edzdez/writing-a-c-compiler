open! Core
module Ast = Frontend.Ast
module Located = Source.Located

module Temp = struct
  type t = { mutable next : int }

  let create () = { next = 0 }

  let fresh t =
    let x = t.next in
    t.next <- x + 1;
    x
  ;;
end

let lower_unary_op unary_op =
  match Located.value unary_op with
  | Ast.Unary_op.Complement -> Ir.Unary_op.Complement
  | Negate -> Negate
  | Not -> Not
;;

let lower_binary_op binary_op =
  match Located.value binary_op with
  | Ast.Binary_op.Add -> Ir.Binary_op.Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | Rem -> Rem
  | Bit_and -> Bit_and
  | Bit_or -> Bit_or
  | Xor -> Xor
  | L_shift -> L_shift
  | R_shift -> R_shift
  | Eq -> Eq
  | Neq -> Neq
  | Lt -> Lt
  | Gt -> Gt
  | Lte -> Lte
  | Gte -> Gte
  | And | Or -> raise_s [%message "nonstandard binary op" (binary_op : Ast.Binary_op.t)]
;;

let match_and_or op ~(and_ @ local) ~(or_ @ local) =
  match Located.value op with
  | Ast.Binary_op.And -> and_ ()
  | Or -> or_ ()
  | _ -> raise_s [%message "not [And]/[Or]" (op : Ast.Binary_op.t)]
;;

let jmp_instr_for op tst tgt =
  match_and_or
    op
    ~and_:(stack_ fun () -> Ir.Instr.Jz { tst; tgt })
    ~or_:(stack_ fun () -> Ir.Instr.Jnz { tst; tgt }) [@nontail]
;;

let rec lower_expr expr ~temp_vars ~temp_lbls ~instrs =
  match Located.value expr with
  | Ast.Expr.Const constant -> Ir.Val.Constant constant
  | Unary { op; expr } ->
    let src = lower_expr expr ~temp_vars ~temp_lbls ~instrs in
    let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
    let op = lower_unary_op op in
    Vec.push_back instrs (Ir.Instr.Unary { op; src; dst });
    dst
  | Binary { op; left; right } ->
    (match Located.value op with
     | And | Or ->
       let tst1 = lower_expr left ~temp_vars ~temp_lbls ~instrs in
       let fresh_lbl = Temp.fresh temp_vars in
       let branch_lbl =
         let branch_lbl_prefix =
           match_and_or
             op
             ~and_:(stack_ fun () -> "and_false")
             ~or_:(stack_ fun () -> "or_true")
         in
         [%string "%{branch_lbl_prefix}.%{fresh_lbl#Int}"]
       in
       let end_lbl =
         let end_lbl_prefix =
           match_and_or
             op
             ~and_:(stack_ fun () -> "and_end")
             ~or_:(stack_ fun () -> "or_end")
         in
         [%string "%{end_lbl_prefix}.%{fresh_lbl#Int}"]
       in
       let res = match_and_or op ~and_:(stack_ fun () -> 1) ~or_:(stack_ fun () -> 0) in
       Vec.push_back instrs (jmp_instr_for op tst1 branch_lbl);
       let tst2 = lower_expr right ~temp_vars ~temp_lbls ~instrs in
       Vec.push_back instrs (jmp_instr_for op tst2 branch_lbl);
       let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
       Vec.push_back instrs (Ir.Instr.Copy { src = Constant res; dst });
       Vec.push_back instrs (Ir.Instr.Jmp end_lbl);
       Vec.push_back instrs (Ir.Instr.Label branch_lbl);
       Vec.push_back instrs (Ir.Instr.Copy { src = Constant (1 - res); dst });
       Vec.push_back instrs (Ir.Instr.Label end_lbl);
       dst
     | _ ->
       let left = lower_expr left ~temp_vars ~temp_lbls ~instrs in
       let right = lower_expr right ~temp_vars ~temp_lbls ~instrs in
       let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
       let op = lower_binary_op op in
       Vec.push_back instrs (Ir.Instr.Binary { op; left; right; dst });
       dst)
;;

let lower_stmt stmt ~temp_vars ~temp_lbls ~instrs =
  match Located.value stmt with
  | Ast.Stmt.Return expr ->
    let val_ = lower_expr expr ~temp_vars ~temp_lbls ~instrs in
    Vec.push_back instrs (Ir.Instr.Return val_)
;;

let lower_fun_def fun_def ~temp_vars ~temp_lbls =
  let { Ast.Fun_def.name; body } = Located.value fun_def in
  let instrs = Vec.create () in
  lower_stmt body ~temp_vars ~temp_lbls ~instrs;
  { Ir.Fun_def.name; instrs }
;;

let program prog =
  let prog = Located.value prog in
  let temp_vars = Temp.create () in
  let temp_lbls = Temp.create () in
  lower_fun_def prog ~temp_vars ~temp_lbls
;;
