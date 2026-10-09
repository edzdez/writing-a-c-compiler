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
  | _ -> raise_s [%message "nonstandard unary op" (unary_op : Ast.Unary_op.t)]
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

let incr_instr_for op left dst =
  match Located.value op with
  | Ast.Unary_op.Pre_incr | Post_incr ->
    Ir.Instr.Binary { op = Add; left; right = Constant 1; dst }
  | Pre_decr | Post_decr -> Ir.Instr.Binary { op = Sub; left; right = Constant 1; dst }
  | _ -> raise_s [%message "not [--]/[++]"]
;;

let lower_lvalue expr =
  match Located.value expr with
  | Ast.Expr.Var v -> Ir.Val.Var v
  | expr -> raise_s [%message "invalid lvalue" (expr : Ast.Expr.node)]
;;

let rec lower_expr expr ~temp_vars ~temp_lbls ~instrs =
  match Located.value expr with
  | Ast.Expr.Const constant -> Ir.Val.Constant constant
  | Var v -> Ir.Val.Var v
  | Unary { op; expr } ->
    (match Located.value op with
     | Pre_incr | Pre_decr ->
       let expr = lower_lvalue expr in
       let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
       Vec.push_back instrs (incr_instr_for op expr dst);
       Vec.push_back instrs (Ir.Instr.Copy { src = dst; dst = expr });
       dst
     | Post_incr | Post_decr ->
       let expr = lower_lvalue expr in
       let old = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
       let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
       Vec.push_back instrs (Ir.Instr.Copy { src = expr; dst = old });
       Vec.push_back instrs (incr_instr_for op expr dst);
       Vec.push_back instrs (Ir.Instr.Copy { src = dst; dst = expr });
       old
     | _ ->
       let src = lower_expr expr ~temp_vars ~temp_lbls ~instrs in
       let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
       let op = lower_unary_op op in
       Vec.push_back instrs (Ir.Instr.Unary { op; src; dst });
       dst)
  | Binary { op; left; right } ->
    (match Located.value op with
     | And | Or ->
       let tst1 = lower_expr left ~temp_vars ~temp_lbls ~instrs in
       let fresh_lbl = Temp.fresh temp_lbls in
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
  | Assign { left; right } ->
    let src = lower_expr right ~temp_vars ~temp_lbls ~instrs in
    let dst = lower_lvalue left in
    Vec.push_back instrs (Ir.Instr.Copy { src; dst });
    dst
  | Compound_assign { op; left; right } ->
    let dst = lower_lvalue left in
    let right = lower_expr right ~temp_vars ~temp_lbls ~instrs in
    let result = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
    let op = lower_binary_op op in
    Vec.push_back instrs (Ir.Instr.Binary { op; left = dst; right; dst = result });
    Vec.push_back instrs (Ir.Instr.Copy { src = result; dst });
    result
  | Conditional { cond; then_; else_ } ->
    let tst = lower_expr cond ~temp_vars ~temp_lbls ~instrs in
    let dst = Ir.Val.Var [%string "tmp.%{Temp.fresh temp_vars#Int}"] in
    let fresh_lbl = Temp.fresh temp_lbls in
    let else_lbl = [%string "cond_else.%{fresh_lbl#Int}"] in
    let end_lbl = [%string "cond_end.%{fresh_lbl#Int}"] in
    Vec.push_back instrs (Ir.Instr.Jz { tst; tgt = else_lbl });
    let then_result = lower_expr then_ ~temp_vars ~temp_lbls ~instrs in
    Vec.push_back instrs (Ir.Instr.Copy { dst; src = then_result });
    Vec.push_back instrs (Ir.Instr.Jmp end_lbl);
    Vec.push_back instrs (Ir.Instr.Label else_lbl);
    let else_result = lower_expr else_ ~temp_vars ~temp_lbls ~instrs in
    Vec.push_back instrs (Ir.Instr.Copy { dst; src = else_result });
    Vec.push_back instrs (Ir.Instr.Label end_lbl);
    dst
;;

let lower_decl decl ~temp_vars ~temp_lbls ~instrs =
  let { Ast.Decl.name; init } = Located.value decl in
  match init with
  | Null -> ()
  | This expr ->
    let src = lower_expr expr ~temp_vars ~temp_lbls ~instrs in
    let dst = Ir.Val.Var name in
    Vec.push_back instrs (Ir.Instr.Copy { src; dst })
;;

let rec lower_stmt stmt ~temp_vars ~temp_lbls ~instrs =
  match Located.value stmt with
  | Ast.Stmt.Return expr ->
    let val_ = lower_expr expr ~temp_vars ~temp_lbls ~instrs in
    Vec.push_back instrs (Ir.Instr.Return val_)
  | Expr expr -> ignore (lower_expr expr ~temp_vars ~temp_lbls ~instrs)
  | If { cond; then_; else_ } ->
    let tst = lower_expr cond ~temp_vars ~temp_lbls ~instrs in
    let fresh_lbl = Temp.fresh temp_lbls in
    let else_lbl = [%string "if_else.%{fresh_lbl#Int}"] in
    let end_lbl = [%string "if_end.%{fresh_lbl#Int}"] in
    Vec.push_back
      instrs
      (Ir.Instr.Jz
         { tst; tgt = Or_null.value_map else_ ~default:end_lbl ~f:(const else_lbl) });
    lower_stmt then_ ~temp_vars ~temp_lbls ~instrs;
    Or_null.iter else_ ~f:(stack_ fun else_ ->
      Vec.push_back instrs (Ir.Instr.Jmp end_lbl);
      Vec.push_back instrs (Ir.Instr.Label else_lbl);
      lower_stmt else_ ~temp_vars ~temp_lbls ~instrs);
    Vec.push_back instrs (Ir.Instr.Label end_lbl)
  | Label { lbl; stmt } ->
    Vec.push_back instrs (Ir.Instr.Label lbl);
    lower_stmt stmt ~temp_vars ~temp_lbls ~instrs
  | Goto lbl -> Vec.push_back instrs (Ir.Instr.Jmp lbl)
  | Null -> ()
;;

let lower_block_item block_item ~temp_vars ~temp_lbls ~instrs =
  match Located.value block_item with
  | Ast.Block_item.Decl decl -> lower_decl decl ~temp_vars ~temp_lbls ~instrs
  | Stmt stmt -> lower_stmt stmt ~temp_vars ~temp_lbls ~instrs
;;

let lower_fun_def fun_def ~temp_vars ~temp_lbls =
  let { Ast.Fun_def.name; body } = Located.value fun_def in
  let instrs = Vec.create () in
  Vec.iter body ~f:(lower_block_item ~temp_vars ~temp_lbls ~instrs);
  Vec.push_back instrs (Ir.Instr.Return (Constant 0));
  { Ir.Fun_def.name; instrs }
;;

let program prog =
  let prog = Located.value prog in
  let temp_vars = Temp.create () in
  let temp_lbls = Temp.create () in
  lower_fun_def prog ~temp_vars ~temp_lbls
;;
