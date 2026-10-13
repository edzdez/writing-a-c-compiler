open! Core
module Ast = Frontend.Ast
module Located = Source.Located

module Builder = struct
  type t =
    { temporary_ids : Common.Fresh_id.t
    ; label_ids : Common.Fresh_id.t
    ; instructions : Ir.Instr.t Vec.t
    }

  let create () =
    { temporary_ids = Common.Fresh_id.create ()
    ; label_ids = Common.Fresh_id.create ()
    ; instructions = Vec.create ()
    }
  ;;

  let emit t instruction = Vec.push_back t.instructions instruction

  let fresh_temporary t =
    Ir.Val.Var [%string "tmp.%{Common.Fresh_id.fresh t.temporary_ids#Int}"]
  ;;

  let fresh_branch_label t ~branch_prefix ~end_prefix =
    let id = Common.Fresh_id.fresh t.label_ids in
    #( ~branch:[%string "%{branch_prefix}.%{id#Int}"]
     , ~end_:[%string "%{end_prefix}.%{id#Int}"] )
  ;;

  let loop_start_label id = [%string "start.%{Ast.Control_id.to_string id}"]
  let break_label id = [%string "break.%{Ast.Control_id.to_string id}"]
  let continue_label id = [%string "continue.%{Ast.Control_id.to_string id}"]
  let default_label id = [%string "case.default.%{Ast.Control_id.to_string id}"]
  let case_label id value = [%string "case.%{value#Int}.%{Ast.Control_id.to_string id}"]
  let finish t = Vec.to_iarray t.instructions
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

let rec lower_expr expr ~builder =
  match Located.value expr with
  | Ast.Expr.Const constant -> Ir.Val.Constant constant
  | Var v -> Ir.Val.Var v
  | Unary { op; expr } ->
    (match Located.value op with
     | Pre_incr | Pre_decr ->
       let expr = lower_lvalue expr in
       let dst = Builder.fresh_temporary builder in
       Builder.emit builder (incr_instr_for op expr dst);
       Builder.emit builder (Ir.Instr.Copy { src = dst; dst = expr });
       dst
     | Post_incr | Post_decr ->
       let expr = lower_lvalue expr in
       let old = Builder.fresh_temporary builder in
       let dst = Builder.fresh_temporary builder in
       Builder.emit builder (Ir.Instr.Copy { src = expr; dst = old });
       Builder.emit builder (incr_instr_for op expr dst);
       Builder.emit builder (Ir.Instr.Copy { src = dst; dst = expr });
       old
     | _ ->
       let src = lower_expr expr ~builder in
       let dst = Builder.fresh_temporary builder in
       let op = lower_unary_op op in
       Builder.emit builder (Ir.Instr.Unary { op; src; dst });
       dst)
  | Binary { op; left; right } ->
    (match Located.value op with
     | And | Or ->
       let tst1 = lower_expr left ~builder in
       let branch_prefix =
         match_and_or
           op
           ~and_:(stack_ fun () -> "and_false")
           ~or_:(stack_ fun () -> "or_true")
       in
       let end_prefix =
         match_and_or
           op
           ~and_:(stack_ fun () -> "and_end")
           ~or_:(stack_ fun () -> "or_end")
       in
       let #(~branch:branch_lbl, ~end_:end_lbl) =
         Builder.fresh_branch_label builder ~branch_prefix ~end_prefix
       in
       let res = match_and_or op ~and_:(stack_ fun () -> 1) ~or_:(stack_ fun () -> 0) in
       Builder.emit builder (jmp_instr_for op tst1 branch_lbl);
       let tst2 = lower_expr right ~builder in
       Builder.emit builder (jmp_instr_for op tst2 branch_lbl);
       let dst = Builder.fresh_temporary builder in
       Builder.emit builder (Ir.Instr.Copy { src = Constant res; dst });
       Builder.emit builder (Ir.Instr.Jmp end_lbl);
       Builder.emit builder (Ir.Instr.Label branch_lbl);
       Builder.emit builder (Ir.Instr.Copy { src = Constant (1 - res); dst });
       Builder.emit builder (Ir.Instr.Label end_lbl);
       dst
     | _ ->
       let left = lower_expr left ~builder in
       let right = lower_expr right ~builder in
       let dst = Builder.fresh_temporary builder in
       let op = lower_binary_op op in
       Builder.emit builder (Ir.Instr.Binary { op; left; right; dst });
       dst)
  | Assign { left; right } ->
    let src = lower_expr right ~builder in
    let dst = lower_lvalue left in
    Builder.emit builder (Ir.Instr.Copy { src; dst });
    dst
  | Compound_assign { op; left; right } ->
    let dst = lower_lvalue left in
    let right = lower_expr right ~builder in
    let result = Builder.fresh_temporary builder in
    let op = lower_binary_op op in
    Builder.emit builder (Ir.Instr.Binary { op; left = dst; right; dst = result });
    Builder.emit builder (Ir.Instr.Copy { src = result; dst });
    result
  | Conditional { cond; then_; else_ } ->
    let tst = lower_expr cond ~builder in
    let dst = Builder.fresh_temporary builder in
    let #(~branch:else_lbl, ~end_:end_lbl) =
      Builder.fresh_branch_label builder ~branch_prefix:"cond_else" ~end_prefix:"cond_end"
    in
    Builder.emit builder (Ir.Instr.Jz { tst; tgt = else_lbl });
    let then_result = lower_expr then_ ~builder in
    Builder.emit builder (Ir.Instr.Copy { dst; src = then_result });
    Builder.emit builder (Ir.Instr.Jmp end_lbl);
    Builder.emit builder (Ir.Instr.Label else_lbl);
    let else_result = lower_expr else_ ~builder in
    Builder.emit builder (Ir.Instr.Copy { dst; src = else_result });
    Builder.emit builder (Ir.Instr.Label end_lbl);
    dst
  | Function_call { name; args } ->
    let args = Iarray.map args ~f:(stack_ fun arg -> lower_expr arg ~builder) in
    let dst = Builder.fresh_temporary builder in
    Builder.emit builder (Ir.Instr.Fun_call { name; args; dst });
    dst
;;

let lower_var_decl decl ~builder =
  let { Ast.Var_decl.name; init } = Located.value decl in
  match init with
  | Null -> ()
  | This expr ->
    let src = lower_expr expr ~builder in
    let dst = Ir.Val.Var name in
    Builder.emit builder (Ir.Instr.Copy { src; dst })
;;

let lower_for_init for_init ~builder =
  match for_init with
  | Ast.For_init.Decl decl -> lower_var_decl decl ~builder
  | Expr expr ->
    Or_null.iter expr ~f:(stack_ fun expr -> ignore (lower_expr expr ~builder)) [@nontail]
;;

let resolved_id node id =
  match id with
  | This id -> id
  | Null -> raise_s [%message "unresolved control-flow ID" (node : Ast.Stmt.t)]
;;

let rec lower_stmt stmt ~builder =
  match Located.value stmt with
  | Ast.Stmt.Return expr ->
    let val_ = lower_expr expr ~builder in
    Builder.emit builder (Ir.Instr.Return val_)
  | Expr expr -> ignore (lower_expr expr ~builder)
  | If { cond; then_; else_ } ->
    let tst = lower_expr cond ~builder in
    let #(~branch:else_lbl, ~end_:end_lbl) =
      Builder.fresh_branch_label builder ~branch_prefix:"if_else" ~end_prefix:"if_end"
    in
    Builder.emit
      builder
      (Ir.Instr.Jz
         { tst; tgt = Or_null.value_map else_ ~default:end_lbl ~f:(const else_lbl) });
    lower_stmt then_ ~builder;
    Or_null.iter else_ ~f:(stack_ fun else_ ->
      Builder.emit builder (Ir.Instr.Jmp end_lbl);
      Builder.emit builder (Ir.Instr.Label else_lbl);
      lower_stmt else_ ~builder);
    Builder.emit builder (Ir.Instr.Label end_lbl)
  | Label { lbl; body } ->
    Builder.emit builder (Ir.Instr.Label lbl);
    lower_stmt body ~builder
  | Compound block -> lower_block block ~builder
  | Goto lbl -> Builder.emit builder (Ir.Instr.Jmp lbl)
  | Break id ->
    let id = resolved_id stmt id in
    Builder.emit builder (Ir.Instr.Jmp (Builder.break_label id))
  | Continue id ->
    let id = resolved_id stmt id in
    Builder.emit builder (Ir.Instr.Jmp (Builder.continue_label id))
  | While { cond; body; id } ->
    let id = resolved_id stmt id in
    Builder.emit builder (Ir.Instr.Label (Builder.continue_label id));
    let tst = lower_expr cond ~builder in
    Builder.emit builder (Ir.Instr.Jz { tst; tgt = Builder.break_label id });
    lower_stmt body ~builder;
    Builder.emit builder (Ir.Instr.Jmp (Builder.continue_label id));
    Builder.emit builder (Ir.Instr.Label (Builder.break_label id))
  | Do_while { body; cond; id } ->
    let id = resolved_id stmt id in
    Builder.emit builder (Ir.Instr.Label (Builder.loop_start_label id));
    lower_stmt body ~builder;
    Builder.emit builder (Ir.Instr.Label (Builder.continue_label id));
    let tst = lower_expr cond ~builder in
    Builder.emit builder (Ir.Instr.Jnz { tst; tgt = Builder.loop_start_label id });
    Builder.emit builder (Ir.Instr.Label (Builder.break_label id))
  | For { init; cond; post; body; id } ->
    let id = resolved_id stmt id in
    lower_for_init init ~builder;
    Builder.emit builder (Ir.Instr.Label (Builder.loop_start_label id));
    Or_null.iter cond ~f:(stack_ fun cond ->
      let tst = lower_expr cond ~builder in
      Builder.emit builder (Ir.Instr.Jz { tst; tgt = Builder.break_label id }));
    lower_stmt body ~builder;
    Builder.emit builder (Ir.Instr.Label (Builder.continue_label id));
    Or_null.iter post ~f:(stack_ fun post -> ignore (lower_expr post ~builder));
    Builder.emit builder (Ir.Instr.Jmp (Builder.loop_start_label id));
    Builder.emit builder (Ir.Instr.Label (Builder.break_label id))
  | Case { value; body; id } ->
    let id = Or_null.value_exn id in
    let value =
      match Located.value value with
      | Ast.Expr.Const value -> value
      | _ -> raise_s [%message "non constant expression case" (value : Ast.Expr.t)]
    in
    Builder.emit builder (Ir.Instr.Label (Builder.case_label id value));
    lower_stmt body ~builder
  | Default { body; id } ->
    let id = Or_null.value_exn id in
    Builder.emit builder (Ir.Instr.Label (Builder.default_label id));
    lower_stmt body ~builder
  | Switch { cases; cond; body; id } ->
    let id = Or_null.value_exn id in
    let value = lower_expr cond ~builder in
    let tst = Builder.fresh_temporary builder in
    Ast.Switch_cases.iter cases ~f:(stack_ fun case ->
      Builder.emit
        builder
        (Ir.Instr.Binary { op = Eq; left = value; right = Constant case; dst = tst });
      Builder.emit builder (Ir.Instr.Jnz { tst; tgt = Builder.case_label id case }));
    if Ast.Switch_cases.has_default cases
    then Builder.emit builder (Ir.Instr.Jmp (Builder.default_label id))
    else Builder.emit builder (Ir.Instr.Jmp (Builder.break_label id));
    lower_stmt body ~builder;
    Builder.emit builder (Ir.Instr.Label (Builder.break_label id))
  | Null -> ()

and lower_block_item block_item ~builder =
  match Located.value block_item with
  | Ast.Block_item.Decl decl ->
    (match Located.value decl with
     | Ast.Decl.Var_decl var_decl -> lower_var_decl var_decl ~builder
     | Fun_decl _ -> ())
  | Stmt stmt -> lower_stmt stmt ~builder

and lower_block block ~builder =
  let block = Located.value block in
  Iarray.iter block ~f:(stack_ fun block_item -> lower_block_item block_item ~builder)
  [@nontail]

and lower_fun_decl fun_decl ~builder =
  let { Ast.Fun_decl.name; params; body } = Located.value fun_decl in
  Or_null.map_to_option body ~f:(stack_ fun body ->
    lower_block body ~builder;
    Builder.emit builder (Ir.Instr.Return (Constant 0));
    { Ir.Fun_def.name; params; instrs = Builder.finish builder })
  [@nontail]
;;

let lower_decl decl ~builder =
  match Located.value decl with
  | Ast.Decl.Var_decl _var_decl -> failwith "todo"
  | Fun_decl fun_decl -> lower_fun_decl fun_decl ~builder
;;

let program prog =
  Iarray.filter_map (Located.value prog) ~f:(stack_ fun decl ->
    let builder = Builder.create () in
    lower_decl decl ~builder)
  [@nontail]
;;
