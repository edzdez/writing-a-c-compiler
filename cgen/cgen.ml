open! Core
module With_location = Util.With_location

let cgen_expr (expr : Ast.Expr.t) : Asm.Operand.t =
  match With_location.value expr with
  | Const constant -> Imm constant
;;

let cgen_stmt (stmt : Ast.Stmt.t) : Asm.Instr.t Vec.t =
  match With_location.value stmt with
  | Return expr ->
    let src = cgen_expr expr in
    Vec.of_array ([| Mov { src; dst = Reg }; Ret |] : Asm.Instr.t array)
;;

let cgen_fun_def (fun_def : Ast.Fun_def.t) : Asm.Fun_def.t =
  let { name; body } : Ast.Fun_def.Node.t = With_location.value fun_def in
  { name; instrs = cgen_stmt body }
;;

let cgen_prog (prog : Ast.Prog.t) : Asm.Prog.t =
  let prog = With_location.value prog in
  cgen_fun_def prog
;;
