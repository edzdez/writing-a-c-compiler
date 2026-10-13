open! Core
module Emit_pass = Emit
module Fixup_pass = Fixup
module Select_pass = Select
module Stack_layout_pass = Stack_layout
module Ir = Ir

module Program = struct
  type selected
  type stack_allocated
  type legal
  type 'state t = Ir.Prog.t Semantics.Compilation_unit.t

  let ir = Semantics.Compilation_unit.program
  let symbols = Semantics.Compilation_unit.symbols
end

module Select = struct
  let program tacky : Program.selected Program.t =
    Semantics.Compilation_unit.map_program tacky ~f:Select_pass.program
  ;;
end

module Stack_layout = struct
  let program (program : Program.selected Program.t) : Program.stack_allocated Program.t =
    let symbols = Program.symbols program in
    Semantics.Compilation_unit.map_program program ~f:(stack_ fun program ->
      Stack_layout_pass.program program ~symbols)
    [@nontail]
  ;;
end

module Fixup = struct
  let program (program : Program.stack_allocated Program.t) : Program.legal Program.t =
    Semantics.Compilation_unit.map_program program ~f:Fixup_pass.program
  ;;
end

module Lower = struct
  let program tacky = tacky |> Select.program |> Stack_layout.program |> Fixup.program
end

module Emit = struct
  let program (program : Program.legal Program.t) ~writer =
    Emit_pass.program (Program.ir program) ~symbol_table:(Program.symbols program) ~writer
  ;;
end
