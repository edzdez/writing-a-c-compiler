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
  type 'state t = Ir.Prog.t

  let ir t = t
end

module Select = struct
  let program tacky : Program.selected Program.t = Select_pass.program tacky
end

module Stack_layout = struct
  let program (program : Program.selected Program.t) : Program.stack_allocated Program.t =
    Stack_layout_pass.program program
  ;;
end

module Fixup = struct
  let program (program : Program.stack_allocated Program.t) : Program.legal Program.t =
    Fixup_pass.program program
  ;;
end

module Lower = struct
  let program tacky = tacky |> Select.program |> Stack_layout.program |> Fixup.program
end

module Emit = struct
  let program (program : Program.legal Program.t) ~writer =
    Emit_pass.program program ~writer
  ;;
end
