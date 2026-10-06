open! Core
module Stage = Stage

val compile_exn
  :  stop_after:Stage.t
  -> input_file:Filename.t
  -> assembly_file:Filename.t
  -> unit
