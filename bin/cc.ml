open! Core

module Stop_after = struct
  type t =
    | Lex
    | Parse
    | Codegen
    | Emit
    | Link
  [@@deriving compare, equal, sexp]

  let of_flags ~lex ~parse ~codegen ~emit =
    match
      List.filter_opt
        [ Option.some_if lex Lex
        ; Option.some_if parse Parse
        ; Option.some_if codegen Codegen
        ; Option.some_if emit Emit
        ]
    with
    | [] -> Ok Link
    | [ stop_after ] -> Ok stop_after
    | _ -> Or_error.error_s [%message "At most one stopping option may be specified"]
  ;;
end

let base_exn file =
  match Filename.split_extension file with
  | base, Some "c" -> base
  | _, _ -> raise_s [%message "expected a C source file" (file : Filename.t)]
;;

let remove file =
  try Core_unix.remove file with
  | Core_unix.Unix_error (ENOENT, _, _) -> ()
;;

let run_gcc_exn args =
  let process_info = Core_unix.create_process ~prog:"gcc" ~args in
  Core_unix.waitpid_exn process_info.pid
;;

let preprocess_exn ~input_file ~output_file =
  run_gcc_exn [ "-E"; "-P"; input_file; "-o"; output_file ]
;;

let compile_exn ~stop_after ~input_file:_ ~assembly_file =
  match (stop_after : Stop_after.t) with
  | Lex | Parse | Codegen -> ()
  | Emit | Link -> Out_channel.with_file assembly_file ~f:(fun _ -> ())
;;

let assemble_and_link_exn ~assembly_file ~output_file =
  run_gcc_exn [ assembly_file; "-o"; output_file ]
;;

let run ~stop_after source_file =
  let local_ run_exn source_file =
    let output_file = base_exn source_file in
    let preprocessed_file = output_file ^ ".i" in
    let assembly_file = output_file ^ ".s" in
    Exn.protect
      ~f:(stack_ fun () ->
        preprocess_exn ~input_file:source_file ~output_file:preprocessed_file;
        compile_exn ~stop_after ~input_file:preprocessed_file ~assembly_file;
        if Stop_after.equal stop_after Link
        then assemble_and_link_exn ~assembly_file ~output_file)
      ~finally:(stack_ fun () ->
        remove preprocessed_file;
        if not (Stop_after.equal stop_after Emit) then remove assembly_file) [@nontail]
  in
  Or_error.try_with (stack_ fun () -> run_exn source_file) [@nontail]
;;

let command =
  Command.basic_or_error
    ~summary:"Compile a C source file."
    (let%map_open.Command lex = flag "--lex" no_arg ~doc:" stop after lexing."
     and parse = flag "--parse" no_arg ~doc:" stop after parsing."
     and codegen = flag "--codegen" no_arg ~doc:" stop after assembly generation."
     and emit = flag "-S" no_arg ~doc:" emit assembly, but do not assemble or link."
     and source_file = anon ("SOURCE_FILE" %: Filename_unix.arg_type) in
     fun () ->
       let%bind.Or_error stop_after = Stop_after.of_flags ~lex ~parse ~codegen ~emit in
       run ~stop_after source_file)
;;

let () = Command_unix.run command
