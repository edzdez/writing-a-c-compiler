open! Core

let rec lex_to_end lexbuf =
  match Lex.next lexbuf with
  | Eof -> ()
  | token ->
    print_s [%sexp (token : Lex.Token.t)];
    lex_to_end lexbuf
;;

let compile_exn ~stop_after ~input_file ~assembly_file =
  In_channel.with_file input_file ~f:(fun ic ->
    let lexbuf = Lexing.from_channel ic in
    Lexing.set_filename lexbuf input_file;
    match (stop_after : Stop_after.t) with
    | Lex -> lex_to_end lexbuf
    | Parse | Codegen -> ()
    | Emit | Link -> Out_channel.with_file assembly_file ~f:(fun _ -> ()))
;;
