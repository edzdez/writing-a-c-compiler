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
    | Parse ->
      let token_stream = Lex.Token_stream.create lexbuf in
      print_s [%sexp (Parse.parse_prog token_stream : Ast.Prog.t)]
    | Codegen ->
      let token_stream = Lex.Token_stream.create lexbuf in
      let ast = Parse.parse_prog token_stream in
      print_s [%sexp (Cgen.cgen_prog ast : Asm.Prog.t)]
    | Emit | Link ->
      let token_stream = Lex.Token_stream.create lexbuf in
      let ast = Parse.parse_prog token_stream in
      let asm = Cgen.cgen_prog ast in
      Out_channel.with_file assembly_file ~f:(fun writer -> Asm.Prog.pp asm ~writer))
;;
