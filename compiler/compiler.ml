open! Core
module Stage = Stage

let rec lex_to_end lexbuf =
  match Frontend.Lexer.next lexbuf with
  | Eof -> ()
  | token ->
    print_s (Frontend.Token.sexp_of_t token);
    lex_to_end lexbuf
;;

let parse lexbuf =
  let token_stream = Frontend.Token_stream.create lexbuf in
  Frontend.Parser.parse token_stream
;;

let lower_to_tacky lexbuf = lexbuf |> parse |> Tacky.Lower.program
let lower_to_x86 lexbuf = lexbuf |> lower_to_tacky |> X86.Lower.program

let compile_exn ~stop_after ~input_file ~assembly_file =
  In_channel.with_file input_file ~f:(fun ic ->
    let lexbuf = Lexing.from_channel ic in
    Lexing.set_filename lexbuf input_file;
    match stop_after with
    | Stage.Lex -> lex_to_end lexbuf
    | Parse -> lexbuf |> parse |> Frontend.Ast.Prog.sexp_of_t |> print_s
    | Tacky -> lexbuf |> lower_to_tacky |> Tacky.Ir.Prog.sexp_of_t |> print_s
    | Codegen -> lexbuf |> lower_to_x86 |> X86.Ir.Prog.sexp_of_t |> print_s
    | Emit | Link ->
      let x86 = lower_to_x86 lexbuf in
      Out_channel.with_file assembly_file ~f:(fun writer -> X86.Emit.program x86 ~writer))
;;
