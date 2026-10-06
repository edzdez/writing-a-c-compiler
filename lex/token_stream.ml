open! Core

type t =
  { lexbuf : Lexing.lexbuf
  ; buffer : Token.t Util.With_location.t Deque.t
  ; mutable eof : bool
  }

let create lexbuf = { lexbuf; buffer = Deque.create (); eof = false }

let read_one t =
  let token = Lexer.next t.lexbuf in
  let location =
    Util.Location.create
      ~start_position:(Lexing.lexeme_start_p t.lexbuf)
      ~end_position:(Lexing.lexeme_end_p t.lexbuf)
  in
  Deque.enqueue_back t.buffer (Util.With_location.create token ~location);
  if Token.equal token Eof then t.eof <- true
;;

let ensure_buffered t ~distance =
  if distance < 0 then invalid_arg "Token_stream.peek: negative distance";
  while (not t.eof) && Deque.length t.buffer <= distance do
    read_one t
  done
;;

let peek t ~distance =
  ensure_buffered t ~distance;
  match distance < Deque.length t.buffer with
  | false -> Deque.peek_back_exn t.buffer
  | true ->
    let index = Deque.front_index_exn t.buffer + distance in
    Deque.get t.buffer index
;;

let pop t =
  let token = peek t ~distance:0 in
  match Util.With_location.value token with
  | Eof -> token
  | _ -> Deque.dequeue_front_exn t.buffer
;;
