open! Core
module Ast = Ast
module Lexer = Lexer
module Token = Token
module Token_stream = Token_stream

module Program : sig
  type parsed
  type validated
  type 'state t

  val ast : _ t -> Ast.Prog.t
end

module Parser : sig
  val parse : Token_stream.t -> Program.parsed Program.t
end

module Semantic_analysis : sig
  val analyze : Program.parsed Program.t -> Program.validated Program.t
end
