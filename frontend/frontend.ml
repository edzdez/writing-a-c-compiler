open! Core
module Parser_pass = Parser
module Semantic_analysis_pass = Semantic_analysis
module Ast = Ast
module Lexer = Lexer
module Token = Token
module Token_stream = Token_stream

module Program = struct
  type parsed
  type validated
  type 'state t = Ast.Prog.t

  let ast t = t
end

module Parser = struct
  let parse token_stream : Program.parsed Program.t = Parser_pass.parse token_stream
end

module Semantic_analysis = struct
  let analyze (program : Program.parsed Program.t) : Program.validated Program.t =
    Semantic_analysis_pass.analyze program
  ;;
end
