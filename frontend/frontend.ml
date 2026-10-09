open! Core
module Parser_pass = Parser
module Semantic_analysis_pass = Semantic_analysis
module Ast = Ast
module Lexer = Lexer
module Token = Token
module Token_stream = Token_stream

module Program = struct
  type parsed = unit
  type validated = Semantics.Symbol_table.t

  type 'state t =
    { ast : Ast.Prog.t
    ; state : 'state
    }

  let parsed ast = { ast; state = () }

  let validated compilation_unit =
    { ast = Semantics.Compilation_unit.program compilation_unit
    ; state = Semantics.Compilation_unit.symbols compilation_unit
    }
  ;;

  let ast t = t.ast
  let symbols t = t.state
end

module Parser = struct
  let parse token_stream : Program.parsed Program.t =
    Program.parsed (Parser_pass.parse token_stream)
  ;;
end

module Semantic_analysis = struct
  let analyze (program : Program.parsed Program.t) : Program.validated Program.t =
    program |> Program.ast |> Semantic_analysis_pass.analyze |> Program.validated
  ;;
end
