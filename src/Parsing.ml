open Core_kernel

module type Parseable = sig
  type t

  module MenhirInterpreter : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE

  module ParseErrors : sig
    val message : int -> string
  end

  module Lexer : sig
    val read : Lexing.lexbuf -> MenhirInterpreter.token
  end

  module Parser : sig
    val parse : Lexing.position -> t MenhirInterpreter.checkpoint
  end
end

module type INCREMENTAL = sig
  type t

  val parse : string -> (t, string) Result.t
end

module Incremental (M : Parseable) = struct
  type t = M.t
  type parse_result = (M.t, ParseError.t) Result.t

  module I = M.MenhirInterpreter

  let stack (checkpoint : M.t I.checkpoint) =
    match checkpoint with I.HandlingError env -> I.stack env | _ -> assert false
  ;;

  let state (checkpoint : M.t I.checkpoint) : int =
    match Lazy.force (stack checkpoint) with
    | MenhirLib.General.Nil -> 0
    | MenhirLib.General.Cons (I.Element (s, _, _, _), _) -> I.number s
  ;;

  let fail (lexbuf : Lexing.lexbuf) (c : M.t I.checkpoint) : parse_result =
    let s : int = state c in
    let position = Position.of_lexbuf lexbuf in
    Error { start_pos = position; end_pos = position; message =  M.ParseErrors.message s }
  ;;

  let loop (lexbuf : Lexing.lexbuf) (result : M.t I.checkpoint) =
    let supplier = I.lexer_lexbuf_to_supplier M.Lexer.read lexbuf in
    try
      I.loop_handle (fun v -> Ok v) (fail lexbuf) supplier result
    with
      | ParseError.SyntaxError err -> Error err
      | LexerUtil.LexicalError { position; message }
      -> Error { start_pos = position; end_pos = position; message }
  ;;

  let parse (str : string) : parse_result =
    let lexbuf = Lexing.from_string str in
    loop lexbuf (M.Parser.parse lexbuf.lex_curr_p)
  ;;
end

module Parseable_term : Parseable with type t = Binding.Nominal.term = struct
  type t = Binding.Nominal.term

  module MenhirInterpreter = Term.Parser.MenhirInterpreter
  module ParseErrors = Term.ParseErrors
  module Lexer = Term.Lexer

  module Parser = struct
    let parse = Term.Parser.Incremental.top_term
  end
end

module Parseable_abstract_syntax : Parseable with type t = AbstractSyntax.abstract_syntax = struct
  type t = AbstractSyntax.abstract_syntax

  module MenhirInterpreter = AbstractSyntax.Parser.MenhirInterpreter
  module ParseErrors = AbstractSyntax.ParseErrors
  module Lexer = AbstractSyntax.Lexer

  module Parser = struct
    let parse = AbstractSyntax.Parser.Incremental.language_def
  end
end

module Parseable_statics : Parseable with type t = Statics.rule list = struct
  type t = Statics.rule list

  module MenhirInterpreter = Statics_Parser.MenhirInterpreter
  module ParseErrors = Statics.ParseErrors
  module Lexer = Statics_Lexer

  module Parser = struct
    let parse = Statics_Parser.Incremental.rules
  end
end

module Parseable_dynamics : Parseable with type t = Dynamics.Core.denotation_chart = struct
  type t = Dynamics.Core.denotation_chart

  module MenhirInterpreter = Dynamics.Parser.MenhirInterpreter
  module ParseErrors = Dynamics.ParseErrors
  module Lexer = Dynamics.Lexer

  module Parser = struct
    let parse = Dynamics.Parser.Incremental.dynamics
  end
end

module Parseable_core : Parseable with type t = Dynamics.Core.core = struct
  type t = Dynamics.Core.core

  module MenhirInterpreter = Dynamics.Parser.MenhirInterpreter
  module ParseErrors = Dynamics.ParseErrors
  module Lexer = Dynamics.Lexer

  module Parser = struct
    let parse = Dynamics.Parser.Incremental.core
  end
end

module Parseable_concrete_syntax :
  Parseable with type t = ConcreteSyntaxDescription.pre_t = struct
  type t = ConcreteSyntaxDescription.pre_t

  module MenhirInterpreter = ConcreteSyntax_Parser.MenhirInterpreter
  module ParseErrors = ConcreteSyntax_ParseErrors
  module Lexer = ConcreteSyntax_Lexer

  module Parser = struct
    let parse = ConcreteSyntax_Parser.Incremental.language
  end
end

module Parseable_regex : Parseable with type t = Regex.t = struct
  type t = Regex.t

  module MenhirInterpreter = Regex_Parser.MenhirInterpreter
  module ParseErrors = Regex_ParseErrors
  module Lexer = Regex_Lexer

  module Parser = struct
    let parse = Regex_Parser.Incremental.regex
  end
end

module Term = Incremental (Parseable_term)
module AbstractSyntax = Incremental (Parseable_abstract_syntax)
module Statics = Incremental (Parseable_statics)
module Dynamics = Incremental (Parseable_dynamics)
module Core = Incremental (Parseable_core)
module ConcreteSyntax = Incremental (Parseable_concrete_syntax)
module Regex = Incremental (Parseable_regex)
