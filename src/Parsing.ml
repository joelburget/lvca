open Belt

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
    val parse: Lexing.position -> t MenhirInterpreter.checkpoint
  end
end

module type INCREMENTAL = sig
  type t
  val parse : string -> (t, string) Result.t
end

module Incremental (M : Parseable) = struct

  type t = M.t
  type parse_result = (M.t, string) Result.t

  module I = M.MenhirInterpreter

  let stack (checkpoint: M.t I.checkpoint) = match checkpoint with
    | I.HandlingError env -> I.stack env
    | _ -> assert false

  let state (checkpoint: M.t I.checkpoint) : int =
    match Lazy.force (stack checkpoint) with
    | MenhirLib.General.Nil -> 0
    | MenhirLib.General.Cons(I.Element(s, _, _, _), _) -> I.number(s)

  let fail (lexbuf: Lexing.lexbuf) (c: M.t I.checkpoint) : parse_result =
    let s: int = state(c) in
    Error (
      Printf.sprintf
        "Syntax error at offset %d:\n%s\n"
        (Lexing.lexeme_start lexbuf)
        (M.ParseErrors.message s)
    )

  let loop (lexbuf: Lexing.lexbuf) (result: M.t I.checkpoint) =
    let supplier = I.lexer_lexbuf_to_supplier M.Lexer.read lexbuf in
    try I.loop_handle (fun v -> Result.Ok v) (fail lexbuf) supplier result
    with LexerUtil.SyntaxError(msg) -> Error(msg)

  let parse (str: string): parse_result =
    let lexbuf = Lexing.from_string str in
    loop lexbuf (M.Parser.parse lexbuf.lex_curr_p)

end

module Parseable_term : (Parseable with type t = Binding.Nominal.term) = struct
  type t                   = Binding.Nominal.term
  module MenhirInterpreter = Term.Parser.MenhirInterpreter
  module ParseErrors       = Term.ParseErrors
  module Lexer             = Term.Lexer
  module Parser            = struct
    let parse = Term.Parser.Incremental.top_term
  end
end

module Parseable_abstract_syntax
  : (Parseable with type t = Types.language) = struct
  type t                   = Types.language
  module MenhirInterpreter = AbstractSyntax.Parser.MenhirInterpreter
  module ParseErrors       = AbstractSyntax.ParseErrors
  module Lexer             = AbstractSyntax.Lexer
  module Parser            = struct
    let parse = AbstractSyntax.Parser.Incremental.language_def
  end
end

module Parseable_statics
  : (Parseable with type t = Statics.rule list) = struct
  type t                   = Statics.rule list
  module MenhirInterpreter = Statics_Parser.MenhirInterpreter
  module ParseErrors       = Statics.ParseErrors
  module Lexer             = Statics_Lexer
  module Parser            = struct
    let parse = Statics_Parser.Incremental.rules
  end
end

module Parseable_dynamics
  : (Parseable with type t = Core.denotation_chart) = struct
  type t                   = Core.denotation_chart
  module MenhirInterpreter = Dynamics.Parser.MenhirInterpreter
  module ParseErrors       = Dynamics.ParseErrors
  module Lexer             = Dynamics.Lexer
  module Parser            = struct
    let parse = Dynamics.Parser.Incremental.dynamics
  end
end

module Parseable_concrete_syntax
  : (Parseable with type t = Types.ConcreteSyntaxDescription.t) = struct
  type t                   = Types.ConcreteSyntaxDescription.t
  module MenhirInterpreter = ConcreteSyntax.Parser.MenhirInterpreter
  module ParseErrors       = ConcreteSyntax.ParseErrors
  module Lexer             = ConcreteSyntax.Lexer
  module Parser            = struct
    let parse = ConcreteSyntax.Parser.Incremental.language
  end
end
