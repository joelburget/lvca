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
    (* val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t *)
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
  module MenhirInterpreter = TermParser.MenhirInterpreter
  module ParseErrors       = TermParseErrors
  module Lexer             = TermLexer
  module Parser            = struct
    let parse = TermParser.Incremental.top_term
  end
end

module Parseable_language : (Parseable with type t = Types.language) = struct
  type t                   = Types.language
  module MenhirInterpreter = LanguageParser.MenhirInterpreter
  module ParseErrors       = LanguageParseErrors
  module Lexer             = LanguageLexer
  module Parser            = struct
    let parse = LanguageParser.Incremental.languageDef
  end
end

module Parseable_statics
  : (Parseable with type t = Types.Statics.rule list) = struct
  type t                   = Types.Statics.rule list
  module MenhirInterpreter = StaticsParser.MenhirInterpreter
  module ParseErrors       = StaticsParseErrors
  module Lexer             = StaticsLexer
  module Parser            = struct
    let parse = StaticsParser.Incremental.rules
  end
end

module Parseable_dynamics
  : (Parseable with type t = Core.denotation_chart) = struct
  type t                   = Core.denotation_chart
  module MenhirInterpreter = DynamicsParser.MenhirInterpreter
  module ParseErrors       = DynamicsParseErrors
  module Lexer             = DynamicsLexer
  module Parser            = struct
    let parse = DynamicsParser.Incremental.dynamics
  end
end
