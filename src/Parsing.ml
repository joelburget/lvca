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
  type parse_result = (t, ParseError.t) Result.t

  val parse : string -> (t, ParseError.t) Result.t
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

module Parseable_core_module : Parseable with type t = Core.Types.core_module = struct
  type t = Core.Types.core_module

  module MenhirInterpreter = Core.Parser.MenhirInterpreter
  module ParseErrors = Core.ParseErrors
  module Lexer = Core.Lexer

  module Parser = struct
    let parse = Core.Parser.Incremental.core_module
  end
end

module Parseable_core_term : Parseable with type t = Core.Types.term = struct
  type t = Core.Types.term

  module MenhirInterpreter = Core.Parser.MenhirInterpreter
  module ParseErrors = Core.ParseErrors
  module Lexer = Core.Lexer

  module Parser = struct
    let parse = Core.Parser.Incremental.term_top
  end
end

module CoreModule = Incremental (Parseable_core_module)
module CoreTerm = Incremental (Parseable_core_term)
