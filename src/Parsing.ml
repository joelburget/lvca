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

module Parseable_term : Parseable with type t = Binding.Nominal.term = struct
  type t = Binding.Nominal.term

  module MenhirInterpreter = Term.Parser.MenhirInterpreter
  module ParseErrors = Term.ParseErrors
  module Lexer = Term.Lexer

  module Parser = struct
    let parse = Term.Parser.Incremental.top_term
  end
end

module NonBindingTerm : INCREMENTAL with type t = NonBinding.term = struct
  module TermP = Incremental (Parseable_term)
  type t = NonBinding.term

  let parse str =
      let open Base.Result.Let_syntax in
      let%bind tm = TermP.parse str in
      match NonBinding.from_nominal tm with
        | None -> Error (ParseError.{
          start_pos = Position.zero_pos;
          end_pos = Position.zero_pos;
          message = "This term was expected to be non-binding but contains binders";
        }) (* TODO: pos *)
        | Some nbtm -> Ok nbtm
end

module Parseable_abstract_syntax : Parseable with type t = AbstractSyntax.t = struct
  type t = AbstractSyntax.t

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

module AbstractSyntax = Incremental (Parseable_abstract_syntax)
module Term = Incremental (Parseable_term)
module Statics = Incremental (Parseable_statics)
module CoreModule = Incremental (Parseable_core_module)
module CoreTerm = Incremental (Parseable_core_term)
