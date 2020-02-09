
(* The type of tokens. *)

type token = 
  | UNDERSCORE
  | TERMINAL_ID of (string)
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_FIXITY
  | RIGHT_BRACKET
  | RIGHT_BRACE
  | RIGHT_ANGLE
  | REGEX of (string)
  | NONTERMINAL_ID of (string)
  | NAT of (int)
  | LEFT_PAREN
  | LEFT_FIXITY
  | LEFT_BRACKET
  | LEFT_BRACE
  | LEFT_ANGLE
  | FORALL
  | EOF
  | DOT
  | DOLLAR
  | COMMA
  | COLON
  | BAR
  | ASSIGN
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val terminal_rule__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.pre_terminal_rule)

val quantifiers__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tablecloth.StrSet.t)

val operator_match__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.operator_match)

val nonterminal_type__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.nonterminal_type)

val nonterminal_token__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.nonterminal_token)

val nonterminal_rule__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.nonterminal_rule)

val language: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list)

val capture_number: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.capture_number)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val terminal_rule__test: Lexing.position -> (ConcreteSyntaxDescription.pre_terminal_rule) MenhirInterpreter.checkpoint
  
  val quantifiers__test: Lexing.position -> (Tablecloth.StrSet.t) MenhirInterpreter.checkpoint
  
  val operator_match__test: Lexing.position -> (ConcreteSyntaxDescription.operator_match) MenhirInterpreter.checkpoint
  
  val nonterminal_type__test: Lexing.position -> (ConcreteSyntaxDescription.nonterminal_type) MenhirInterpreter.checkpoint
  
  val nonterminal_token__test: Lexing.position -> (ConcreteSyntaxDescription.nonterminal_token) MenhirInterpreter.checkpoint
  
  val nonterminal_rule__test: Lexing.position -> (ConcreteSyntaxDescription.nonterminal_rule) MenhirInterpreter.checkpoint
  
  val language: Lexing.position -> (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.nonterminal_rule list) MenhirInterpreter.checkpoint
  
  val capture_number: Lexing.position -> (ConcreteSyntaxDescription.capture_number) MenhirInterpreter.checkpoint
  
end
