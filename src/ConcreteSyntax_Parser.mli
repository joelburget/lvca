
(* The type of tokens. *)

type token = 
  | UNDERSCORE
  | TERMINAL_ID of (string)
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_FIXITY
  | RIGHT_BRACE
  | REGEX of (string)
  | NONTERMINAL_ID of (string)
  | NAT of (int)
  | LEFT_PAREN
  | LEFT_FIXITY
  | LEFT_BRACE
  | GREATER
  | EOF
  | DOT
  | DOLLAR
  | BAR
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val terminal_rule__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.pre_terminal_rule)

val sort_rule__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.sort_rule)

val operator_match__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.operator_match)

val nonterminal_token__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.nonterminal_token)

val language: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list)

val capture_number: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ConcreteSyntaxDescription.capture_number)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val terminal_rule__test: Lexing.position -> (ConcreteSyntaxDescription.pre_terminal_rule) MenhirInterpreter.checkpoint
  
  val sort_rule__test: Lexing.position -> (ConcreteSyntaxDescription.sort_rule) MenhirInterpreter.checkpoint
  
  val operator_match__test: Lexing.position -> (ConcreteSyntaxDescription.operator_match) MenhirInterpreter.checkpoint
  
  val nonterminal_token__test: Lexing.position -> (ConcreteSyntaxDescription.nonterminal_token) MenhirInterpreter.checkpoint
  
  val language: Lexing.position -> (ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list) MenhirInterpreter.checkpoint
  
  val capture_number: Lexing.position -> (ConcreteSyntaxDescription.capture_number) MenhirInterpreter.checkpoint
  
end
