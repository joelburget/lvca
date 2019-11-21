
(* The type of tokens. *)

type token = 
  | TERMINAL_ID of (string)
  | STRING of (string)
  | STAR
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_FIXITY
  | RIGHT_BRACE
  | REGEX of (string)
  | QUESTION
  | PLUS
  | NONTERMINAL_ID of (string)
  | NAT of (int)
  | LEFT_PAREN
  | LEFT_FIXITY
  | LEFT_BRACE
  | GREATER
  | EOF
  | DOT
  | DOLLAR
  | CHARACTER_SET of (string)
  | CHARACTER_CLASS of (string)
  | BAR
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val terminal_rule__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.ConcreteSyntaxDescription.pre_terminal_rule)

val sort_rule__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.ConcreteSyntaxDescription.sort_rule)

val operator_match__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.ConcreteSyntaxDescription.operator_match)

val nonterminal_token: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.ConcreteSyntaxDescription.nonterminal_token)

val language: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.ConcreteSyntaxDescription.t)

val capture_number: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.ConcreteSyntaxDescription.capture_number)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val terminal_rule__test: Lexing.position -> (Types.ConcreteSyntaxDescription.pre_terminal_rule) MenhirInterpreter.checkpoint
  
  val sort_rule__test: Lexing.position -> (Types.ConcreteSyntaxDescription.sort_rule) MenhirInterpreter.checkpoint
  
  val operator_match__test: Lexing.position -> (Types.ConcreteSyntaxDescription.operator_match) MenhirInterpreter.checkpoint
  
  val nonterminal_token: Lexing.position -> (Types.ConcreteSyntaxDescription.nonterminal_token) MenhirInterpreter.checkpoint
  
  val language: Lexing.position -> (Types.ConcreteSyntaxDescription.t) MenhirInterpreter.checkpoint
  
  val capture_number: Lexing.position -> (Types.ConcreteSyntaxDescription.capture_number) MenhirInterpreter.checkpoint
  
end
