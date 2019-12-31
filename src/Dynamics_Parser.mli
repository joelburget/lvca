
(* The type of tokens. *)

type token = 
  | WITH
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACKET
  | RIGHT_BRACE
  | MATCH
  | LET
  | LEFT_PAREN
  | LEFT_BRACKET
  | LEFT_BRACE
  | INT of (Bigint.t)
  | IN
  | ID of (string)
  | EQ
  | EOF
  | DOT
  | COMMA
  | COLON
  | BAR
  | BACKSLASH
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val dynamics: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Core.denotation_chart)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val dynamics: Lexing.position -> (Core.denotation_chart) MenhirInterpreter.checkpoint
  
end
