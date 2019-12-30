
(* The type of tokens. *)

type token = 
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACKET
  | LEFT_PAREN
  | LEFT_BRACKET
  | INT of (Bigint.t)
  | ID of (string)
  | EOF
  | DOT
  | COMMA

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val dynamics: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Core.core list)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val dynamics: Lexing.position -> (Core.core list) MenhirInterpreter.checkpoint
  
end
