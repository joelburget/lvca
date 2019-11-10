
(* The type of tokens. *)

type token = 
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACK
  | RIGHT_BRACE
  | LEFT_PAREN
  | LEFT_BRACK
  | LEFT_BRACE
  | IMPORT
  | ID of (string)
  | FROM
  | EOF
  | DOT
  | COMMA
  | BAR
  | ASSIGN
  | AS

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val language_def: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.abstract_syntax)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val language_def: Lexing.position -> (Types.abstract_syntax) MenhirInterpreter.checkpoint
  
end
