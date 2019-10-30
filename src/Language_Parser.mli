
(* The type of tokens. *)

type token = 
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACK
  | LEFT_PAREN
  | LEFT_BRACK
  | ID of (string)
  | EOF
  | DOT
  | COMMA
  | BAR
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val language_def: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.language)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val language_def: Lexing.position -> (Types.language) MenhirInterpreter.checkpoint
  
end
