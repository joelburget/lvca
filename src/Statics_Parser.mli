
(* The type of tokens. *)

type token = 
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_D_ARR
  | LINE of (string option)
  | LEFT_PAREN
  | LEFT_D_ARR
  | ID of (string)
  | EOF
  | DOT
  | CTX_SEPARATOR
  | CTX
  | COMMA
  | COLON

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val term_top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Statics.term)

val rules: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Statics.rule list)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val term_top: Lexing.position -> (Statics.term) MenhirInterpreter.checkpoint
  
  val rules: Lexing.position -> (Statics.rule list) MenhirInterpreter.checkpoint
  
end
