
(* The type of tokens. *)

type token = 
  | STRING of (string)
  | SEMICOLON
  | RIGHT_PAREN
  | RIGHT_BRACK
  | LEFT_PAREN
  | LEFT_BRACK
  | INT of (Bigint.t)
  | ID of (string)
  | EOF
  | DOT

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top_term: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Binding.Nominal.term)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val top_term: Lexing.position -> (Binding.Nominal.term) MenhirInterpreter.checkpoint
  
end
