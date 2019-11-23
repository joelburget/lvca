
(* The type of tokens. *)

type token = 
  | STAR
  | RIGHT_PAREN
  | QUESTION
  | PLUS
  | LEFT_PAREN
  | ESCAPED of (string)
  | EOF
  | DOT
  | CHARS of (string)
  | CHARACTER_SET of (string)
  | CHARACTER_CLASS of (string)
  | BAR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val regex__test: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Regex.t)

val regex: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Regex.t)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val regex__test: Lexing.position -> (Regex.t) MenhirInterpreter.checkpoint
  
  val regex: Lexing.position -> (Regex.t) MenhirInterpreter.checkpoint
  
end
