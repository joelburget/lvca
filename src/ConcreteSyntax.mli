open Binding
open Types
module Lexer = ConcreteSyntax_Lexer
module Parser = ConcreteSyntax_Parser
module ParseErrors = ConcreteSyntax_ParseErrors

type prim_ty =
  | Integer
  | String

type node_type =
  | SingleCapture
  | Operator of string
  | Sequence
  | Primitive of prim_ty

type terminal_capture =
  { content : string
  ; mutable leading_trivia : string
  ; mutable trailing_trivia : string
  }

type 'a nonterminal_capture = 'a tree

and 'a capture =
  | TerminalCapture of terminal_capture
  | NonterminalCapture of 'a nonterminal_capture

and 'a tree =
  { sort_name : sort_name
  ; node_type : node_type
  ; children : 'a capture array
  ; metadata : 'a
  }

(** Are two trees equivalent, ignoring trivia *)
val equivalent : 'a tree -> 'a tree -> bool

(** Convert an abstract syntax tree to a concrete syntax tree *)
val of_ast
  : language
  -> ConcreteSyntaxDescription.t
  -> sort
  -> int
  -> Nominal.term
  -> unit tree

(** Print a concrete syntax tree to a string *)
val to_string : 'a tree -> string

(** Parse from a string to a concrete syntax tree *)
val parse
  :  ConcreteSyntaxDescription.t
  -> string (* root name *)
  -> string (* string to parse *)
  -> (unit tree, string) Result.t

(** Convert form a concrete to an abstract syntax tree *)
val to_ast
  :  language
  -> ConcreteSyntaxDescription.t
  -> string
  -> 'a tree
  -> (Nominal.term, string) Result.t

val to_grammar
  :  ConcreteSyntaxDescription.t
  -> LrParsing.grammar
     * (ConcreteSyntaxDescription.nonterminal_token list
        * ConcreteSyntaxDescription.operator_match_pattern option)
         Belt.MutableMap.Int.t

type invalid_grammar

val check_description_validity : ConcreteSyntaxDescription.t -> invalid_grammar option

(* exported for debugger: *)
val lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer

(* exported for testing: *)
val mk_tree : sort_name -> node_type -> 'a -> 'a capture array -> 'a tree
val remove_spaces : 'a tree -> 'a tree

(** Make a concrete syntax description from its parsed rules. This morally
    belongs to the ConcreteSyntaxDescription module, but it's here to break a
    dependency cycle with Parsing -> ConcreteSyntaxDescription.
*)
val make_concrete_description
  :  ConcreteSyntaxDescription.pre_terminal_rule list
  -> ConcreteSyntaxDescription.sort_rule list
  -> ConcreteSyntaxDescription.t
