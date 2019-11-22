open Binding
open Types

module Lexer = ConcreteSyntax_Lexer
module Parser = ConcreteSyntax_Parser
module ParseErrors = ConcreteSyntax_ParseErrors

type prim_ty =
  | Integer
  | String

type node_type =
  | Operator  of string
  | Var
  | Sequence
  | Primitive of prim_ty

type terminal_capture =
  { content         : string;
    leading_trivia  : string;
    trailing_trivia : string;
  }

type nonterminal_capture = tree

and capture =
  | TerminalCapture    of terminal_capture
  | NonterminalCapture of nonterminal_capture

and tree =
  { sort_name       : sort_name;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : capture array;
  }

val equivalent : tree -> tree -> bool

val mk_tree : sort_name -> node_type -> capture array -> tree

(** Convert an abstract syntax tree to a concrete syntax tree *)
val of_ast
  : language -> ConcreteSyntaxDescription.t -> sort -> Nominal.term -> tree

(** Print a concrete syntax tree to a string *)
val to_string : tree -> string

(** Parse from a string to a concrete syntax tree *)
val parse
  : ConcreteSyntaxDescription.t -> string -> (tree, string) Result.t

(** Convert form a concrete to an abstract syntax tree *)
val to_ast : language -> tree -> (Nominal.term, string) Result.t

val to_grammar : ConcreteSyntaxDescription.t -> LrParsing.grammar

type invalid_grammar

val check_description_validity
  : ConcreteSyntaxDescription.t -> invalid_grammar option

(* exported for debugger: *)
val lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer
