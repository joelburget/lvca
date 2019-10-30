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
  | SpaceCapture       of string

and tree =
  { sort            : sort;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : capture array;
  }

val equivalent : tree -> tree -> bool

val mk_tree : sort -> node_type -> capture array -> tree

val of_ast    : language -> ConcreteSyntaxDescription.t -> sort -> Nominal.term -> tree
val to_string : tree -> string
val parse     : ConcreteSyntaxDescription.t -> string -> (tree, string) Result.t
val to_ast    : language -> tree -> (Nominal.term, string) Result.t

val to_grammar : ConcreteSyntaxDescription.t -> LrParsing.grammar

type invalid_grammar

val check_description_validity
  : ConcreteSyntaxDescription.t -> invalid_grammar option

(* exported for testing: *)
val regex_piece_to_string : ConcreteSyntaxDescription.regex_piece -> string

(* exported for debugger: *)
val lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer
