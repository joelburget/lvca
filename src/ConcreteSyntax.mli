open Binding
open Types

type prim_ty =
  | Integer
  | String

type node_type =
  | Operator  of string
  | Var
  | Sequence
  | Primitive of prim_ty

type terminal_capture    = string
type nonterminal_capture = tree

and tree =
  { sort            : sort;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : (terminal_capture, nonterminal_capture) Either.t array;
  }

val equivalent : tree -> tree -> bool

val mk_tree
  : sort
  -> node_type
  -> (terminal_capture, nonterminal_capture) Either.t array
  -> tree

val of_ast    : language -> ConcreteSyntaxDescription.t -> sort -> Nominal.term -> tree
val to_string : tree -> string
val parse     : ConcreteSyntaxDescription.t -> string -> (tree, string) Result.t
val to_ast    : language -> tree -> (Nominal.term, string) Result.t

(* exported for testing: *)
val regex_piece_to_string : ConcreteSyntaxDescription.regex_piece -> string
