open Belt
open Binding
open Either
open Types

type prim_ty =
  | Integer
  | Bool
  | String

type node_type =
  | Operator
  | Var
  | Sequence
  | Primitive of prim_ty

type terminal_capture    = string
type nonterminal_capture = tree

and tree =
    (* rule            : ConcreteSyntax.sort_rule; *)
  { sort            : sort;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : (terminal_capture, nonterminal_capture) either list;
    size            : int;
  }

val of_ast    : ConcreteSyntax.t -> sort -> Nominal.term -> tree
val to_string : tree -> string
val parse     : ConcreteSyntax.sort_rule -> string -> (tree, string) Result.t
val to_ast    : language -> tree -> (Nominal.term, string) Result.t