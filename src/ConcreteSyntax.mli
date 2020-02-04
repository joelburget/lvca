(** Types and functions for dealing with concrete syntax.
 *
 * This module is all about this diagram:
 *
 *        ---parse--->                -to_ast->
 * string              formatted_tree           Nominal.term
 *        <-to_string-                <-of_ast-
 *
 * The other important functions are to_grammar, lexer_of_desc, and
 * make_concrete_description
 *)
open Binding
open Types
module Lexer = ConcreteSyntax_Lexer
module Parser = ConcreteSyntax_Parser
module ParseErrors = ConcreteSyntax_ParseErrors

type tree_info = string * int

(** Terminals capture text from the input buffer *)
type formatted_terminal_capture =
  { content : string
  ; leading_trivia : string
  ; trailing_trivia : string
  }

(** Nonterminals capture their children *)
type formatted_nonterminal_capture = formatted_tree

(** Terminals and nonterminals both capture data about why they were
    constructed
*)
and formatted_capture =
  | TerminalCapture of formatted_terminal_capture
  | NonterminalCapture of formatted_nonterminal_capture

(* Inspired by:
 * - https://github.com/apple/swift/tree/master/lib/Syntax
 * - https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trees
 *
 * Rules of trivia (same as for swift):
 * - A token owns all of its trailing trivia up to, but not including, the
 *   next newline character.
 * - Looking backward in the text, a token owns all of the leading trivia up
 *   to and including the first newline character.
 *
 * In other words, a contiguous stretch of trivia between two tokens is split
 * on the leftmost newline.
*)
and formatted_tree =
  { tree_info : tree_info
  ; children : formatted_capture array
  }

(** Are two trees equivalent, ignoring trivia *)
val equivalent : formatted_tree -> formatted_tree -> bool

(** Convert an abstract syntax tree to a concrete syntax tree *)
val of_ast
  : sort_defs
  -> ConcreteSyntaxDescription.t
  -> string
  -> int
  -> Nominal.term
  -> formatted_tree

(** Print a concrete syntax tree to a string *)
val to_string : formatted_tree -> string

(** Parse from a string to a concrete syntax tree *)
val parse
  :  ConcreteSyntaxDescription.t
  -> string (* root name *)
  -> string (* string to parse *)
  -> (formatted_tree, string) Belt.Result.t

(** Convert form a concrete to an abstract syntax tree *)
val to_ast
  :  ConcreteSyntaxDescription.t
  -> formatted_tree
  -> (Nominal.term, string) Belt.Result.t

val to_grammar
  :  ConcreteSyntaxDescription.t
  -> string
  -> LrParsing.grammar
     * (tree_info
        * ConcreteSyntaxDescription.nonterminal_token list
        * ConcreteSyntaxDescription.operator_match_pattern option)
         Belt.MutableMap.Int.t
     * string option Belt.Map.String.t

type invalid_grammar

val check_description_validity : ConcreteSyntaxDescription.t -> invalid_grammar option

(* exported for debugger: *)
val lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer

(* exported for testing: *)
val remove_spaces : formatted_tree -> formatted_tree

(** Make a concrete syntax description from its parsed rules. This morally
    belongs to the ConcreteSyntaxDescription module, but it's here to break a
    dependency cycle with Parsing -> ConcreteSyntaxDescription.
*)
val make_concrete_description
  :  ConcreteSyntaxDescription.pre_terminal_rule list
  -> ConcreteSyntaxDescription.nonterminal_rule list
  -> ConcreteSyntaxDescription.t

type nonterminal_operators =
  (int option * ConcreteSyntaxDescription.operator_match) list Belt.Map.String.t

val derived_nonterminal_rules
  : ConcreteSyntaxDescription.nonterminal_rules -> nonterminal_operators array

val string_of_nonterminal_operators : nonterminal_operators -> string
val string_of_derived_rules : nonterminal_operators array -> string
