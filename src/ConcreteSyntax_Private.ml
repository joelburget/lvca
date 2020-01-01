open Types
open ConcreteSyntaxDescription

type prim_ty =
  | Integer
  | String

type node_type =
  | SingleCapture
  | Operator of string
  | Sequence
  | Primitive of prim_ty

(** Terminals capture text from the input buffer *)
type terminal_capture =
  { content : string
  ; mutable leading_trivia : string
  ; mutable trailing_trivia : string
  }

(** Nonterminals capture their children *)
type nonterminal_capture = tree

(** Terminals and nonterminals both capture data about why they were
    constructed
*)
and capture =
  | TerminalCapture of terminal_capture
  | NonterminalCapture of nonterminal_capture

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
and tree =
  { sort_name : sort_name
  ; node_type : node_type
  ; children : capture array
  }

(* tree equality mod trivia *)
let rec equivalent t1 t2 =
  t1.sort_name = t2.sort_name
  && t1.node_type = t2.node_type
  && Belt.Array.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 =
  match child1, child2 with
  | TerminalCapture tc1, TerminalCapture tc2 -> tc1.content = tc2.content
  | NonterminalCapture ntc1, NonterminalCapture ntc2 -> equivalent ntc1 ntc2
  | _, _ -> false
;;

let find_operator_match
  : operator_match list list -> string -> operator_match
  = fun matches opname -> matches
    |. Belt.List.flatten
    |> Util.find
      (* TODO now need to match *)
      (fun (OperatorMatch { operator_match_pattern }) ->
         match operator_match_pattern with
         | OperatorPattern (opname', _) -> opname' = opname
         | SingleCapturePattern _ -> false)
    |> Util.get_option' ("failed to find a rule matching operator " ^ opname)
;;
