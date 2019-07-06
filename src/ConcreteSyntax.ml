open Belt
open Binding
open Either
open Types
open Types.ConcreteSyntax
let (find, sum) = Util.(find, sum)

let sprintf = Printf.sprintf

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
  { (* rule            : sort_rule; *)
    sort            : sort;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : (terminal_capture, nonterminal_capture) either list;
    size            : int;
  }

let find_operator_match
  (matches: operator_match list)
  (opname : string)
  : operator_match
  = let maybeMatch = find
    (fun (OperatorMatch { term_pattern = (opname', _) }) -> opname' = opname)
    matches in
    match maybeMatch with
      | Some m -> m
      | None -> failwith "TODO: default match"

type subterm_result =
  | NotFound
  | FoundTerm of Nominal.term
  | FoundBinder

let rec find_subtm
  (token_ix: int)
  (scopes: Nominal.scope list)
  (term_pattern: term_scope list)
  : subterm_result
  = match scopes, term_pattern with
    | _, [] -> NotFound
    | Scope (_, body) :: scopes', TermScope (binder_nums, body_num) :: pattern_scopes
    -> (match find (fun num -> num = token_ix) binder_nums with
      | Some _ -> FoundBinder
      | None -> if token_ix = body_num
        then FoundTerm body
        else find_subtm token_ix scopes' pattern_scopes
      )
    | _, _ -> failwith "invariant violation"

exception BadRules

(* val of_ast    : ConcreteSyntax.t -> Nominal.term -> tree *)
let rec of_ast ({ terminal_rules; sort_rules } as rules) current_sort tm
  = match current_sort, tm with
  | _, Nominal.Operator (name, scopes) ->
    let SortRule { operator_rules } = M.getExn sort_rules name in
    (* TODO: remove possible exception. possible to have var-only sort? *)
    let OperatorMatch { tokens; term_pattern } = find_operator_match operator_rules name in
    let sizes, children = List.unzip (List.mapWithIndex tokens (fun token_ix token ->
      match find_subtm token_ix scopes (snd term_pattern), token with

        | FoundTerm subtm, NonterminalName sort
        -> let subtree = of_ast rules (failwith "TODO") subtm in
           subtree.size, Right subtree
        | FoundTerm subtm, TerminalName _name
        -> let subtree = of_ast rules current_sort subtm in
           subtree.size, Right subtree

        (* *)
        | FoundBinder, TerminalName name
        | NotFound   , TerminalName name
        -> let lit = M.getExn terminal_rules name in String.length lit, Left lit

        (* if the current token is naming a nonterminal, there has to be a
         * subterm *)
        | FoundBinder, NonterminalName _
        | NotFound   , NonterminalName _
        -> raise BadRules
    )) in
    let size = sum sizes in

    { sort            = current_sort;
      node_type       = Operator;
      leading_trivia  = "";
      trailing_trivia = "";
      children;
      size;
    }

  | _, Nominal.Var name ->
    { sort            = current_sort;
      node_type       = Var;
      leading_trivia  = "";
      trailing_trivia = "";
      children        = [Left name];
      size            = String.length name;
    }

  | SortAp (SortName "sequence", sort), Nominal.Sequence tms ->
    let sizes, children = List.unzip (List.map tms (fun tm ->
      let child = of_ast rules sort tm in
        child.size, Right child)) in
    { sort            = current_sort;
      node_type       = Sequence;
      leading_trivia  = "";
      trailing_trivia = "";
      children;
      size            = sum sizes;
    }

  | SortName "string", Nominal.Primitive (PrimString str) ->
    { sort            = current_sort;
      node_type       = Primitive String;
      leading_trivia  = "";
      trailing_trivia = "";
      children        = [Left str];
      size            = String.length str;
    }

  | SortName "bool", Nominal.Primitive (PrimBool b) ->
    let str = sprintf "%b" b in
    { sort            = current_sort;
      node_type       = Primitive Bool;
      leading_trivia  = "";
      trailing_trivia = "";
      children        = [Left str];
      size            = String.length str;
    }

  | SortName "integer", Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    { sort            = current_sort;
      node_type       = Primitive Integer;
      leading_trivia  = "";
      trailing_trivia = "";
      children        = [Left str];
      size            = String.length str;
    }

let rec to_string { leading_trivia; children; trailing_trivia } =
  let children_str = String.concat "" (List.map children (function
    | Left terminal_capture     -> terminal_capture
    | Right nonterminal_capture -> to_string nonterminal_capture
    ))
  in leading_trivia ^ children_str ^ trailing_trivia

let parse concrete str = failwith "TODO"

let to_ast tree = failwith "TODO"
