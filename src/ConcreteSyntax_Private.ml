open Types
open ConcreteSyntaxDescription

(* type construction_type = Operator of int | Var *)

(** This type lives in [formatted_tree] (ie [formatted_nonterminal_capture]),
 * and holds enough info to produce an AST.
 *
 * We use the nonterminal name and construction number. We use a construction
 * number rather than an operator name, because there could be multiple
 * constructions producing the same operator
 *)
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
 *   back and including the first newline character.
 *
 * In other words, a contiguous stretch of trivia between two tokens is split
 * on the leftmost newline.
*)
and formatted_tree =
  { tree_info : tree_info
  ; children : formatted_capture array
  }

type terminal_doc =
  | DocText of string
  | DocNest of int * terminal_doc
  | DocBreak of int

type nonterminal_doc = doc list * tree_info

and doc_group = (terminal_doc, nonterminal_doc) Either.t list

and doc =
  | TerminalDoc of terminal_doc
  | NonterminalDoc of nonterminal_doc
  | DocGroup of doc_group

(* tree equality mod trivia *)
let rec equivalent : formatted_tree -> formatted_tree -> bool
  = fun t1 t2 ->
  t1.tree_info = t2.tree_info
  && Belt.Array.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 =
  match child1, child2 with
  | TerminalCapture tc1, TerminalCapture tc2 -> tc1.content = tc2.content
  | NonterminalCapture ntc1, NonterminalCapture ntc2 -> equivalent ntc1 ntc2
  | _, _ -> false
;;

(** Points to the current sort among the whole language.
 *)
type nonterminal_pointer =
  { nonterminals : nonterminal_rules
  ; current_nonterminal : string
  ; bound_sorts : sort Belt.Map.String.t
  }

let current_sort : nonterminal_pointer -> sort
  = fun { nonterminals; current_nonterminal; bound_sorts } ->
  match Belt.Map.String.get nonterminals current_nonterminal with
    | None -> failwith "TODO: error"
    | Some (NonterminalRule nonterminal_rule)
    -> let NonterminalType (_, result) = nonterminal_rule.nonterminal_type in
       instantiate_sort bound_sorts result

let current_nonterminal : nonterminal_pointer -> nonterminal_rule
  = fun { nonterminals; current_nonterminal } ->
    match Belt.Map.String.get nonterminals current_nonterminal with
      | None -> failwith "TODO: error"
      | Some nt -> nt

(* TODO: move with binding *)
let move_to : nonterminal_pointer -> string -> nonterminal_pointer
  = fun { nonterminals; current_nonterminal; bound_sorts } nt_name ->
    { nonterminals
    ; current_nonterminal = nt_name
    ; bound_sorts = Belt.Map.String.empty
    }

type subterm_result =
  | CapturedTerm   of sort * nonterminal_pointer * Binding.Nominal.term
  | CapturedBinder of sort * nonterminal_pointer * Pattern.t

exception UserError of string
exception NoMatch of string

(**
 * Retrieve all the numbered subterms / binders for a term pattern template (eg
 * `foo($1. bar($2); $3)`). Given the term `foo(x. bar(baz()); y)`, this returns
 *
 * 1 -> x
 * 2 -> baz()
 * 3 -> y
 *
 * Invariants assumed:
 * - Scopes and numbered scope patterns mirror each other (are the same length).
 * - Each numbering in the pattern is unique
 * - The pattern is numbered contiguously from 1 to some n >= 1.
 *
 * raises: NoMatch, UserError
 *)
let rec get_subterms
  :  nonterminal_token list
  -> nonterminal_pointer
  -> operator_match_pattern
  -> Binding.Nominal.term
  -> subterm_result Belt.Map.Int.t
= fun tokens nonterminal_pointer pat tm -> match pat, tm with
  | SingleCapturePattern num, _
  -> let tok = tokens
       |. Belt.List.get num
       |> Util.get_option' "TODO"
     in
     let nt_name = match tok with
       | NonterminalName nt_name -> nt_name
       | _ -> failwith "TODO: error"
     in
     let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
     let capture = CapturedTerm
       (current_sort nonterminal_pointer, nonterminal_pointer', tm)
     in
     Belt.Map.Int.fromArray [| num, capture |]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if pat_op_name = op_name && Belt.List.(length body_pats = length body_scopes)
     then body_pats
       |. Belt.List.zipBy body_scopes
         (get_scope_subterms tokens nonterminal_pointer)
       |> Util.int_map_unions
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")
  | OperatorPattern _, _
  -> raise (NoMatch "operator pattern and value don't match")

(**
 * See get_subterms.
 *
 * raises: NoMatch, UserError *)
and get_scope_subterms
  :  nonterminal_token list
  -> nonterminal_pointer
  -> numbered_scope_pattern
  -> Binding.Nominal.scope
  -> subterm_result Belt.Map.Int.t
  = fun tokens nonterminal_pointer
    (NumberedScopePattern (binder_nums, body_pat)) (Scope (binders, body)) ->
    if Belt.List.(length binder_nums != length binders)
    then raise
      (NoMatch "numbered scope pattern and term scope are of different arity")
    else
      let results1 = binder_nums
        (* TODO: should be patterns coming in on lhs *)
        |. Belt.List.zipBy binders (fun num name ->
          let tok = tokens
            |. Belt.List.get num
            |> Util.get_option' "TODO"
          in
          let nt_name = match tok with
            | NonterminalName nt_name -> nt_name
            | _ -> failwith "TODO: error"
          in
          let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
          let capture = CapturedBinder
            (current_sort nonterminal_pointer, nonterminal_pointer', name)
          in
          num, capture
        )
        |. Belt.List.toArray
        |> Belt.Map.Int.fromArray
      in
      let results2 = get_subterms tokens nonterminal_pointer body_pat body in
      Belt.Map.Int.merge results1 results2 (fun k v1 v2 -> match v1, v2 with
        | Some _, Some _ -> raise
          (UserError (Printf.sprintf "duplicate token capture: $%n" k))
        | Some v, None
        | None, Some v -> Some v
        | None, None -> failwith
          "invariant violation: no value on either side of a union"
      )

(**
 * Find a matching syntactical description for the given term. This traverses
 * the set of possible forms from top to bottom until it finds one that
 * matches.
 *
 * Invariants assumed:
 * - Each pattern is numbered contiguously from 1 to n, where n is the number
 * of nonterminal tokens associated with the pattern.
 *
 * Example:
 *
 * {[
 * foo :=
 *   | STRING ARROW foo { bar($1. $2) }
 *   | INTEGER { lit(integer($1)) }
 *   | TRUE { true() }
 * ]}
 *
 * The term `bar(x. true())` would match the first form, returning:
 * 1 -> [CapturedBinder ... ...]
 * 1 -> [CapturedBinder ... ...]
*)
let find_operator_match
  :  nonterminal_pointer
  -> operator_match list list
  -> Binding.Nominal.term
  -> int * operator_match_pattern * nonterminal_token list * subterm_result Belt.Map.Int.t
  = fun nonterminal_pointer matches tm -> matches
    |. Belt.List.flatten
    |. Belt.List.mapWithIndex (fun i x -> i, x)
    |. Util.find_by (fun (i, OperatorMatch op_match) ->
      let pat = op_match.operator_match_pattern in
      let tokens = op_match.tokens in
      try
        Some (i, pat, tokens, get_subterms tokens nonterminal_pointer pat tm)
      with
        _ -> None
    )
    |> Util.get_option'
      ("failed to find a rule matching term " ^ Binding.Nominal.pp_term' tm)
    |> (fun (i, op_match, tokens, subterms) ->
      let num_nonterminal_tokens = tokens
        |. Belt.List.keep (function
          | NonterminalName _ -> true
          | _ -> false
        )
        |. Belt.List.length
      in
      for i = 0 to num_nonterminal_tokens do
        if not (Belt.Map.Int.has subterms i)
        then failwith (Printf.sprintf "error: key missing in pattern: $%n" i)
      done;
      i, op_match, tokens, subterms
    )
;;
