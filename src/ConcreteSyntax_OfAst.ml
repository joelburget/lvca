(* Heavily borrowing from "Strictly Pretty" *)
module Nominal = Binding.Nominal
open Types
open ConcreteSyntaxDescription
open! ConcreteSyntax_Private
let find, get_option' = Util.(find, get_option')

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from of_ast when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of BadRules *)
exception CantEmitTokenRegex of string * Regex.t

type mode = Flat | Break

type subterm_result =
  | CapturedTerm   of sort * Nominal.term
  | CapturedBinder of sort * Pattern.t

type box_type =
  | HBox
  | VBox
  | HovBox
  | BBox
  | HvBox

(** how many spaces to break
 * TODO other params from Oppen
 *)
type break_type = int

type box_break_info =
  { box_type : box_type
  ; breakpoints : (int * break_type) list
  }

exception UserError of string
exception NoMatch of string

(*
let sort_of_index : int -> operatorDef -> sort
  = fun tm_ix (OperatorDef (_, Arity (_, valences))) -> valences
    |. Belt.List.get tm_ix
    |> get_option' ("sort_of_index: failed to get term " ^ string_of_int tm_ix)
    |> (function
      FixedValence (_, new_sort) | VariableValence (_, new_sort) -> new_sort
    )

let get_operator : language -> sort_name -> string -> operatorDef
  = fun (Language sorts) sort_name op_name -> sorts
    |. Belt.Map.String.get sort_name
    |> get_option' ("get_operator: failed to get sort" ^ sort_name)
    |> (fun (SortDef (_, operator_defs)) -> operator_defs)
    |> find (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
    |> get_option' ("get_operator: failed to get operator " ^ op_name)
*)

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
  : operator_match_pattern -> Nominal.term -> subterm_result Belt.Map.Int.t
= fun pat tm -> match pat, tm with
  | SingleCapturePattern num, _
  -> Belt.Map.Int.fromArray [| num, CapturedTerm (failwith "TODO: sort", tm) |]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if pat_op_name = op_name && Belt.List.(length body_pats = length body_scopes)
     then body_pats
       |. Belt.List.zipBy body_scopes get_scope_subterms
       |> Util.int_map_unions
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")

(**
 * See get_subterms.
 *
 * raises: NoMatch, UserError *)
and get_scope_subterms
  : numbered_scope_pattern -> Nominal.scope -> subterm_result Belt.Map.Int.t
  = fun (NumberedScopePattern (binder_nums, body_pat)) (Scope (binders, body)) ->
    if Belt.List.(length binder_nums != length binders)
    then raise
      (NoMatch "numbered scope pattern and term scope are of different arity")
    else
      let results1 = binder_nums
        (* TODO: should be patterns coming in on lhs *)
        |. Belt.List.zipBy binders (fun num name -> num, CapturedBinder (failwith "TODO: sort", name))
        |. Belt.List.toArray
        |> Belt.Map.Int.fromArray
      in
      let results2 = get_subterms body_pat body in
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
  : operator_match list list
  -> Nominal.term
  -> operator_match_pattern * nonterminal_token list * subterm_result Belt.Map.Int.t
  = fun matches tm -> matches
    |. Belt.List.flatten
    |. Util.find_by (fun (OperatorMatch op_match) ->
      let pat = op_match.operator_match_pattern in
      try
        Some (pat, op_match.tokens, get_subterms pat tm)
      with
        _ -> None
    )
    |> Util.get_option'
      ("failed to find a rule matching term " ^ Nominal.pp_term' tm)
    |> (fun (op_match, tokens, subterms) ->
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
      op_match, tokens, subterms
    )
;;

let rec pattern_to_tree : sort -> Pattern.t -> doc =
  fun sort -> function
  | Var name
  -> NonterminalDoc ([TerminalDoc (DocText name)], NoInfo)
  | Operator (name, pats) -> NonterminalDoc
    ( Belt.List.map pats
      (pattern_to_tree (failwith "TODO: pattern_to_tree error 1"))
    , NoInfo
    )

  | Sequence pats -> NonterminalDoc
    ( Belt.List.map pats
      (pattern_to_tree (failwith "TODO: pattern_to_tree error 2"))
    , NoInfo
    )
  | Primitive p ->
    (* TODO: what about other integral types? *)
    let prim_ty = match sort with
      | SortAp ("string", [||]) -> String
      | SortAp ("integer", [||]) -> Integer
      | SortAp (sort_name, _)
      -> failwith ("unexpected primitive sort name: " ^ sort_name)
    in
    let str = match p with
      | PrimString str -> str
      | PrimInteger i -> Bigint.to_string i
    in

    TerminalDoc (DocText str)
;;

(** Pretty-print an abstract term to a concrete syntax tree
 * raises: NoMatch, UserError, InvariantViolation, BadRules
 *)
let rec term_to_tree
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> Nominal.term
  -> doc
  = fun lang rules (SortAp (sort_name, _) as current_sort) tm ->
  match current_sort, tm with
  | _, Operator (op_name, scopes)
  -> NonterminalDoc (go_operator lang rules current_sort op_name scopes, NoInfo)
  | _, Var name
  -> NonterminalDoc ([TerminalDoc (DocText name)], NoInfo)
  | SortAp ("sequence", [| sort |]), Sequence tms ->
    let children = Belt.List.map tms (term_to_tree lang rules sort) in
    NonterminalDoc (children, Sequence)
    (* XXX how to format sequences? *)
  | SortAp ("string", [||]), Primitive (PrimString str) ->
    TerminalDoc (DocText str)
  | SortAp ("integer", [||]), Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    TerminalDoc (DocText str)
  | _, _ -> raise (BadSortTerm (current_sort, tm))

(**
 * raises: NoMatch, UserError, InvariantViolation, BadRules
 *)
and go_operator
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> string
  -> Nominal.scope list
  -> doc list
  = fun
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  (SortAp (sort_name, _) as current_sort)
  op_name
  scopes ->

  let SortRule { operator_rules } = sort_rules
    |. Belt.Map.String.get sort_name
    |> get_option' ("term_to_tree: failed to get sort " ^ sort_name)
  in

  let operator_match_pattern, operator_match_tokens, subterms =
    find_operator_match operator_rules (Operator (op_name, scopes))
  in

  let breakpoints : (int * int) list = operator_match_tokens
    |. Belt.List.mapWithIndex (fun ix tok -> match tok with
      | Underscore n -> Some (ix, n)
      | _ -> None
    )
    |. Util.keep_some
  in

  (* Map each token to a subtree. For each token:
     - if it's a space or terminal, print it
     - if it's a nonterminal, look up the subterm (by token number)
  *)
  operator_match_tokens
    (* Go through every token giving it an index. Underscores are not indexed *)
    |. Belt.List.reduce (1, [])
      (fun (ix, indexed_toks) tok -> match tok with
        | Underscore _ -> ix, (0, tok) :: indexed_toks
        | _ -> ix + 1, (ix, tok) :: indexed_toks)
    |> fun (_, lst) -> lst
    |. Belt.List.reverse (* TODO: O(n^2) reverse *)
    |. Belt.List.map (fun (token_ix, token) ->

    match Belt.Map.Int.get subterms token_ix, token with
    (* if the current token is a terminal, and we didn't capture a binder
     * or term, we just emit the contents of the token *)
    | None, TerminalName name ->
      let terminal_rule = terminal_rules
        |. Belt.Map.String.fromArray
        |. Belt.Map.String.get name
        |> get_option' ("term_to_tree: failed to get terminal rule " ^ name)
      in
      (match Regex.is_literal terminal_rule with
      | Some re_str -> TerminalDoc (DocText re_str)
      | None -> raise (CantEmitTokenRegex (name, terminal_rule))
      )
    | Some (CapturedBinder (sort, pattern)), NonterminalName name ->
      pattern_to_tree sort pattern
    (* TODO: this clause seems redundant *)
    | Some (CapturedBinder (sort, Var var_name)), TerminalName terminal_name ->
      TerminalDoc (DocText var_name)

    | Some (CapturedTerm (sort, tm)), NonterminalName new_sort ->
      term_to_tree lang rules sort tm
    | Some (CapturedTerm (sort, tm)), TerminalName name ->
      term_to_tree lang rules sort tm
    | Some (CapturedBinder (_, pattern)), TerminalName name -> raise
      (BadRules
         (Printf.sprintf
            "term_to_tree: binder (%s) found in match pattern %s, terminal \
            name: %s"
            (Pattern.string_of_pattern pattern)
            (string_of_operator_match_pattern operator_match_pattern)
            name))
    | _, Underscore n -> TerminalDoc (DocBreak n)
    | _, OpenBox
    | _, CloseBox -> failwith
      "invariant violation: term_to_tree->go_operator: box hints are filtered \
      out in the previous `keep` stage"
    | None, NonterminalName name -> raise
      (BadRules
         (Printf.sprintf
            "term_to_tree: subterm %n not found in match pattern %s, \
            nonterminal name: %s"
            token_ix
            (string_of_operator_match_pattern operator_match_pattern)
            name))
  )
;;

let coerce_doc_child
  : (terminal_doc, nonterminal_doc) Either.t -> doc
  = function
  | Left doc -> TerminalDoc doc
  | Right doc -> NonterminalDoc doc

type fit_info = Fits of int | DoesntFit

let rec tree_fits : int -> int -> mode -> doc -> fit_info
  = fun max_width start_col mode -> function
    | _ when start_col >= max_width -> DoesntFit
    | TerminalDoc (DocText str) ->
      let len = String.length str in
      if start_col + len < max_width then Fits (start_col + len) else DoesntFit
    | TerminalDoc (DocNest (n, doc)) -> tree_fits max_width n mode (TerminalDoc doc)
    | TerminalDoc (DocBreak size) when mode = Flat
    -> if size < max_width then Fits size else DoesntFit
    | TerminalDoc (DocBreak _)
    -> failwith "impossible"
    | NonterminalDoc (children, _)
    -> group_fits max_width start_col mode children
    | DocGroup group -> group
      |. Belt.List.map coerce_doc_child
      |> group_fits max_width start_col Flat

and group_fits max_width start_col mode children = Util.fold_right
  (fun child rest_fits -> match rest_fits with
    | DoesntFit -> DoesntFit
    | Fits col -> tree_fits max_width col mode child
  )
  children
  (Fits start_col)

type indentation = int

type space =
  | SSpace of int
  | SLine of int

type pre_formatted_nonterminal =
  { children : pre_formatted array
  ; tree_info : tree_info
  (*
  ; sort_name : sort_name
  ; node_type : node_type
  *)
  }

and pre_formatted =
  | Terminal of string
  | Nonterminal of pre_formatted_nonterminal
  | Space of space
  | Group of pre_formatted array

let rec tree_format
  (* takes the starting column, returns the ending column *)
  : int -> indentation -> mode -> doc -> indentation * pre_formatted
  = fun max_width indentation mode -> function
  | TerminalDoc (DocText str)
  -> indentation + String.length str, Terminal str
  | TerminalDoc (DocNest (len, doc))
  -> tree_format max_width (indentation + len) mode (TerminalDoc doc)
  | TerminalDoc (DocBreak len) when mode = Flat
  -> indentation + len, Space (SSpace len)
  | TerminalDoc (DocBreak _) (* when mode = Break *)
  -> indentation, Space (SLine indentation)
  | NonterminalDoc (children, tree_info) ->
    let indentation', children' =
      format_group max_width indentation mode children
    in
    indentation', Nonterminal { children = children'; tree_info }
  | DocGroup group as doc ->
    let mode' = match tree_fits max_width indentation Flat doc with
      | DoesntFit -> Break
      | Fits _ -> Flat
    in
    let indentation', group' = group
      |. Belt.List.map (function
        | Left t_doc -> TerminalDoc t_doc
        | Right nt_doc -> NonterminalDoc nt_doc
      )
      |> format_group max_width indentation mode'
    in
    indentation', Group group'

and format_group max_width indentation mode children =
  let children' = [||] in
  let indentation' = Util.fold_left
    (fun indentation' child ->
      let indentation'', child' =
        tree_format max_width indentation' mode child
      in
      let _ = Js.Array2.push children' child' in
      indentation''
    )
    indentation
    children
  in
  indentation', children'

(* Accumulate all leading trivia for each terminal. So, all whitespace leading
 * back to and including the first newline. *)
let walk_leading_trivia : pre_formatted_nonterminal -> string array
  = fun tree ->

    let accum = ref "" in
    let accumulating = ref false in
    let result = [||] in

    let rec go_nt = fun { children } -> Belt.Array.forEach children go_pft

    and go_pft = (function
      (* Stop accumulating when we hit a token, clear accumulator *)
      | Terminal _ ->
        let _ = Js.Array2.push result !accum in
        accumulating := false;
        accum := ""
      | Space (SSpace n)
      -> if !accumulating then accum := !accum ^ String.make n ' '
      (* Start accumulating when we hit a newline.
       * Invariant relied upon: accum = "" if not accumulating
       *)
      | Space (SLine n) ->
        accumulating := true;
        accum := !accum ^ "\n" ^ String.make n ' '
      | Group children' -> Belt.Array.forEach children' go_pft
      | Nonterminal nt -> go_nt nt
    )
    in

    go_nt tree;
    result

(* Accumulate all trailing trivia up to, but not including the next newline.
 *
 * Important: returns trivia in reverse order!
 *)
let walk_trailing_trivia : pre_formatted_nonterminal -> string array
  = fun tree ->

    let accum = ref "" in
    let result = [||] in

    (* Traverse all children in reverse *)
    let rec go_nt = fun { children } -> children
      |. Belt.Array.reverse
      |. Belt.Array.forEach go_pft

    and go_pft = (function
      | Terminal _ ->
        let _ = Js.Array2.push result !accum in
        accum := ""
      | Space (SSpace n) -> accum := !accum ^ String.make n ' '
      (* Every time we hit a newline, clear the accumulator. *)
      | Space (SLine _) -> accum := ""
      | Group children' -> children'
        |. Belt.Array.reverse
        |. Belt.Array.forEach go_pft
      | Nonterminal nt -> go_nt nt
    )
    in

    go_nt tree;

    result

(* Traverse the pre-formatted tree, normalizing spacing.
 *)
let normalize_nonterminal : pre_formatted_nonterminal -> formatted_tree
  = fun tree ->

    let forward_trivia = tree |. walk_leading_trivia in
    let reverse_trivia = tree |. walk_trailing_trivia |. Belt.Array.reverse in
    let overall_ix = ref 0 in

    let rec go_nt = fun { children; tree_info } ->

      let formatted_tree_children = [||] in

      let rec go = fun children ->
        Belt.Array.forEach children (function
          | Terminal content ->
            let leading_trivia = forward_trivia.(!overall_ix) in
            let trailing_trivia = reverse_trivia.(!overall_ix) in
            overall_ix := !overall_ix + 1;
            let _ = Js.Array2.push formatted_tree_children
              (TerminalCapture { content; leading_trivia; trailing_trivia })
            in ()
          | Space _ -> ()
          | Nonterminal pfnt ->
            let _ = Js.Array2.push formatted_tree_children
              (NonterminalCapture (go_nt pfnt))
            in ()
          | Group group_children -> go group_children
        );
      in

      { children = formatted_tree_children; tree_info }

    in go_nt tree

let of_ast
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> int
  -> Binding.Nominal.term
  -> formatted_tree
  = fun lang desc sort width tm ->
    let doc = term_to_tree lang desc sort tm in
    let _, pre_formatted = tree_format width 0 Flat doc in
    match pre_formatted with
      | Nonterminal pre_formatted_tree
      -> normalize_nonterminal pre_formatted_tree
      | _ -> failwith "invariant violation"
