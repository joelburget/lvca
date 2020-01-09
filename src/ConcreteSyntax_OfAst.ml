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
  | NotFound
  | FoundCapture
  | FoundTerm of int * Nominal.term
  | FoundBinder of Pattern.t

type box_type =
  | HBox
  | VBox
  | HovBox
  | BBox
  | HvBox

type break_type = int (* how many spaces to break TODO other params from Oppen *)

type box_break_info =
  { box_type : box_type
  ; breakpoints : (int * break_type) list
  }

let rec pattern_to_tree : sort_name -> Pattern.t -> nonterminal_doc_child =
  fun sort_name pat ->
  match pat with
  | Var name
  -> NonterminalDoc ([TerminalDoc (DocText name)], sort_name, SingleCapture)
  | Operator (name, pats) -> NonterminalDoc
    ( Belt.List.map pats
      (pattern_to_tree (failwith "TODO: pattern_to_tree error 1"))
    , sort_name
    , Operator name
    )

  | Sequence pats -> NonterminalDoc
    ( Belt.List.map pats
      (pattern_to_tree (failwith "TODO: pattern_to_tree error 2"))
    , sort_name
    , Sequence
    )
  | Primitive p ->
    (* TODO: what about other integral types? *)
    let prim_ty = match sort_name with
      | "string" -> String
      | "integer" -> Integer
      | _ -> failwith ("unexpected primitive sort name: " ^ sort_name)
    in
    let str = match p with
      | PrimString str -> str
      | PrimInteger i -> Bigint.to_string i
    in

    TerminalDoc (DocText str)
;;

let rec find_subtm' slot_num token_ix scopes operator_match_pattern =
  match scopes, operator_match_pattern with
  | _, [] -> NotFound
  | ( Nominal.Scope (binders, body) :: scopes'
    , NumberedScopePattern (binder_nums, body_num) :: pattern_scopes ) ->
    let binder_matches = binders
      |. Belt.List.zip binder_nums
      |> find (fun (_, num) -> num = token_ix)
    in
    (match binder_matches with
     | Some (pat, _ix) -> FoundBinder pat
     | None ->
       if token_ix = body_num
       then FoundTerm (slot_num, body)
       else find_subtm' (slot_num + 1) token_ix scopes' pattern_scopes)
  | _, _ -> failwith "invariant violation: mismatched scopes / term patterns"
;;

(** Find a subterm or binder given a term pattern template and the index of the
    subterm / binder we're looking for. We either (a) don't find it, (b) find a
    term, or (c) find a binder. Example:

    scopes:
      - a. b. c
      - e
         numbered scope patterns:
      - $1. $2. $3
      - $4

    - If we're looking for term $1, we'll return binder a
    - term $4 -> term e
    - term $5 -> not found

    Note that the scopes and numbered scope patterns should mirror each other in
    structure, otherwise an invariant violation may be raised.
*)
let find_subtm
  : int -> Nominal.scope list -> numbered_scope_pattern list -> subterm_result
  = find_subtm' 0
;;

(** Pretty-print an abstract term to a concrete syntax tree
    Raises: InvariantViolation, BadRules
*)
let rec term_to_tree
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> Nominal.term
  -> nonterminal_doc_child
  = fun lang rules (SortAp (sort_name, _) as current_sort) tm ->
  match current_sort, tm with
  | _, Operator (op_name, scopes)
  -> NonterminalDoc (go_operator lang rules current_sort op_name scopes, sort_name, Operator op_name)
  | _, Var name
  -> NonterminalDoc ([TerminalDoc (DocText name)], sort_name, SingleCapture)
  | SortAp ("sequence", [| sort |]), Sequence tms ->
    let children = Belt.List.map tms (term_to_tree lang rules sort) in
    NonterminalDoc (children, sort_name, Sequence)
    (* XXX how to format sequences? *)
  | SortAp ("string", [||]), Primitive (PrimString str) ->
    TerminalDoc (DocText str)
  | SortAp ("integer", [||]), Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    TerminalDoc (DocText str)
  | _, _ -> raise (BadSortTerm (current_sort, tm))

and go_operator
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> string
  -> Nominal.scope list
  -> nonterminal_doc_child list
  =
  fun
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  (SortAp (sort_name, _) as current_sort)
  op_name
  scopes ->

  let (SortRule { operator_rules }) = sort_rules
    |. Belt.Map.String.get sort_name
    |> get_option' ("term_to_tree: failed to get sort " ^ sort_name)
  in

  (* TODO: remove possible exception. possible to have var-only sort? *)
  let OperatorMatch { tokens = operator_match_tokens; operator_match_pattern } =
    find_operator_match operator_rules op_name
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
        | Underscore _ -> (ix, (0, tok) :: indexed_toks)
        | _ -> (ix + 1, (ix, tok) :: indexed_toks))
    |> fun (_, lst) -> lst
    |. Belt.List.reverse (* TODO: O(n^2) reverse *)
    |. Belt.List.map (fun (token_ix, token) ->

    (* Helper to look up (Nominal) subterms by token index (specialized to this
     * operator). See find_subtm. *)
    let get_subtm ix = match operator_match_pattern with
      | SingleCapturePattern _ -> FoundCapture
      | OperatorPattern (_term_name, numbered_scope_patterns) ->
        find_subtm ix scopes numbered_scope_patterns
    in

    match get_subtm token_ix, token with
    | FoundTerm (tm_ix, subtm), NonterminalName sort_name ->
      let SortDef (_, operator_defs) = sorts
        |. Belt.Map.String.get sort_name
        |> get_option' ("term_to_tree: failed to get sort" ^ sort_name)
      in
      let new_sort = operator_defs
        |> find (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
        |> (function
          | Some (OperatorDef (_, Arity (_, valences))) -> valences
          | None -> assert false
        )
        |. Belt.List.get tm_ix
        |> get_option' ("term_to_tree: failed to get term " ^ string_of_int tm_ix)
        |> (function
          FixedValence (_, new_sort) | VariableValence (_, new_sort) -> new_sort
        )
      in
      term_to_tree lang rules new_sort subtm
    | FoundTerm (_tm_ix, subtm), TerminalName _name ->
      term_to_tree lang rules current_sort subtm
    (* if the current token is a terminal, and we didn't capture a binder
     * or term, we just emit the contents of the token *)
    | NotFound, TerminalName name ->
      let terminal_rule = terminal_rules
        |. Belt.Map.String.fromArray
        |. Belt.Map.String.get name
        |> get_option' ("term_to_tree: failed to get terminal rule " ^ name)
      in
      (match Regex.is_literal terminal_rule with
      | Some re_str -> TerminalDoc (DocText re_str)
      | None -> raise (CantEmitTokenRegex (name, terminal_rule))
      )
    | FoundBinder pattern, NonterminalName name ->
      pattern_to_tree sort_name pattern
    | FoundBinder (Var var_name), TerminalName terminal_name ->
      TerminalDoc (DocText var_name)

    (* TODO: we never use FoundCapture successfully! *)
    | FoundCapture, NonterminalName _sort -> raise
      (BadRules "term_to_tree: found a capture matched with a nonterminal token")
    | FoundCapture, TerminalName name ->
      raise (BadRules ("capture found, terminal name: " ^ name))
    | FoundBinder pattern, TerminalName name -> raise
      (BadRules
         (Printf.sprintf
            "term_to_tree: binder (%s) found in match pattern %s, terminal name: %s"
            (Pattern.string_of_pattern pattern)
            (string_of_operator_match_pattern operator_match_pattern)
            name))
    | _, Underscore n -> TerminalDoc (DocBreak n)
    | _, OpenBox
    | _, CloseBox -> failwith
      "invariant violation: term_to_tree->go_operator: underscores are filtered out in the previous `keep` stage"
    | NotFound, NonterminalName name -> raise
      (BadRules
         (Printf.sprintf
            "term_to_tree: subterm %n not found in match pattern %s, nonterminal \
             name: %s"
            token_ix
            (string_of_operator_match_pattern operator_match_pattern)
            name))
  )
;;

let coerce_doc_child
  : (terminal_doc, nonterminal_doc) Either.t -> nonterminal_doc_child
  = function
  | Left doc -> TerminalDoc doc
  | Right doc -> NonterminalDoc doc

type fit_info = Fits of int | DoesntFit

let rec tree_fits : int -> int -> mode -> nonterminal_doc_child -> fit_info
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
    | NonterminalDoc (children, _, _)
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
  ; sort_name : sort_name
  ; node_type : node_type
  }

and pre_formatted =
  | Terminal of string
  | Nonterminal of pre_formatted_nonterminal
  | Space of space
  | Group of pre_formatted array

let rec tree_format
  (* takes the starting column, returns the ending column *)
  : int -> indentation -> mode -> nonterminal_doc_child -> indentation * pre_formatted
  = fun max_width indentation mode -> function
  | TerminalDoc (DocText str)
  -> indentation + String.length str, Terminal str
  | TerminalDoc (DocNest (len, doc))
  -> tree_format max_width (indentation + len) mode (TerminalDoc doc)
  | TerminalDoc (DocBreak len) when mode = Flat
  -> indentation + len, Space (SSpace len)
  | TerminalDoc (DocBreak _) (* when mode = Break *)
  -> indentation, Space (SLine indentation)
  | NonterminalDoc (children, sort_name, node_type) ->
    let indentation', children' =
      format_group max_width indentation mode children
    in
    indentation', Nonterminal { children = children'; sort_name; node_type }
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
let walk_forwards : pre_formatted_nonterminal -> string array
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
 *)
let walk_reverse : pre_formatted_nonterminal -> string array
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

    let forward_trivia = tree |. walk_forwards in
    let reverse_trivia = tree |. walk_reverse |. Belt.Array.reverse in
    let overall_ix = ref 0 in

    let rec go_nt = fun { children; sort_name; node_type } ->

      let children' = [||] in

      Belt.Array.forEach children (function
        | Terminal content ->
          let leading_trivia = forward_trivia.(!overall_ix) in
          let trailing_trivia = reverse_trivia.(!overall_ix) in
          overall_ix := !overall_ix + 1;
          let _ = Js.Array2.push children'
            (TerminalCapture { content; leading_trivia; trailing_trivia })
          in ()
        | Space _ -> ()
        | Nonterminal pfnt ->
          let _ = Js.Array2.push children' (NonterminalCapture (go_nt pfnt))
          in ()
        | Group _ -> failwith "TODO"
      );

      { sort_name; node_type; children = children' }

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
