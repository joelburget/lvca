module Nominal = Binding.Nominal
open Types
open ConcreteSyntaxDescription
open ConcreteSyntax_Private
let find, get_option' = Util.(find, get_option')

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from of_ast when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of BadRules *)
exception CantEmitTokenRegex of string * Regex.t

type subterm_result =
  | NotFound
  | FoundCapture
  | FoundTerm of int * Nominal.term
  | FoundBinder of Pattern.t

(** Sorted (from longer to shorter) array of possible lengths
 * Doesn't include lengths over the limit
 * TODO finish description *)
type possible_lengths = (int * Bitstring.t) array

type break_type = int (* how many spaces to break TODO other params from Oppen *)

type box_type =
  | HBox
  | VBox
  | HovBox
  | BBox
  | HvBox

type box_break_info =
  { box_type : box_type
  ; breakpoints : (int * break_type) list
  }

type break_info =
  | DontBreak
  | DoBreak of box_break_info

let mk_tree sort_name node_type children metadata =
  { sort_name; node_type; children; metadata }

(* Helper for use in of_ast *)
let mk_terminal_capture content =
  TerminalCapture { leading_trivia = ""; content; trailing_trivia = "" }
;;

let rec pattern_to_tree : sort_name -> Pattern.t -> break_info tree =
  fun sort_name pat ->
  match pat with
  | Var name
  -> mk_tree sort_name SingleCapture [| mk_terminal_capture name |] DontBreak
  | Operator (name, pats) -> mk_tree
    sort_name
    (Operator name)
    (pats
     |. Belt.List.toArray
     |. Belt.Array.map (fun pat -> NonterminalCapture
       (pattern_to_tree (failwith "TODO: pattern_to_tree error 1") pat)))
    DontBreak
  | Sequence pats -> mk_tree
    sort_name
    Sequence
    (pats
     |. Belt.List.toArray
     |. Belt.Array.map (fun pat -> NonterminalCapture
       (pattern_to_tree (failwith "TODO: pattern_to_tree error 2") pat)))
    DontBreak
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
    mk_tree sort_name (Primitive prim_ty) [| mk_terminal_capture str |] DontBreak
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
let rec of_ast'
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> Binding.Nominal.term
  -> break_info tree
  (* * possible_lengths *)
  = fun lang rules (SortAp (sort_name, _) as current_sort) tm ->
  match current_sort, tm with
  | _, Operator (op_name, scopes) -> go_operator lang rules current_sort op_name scopes
  | _, Var name
  -> mk_tree sort_name SingleCapture [| mk_terminal_capture name |] DontBreak
  | SortAp ("sequence", [| sort |]), Sequence tms ->
    let children = tms
      |. Belt.List.map (fun tm -> NonterminalCapture (of_ast' lang rules sort tm))
      |. Belt.List.toArray
    in
    (* XXX how to format sequences? *)
    mk_tree sort_name Sequence children DontBreak
  | SortAp ("string", [||]), Primitive (PrimString str) ->
    mk_tree sort_name (Primitive String) [| mk_terminal_capture str |] DontBreak
  | SortAp ("integer", [||]), Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree sort_name (Primitive Integer) [| mk_terminal_capture str |] DontBreak
  | _, _ -> raise (BadSortTerm (current_sort, tm))

and go_operator = fun
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  (SortAp (sort_name, _) as current_sort)
  op_name
  scopes ->

  let (SortRule { operator_rules }) = sort_rules
    |. Belt.Map.String.get sort_name
    |> get_option' ("of_ast': failed to get sort " ^ sort_name)
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

  let m_box_type : box_type option = failwith "TODO" in

  let break_info = match m_box_type with
    | None -> DontBreak
    | Some box_type -> DoBreak { box_type; breakpoints }
  in

  (* Map each token to a subtree. For each token:
     - if it's a space, ignore it
     - if it's a terminal, print it
     - if it's a nonterminal, look up the subterm (by token number)
  *)
  operator_match_tokens
    |. Belt.List.keep (function Underscore _n -> false | _ -> true)
    (* switch from 0- to 1-based indexing *)
    |. Belt.List.mapWithIndex (fun token_ix token -> token_ix + 1, token)
    |. Belt.List.toArray
    |. Belt.Array.map (fun (token_ix, token) ->

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
        |> get_option' ("of_ast': failed to get sort" ^ sort_name)
      in
      let new_sort = operator_defs
        |> find (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
        |> (function
          | Some (OperatorDef (_, Arity (_, valences))) -> valences
          | None -> assert false
        )
        |. Belt.List.get tm_ix
        |> get_option' ("of_ast': failed to get term " ^ string_of_int tm_ix)
        |> (function
          FixedValence (_, new_sort) | VariableValence (_, new_sort) -> new_sort
        )
      in
      NonterminalCapture (of_ast' lang rules new_sort subtm)
    | FoundTerm (_tm_ix, subtm), TerminalName _name ->
      NonterminalCapture (of_ast' lang rules current_sort subtm)
    (* if the current token is a terminal, and we didn't capture a binder
     * or term, we just emit the contents of the token *)
    | NotFound, TerminalName name ->
      let terminal_rule = terminal_rules
        |. Belt.Map.String.fromArray
        |. Belt.Map.String.get name
        |> get_option' ("of_ast': failed to get terminal rule " ^ name)
      in
      (match Regex.is_literal terminal_rule with
      | Some re_str -> mk_terminal_capture re_str
      | None -> raise (CantEmitTokenRegex (name, terminal_rule))
      )
    | FoundBinder pattern, NonterminalName name ->
      NonterminalCapture (pattern_to_tree sort_name pattern)
    | FoundBinder (Var var_name), TerminalName terminal_name ->
      mk_terminal_capture var_name

    (* TODO: we never use FoundCapture successfully! *)
    | FoundCapture, NonterminalName _sort -> raise
      (BadRules "of_ast': found a capture matched with a nonterminal token")
    | FoundCapture, TerminalName name ->
      raise (BadRules ("capture found, terminal name: " ^ name))
    | FoundBinder pattern, TerminalName name -> raise
      (BadRules
         (Printf.sprintf
            "of_ast': binder (%s) found in match pattern %s, terminal name: %s"
            (Pattern.string_of_pattern pattern)
            (string_of_operator_match_pattern operator_match_pattern)
            name))
    | _, Underscore _
    | _, OpenBox
    | _, CloseBox -> failwith
      "invariant violation: of_ast'->go_operator: underscores are filtered out in the previous `keep` stage"
    | NotFound, NonterminalName name -> raise
      (BadRules
         (Printf.sprintf
            "of_ast': subterm %n not found in match pattern %s, nonterminal \
             name: %s"
            token_ix
            (string_of_operator_match_pattern operator_match_pattern)
            name))
  )
  |> (fun children -> mk_tree sort_name (Operator op_name) children break_info)
;;

let of_ast
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> Binding.Nominal.term
  -> unit tree
  = failwith "TODO"
