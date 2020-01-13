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

let rec instantiate : sort Belt.Map.String.t -> sort -> sort
  = fun arg_mapping sort -> match sort with
      | SortAp (name, [||]) -> (match Belt.Map.String.get arg_mapping name with
        | None -> sort
        | Some sort' -> sort')
      | SortAp (name, args) ->
        let args' = Belt.Array.map args (instantiate arg_mapping) in
        SortAp (name, args')

let subsorts : language_pointer -> string -> language_pointer list
  = fun { sorts; current_sort } operator_name ->
    let SortAp (sort_name, arg_values) = current_sort in
    let SortDef (arg_names, operator_defs) = Belt.Map.String.get sorts sort_name
      |> get_option' ("sorts must hold " ^ sort_name)
    in
    let OperatorDef (_, arity) = operator_defs
      |. Belt.List.getBy (fun (OperatorDef (name, _)) -> name = operator_name)
      |> get_option' (Printf.sprintf "operator %s must be present" operator_name)
    in
    let args = Belt.Array.zip (Belt.List.toArray arg_names) arg_values
      |. Belt.Map.String.fromArray
    in
    let Arity (variable_names, valences) = arity in
    valences
      |. Belt.List.map (function
        | VariableValence _
        -> failwith "TODO: implement variable valence subsorts"
        | FixedValence (sort_args, sort)
        -> if Belt.List.length sort_args > 0
           then failwith "TODO: implement fixed valence w/ sort args subsorts"
           else
             let current_sort = instantiate args sort in
             { sorts; current_sort }
      )

  (* | Arity of string list * valence list *)
  (* | SortDef of string list * operatorDef list *)

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

let rec pattern_to_tree : language_pointer -> Pattern.t -> doc =
  fun ({current_sort = sort} as language_pointer) -> function
  | Var name
  -> NonterminalDoc ([TerminalDoc (DocText name)], SortConstruction (sort, Var))
  | Operator (name, pats) -> NonterminalDoc
    ( Belt.List.zipBy (subsorts language_pointer name) pats pattern_to_tree
    , SortConstruction (sort, Operator name)
    )
  | Sequence pats ->
    let subsort = failwith "TODO" in
    NonterminalDoc
    ( Belt.List.map pats (pattern_to_tree subsort)
    , Sequence
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

    NonterminalDoc ([TerminalDoc (DocText str)], Primitive prim_ty)
;;

(** Pretty-print an abstract term to a concrete syntax tree
 * raises: NoMatch, UserError, InvariantViolation, BadRules
 *)
let rec term_to_tree
  : language_pointer -> ConcreteSyntaxDescription.t -> Nominal.term -> doc
  = fun ({sorts; current_sort} as language_pointer) rules tm ->
  match current_sort, tm with
  | _, Operator (op_name, scopes)
  -> NonterminalDoc
    ( go_operator language_pointer rules op_name scopes
    , SortConstruction (current_sort, Operator op_name)
    )
  | _, Var name
  -> NonterminalDoc
    ( [TerminalDoc (DocText name)]
    , SortConstruction (current_sort, Var)
    )
  | SortAp ("sequence", [| sort |]), Sequence tms ->
    let language_pointer' = { sorts; current_sort = sort } in
    let children = Belt.List.map tms (term_to_tree language_pointer' rules) in
    NonterminalDoc (children, Sequence)
    (* XXX how to format sequences? *)
  | SortAp ("string", [||]), Primitive (PrimString str) ->
    NonterminalDoc ([TerminalDoc (DocText str)], Primitive String)
  | SortAp ("integer", [||]), Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    NonterminalDoc ([TerminalDoc (DocText str)], Primitive Integer)
  | _, _ -> raise (BadSortTerm (current_sort, tm))

(**
 * raises: NoMatch, UserError, InvariantViolation, BadRules
 *)
and go_operator
   : language_pointer
  -> ConcreteSyntaxDescription.t
  -> string
  -> Nominal.scope list
  -> doc list
  = fun
  ({ sorts; current_sort = SortAp (sort_name, _) } as language_pointer)
  ({ terminal_rules; sort_rules } as rules)
  op_name
  scopes ->

  let SortRule { operator_rules } = sort_rules
    |. Belt.Map.String.get sort_name
    |> get_option' ("term_to_tree: failed to get sort " ^ sort_name)
  in

  let operator_match_pattern, operator_match_tokens, subterms =
    find_operator_match language_pointer operator_rules (Operator (op_name, scopes))
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
    | Some (CapturedBinder (current_sort, pattern)), NonterminalName _name ->
      let language_pointer' = { sorts; current_sort } in
      pattern_to_tree language_pointer' pattern

    (* I'm leaving this here for the moment, in case I'm missing something and
     * it was here for a good reason, but it seems subsumed by the above *)
    (*
    | Some (CapturedBinder (sort, Var var_name)), TerminalName terminal_name ->
      TerminalDoc (DocText var_name)
    *)

    | Some (CapturedTerm (current_sort, tm)), NonterminalName _new_sort ->
      let language_pointer' = { sorts; current_sort } in
      term_to_tree language_pointer' rules tm

    (* I'm leaving this here for the moment, in case I'm missing something and
     * it was here for a good reason, but it seems subsumed by the above *)
    (*
    | Some (CapturedTerm (current_sort, tm)), TerminalName _name ->
      let language_pointer' = { sorts; current_sort } in
      term_to_tree language_pointer' rules tm
      *)

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

    let reverse, forEach = Belt.Array.(reverse, forEach) in

    let accum = ref "" in
    let result = [||] in

    (* Traverse all children in reverse *)
    let rec go_nt = fun { children } -> children |. reverse |. forEach go_pft

    and go_pft = function
      | Terminal _ ->
        let _ = Js.Array2.push result !accum in
        accum := ""
      | Space (SSpace n) -> accum := !accum ^ String.make n ' '
      (* Every time we hit a newline, clear the accumulator. *)
      | Space (SLine _) -> accum := ""
      | Group children' -> children' |. reverse |. forEach go_pft
      | Nonterminal nt -> go_nt nt
    in

    go_nt tree;

    result

(* Traverse the pre-formatted tree, normalizing spacing.
 *)
let normalize_nonterminal : pre_formatted_nonterminal -> formatted_tree
  = fun tree ->

    let reverse, forEach = Belt.Array.(reverse, forEach) in

    let forward_trivia = tree |. walk_leading_trivia in
    let reverse_trivia = tree |. walk_trailing_trivia |. reverse in
    let overall_ix = ref 0 in

    (* Note that for each nonterminal [go_nt] we create a flat list of children
     * [formatted_tree_children]. Initially empty, it's modified by [go_pft].
     *)
    let rec go_nt
      : pre_formatted_nonterminal -> formatted_tree
      = fun { children; tree_info } ->
      let formatted_tree_children = [||] in
      forEach children (go_pft formatted_tree_children);
      { children = formatted_tree_children; tree_info }

    and go_pft
      : formatted_capture array -> pre_formatted -> unit
      = fun formatted_tree_children -> function
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
      | Group group_children
      -> forEach group_children (go_pft formatted_tree_children)

    in go_nt tree

let of_ast
   : Types.language
  -> ConcreteSyntaxDescription.t
  -> Types.sort
  -> int
  -> Binding.Nominal.term
  -> formatted_tree
  = fun (Language sorts) desc current_sort width tm ->
    let language_pointer = { sorts; current_sort } in
    let doc = term_to_tree language_pointer desc tm in
    let _, pre_formatted = tree_format width 0 Flat doc in
    match pre_formatted with
      | Nonterminal pre_formatted_tree
      -> normalize_nonterminal pre_formatted_tree
      | _ -> failwith "invariant violation"
