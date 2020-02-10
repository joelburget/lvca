(* Heavily borrowing from "Strictly Pretty" *)
module Nominal = Binding.Nominal
open Types
open ConcreteSyntaxDescription
open! ConcreteSyntax_Private
let find, get_option' = Util.(find, get_option')
module IntDict = Tablecloth.IntDict
module StrDict = Tablecloth.StrDict
module List = Tablecloth.List

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from [of_ast] when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of [BadRules] *)
exception CantEmitTokenRegex of string * Regex.t

type mode = Flat | Break

(** how many spaces to break
 * TODO other params from Oppen
 *)
type break_type = int

type box_break_info =
  { box_type : box_type
  ; breakpoints : (int * break_type) list
  }

(** Pretty-print an abstract term to a concrete syntax tree
 * raises: [UserError], [InvariantViolation], [BadRules]
 *)
let rec term_to_tree
  : nonterminal_pointer -> ConcreteSyntaxDescription.t -> Nominal.term -> doc
  = fun nonterminal_pointer rules tm ->

    Printf.printf "term_to_tree %s\n" (Nominal.pp_term' tm);

    let { terminal_rules } = rules in

    let NonterminalRule { operator_rules } =
      current_nonterminal nonterminal_pointer
    in

    let form_no, operator_match_pattern, operator_match_tokens, subterms =
      find_operator_match nonterminal_pointer operator_rules tm
    in

    Printf.printf "%s\n" (string_of_operator_match_pattern operator_match_pattern);

    let tree_info = nonterminal_pointer.current_nonterminal, form_no in

    let breakpoints : (int * int) list = operator_match_tokens
      |> Placemat.List.map_with_index ~f:(fun ix tok -> match tok with
        | Underscore n -> Some (ix, n)
        | _ -> None
      )
      |> Util.keep_some
    in

    (* Map each token to a subtree. For each token:
       - if it's a space or terminal, print it
       - if it's a nonterminal, look up the subterm (by token number)
    *)
    operator_match_tokens
      |> index_tokens
      |. List.reverse (* TODO: O(n^2) reverse *)
      |. List.map ~f:(fun (token_ix, token) ->

      Printf.printf "token: %s\n" (string_of_token token);

      match IntDict.get subterms ~key:token_ix, token with

    (* if the current token is a terminal, and we didn't capture a binder
     * or term, we just emit the contents of the token *)
    | None, TerminalName name -> Printf.printf "case 1\n";
      let terminal_rule = terminal_rules
        |. Belt.Map.String.fromArray
        |. StrDict.get ~key:name
        |> get_option' (fun () -> "term_to_tree: failed to get terminal rule " ^ name)
      in
      (match Regex.is_literal terminal_rule with
      | Some re_str -> TerminalDoc (DocText re_str)
      | None -> raise (CantEmitTokenRegex (name, terminal_rule))
      )

    | Some (CapturedBinder (_current_sort, nonterminal_pointer', pat)),
      NonterminalName _nt_name
    -> Printf.printf "case 2\n"; term_to_tree nonterminal_pointer' rules (Nominal.pattern_to_term pat)

    | Some (CapturedTerm (_current_sort, nonterminal_pointer', tm')),
      NonterminalName _nt_name
    -> Printf.printf "case 3\n"; term_to_tree nonterminal_pointer' rules tm'

    | _, Underscore n -> TerminalDoc (DocBreak n)

    | None, NonterminalName nt_name -> failwith (Printf.sprintf
      "term_to_tree: failed to find token $%n (%s)" token_ix nt_name
    )

    (* TODO: other primitives as well *)
    | Some (CapturedTerm (_sort, _nt_ptr, Var v)), TerminalName _t_name
    -> TerminalDoc (DocText v)

    | Some (CapturedBinder _), TerminalName t_name
    | Some (CapturedTerm _), TerminalName t_name -> failwith (Printf.sprintf
      "term_to_tree: unexpectedly directly captured a terminal (%s)" t_name
    )

    | _, OpenBox _
    | _, CloseBox -> failwith
      "invariant violation: term_to_tree->go_operator: box hints are filtered \
      out in the previous `keep` stage"
    )
    |> fun toks -> NonterminalDoc (toks, tree_info)
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
      |> List.map ~f:coerce_doc_child
      |> group_fits max_width start_col Flat

and group_fits max_width start_col mode children = Util.fold_right
  (fun child -> function
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
      |> List.map ~f:(function
        | Either.Left t_doc -> TerminalDoc t_doc
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

    let rec go_nt = fun { children } -> Tablecloth.Array.forEach children ~f:go_pft

    and go_pft = (function
      (* Stop accumulating when we hit a token, clear accumulator *)
      | Terminal _ ->
        let _ = Js.Array2.push result !accum in
        accumulating := false;
        accum := ""
      | Space (SSpace n)
      -> if !accumulating then accum := !accum ^ Caml.String.make n ' '
      (* Start accumulating when we hit a newline.
       * Invariant relied upon: accum = "" if not accumulating
       *)
      | Space (SLine n) ->
        accumulating := true;
        accum := !accum ^ "\n" ^ Caml.String.make n ' '
      | Group children' -> Tablecloth.Array.forEach children' ~f:go_pft
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

    let reverse, forEach = Tablecloth.Array.(reverse, forEach) in

    let accum = ref "" in
    let result = [||] in

    (* Traverse all children in reverse *)
    let rec go_nt = fun { children } -> children
      |> reverse
      |> forEach ~f:go_pft

    and go_pft = function
      | Terminal _ ->
        let _ = Js.Array2.push result !accum in
        accum := ""
      | Space (SSpace n) -> accum := !accum ^ Caml.String.make n ' '
      (* Every time we hit a newline, clear the accumulator. *)
      | Space (SLine _) -> accum := ""
      | Group children' -> children' |> reverse |> forEach ~f:go_pft
      | Nonterminal nt -> go_nt nt
    in

    go_nt tree;

    result

(* Traverse the pre-formatted tree, normalizing spacing.
 *)
let normalize_nonterminal : pre_formatted_nonterminal -> formatted_tree
  = fun tree ->

    let reverse, forEach = Tablecloth.Array.(reverse, forEach) in

    let forward_trivia = tree |> walk_leading_trivia in
    let reverse_trivia = tree |> walk_trailing_trivia |> reverse in
    let overall_ix = ref 0 in

    (* Note that for each nonterminal [go_nt] we create a flat list of children
     * [formatted_tree_children]. Initially empty, it's modified by [go_pft].
     *)
    let rec go_nt
      : pre_formatted_nonterminal -> formatted_tree
      = fun { children; tree_info } ->
      let formatted_tree_children = [||] in
      forEach children ~f:(go_pft formatted_tree_children);
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
      -> forEach group_children ~f:(go_pft formatted_tree_children)

    in go_nt tree

(* raises: [UserError], [InvariantViolation], [BadRules] *)
let of_ast
   : Types.sort_defs
  -> ConcreteSyntaxDescription.t
  -> string
  -> int
  -> Binding.Nominal.term
  -> formatted_tree
  = fun (SortDefs sorts) desc start_nonterminal width tm ->
    let nonterminal_pointer =
      { nonterminals = desc.nonterminal_rules
      ; current_nonterminal = start_nonterminal
      ; bound_sorts = StrDict.empty
      }
    in
    let doc = term_to_tree nonterminal_pointer desc tm in
    let _, pre_formatted = tree_format width 0 Flat doc in
    match pre_formatted with
      | Nonterminal pre_formatted_tree
      -> normalize_nonterminal pre_formatted_tree
      | _ -> failwith "invariant violation"
