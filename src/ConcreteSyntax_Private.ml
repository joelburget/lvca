open Tablecloth
open Types
open ConcreteSyntaxDescription

let invariant_violation = Util.invariant_violation

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

(** Pretty-printing declarations that have no children.
 *
 * Perhaps "atomic" is a better word than "terminal".
 *)
type terminal_doc =
  | DocText of string
  | DocNest of int * terminal_doc
  | DocBreak of int

type nonterminal_doc = doc list * tree_info

and doc_group = (terminal_doc, nonterminal_doc) Either.t list

(** Pretty-printing declarations with children.
 *)
and doc =
  | TerminalDoc of terminal_doc
  | NonterminalDoc of nonterminal_doc
  | DocGroup of doc_group

(** tree equality mod trivia *)
let rec equivalent : formatted_tree -> formatted_tree -> bool
  = fun t1 t2 ->
  t1.tree_info = t2.tree_info
  && Array.all (Array.map2 t1.children t2.children ~f:equivalent') ~f:(fun b -> b)

and equivalent' child1 child2 =
  match child1, child2 with
  | TerminalCapture tc1, TerminalCapture tc2 -> tc1.content = tc2.content
  | NonterminalCapture ntc1, NonterminalCapture ntc2 -> equivalent ntc1 ntc2
  | _, _ -> false
;;

let rec to_string : formatted_tree -> string
  = fun { children } -> children
  |> Array.map ~f:(function
    | TerminalCapture { leading_trivia; content; trailing_trivia } ->
      leading_trivia ^ content ^ trailing_trivia
    | NonterminalCapture nonterminal_capture -> to_string nonterminal_capture)
  |> Array.to_list
  |> Caml.String.concat ""
;;

let string_of_tree_info : tree_info -> string
  = fun (name, i) -> Printf.sprintf "%s:%n" name i
;;

let rec to_debug_string : formatted_tree -> string
  = fun { children; tree_info } -> children
  |> Array.map ~f:string_of_formatted_capture
  |. Js.Array2.joinWith ""
  |> Printf.sprintf "%s(%s)" (string_of_tree_info tree_info)

and string_of_formatted_capture = function
  | TerminalCapture { leading_trivia; content; trailing_trivia }
  -> "t:\"" ^ leading_trivia ^ content ^ trailing_trivia ^ "\""
  | NonterminalCapture nonterminal_capture
  -> "nt:" ^ to_debug_string nonterminal_capture
;;

(** Points to the current sort among the whole language.
 *)
type nonterminal_pointer =
  { nonterminals : nonterminal_rules
  ; current_nonterminal : string
  ; bound_sorts : sort StrDict.t
  }

let current_sort : nonterminal_pointer -> sort
  = fun { nonterminals; current_nonterminal; bound_sorts } ->
  match StrDict.get nonterminals ~key:current_nonterminal with
    | None -> invariant_violation (Printf.sprintf
      "current_sort: didn't find current nonterminal %s in set of nonterminals"
      current_nonterminal
    )
    | Some (NonterminalRule nonterminal_rule)
    -> let NonterminalType (_, result) = nonterminal_rule.nonterminal_type in
       instantiate_sort bound_sorts result

let current_nonterminal : nonterminal_pointer -> nonterminal_rule
  = fun { nonterminals; current_nonterminal } ->
    match StrDict.get nonterminals ~key:current_nonterminal with
      | None -> invariant_violation (Printf.sprintf
        "current_nonterminal: didn't find current nonterminal %s in set of \
         nonterminals"
        current_nonterminal
      )
      | Some nt -> nt

(* TODO: move with binding *)
let move_to : nonterminal_pointer -> string -> nonterminal_pointer
  = fun { nonterminals; bound_sorts } nt_name ->
    { nonterminals
    ; current_nonterminal = nt_name
    ; bound_sorts = StrDict.empty
    }

type subpattern_result =
  CapturedPattern of sort * nonterminal_pointer * Pattern.t

type subterm_result =
  | CapturedTerm   of sort * nonterminal_pointer * Binding.Nominal.term
  | CapturedBinder of sort * nonterminal_pointer * Pattern.t

exception UserError of string
exception NoMatch of string

(** Go through every token giving it an index. Underscores and boxes are all
 * indexed 0, terminals and nonterminals are indexed from 1.
 *)
let index_tokens : nonterminal_token list -> (int * nonterminal_token) list
  = fun tokens -> tokens
    |. Placemat.List.fold_left
      ~initial:(1, [])
      ~f:(fun (ix, indexed_toks) tok -> match tok with
        | Underscore _
        | OpenBox _
        | CloseBox
        -> ix, (0, tok) :: indexed_toks
        | _
        -> ix + 1, (ix, tok) :: indexed_toks)
    |> fun (_, lst) -> lst

(** Go through every token giving it an index. Underscores and boxes are not
 * indexed *)
let map_index_tokens
  : nonterminal_token list -> nonterminal_token IntDict.t
  = fun tokens -> tokens
    |> index_tokens
    |> IntDict.from_list
    |. Placemat.IntDict.remove 0

let string_of_op_match_line
  : nonterminal_token list -> operator_match_pattern -> string
  = fun tokens op_match_pat -> Printf.sprintf "%s { ... %s ... }"
    (string_of_tokens tokens)
    (string_of_operator_match_pattern op_match_pat)

let string_of_op_match_line'
  : nonterminal_token list -> numbered_scope_pattern -> string
  = fun tokens op_match_pat -> Printf.sprintf "%s { ... %s ... }"
    (string_of_tokens tokens)
    (string_of_numbered_scope_pattern op_match_pat)

let rec get_subpatterns
  :  nonterminal_token list
  -> nonterminal_pointer
  -> operator_match_pattern
  -> Pattern.t
  -> subpattern_result IntDict.t
  = fun tokens nonterminal_pointer op_match_pat pat -> match op_match_pat, pat with
  | SingleCapturePattern num, _
  -> let tok = tokens
       |> map_index_tokens
       |> IntDict.get ~key:num
       (* TODO: change to InvariantViolation if we validate this *)
       |> Util.get_option (UserError (Printf.sprintf
         "Couldn't find token $%n in tokens: %s"
         num
         (string_of_op_match_line tokens op_match_pat)
       ))
     in
     let nt_name = match tok with
       | NonterminalName nt_name -> nt_name
       | TerminalName t_name -> raise (UserError (Printf.sprintf
         "Token $%n captures a nonterminal (%s) directly, but only \
          nonterminals can be captured and put in an AST: %s"
         num t_name (string_of_op_match_line tokens op_match_pat)
       ))
       | Underscore _ | OpenBox _ | CloseBox
       -> invariant_violation "boxes and underscores are not indexed"
     in
     let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
     let capture = CapturedPattern
       (current_sort nonterminal_pointer, nonterminal_pointer', pat)
     in
     IntDict.from_list [ num, capture ]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if pat_op_name = op_name && List.(length body_pats = length body_scopes)
     then body_pats
       |. Placemat.List.zipBy body_scopes
         (fun (NumberedScopePattern (caps, body_pat)) ->
           if List.length caps > 0 then failwith "TODO: error 6";
           get_subpatterns tokens nonterminal_pointer body_pat)
       |> Util.int_map_unions
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")
  | OperatorPattern _, _
  -> raise (NoMatch "operator pattern and value don't match")

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
 * raises: [NoMatch], [UserError], [InvariantViolation]
 *)
let rec get_subterms
  :  nonterminal_token list
  -> nonterminal_pointer
  -> operator_match_pattern
  -> Binding.Nominal.term
  -> subterm_result IntDict.t
  = fun tokens nonterminal_pointer op_match_pat tm -> match op_match_pat, tm with
  | SingleCapturePattern num, _
  -> let tok = tokens
       |> map_index_tokens
       |> IntDict.get ~key:num
       (* TODO: change to InvariantViolation if we validate this *)
       |> Util.get_option (UserError (Printf.sprintf
         "Couldn't find token $%n in tokens: %s"
         num
         (string_of_op_match_line tokens op_match_pat)
       ))
     in
     let nt_name = match tok with
       | NonterminalName nt_name -> nt_name
       | TerminalName t_name -> raise (UserError (Printf.sprintf
         "Token $%n captures a nonterminal (%s) directly, but only \
          nonterminals can be captured and put in an AST: %s"
         num t_name (string_of_op_match_line tokens op_match_pat)
       ))
       | Underscore _ | OpenBox _ | CloseBox
       -> invariant_violation "boxes and underscores are not indexed"
     in
     let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
     let capture = CapturedTerm
       (current_sort nonterminal_pointer, nonterminal_pointer', tm)
     in
     IntDict.from_list [ num, capture ]
  | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern num)])
  , Var name
  -> let tok = tokens
       |> map_index_tokens
       |. IntDict.get ~key:num
       (* TODO: change to InvariantViolation if we validate this *)
       |> Util.get_option (UserError (Printf.sprintf
         "Couldn't find token $%n in tokens: %s"
         num
         (string_of_op_match_line tokens op_match_pat)
       ))
     in
     let t_name = match tok with
       | TerminalName t_name -> t_name
       | NonterminalName nt_name -> raise (UserError (Printf.sprintf
         "var must capture a terminal, found nonterminal %s: %s"
         nt_name (string_of_op_match_line tokens op_match_pat)
       ))
       | Underscore _ | OpenBox _ | CloseBox
       -> invariant_violation "boxes and underscores are not indexed"
     in
     let capture = CapturedTerm
       (current_sort nonterminal_pointer, nonterminal_pointer, Var name)
     in
     IntDict.from_list [ num, capture ]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if pat_op_name = op_name && List.(length body_pats = length body_scopes)
     then (body_pats
       |. Placemat.List.zipBy body_scopes
         (get_scope_subterms tokens nonterminal_pointer)
       |> Util.int_map_unions
     )
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")
  | OperatorPattern _, _
  -> raise (NoMatch "operator pattern and value don't match")

(**
 * See get_subterms.
 *
 * raises: [NoMatch], [UserError], [InvariantViolation] *)
and get_scope_subterms
  :  nonterminal_token list
  -> nonterminal_pointer
  -> numbered_scope_pattern
  -> Binding.Nominal.scope
  -> subterm_result IntDict.t
  = fun tokens nonterminal_pointer
    (NumberedScopePattern (numbered_patterns, body_pat) as ns_pat)
    (Scope (term_patterns, body)) ->
      (*
    Printf.printf "trying to match %n / %n binders\n"
      (List.length numbered_patterns)
      (List.length term_patterns) ;
      *)
    if List.(length numbered_patterns != length term_patterns)
    then raise
      (NoMatch "numbered scope pattern and term scope have different arity")
    else
      let pattern_bindings = numbered_patterns
        |. Placemat.List.zipBy term_patterns (fun numbered_pat term_pat ->
          let captured_token_num = match numbered_pat with
            | VarCapture num
            | PatternCapture num -> num
          in
          let tok = tokens
            |> map_index_tokens
            |. IntDict.get ~key:captured_token_num
            (* TODO: change to InvariantViolation if we validate this *)
            |> Util.get_option (UserError (Printf.sprintf
              "Couldn't find token $%n in tokens: %s"
              captured_token_num
              (string_of_op_match_line' tokens ns_pat)
            ))
          in
          let nt_name = match tok with
            | NonterminalName nt_name -> nt_name
            | TerminalName t_name -> raise (UserError (Printf.sprintf
              "Token $%n captures a nonterminal (%s) directly, but only \
               nonterminals can be captured and put in an AST: %s"
              captured_token_num t_name (string_of_op_match_line' tokens ns_pat)
            ))
            | Underscore _ | OpenBox _ | CloseBox
            -> invariant_violation "boxes and underscores are not indexed"
          in
          let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
          let capture = CapturedBinder
            (current_sort nonterminal_pointer, nonterminal_pointer', term_pat)
          in
          captured_token_num, capture
        )
        |> IntDict.from_list
      in
      (* Printf.printf "trying to match body\n"; *)
      let body_bindings = get_subterms tokens nonterminal_pointer body_pat body in
      (* Printf.printf "matched body\n"; *)
      IntDict.merge pattern_bindings body_bindings ~f:(fun k v1 v2 ->
        match v1, v2 with
        | Some _, Some _ -> raise
          (UserError (Printf.sprintf "duplicate token capture: $%n" k))
        | Some v, None
        | None, Some v -> Some v
        | None, None -> failwith
          "invariant violation: no value on either side of a union"
      )

(** Check that nonterminal mentioned in tokens appears in the subterm mapping.
 *)
let check_tokens subterms tokens : unit = tokens
  |> index_tokens
  |> Array.from_list
  |> Array.forEach ~f:(fun (tok_ix, tok) ->
    match tok with
    | NonterminalName _ ->
      if not (Placemat.IntDict.has subterms tok_ix)
      then (
        let available_keys = subterms
          |> IntDict.keys
          |> Util.stringify_list string_of_int ", "
        in
        failwith (Printf.sprintf
          "error: key missing in pattern: $%n. available keys: %s"
          tok_ix available_keys)
      )
    | _ -> ()
  )

(**
 * Find a matching syntactical description for the given term. This traverses
 * the set of possible forms from bottom to top until it finds one that
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
 *
 * raises: [InvariantViolation], [UserError]
*)
let find_operator_match
  :  nonterminal_pointer
  -> operator_match list list
  -> Binding.Nominal.term
  -> int *
     operator_match_pattern *
     nonterminal_token list *
     subterm_result IntDict.t
  = fun nonterminal_pointer matches tm ->

    matches
      |> List.flatten
      |> Placemat.List.map_with_index ~f:(fun match_ix x -> match_ix, x)
      |> List.reverse (* TODO: O(n^2) reverse *)
      |. Util.find_by (fun (match_ix, OperatorMatch op_match) ->
        let { operator_match_pattern = pat; tokens } = op_match in
        try
          (* TODO: preprocess tokens so we don't have to create a map each time *)
          Some (match_ix, pat, tokens, get_subterms tokens nonterminal_pointer pat tm)
        with
          NoMatch _ -> None
      )
      |> Util.get_option' (fun () ->
        "failed to find a rule matching term " ^ Binding.Nominal.pp_term' tm)
      |> (fun ((_, _, tokens, subterms) as result) ->
        check_tokens subterms tokens;
        result
      )

(** See [find_operator_match]
 * raises: [InvariantViolation], [UserError]
 *)
let find_pat_operator_match
  :  nonterminal_pointer
  -> operator_match list list
  -> Pattern.t
  -> int
   * operator_match_pattern * nonterminal_token list
   * subpattern_result IntDict.t
  = fun nonterminal_pointer matches pattern -> matches
    |. List.flatten
    |. Placemat.List.map_with_index ~f:(fun match_ix x -> match_ix, x)
    |. Util.find_by (fun (match_ix, OperatorMatch op_match) ->
      let { operator_match_pattern = pat; tokens } = op_match in
      try
        Some (match_ix, pat, tokens, get_subpatterns tokens nonterminal_pointer pat pattern)
      with
        NoMatch _ -> None
    )
    |> Util.get_option' (fun () ->
      "failed to find a rule matching pattern " ^ Pattern.string_of_pattern pattern)
    |> (fun ((_, _, tokens, subterms) as result) ->
      check_tokens subterms tokens;
      result
    )
;;
