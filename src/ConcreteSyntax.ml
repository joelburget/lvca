module Result = Belt.Result
open Binding
open Either
open Types
open Types.ConcreteSyntaxDescription
let (find, sum, traverse_list_result) = Util.(find, sum, traverse_list_result)

let sprintf = Printf.sprintf

type prim_ty =
  | Integer
  | String

type node_type =
  | Operator  of string
  | Var
  | Sequence
  | Primitive of prim_ty

type    terminal_capture = string
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
  { sort            : sort;
    node_type       : node_type;
    leading_trivia  : string;
    trailing_trivia : string;
    children        : (terminal_capture, nonterminal_capture) either list;
    size            : int;
  }

let rec equivalent t1 t2 =
  t1.sort = t2.sort &&
  t1.node_type = t2.node_type &&
  Belt.List.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 = match child1, child2 with
  | Left   tc1, Left   tc2 -> tc1 = tc2
  | Right ntc1, Right ntc2 -> equivalent ntc1 ntc2
  | _, _ -> false

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
  | FoundTerm   of int * Nominal.term
  | FoundBinder of string

(* Find a subterm or binder given a term pattern template and the index of the
 * subterm / binder we're looking for *)
let rec find_subtm' slot_num token_ix scopes term_pattern
  = match scopes, term_pattern with
    | _, [] -> NotFound
    | Nominal.Scope (binders, body) :: scopes', NumberedScopePattern (binder_nums, body_num) :: pattern_scopes
    -> (match Belt.List.zip binders binder_nums |> find (fun (_, num) -> num = token_ix) with
      | Some (name, _ix) -> FoundBinder name
      | None -> if token_ix = body_num
        then FoundTerm (slot_num, body)
        else find_subtm' (slot_num + 1) token_ix scopes' pattern_scopes
      )
    | _, _ -> failwith "invariant violation: mismatched scopes / term patterns"

let find_subtm
  : int -> Nominal.scope list -> numbered_scope_pattern list -> subterm_result
  = find_subtm' 0

exception BadRules of string

(** raised from of_ast when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of BadRules *)
exception CantEmitTokenRegex of string * regex

let regex_is_literal : regex -> string option = function
  | [ReString str] -> Some str
  | _              -> None

let rec regex_piece_to_string : regex_piece -> string = function
  | ReString str   -> "\"" ^ str ^ "\""
  | ReSet    str   -> str
  | ReStar   piece -> regex_piece_to_string piece ^ "*"
  | RePlus   piece -> regex_piece_to_string piece ^ "+"
  | ReOption piece -> regex_piece_to_string piece ^ "?"

let regex_to_string : regex -> string = fun re_parts -> re_parts
  |> List.map regex_piece_to_string
  |> String.concat ""

let mk_tree sort node_type children size =
  { sort;
    node_type;
    leading_trivia  = "";
    trailing_trivia = "";
    children;
    size;
  }

(* TODO: handle non-happy cases *)
let rec of_ast (Language sorts as lang) ({ terminal_rules; sort_rules } as rules) current_sort tm
  = match current_sort, tm with
  | SortAp (sort_name, _), Nominal.Operator (op_name, scopes) ->
    let SortRule { operator_rules } = M.getExn sort_rules sort_name in
    (* TODO: remove possible exception. possible to have var-only sort? *)
    let OperatorMatch { tokens; term_pattern }
      = find_operator_match operator_rules op_name
    in
    let _term_name, numbered_scope_patterns = term_pattern in

    (* Map each token to a subtree *)
    let sizes, children = Belt.List.unzip (Belt.List.mapWithIndex tokens (fun token_ix token ->
      let token_ix' = token_ix + 1 in (* switch from 0- to 1-based indexing *)
      match find_subtm token_ix' scopes numbered_scope_patterns, token with

        | FoundTerm (tm_ix, subtm), NonterminalName sort
        -> let SortDef (_, operator_defs) = M.getExn sorts sort in
           let Some (OperatorDef (_, Arity (_, valences))) = find
             (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
             operator_defs
           in
           let valence = Belt.List.getExn valences tm_ix in
           let new_sort = (match valence with
             | FixedValence    (_, new_sort)
             | VariableValence (_, new_sort)
             -> new_sort
           ) in
           let subtree = of_ast lang rules new_sort subtm in
           subtree.size, Right subtree

        | FoundTerm (_tm_ix, subtm), TerminalName _name
        -> let subtree = of_ast lang rules current_sort subtm in
           subtree.size, Right subtree

        | FoundBinder binder_name, TerminalName name
        -> String.length binder_name, Left binder_name

        (* if the current token is a terminal, and we didn't capture a binder
         * or term, we just emit the contents of the token *)
        | NotFound, TerminalName name
        -> let terminal_rule = M.getExn terminal_rules name in
           (match regex_is_literal terminal_rule with
             | Some re_str -> String.length re_str, Left re_str
             | None -> raise (CantEmitTokenRegex (name, terminal_rule))
           )

        (* XXX right now emitting the token name, not contents
        -> String.length name, Left name *)

        (* if the current token is naming a nonterminal, there has to be a
         * subterm *)
        | FoundBinder binder_name, NonterminalName name
        -> raise (BadRules
          ("binder (" ^ binder_name ^ ") found, nonterminal name: " ^ name))
        | NotFound     , NonterminalName name
        -> raise (BadRules ("subterm not found, nonterminal name: " ^ name))
    )) in
    let size = sum sizes in

    mk_tree current_sort (Operator op_name) children size

  | _, Nominal.Var name
  -> mk_tree current_sort Var [Left name] (String.length name)

  | SortAp ("sequence", [sort]), Nominal.Sequence tms ->
    let sizes, children = tms
      |> List.map (fun tm ->
        let child = of_ast lang rules sort tm in
          child.size, Right child)
      |> Belt.List.unzip
    in
    mk_tree current_sort Sequence children (sum sizes)

  | SortAp ("string", []), Nominal.Primitive (PrimString str) ->
    mk_tree current_sort (Primitive String) [Left str] (String.length str)

  | SortAp ("integer", []), Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree current_sort (Primitive Integer) [Left str] (String.length str)

let rec to_string { leading_trivia; children; trailing_trivia } =
  let children_str = children
    |> List.map
      (function
      | Left terminal_capture     -> terminal_capture
      | Right nonterminal_capture -> to_string nonterminal_capture
      )
    |> String.concat ""
  in leading_trivia ^ children_str ^ trailing_trivia

(* Convert a concrete tree to an AST. We ignore trivia (and size). *)
let rec to_ast lang { sort; node_type; children; } =
  match node_type, children with
    | Var, [ Left name ] -> Result.Ok (Nominal.Var name)
    | Sequence, _ -> Result.map
      (traverse_list_result
        (function
          | Left _      -> Result.Error "TODO: message"
          | Right child -> to_ast lang child
        )
        children)
      (fun children' -> Nominal.Sequence children')
    | Primitive prim_ty, [ Left str ] -> (match prim_ty with
      | String  -> Ok (Nominal.Primitive (PrimString str))
      | Integer ->
        try
          Ok (Nominal.Primitive (PrimInteger (Bigint.of_string str)))
        with
          _ -> Error "TODO: message"
      )

    (* TODO: check validity *)
    | Operator op_name, _ ->
      let children' = children
        |> Util.list_flat_map (function
          | Left _      -> []
          | Right child -> [scope_to_ast lang child]
        )
        |> sequence_list_result
      in Result.map children'
           (fun children'' -> Nominal.Operator (op_name, children''))

and scope_to_ast lang ({ children } as tree) = match List.rev children with
  | body :: binders -> (match to_ast lang {tree with children = [ body ]} with
    | Ok body' -> binders
      |> traverse_list_result
        (function
          | Left binder_name
          -> Ok binder_name
          | Right _nonterminal_capture
          -> Error "expected binder name, got TODO"
        )
      |> Util.flip Result.map
        (fun binders' -> Nominal.Scope (List.rev binders', body'))
    | Error err -> Error err
  )
  | [] -> Error "scope_to_ast called on no children"

let to_grammar ({terminal_rules; sort_rules}: ConcreteSyntaxDescription.t)
  : Jison.grammar
  =
    let rules = terminal_rules
      |> M.toList
      |> List.map
        (fun (name, regex) -> (regex_to_string regex, "return '" ^ name ^ "'"))
      |> Belt.List.toArray
    in
    let lex = Jison.js_lex ~rules: rules in
    let print_token = function
      | TerminalName    name -> name
      | NonterminalName name -> name
    in
    let print_tokens toks = toks
      |> List.map print_token
      |> String.concat " "
    in

    let mk_variable: variable_rule option -> string array = function
      | None
      -> [||]
      | Some { tokens; var_capture }
      -> [| print_tokens tokens; Printf.sprintf "$$ = %i" var_capture |]
    in
    let mk_operator_rules: operator_match list -> string array list
      = List.map (fun (OperatorMatch { tokens; term_pattern }) ->
          let operator_name, term_scopes = term_pattern in
          [| print_tokens tokens;
            (* TODO: conversion from this format to a term *)
            Printf.sprintf
            "$$ = { name: %s, scopes: %s }"
            operator_name
            "TODO"
          |]
        )
    in
    let mk_sort_rule = fun (sort_name, SortRule { operator_rules; variable }) ->
      (sort_name, Belt.List.toArray (mk_variable variable :: mk_operator_rules operator_rules))
    in
    let bnf = sort_rules
      |> M.toList
      |> List.map mk_sort_rule
      |> Js.Dict.fromList
    in
    Jison.grammar ~lex:lex ~bnf:bnf

(* XXX: need to fix return value *)
let jison_parse (parser: Jison.parser) (str: string) : tree =
  ([%raw "function(parser, str) { return parser.parse(str); }"]
  : Jison.parser -> string -> tree
  ) parser str

let parse desc str =
  let grammar = to_grammar desc in
  let parser = Jison.to_parser grammar in
  try
    Result.Ok (jison_parse parser str)
  with
    _ -> Result.Error "parse error"
