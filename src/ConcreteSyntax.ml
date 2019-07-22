module Result = Belt.Result
module Nominal = Binding.Nominal
open Either
open Types
open Types.ConcreteSyntaxDescription
module AA = Util.ArrayApplicative(struct type t = string end)
open AA
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
    children        : (terminal_capture, nonterminal_capture) Either.t array;
  }

(* equality mod trivia *)
let rec equivalent t1 t2 =
  t1.sort = t2.sort &&
  t1.node_type = t2.node_type &&
  Belt.Array.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

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

(* TODO: do we need to insert \b? *)
let rec regex_piece_to_string : regex_piece -> string = function
  | ReString str   -> str
    |> Js.String.replaceByRe [%re "/\\+/g"] "\\+"
    |> Js.String.replaceByRe [%re "/\\*/g"] "\\*"
    |> Js.String.replaceByRe [%re "/\\?/g"] "\\?"
    |> Js.String.replaceByRe [%re "/\\-/g"] "\\-"
  | ReSet    str   -> "[" ^ str ^ "]"
  | ReStar   piece -> regex_piece_to_string piece ^ "*"
  | RePlus   piece -> regex_piece_to_string piece ^ "+"
  | ReOption piece -> regex_piece_to_string piece ^ "?"

let regex_to_string : regex -> string = fun re_parts -> re_parts
  |> List.map regex_piece_to_string
  |> String.concat ""

let mk_tree sort node_type children =
  { sort;
    node_type;
    leading_trivia  = "";
    trailing_trivia = "";
    children;
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
    let children = Belt.Array.mapWithIndex (Belt.List.toArray tokens)
      (fun token_ix token ->
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
           Right subtree

        | FoundTerm (_tm_ix, subtm), TerminalName _name
        -> let subtree = of_ast lang rules current_sort subtm in
           Right subtree

        | FoundBinder binder_name, TerminalName name
        -> Left binder_name

        (* if the current token is a terminal, and we didn't capture a binder
         * or term, we just emit the contents of the token *)
        | NotFound, TerminalName name
        -> let terminal_rule = M.getExn terminal_rules name in
           (match regex_is_literal terminal_rule with
             | Some re_str -> Left re_str
             | None -> raise (CantEmitTokenRegex (name, terminal_rule))
           )

        (* if the current token is naming a nonterminal, there has to be a
         * subterm *)
        | FoundBinder binder_name, NonterminalName name
        -> raise (BadRules
          ("binder (" ^ binder_name ^ ") found, nonterminal name: " ^ name))
        | NotFound, NonterminalName name
        -> raise (BadRules ("subterm not found, nonterminal name: " ^ name))
    ) in

    mk_tree current_sort (Operator op_name) children

  | _, Nominal.Var name
  -> mk_tree current_sort Var [| Left name |]

  | SortAp ("sequence", [|sort|]), Nominal.Sequence tms ->
    let children = tms
      |> List.map (fun tm -> Right (of_ast lang rules sort tm))
      |> Belt.List.toArray
    in
    mk_tree current_sort Sequence children

  | SortAp ("string", [||]), Nominal.Primitive (PrimString str) ->
    mk_tree current_sort (Primitive String) [| Left str |]

  | SortAp ("integer", [||]), Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree current_sort (Primitive Integer) [| Left str |]

let rec to_string { leading_trivia; children; trailing_trivia } =
  let children_str = children
    |> Array.map
      (function
      | Left terminal_capture     -> terminal_capture
      | Right nonterminal_capture -> to_string nonterminal_capture
      )
    |> Belt.List.fromArray
    |> String.concat ""
  in leading_trivia ^ children_str ^ trailing_trivia

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec to_ast lang { sort; node_type; children; }
  : (Nominal.term, string) Result.t
  = match node_type, children with
    | Var, [| Left name |] -> Result.Ok (Nominal.Var name)
    | Sequence, _ -> Result.map
      (traverse_array_result
        (function
          | Left _      -> Error "TODO: message"
          | Right child -> to_ast lang child
        )
        children)
      (fun children' -> Nominal.Sequence (Belt.List.fromArray children'))
    | Primitive prim_ty, [| Left str |] -> (match prim_ty with
      | String  -> Ok (Nominal.Primitive (PrimString str))
      | Integer ->
        try
          Ok (Nominal.Primitive (PrimInteger (Bigint.of_string str)))
        with
          _ -> Error "failed to read integer literal"
      )

    (* TODO: check validity *)
    | Operator op_name, _ ->
      let children' =
        Belt.Array.keepMap children (function
          | Left _      -> None
          | Right child -> Some (scope_to_ast lang child)
        )
        |> sequence_array_result
      in Result.map children'
           (fun children'' -> Nominal.Operator (op_name, Belt.List.fromArray children''))

and scope_to_ast lang ({ children } as tree)
  : (Nominal.scope, string) Result.t
  = match Belt.List.fromArray (Belt.Array.reverse children) with
  | body :: binders -> (match to_ast lang {tree with children = [| body |]} with
    | Ok body' -> binders
      |> traverse_list_result
        (function
          | Left binder_name
          -> Ok binder_name
          | Right _nonterminal_capture
          -> Error "expected binder name, got TODO"
        )
      |. Result.map
        (fun binders' -> Nominal.Scope (List.rev binders', body'))
    | Error err -> Error err
  )
  | [] -> Error "scope_to_ast called on no children"

let to_grammar ({terminal_rules; sort_rules}: ConcreteSyntaxDescription.t)
  : Jison.grammar
  =
    let rules = Belt.List.(terminal_rules
      |. M.toList
      |. map
        (fun (name, regex) -> (regex_to_string regex, "return '" ^ name ^ "'"))
      |. add ("$", "return 'EOF'")
      (* TODO: we probably don't want to do this in general *)
      |. add ("\\s+", "/* skip whitespace */")
      |. toArray
    )
    in
    let lex = Jison.js_lex ~rules: rules in

    let nonterminal_tok_num = function
      | TerminalName    _ -> 0
      | NonterminalName _ -> 1
    in

    let print_tokens toks = toks
      |> List.map token_name
      |> String.concat " "
    in

    let mk_variable sort_name: variable_rule option -> string array = function
      | None
      -> [||]
      | Some { tokens; var_capture }
      -> [|
        print_tokens tokens;
        Printf.sprintf {|
          $$ = /* record */[
            /* SortAp */['%s',[]],
            /* Var */0,
            '',
            '',
            /* array */[(function(tag,x){x.tag=tag;return x;})(/* TerminalName */0, [$%i])]
          ]
        |}
        sort_name
        var_capture
      |]
    in

    let mk_scope (NumberedScopePattern (binder_captures, body_capture)) =
      Printf.sprintf "/* Scope */[[%s], $%i]"
      (binder_captures |> List.map (Printf.sprintf "$%i") |> String.concat ", ")
      body_capture
    in

    let mk_operator_rule
      sort_name
      (OperatorMatch { tokens; term_pattern = operator_name, _ })
      : string array =
      [| print_tokens tokens;
        Printf.sprintf
        {|
          $$ = /* record */[
            /* SortAp */['%s', []],
            /* Operator */(function(tag,x){x.tag=tag;return x;})(0, ['%s']),
            '',
            '',
            /* array */[%s]
          ]
        |}
        sort_name
        operator_name
        (tokens
          |> List.mapi (fun i tok -> Printf.sprintf
            "(function(tag,x){x.tag=tag;return x;})(%i, [$%i])"
            (nonterminal_tok_num tok)
            (i + 1) (* convert from 0- to 1- indexed *)
          )
          |> String.concat ", "
        )
      |]
    in

    let mk_sort_rule = fun (sort_name, SortRule { operator_rules; variable }) ->
      (sort_name, Belt.List.toArray
        (mk_variable sort_name variable ::
          List.map (mk_operator_rule sort_name) operator_rules
        )
      )
    in

    (* XXX hardcoded *)
    let operators = [|
      [| "left"; "ADD"; "SUB" |];
      |]
    in
    let bnf = sort_rules
      |. M.toList
      |. Belt.List.map mk_sort_rule
      (* XXX what is the start? *)
      |. Belt.List.add ("start", [%raw {|
        [["arith EOF", "/*console.log($1);*/ return $1"]]
      |}])
      |. Js.Dict.fromList
    in
    Jison.grammar ~lex:lex ~operators:operators ~bnf:bnf

let jison_parse (parser: Jison.parser) (str: string) : tree =
  ([%raw "function(parser, str) { return parser.parse(str); }"]
  : Jison.parser -> string -> tree
  ) parser str

let parse desc str =
  let grammar = to_grammar desc in
  let parser = Jison.to_parser grammar in
  Result.Ok (jison_parse parser str)
  (*
  try
    Result.Ok (jison_parse parser str)
  with
    _ -> Result.Error "parse error"
    *)
