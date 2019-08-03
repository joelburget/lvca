module Result = Belt.Result
module BL = Belt.List
module BA = Belt.Array
module Nominal = Binding.Nominal
open Types
open Types.ConcreteSyntaxDescription
module AA = Util.ArrayApplicative(struct type t = string end)
open AA
let (find, traverse_list_result) = Util.(find, traverse_list_result)

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
  BA.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 = match child1, child2 with
  | Left   tc1, Left   tc2 -> tc1 = tc2
  | Right ntc1, Right ntc2 -> equivalent ntc1 ntc2
  | _, _ -> false

let find_operator_match
  (matches: operator_match list list)
  (opname : string)
  : operator_match
  = let maybe_match = find
      (* TODO now need to match *)
      (fun (OperatorMatch { term_pattern }) -> match term_pattern with
        | TermPattern (opname', _) -> opname' = opname
        | ParenthesizingPattern _  -> false
      )
      (BL.flatten matches)
    in
    match maybe_match with
      | Some m -> m
      | None -> failwith "TODO: default match"

type subterm_result =
  | NotFound
  | FoundCapture
  | FoundTerm   of int * Nominal.term
  | FoundBinder of string

(* Find a subterm or binder given a term pattern template and the index of the
 * subterm / binder we're looking for *)
let rec find_subtm' slot_num token_ix scopes term_pattern
  = match scopes, term_pattern with
    | _, [] -> NotFound
    | Nominal.Scope (binders, body) :: scopes', NumberedScopePattern (binder_nums, body_num) :: pattern_scopes
    ->
      let binder_matches = binders
        |. BL.zip binder_nums
        |> find (fun (_, num) -> num = token_ix)
      in
      (match binder_matches with
      | Some (name, _ix) -> FoundBinder name
      | None -> if token_ix = body_num
        then FoundTerm (slot_num, body)
        else find_subtm' (slot_num + 1) token_ix scopes' pattern_scopes
      )
    | _, _ -> failwith "invariant violation: mismatched scopes / term patterns"

let find_subtm
  : int -> Nominal.scope list -> numbered_scope_pattern list -> subterm_result
  = find_subtm' 0

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from of_ast when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of BadRules *)
exception CantEmitTokenRegex of string * regex

let regex_is_literal : regex -> string option = function
  | [ReString str] -> Some str
  | _              -> None

(* TODO: do we need to insert \b? *)
(* Escape special characters *)
let rec regex_piece_to_string : regex_piece -> string = function
  | ReString str   -> Js.String.(str
    |> replaceByRe [%re "/\\+/g"] "\\+"
    |> replaceByRe [%re "/\\*/g"] "\\*"
    |> replaceByRe [%re "/\\?/g"] "\\?"
    |> replaceByRe [%re "/\\-/g"] "\\-"
    |> replaceByRe [%re "/\\(/g"] "\\("
    |> replaceByRe [%re "/\\)/g"] "\\)"
  )
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
let rec of_ast
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  current_sort
  tm
  = match current_sort, tm with
  | SortAp (sort_name, _), Nominal.Operator (op_name, scopes) ->
    let SortRule { operator_rules } = M.getExn sort_rules sort_name in
    (* TODO: remove possible exception. possible to have var-only sort? *)
    let OperatorMatch { tokens; term_pattern } (* TODO: variable rule? *)
      = find_operator_match operator_rules op_name
    in

    let find_subtm' = fun ix -> match term_pattern with
      | ParenthesizingPattern _
      -> FoundCapture

      | TermPattern (_term_name, numbered_scope_patterns)
      -> find_subtm ix scopes numbered_scope_patterns
    in

    (* Map each token to a subtree. For each token:
      * if it's a space, ignore it
      * if it's a terminal, print it
      * if it's a nonterminal, look up the subterm (by token number)
    *)
    let children = BL.(tokens
      |. mapWithIndex (fun token_ix token -> token_ix, token)
      |. keep (fun (_, tok) -> match tok with
        | Underscore -> false
        | _          -> true
        )
      |. toArray
      )
      |. BA.map (fun (token_ix, token) ->

      let token_ix' = token_ix + 1 in (* switch from 0- to 1-based indexing *)
      match find_subtm' token_ix', token with

        | FoundCapture, NonterminalName _sort
        -> assert false

        | FoundCapture, TerminalName name
        -> raise (BadRules ("capture found, terminal name: " ^ name))

        | FoundTerm (tm_ix, subtm), NonterminalName sort
        -> let SortDef (_, operator_defs) = M.getExn sorts sort in
           let some_operator = find
             (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
             operator_defs
           in
           let valences = match some_operator with
             | Some (OperatorDef (_, Arity (_, valences))) -> valences
             | None -> assert false
           in
           let valence = BL.getExn valences tm_ix in
           let new_sort = (match valence with
             | FixedValence    (_, new_sort)
             | VariableValence (_, new_sort)
             -> new_sort
           ) in
           Either.Right (of_ast lang rules new_sort subtm)

        | FoundTerm (_tm_ix, subtm), TerminalName _name
        -> Right (of_ast lang rules current_sort subtm)

        | FoundBinder binder_name, TerminalName _name
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

        (* need to attach trivia to node *)
        | _, Underscore -> assert false
    ) in

    mk_tree current_sort (Operator op_name) children

  | _, Nominal.Var name
  -> mk_tree current_sort Var [| Left name |]

  | SortAp ("sequence", [|sort|]), Nominal.Sequence tms ->
    let children = tms
      |> List.map (fun tm -> Either.Right (of_ast lang rules sort tm))
      |> BL.toArray
    in
    mk_tree current_sort Sequence children

  | SortAp ("string", [||]), Nominal.Primitive (PrimString str) ->
    mk_tree current_sort (Primitive String) [| Left str |]

  | SortAp ("integer", [||]), Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree current_sort (Primitive Integer) [| Left str |]

  | _, _ -> raise (BadSortTerm (current_sort, tm))

let rec to_string { leading_trivia; children; trailing_trivia } =
  let children_str = children
    |> Array.map
      (function
      | Either.Left terminal_capture -> terminal_capture
      | Right nonterminal_capture    -> to_string nonterminal_capture
      )
    |> BL.fromArray
    |> String.concat ""
  in leading_trivia ^ children_str ^ trailing_trivia

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec to_ast lang { node_type; children; }
  : (Nominal.term, string) Result.t
  = match node_type, children with
    | Var, [| Left name |] -> Result.Ok (Nominal.Var name)
    | Sequence, _ -> Result.map
      (traverse_array_result
        (function
          | Either.Left _ -> Error "TODO: message"
          | Right child   -> to_ast lang child
        )
        children)
      (fun children' -> Nominal.Sequence (BL.fromArray children'))
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
        BA.keepMap children (function
          | Left _      -> None
          | Right child -> Some (scope_to_ast lang child)
        )
        |> sequence_array_result
      in Result.map children'
           (fun children'' -> Nominal.Operator (op_name, BL.fromArray children''))

    | Primitive _, _
    | Var        , _
    -> assert false

and scope_to_ast lang ({ children } as tree)
  : (Nominal.scope, string) Result.t
  = match BL.fromArray (BA.reverse children) with
  | body :: binders -> (match to_ast lang {tree with children = [| body |]} with
    | Ok body' -> binders
      |> traverse_list_result
        (function
          | Either.Left binder_name
          -> Ok binder_name
          | Right _nonterminal_capture
          -> Error "expected binder name, got TODO"
        )
      |. Result.map
        (fun binders' -> Nominal.Scope (List.rev binders', body'))
    | Error err -> Error err
  )
  | [] -> Error "scope_to_ast called on no children"

exception NonMatchingFixities of string * string list
exception MixedFixities of bool * int

module ToGrammarHelp = struct
  (* space-separated list of token names, eg "arith ADD arith". We strip out
   whitespace tokens (via token_name) *)
  let print_tokens toks = toks
    |> List.map token_name
    |> Util.keep_some
    |> String.concat " "

  (* make a variable rule *)
  let mk_variable sort_name: variable_rule -> string * string
    = fun { tokens; var_capture }
    -> ( print_tokens tokens
       , Printf.sprintf {|
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
       )

  (* Very dangerous function used to capture an implementation detail of
   * bucklescript's implementation. Return the tag number for a nonterminal_token
   * (except for Underscore)
   * *)
  let nonterminal_tok_num = function
    | TerminalName    _ -> 0
    | NonterminalName _ -> 1
    | Underscore        -> assert false

  (* Fixity and the token to attach it to *)
  type operator_fixity_info = fixity * string option

  (* The list of tokens, the action to execute *)
  type operator_parse_rule = string * string

  let mk_operator_rule
    sort_name
    (* TODO: now need to match *)
    (OperatorMatch { tokens; term_pattern; fixity })
    : operator_fixity_info * operator_parse_rule
    = match term_pattern with
    | ParenthesizingPattern cap_num ->
      (fixity, None),
      ( print_tokens tokens
      , Printf.sprintf "$$ = $%i" cap_num
      )
    | TermPattern (operator_name, _) ->
      (fixity, match tokens with
        | [ NonterminalName _;             TerminalName name;             NonterminalName _ ]
        | [ NonterminalName _; Underscore; TerminalName name; Underscore; NonterminalName _ ]
        | [ NonterminalName _; Underscore; TerminalName name;             NonterminalName _ ]
        | [ NonterminalName _;             TerminalName name; Underscore; NonterminalName _ ]
        -> Some name
        | _
        -> None
      ),
      ( print_tokens tokens
      , Printf.sprintf
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
          |> List.filter (function Underscore -> false | _ -> true)
          |> List.mapi (fun i tok -> Printf.sprintf
            "(function(tag,x){x.tag=tag;return x;})(%i, [$%i])"
            (nonterminal_tok_num tok)
            (i + 1) (* convert from 0- to 1- indexed *)
          )
          |> String.concat ", "
        )
      )

  type level_fixity_info = fixity * string list

  let mk_precedence_level_rule sort_name matches
    : (level_fixity_info option * operator_parse_rule array) =
    let fixity_infos, rules = BL.(matches
      |. map (mk_operator_rule sort_name)
      |. unzip
    )
    in
    (* All operators in this precedence level must have the same fixity *)
    let fixities, fixity_token_names = BL.unzip fixity_infos in
    let fixity = BL.headExn fixities in
    Js.log "precedence level:";
    List.iter (fun name -> Js.log (Util.is_none name)) fixity_token_names;
    match BL.every fixity_token_names Util.is_none, BL.every fixity_token_names Util.is_some with
      | true, false -> None, rules |. BL.toArray
      | _ ->
        let fixity_token_names = fixity_token_names |. Util.keep_some in
        if not (BL.every fixities (fun fixity' -> fixity' = fixity))
          then raise (NonMatchingFixities (sort_name, fixity_token_names));
        (Some (fixity, fixity_token_names), rules |. BL.toArray)
        (*
      | true, true -> raise (MixedFixities (true, BL.length fixity_token_names))
      | false, false -> raise (MixedFixities (false, BL.length fixity_token_names))
      *)

  let mk_sort_rule
    : string * sort_rule -> level_fixity_info list * (string * operator_parse_rule array)
    = fun (sort_name, SortRule { operator_rules; variable }) ->
    let fixity_levels, (prec_level_rules : operator_parse_rule array list) = BL.(operator_rules
      |. map (mk_precedence_level_rule sort_name)
      |. unzip
      )
    in
    let prec_level_rules = prec_level_rules |. BL.toArray |. BA.concatMany in
    (fixity_levels |. Util.keep_some : level_fixity_info list),
    ( sort_name
    , match variable with
      | None -> prec_level_rules
      | Some variable_rule
      -> BA.concat [| mk_variable sort_name variable_rule |] prec_level_rules
    )
end

let to_grammar ({terminal_rules; sort_rules}: ConcreteSyntaxDescription.t)
  : Jison.grammar
  = let open ToGrammarHelp in
    let rules = BL.(terminal_rules
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

    let (pre_operators : (fixity * string list) list list),
        (pre_bnf : (string * (string * string) array) list) = BL.(sort_rules
      |. M.toList
      |. map mk_sort_rule
      |. unzip
    )
    in

    let bnf = pre_bnf
      (* XXX what is the start? *)
      |. BL.add ("start", [%raw {|
        [["tm EOF", "/*console.log($1);*/ return $1"]]
      |}])
      |. Js.Dict.fromList
    in

    let operators = BL.(pre_operators
      |. flatten
      |. map (fun (fixity, names) ->
          BA.concat [| fixity_str fixity |] (toArray names))
      |. toArray
      (* Jison has highest precedence at the bottom, we have highest precedence
       * at the top *)
      |. BA.reverse
    )
    in
    Jison.grammar ~lex:lex ~operators:operators ~bnf:bnf

let jison_parse (parser: Jison.parser) (str: string) : tree =
  ([%raw "function(parser, str) { return parser.parse(str); }"]
  : Jison.parser -> string -> tree
  ) parser str

let parse desc str =
  try
    let grammar = to_grammar desc in
    let parser = Jison.to_parser grammar in
    Result.Ok (jison_parse parser str)
  with
    | NonMatchingFixities (sort_name, token_names) -> Result.Error
      ("In sort " ^ sort_name ^ ": all fixities in a precedence level must be the same fixity (this is a limitation of Bison-style parsers (Jison in particular). The operators identified by [" ^ String.concat ", " token_names ^ "] must all share the same fixity.")
    | MixedFixities (b, l) -> Error ("Found a mix of fixities -- all must be uniform " ^ string_of_bool b ^ " " ^ string_of_int l)
  (*
  try
    Result.Ok (jison_parse parser str)
  with
    _ -> Result.Error "parse error"
    *)
