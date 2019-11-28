module BL = Belt.List
module BA = Belt.Array
module Nominal = Binding.Nominal
open Types
open ConcreteSyntaxDescription
module AA = Util.ArrayApplicative(struct type t = string end)
module MS = Belt.Map.String
let (find, get_option') = Util.(find, get_option')
module MMI = Belt.MutableMap.Int
module MSI = Belt.MutableSet.Int
module SI = Belt.Set.Int

module Lexer = ConcreteSyntax_Lexer
module Parser = ConcreteSyntax_Parser
module ParseErrors = ConcreteSyntax_ParseErrors

type prim_ty =
  | Integer
  | String

type node_type =
  | Parenthesizing
  | Operator  of string
  | Var
  | Sequence
  | Primitive of prim_ty

(** Terminals capture text from the input buffer *)
type terminal_capture =
  { content         : string;
    leading_trivia  : string;
    trailing_trivia : string;
  }

(** Nonterminals capture their children *)
type nonterminal_capture = tree

(** Terminals and nonterminals both capture data about why they were
 constructed
 *)
and capture =
  | TerminalCapture    of terminal_capture
  | NonterminalCapture of nonterminal_capture

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
  { sort_name       : sort_name;
    node_type       : node_type;
    children        : capture array;
  }

(* tree equality mod trivia *)
let rec equivalent t1 t2 =
  t1.sort_name = t2.sort_name &&
  t1.node_type = t2.node_type &&
  BA.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 = match child1, child2 with
  | TerminalCapture     tc1, TerminalCapture     tc2 -> tc1.content = tc2.content
  | NonterminalCapture ntc1, NonterminalCapture ntc2 -> equivalent ntc1 ntc2
  |                       _,                       _ -> false

let find_operator_match
  (matches: operator_match list list)
  (opname : string)
  : operator_match
  = let maybe_match = find
      (* TODO now need to match *)
      (fun (OperatorMatch { operator_match_pattern }) ->
        match operator_match_pattern with
        | OperatorPattern (opname', _) -> opname' = opname
        | ParenthesizingPattern _  -> false
      )
      (BL.flatten matches)
    in
    match maybe_match with
      | Some m -> m
      | None -> failwith ("failed to find a rule matching operator " ^ opname)

type subterm_result =
  | NotFound
  | FoundCapture
  | FoundTerm   of int * Nominal.term
  | FoundBinder of Pattern.t

let rec find_subtm' slot_num token_ix scopes operator_match_pattern
  = match scopes, operator_match_pattern with
    | _, [] -> NotFound
    | Nominal.Scope (binders, body) :: scopes',
      NumberedScopePattern (binder_nums, body_num) :: pattern_scopes
    ->
      let binder_matches = binders
        |. BL.zip binder_nums
        |> find (fun (_, num) -> num = token_ix)
      in
      (match binder_matches with
      | Some (pat, _ix) -> FoundBinder pat
      | None -> if token_ix = body_num
        then FoundTerm (slot_num, body)
        else find_subtm' (slot_num + 1) token_ix scopes' pattern_scopes
      )
    | _, _ -> failwith "invariant violation: mismatched scopes / term patterns"

(** Find a subterm or binder given a term pattern template and the index of the
  subterm / binder we're looking for. We either (a) don't find it, (b) find a
  term, or (c) find a binder. Example:

  scopes:
    * a. b. c
    * e
  numbered scope patterns:
    * $1. $2. $3
    * $4

  * If we're looking for term $1, we'll return binder a
  * term $4 -> term e
  * term $5 -> not found

  Note that the scopes and numbered scope patterns should mirror each other in
  structure, otherwise an invariant violation may be raised.
 *)
let find_subtm
  : int -> Nominal.scope list -> numbered_scope_pattern list -> subterm_result
  = find_subtm' 0

(** The current term and sort don't match *)
exception BadSortTerm of sort * Nominal.term

exception BadRules of string

(** raised from of_ast when we need to emit a token but don't have a capture,
 * and the terminal match is a regex, not a string literal. This could actually
 * be a form of BadRules *)
exception CantEmitTokenRegex of string * Regex.t

type invalid_grammar = InvalidGrammar of string
exception CheckValidExn of invalid_grammar

(* Used to analyze token usage (see `token_usage`). We use this to check if
  there are any tokens not captured (a problem in some circumstances), or if
  there are tokens captured twice (always a problem). *)
type tokens_info =
  { captured_tokens : SI.t;
    repeated_tokens : SI.t;
  }

let accumulate_tokens = fun
  { captured_tokens = seen_toks;  repeated_tokens = repeated_toks }
  { captured_tokens = seen_toks'; repeated_tokens = repeated_toks' } ->
  let isect = SI.intersect seen_toks seen_toks' in
  { captured_tokens = SI.diff (SI.union seen_toks seen_toks') isect;
    repeated_tokens = SI.union isect (SI.union repeated_toks repeated_toks');
  }

let empty_tokens_info =
  { captured_tokens = SI.empty; repeated_tokens = SI.empty }

let rec token_usage
  : operator_match_pattern -> tokens_info
  = function
  | OperatorPattern (_, scope_patterns) -> scope_patterns
    |. BL.reduce empty_tokens_info
      (fun accum scope_pat ->
        accumulate_tokens accum (scope_token_usage scope_pat)
      )
  | ParenthesizingPattern cap_num ->
      { captured_tokens = SI.fromArray [| cap_num |];
        repeated_tokens = SI.empty;
      }

and scope_token_usage (NumberedScopePattern (binder_captures, body_capture))
  = (body_capture :: binder_captures)
    |. BL.reduce empty_tokens_info (fun accum tok -> accumulate_tokens accum
        { captured_tokens = SI.fromArray [| tok |];
          repeated_tokens = SI.empty;
        }
    )

let check_operator_match_validity
  : nonterminal_token list -> operator_match_pattern
  -> MSI.t * SI.t * (int * nonterminal_token) list
  = fun token_list term_pat ->
    let numbered_toks = token_list
      |. BL.toArray
      |. BA.mapWithIndex (fun i tok -> i, tok)
      |. MMI.fromArray
    in
    let { captured_tokens; repeated_tokens } = token_usage term_pat in
    let non_existent_tokens = MSI.make () in
    SI.forEach captured_tokens (fun tok_num ->
      if MMI.has numbered_toks tok_num
      then MMI.remove numbered_toks tok_num
      else MSI.add non_existent_tokens tok_num
    );
    non_existent_tokens, repeated_tokens, MMI.toList numbered_toks

(* Check invariants of concrete syntax descriptions:
 * 1. For all tokens on the LHS (token list),
 *    if the token is not captured on the RHS (term pattern):
 *   a. If the token refers to a nonterminal, this is an error.
 *   b. If the token refers to a terminal, it must be a string literal.
 * 2. No token is used twice on the RHS.
 * 3. No token is mentioned on the RHS that doesn't exist on the left
 * 4. No regex admits empty strings (these tokens could be arbitrarily inserted
 *    everywhere)
 *
 * Examples:
 * * FOO bar BAZ { op($1; $2; $3) } valid
 * * FOO bar BAZ { op($1; $3) } invalid (uncaptured nonterminal)
 * * FOO bar BAZ { op($1; $2) } possibly invalid (if BAZ isn't a string literal)
 * * FOO bar BAZ { op($2; $2) } invalid (repeated token)
 * * FOO bar BAZ { op($2; $4) } invalid (non-existent token)
 *)
let check_description_validity { terminal_rules; sort_rules } =
  let terminal_rules' = Belt.Map.String.fromArray terminal_rules in

  try
    sort_rules
      |. Belt.Map.String.forEach (fun _i (SortRule { operator_rules }) ->
        let operator_maches = BL.flatten operator_rules in
        BL.forEach operator_maches (fun _i
          (OperatorMatch { tokens; operator_match_pattern }) ->
          let non_existent_tokens, duplicate_captures, uncaptured_tokens =
            check_operator_match_validity tokens operator_match_pattern
          in
          if not (SI.isEmpty duplicate_captures) then (
            let tok_names = duplicate_captures
              |. SI.toArray
              |. BA.map (Printf.sprintf "$%n")
              |. Js.Array2.joinWith ", "
            in
            raise (CheckValidExn (InvalidGrammar
              ("tokens captured more than once: " ^ tok_names)));
          );
          if not (MSI.isEmpty non_existent_tokens) then (
            let tok_names = non_existent_tokens
              |. MSI.toArray
              |. BA.map (Printf.sprintf "$%n")
              |. Js.Array2.joinWith ", "
            in
            raise (CheckValidExn (InvalidGrammar
              ("non-existent tokens mentioned: " ^ tok_names)));
          );
          BL.map uncaptured_tokens (fun (_i, tok) -> match tok with
            | NonterminalName name -> raise (CheckValidExn
              (InvalidGrammar ("uncaptured nonterminal: " ^ name)))
            | TerminalName nt_name -> (match MS.get terminal_rules' nt_name with
              | None -> raise (CheckValidExn (InvalidGrammar
                ("Named terminal " ^ nt_name ^ " does not exist")))
              (* TODO: switch to using canonical representatives *)
              | Some regex -> (match Regex.canonical_representative regex with
                | Some str -> str
                | None -> raise (CheckValidExn (InvalidGrammar
                  ("Uncaptured regex with no canonical representative: " ^
                    Regex.to_string regex)))
              )
                  (*
                  if Util.is_none (Regex.is_literal regex)
                then raise (CheckValidExn (InvalidGrammar
                  ("Uncaptured regex which is not a string literal: " ^
                    Regex.to_string regex)))
            *)
            )
            | Underscore _n -> ""
          )
        );
      );
    Belt.Map.String.forEach terminal_rules' (fun _i regex ->
      if Regex.accepts_empty regex then
        raise (CheckValidExn (InvalidGrammar
          ("Regex accepts empty strings: " ^ Regex.to_string regex)
        ))
    );
    None
  with
    CheckValidExn err -> Some err

let mk_tree sort_name node_type children =
  { sort_name; node_type; children; }

(* Helper for use in of_ast *)
let mk_terminal_capture content =
  TerminalCapture { leading_trivia = ""; content; trailing_trivia = "" }

let rec pattern_to_tree : sort_name -> Pattern.t -> tree
  = fun sort_name pat -> match pat with
  | Var name -> mk_tree sort_name Var [||]
  | Operator (name, pats) -> mk_tree sort_name (Operator name)
    (pats
      |. Belt.List.toArray
      |. Belt.Array.map (fun pat ->
          NonterminalCapture
            (pattern_to_tree (failwith "TODO: pattern_to_tree 1") pat)
      )
    )
  | Sequence pats -> mk_tree sort_name Sequence
    (pats
      |. Belt.List.toArray
      |. Belt.Array.map (fun pat ->
          NonterminalCapture
            (pattern_to_tree (failwith "TODO: pattern_to_tree 2") pat)
      )
    )
  | Primitive p ->
    (* TODO: what about other integral types? *)
    let prim_ty = match sort_name with
      | "string" -> String
      | "integer" -> Integer
      | _ -> failwith ("unexpected primitive sort name: " ^ sort_name)
    in
    mk_tree sort_name (Primitive prim_ty) [||]

(* TODO: handle non-happy cases *)
(** Pretty-print an abstract term to a concrete syntax tree
   Raises: InvariantViolation, BadRules
  *)
let rec of_ast
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  (SortAp (sort_name, _) as current_sort)
  tm
  = let terminal_rules' = Belt.Map.String.fromArray terminal_rules in
    match current_sort, tm with
  | _, Nominal.Operator (op_name, scopes) ->
    let SortRule { operator_rules } = get_option'
      ("of_ast: failed to get sort " ^ sort_name)
      (MS.get sort_rules sort_name)
    in
    (* TODO: remove possible exception. possible to have var-only sort? *)
    let OperatorMatch { tokens = operator_match_tokens; operator_match_pattern }
      (* TODO: variable rule? *)
      = find_operator_match operator_rules op_name
    in

    let find_subtm' = fun ix -> match operator_match_pattern with
      | ParenthesizingPattern _
      -> FoundCapture

      | OperatorPattern (_term_name, numbered_scope_patterns)
      -> find_subtm ix scopes numbered_scope_patterns
    in

    (* Map each token to a subtree. For each token:
      * if it's a space, ignore it
      * if it's a terminal, print it
      * if it's a nonterminal, look up the subterm (by token number)
    *)
    let children = BL.(operator_match_tokens
      |. mapWithIndex (fun token_ix token -> token_ix, token)
      |. keep (fun (_, tok) -> match tok with
        | Underscore _n -> false
        | _             -> true
      )
      |. toArray
      )
      |. BA.map (fun (token_ix, token) ->

      let token_ix' = token_ix + 1 in (* switch from 0- to 1-based indexing *)
      match find_subtm' token_ix', token with

        | FoundCapture, NonterminalName _sort
        -> assert false (* TODO: raise invariant violation *)

        | FoundCapture, TerminalName name
        -> raise (BadRules ("capture found, terminal name: " ^ name))

        | FoundTerm (tm_ix, subtm), NonterminalName sort_name
        -> let SortDef (_, operator_defs) = get_option'
             ("of_ast: failed to get sort" ^ sort_name) @@
             MS.get sorts sort_name
           in
           let some_operator = find
             (fun (OperatorDef (op_name', _)) -> op_name' = op_name)
             operator_defs
           in
           let valences = match some_operator with
             | Some (OperatorDef (_, Arity (_, valences))) -> valences
             | None -> assert false
           in
           let valence = get_option'
               ("of_ast: failed to get term " ^ string_of_int tm_ix) @@
             BL.get valences tm_ix
           in
           let new_sort = (match valence with
             | FixedValence    (_, new_sort)
             | VariableValence (_, new_sort)
             -> new_sort
           ) in
           NonterminalCapture (of_ast lang rules new_sort subtm)

        | FoundTerm (_tm_ix, subtm), TerminalName _name
        -> NonterminalCapture (of_ast lang rules current_sort subtm)

        (* if the current token is a terminal, and we didn't capture a binder
         * or term, we just emit the contents of the token *)
        | NotFound, TerminalName name
        -> let terminal_rule = get_option'
              ("of_ast: failed to get terminal rule " ^ name) @@
              MS.get terminal_rules' name
           in
           (match Regex.canonical_representative terminal_rule with
             | Some str -> mk_terminal_capture str
             | None ->
                 Js.log ("re: " ^ Regex.to_string terminal_rule);
                 raise (CantEmitTokenRegex (name, terminal_rule))
           )

           (*
           (match Regex.is_literal terminal_rule with
             | Some re_str -> mk_terminal_capture re_str
             | None -> raise (CantEmitTokenRegex (name, terminal_rule))
           )
      *)

        | FoundBinder pattern, NonterminalName _name
        -> NonterminalCapture (pattern_to_tree sort_name pattern)

        (* Invariant: underscores are filtered out in the previous `keep` stage
         *)
        | _, Underscore _ -> assert false

        | FoundBinder pattern, TerminalName name
        -> raise (BadRules
          (Printf.sprintf
            "binder (%s) found, terminal name: %s"
            (Pattern.string_of_pattern pattern)
            name
        ))

        | NotFound, NonterminalName name
        -> raise (BadRules ("subterm not found, nonterminal name: " ^ name))
    ) in

    mk_tree sort_name (Operator op_name) children

  | _, Nominal.Var name
  -> mk_tree sort_name Var [| mk_terminal_capture name |]

  | SortAp ("sequence", [|sort|]), Nominal.Sequence tms ->
    let children = tms
      |> List.map (fun tm -> NonterminalCapture (of_ast lang rules sort tm))
      |> BL.toArray
    in
    mk_tree sort_name Sequence children

  | SortAp ("string", [||]), Nominal.Primitive (PrimString str) ->
    mk_tree sort_name (Primitive String) [| mk_terminal_capture str |]

  | SortAp ("integer", [||]), Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree sort_name (Primitive Integer) [| mk_terminal_capture str |]

  | _, _ -> raise (BadSortTerm (current_sort, tm))

let rec to_string { children } = children
  |> Array.map
    (function
    | TerminalCapture { leading_trivia; content; trailing_trivia }
    -> leading_trivia ^ content ^ trailing_trivia
    | NonterminalCapture nonterminal_capture
    -> to_string nonterminal_capture
    )
  |> BL.fromArray
  |> String.concat ""

let rec remove_spaces : tree -> tree
  = fun { sort_name; node_type; children } ->
    let children' = children
      |. Belt.Array.map (function
        | TerminalCapture { content } -> TerminalCapture
          { content; leading_trivia = ""; trailing_trivia = "" }
        | NonterminalCapture ntc -> NonterminalCapture (remove_spaces ntc)
      )
    in { sort_name; node_type; children = children' }

exception ToAstError of string

let prim_to_ast : prim_ty -> string -> primitive
  = fun prim_ty str -> match prim_ty with
    | String  -> PrimString str
    | Integer ->
      try
        PrimInteger (Bigint.of_string str)
      with
        _ -> raise (ToAstError "failed to read integer literal")

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec tree_to_ast lang tree : Nominal.term
    = match tree.node_type, tree.children with
    | Var, [| TerminalCapture { content = name } |]
    -> Var name
    | Sequence, children ->
      let children' = children
        |. Belt.Array.map
          (function
            | TerminalCapture _ -> raise @@
              ToAstError "Unexpected terminal found in a sequence"
            | NonterminalCapture child -> tree_to_ast lang child
          )
        |. Belt.List.fromArray
      in
      Sequence children'
    | Primitive prim_ty, [| TerminalCapture { content } |]
    -> Primitive (prim_to_ast prim_ty content)

    (* TODO: check validity *)
    | Operator op_name, children ->
      let children' = children
        |. BA.keepMap (function
          | TerminalCapture { content } -> None
          | NonterminalCapture child    -> Some
            (Nominal.Scope ([], tree_to_ast lang child))
            (* XXX *)
            (* (scope_to_ast lang child) *)
        )
        |. Belt.List.fromArray
      in Operator (op_name, children')

    | Parenthesizing, children ->
      Js.log children;
      let children' = children
        |. Belt.Array.keepMap (function
          | TerminalCapture _ -> None
          | NonterminalCapture child -> Some
            (Nominal.Scope ([], tree_to_ast lang child))
            (* XXX *)
            (* (scope_to_ast lang child) *)
        )
      in
      Js.log children';
      (match children' with
        [| Scope ([], child) |] -> child
      )

    | Primitive _, _
    | Var        , _
    -> assert false

and capture_to_pat : capture -> Pattern.t
  = function
    | TerminalCapture { content = name } -> raise @@
      ToAstError "Unexpected bare terminal capture in capture_to_pat"
    | NonterminalCapture { sort_name; node_type; children } ->
      match node_type with
      | Operator op_name ->
        let children' = children
          |. BA.keepMap (fun cap -> match cap with
            | TerminalCapture { content } -> None
            | NonterminalCapture child    -> Some (capture_to_pat cap)
          )
          |. Belt.List.fromArray
        in
        Operator (op_name, children')
      | Var -> (match children with
        | [| TerminalCapture { content = name } |] -> Var name
        | _ -> raise @@ ToAstError
          "Unexpected children of a var capture (expected a single terminal capture)"
      )
      | Sequence ->
        let children' = children
          |. Belt.Array.map capture_to_pat
          |. Belt.List.fromArray
        in Sequence children'
      | Primitive prim_ty -> (match children with
        | [| TerminalCapture { content } |]
        -> Primitive (prim_to_ast prim_ty content)
        | _ -> raise @@
          ToAstError "Unexpected primitive capture in capture_to_pat"
      )

and scope_to_ast lang ({ children } as tree) : Nominal.scope
  = match children |. BA.reverse |. BL.fromArray with
  | body :: binders ->
    let body' = tree_to_ast lang {tree with children = [| body |]} in
    let binders' = binders
      |. Belt.List.map capture_to_pat
      |. List.rev
    in
    Scope (binders', body')
  | [] -> raise (ToAstError "scope_to_ast called on no children")

let to_ast : language -> tree -> (Nominal.term, string) Belt.Result.t
  = fun lang tree ->
    try
      Ok (tree_to_ast lang tree)
    with
      ToAstError msg -> Error msg

(* TODO: now that we're no longer using Jison we can remove NonMatchingFixities
 *)
exception NonMatchingFixities of string * string list
exception MixedFixities of bool * int

(* Produce an augmented grammar *)
let to_grammar
  : ConcreteSyntaxDescription.t
  -> (LrParsing.grammar * ConcreteSyntaxDescription.operator_match' Belt.MutableMap.Int.t)
  = fun {terminal_rules; sort_rules} ->
    let terminal_key_arr = BA.map terminal_rules (fun (k, _) -> k) in
    let terminal_nums = terminal_key_arr
      (* start other terminals (besides $ and SPACE) at 2 *)
      |. BA.mapWithIndex (fun i name -> name, i + 2)
      |. BA.concat [| "$", 0; "SPACE", 1 |]
    in
    let terminal_nums_map = MS.fromArray terminal_nums in

    let nonterminal_names_map = sort_rules
      |. MS.keysToArray
      (* start other nonterminals (besides root) at 1 *)
      |. BA.mapWithIndex (fun i name -> name, i + 1)
      |. MS.fromArray
      |. MS.set "root" 0
    in
    let nonterminal_nums = MS.toArray nonterminal_names_map in

    (* We're dealing with a non-augmented grammar here. Start counting from 0.
     *)
    let prod_num = ref 1 in
    let production_rule_map = Belt.MutableMap.Int.make () in

    let nonterminals = sort_rules
      |. MS.valuesToArray
      |. BA.mapWithIndex (fun i (SortRule { operator_rules; variable }) ->
        let productions : LrParsing.symbol list list = operator_rules
          |. BL.map (fun operator_level -> operator_level |.
            BL.map (fun (OperatorMatch ({ tokens } as rule)) ->
              Belt.MutableMap.Int.set production_rule_map !prod_num rule;
              prod_num := !prod_num + 1;
              tokens |. BL.keepMap (function
                | TerminalName tn
                -> Some (LrParsing.Terminal (terminal_nums_map
                  |. MS.get tn
                  |> get_option'
                    ("to_grammar: failed to get " ^ tn))
                )
                | NonterminalName ntn
                -> Some (Nonterminal (nonterminal_names_map
                  |. MS.get ntn
                  |> get_option'
                    ("to_grammar: failed to get " ^ ntn))
                )
                | Underscore _ -> None
                (* | Underscore n -> Nonterminal 1 *)
              )
            )
          )
          (* TODO: temporary pending precedence parsing *)
          |. BL.flatten
        in
        i + 1, { LrParsing.productions = productions }
      )
      |. Belt.Map.Int.fromArray
      (* TODO: we don't necessarily start with production 1 *)
      |. Belt.Map.Int.set 0 { productions = [ [ Nonterminal 1 ] ] }
    in

    { nonterminals; terminal_nums; nonterminal_nums }, production_rule_map

let production_sort_name
  : LrParsing.nonterminal_num Belt.MutableMap.Int.t
  -> LrParsing.nonterminal_num Belt.Map.String.t
  -> LrParsing.production_num
  -> string
  = fun nt_map nonterminal_nums prod_num ->
    let nt_num = nt_map
      |. MMI.get prod_num
      |> get_option'
        ("production_sort_name: failed to get " ^ string_of_int prod_num)
    in
    let f _ nt_num' = nt_num' = nt_num in
    match Belt.Map.String.findFirstBy nonterminal_nums f with
      | None -> failwith
        "production_sort_name: invariant violation: sort not found"
      | Some (name, _) -> name

let tree_of_parse_result
  (module Lr0 : LrParsing.LR0)
  : ConcreteSyntaxDescription.operator_match' Belt.MutableMap.Int.t
  -> LrParsing.nonterminal_num MS.t
  -> ConcreteSyntaxDescription.sort_rules
  -> string (* root name *)
  -> string (* parsed string *)
  -> LrParsing.parse_result
  -> tree
  = fun production_rule_map nonterminal_nums sort_rules root_name str root ->
    let str_pos = ref 0 in
    let str_len = Js.String2.length str in

    let get_trivia : int -> int -> string * string
      = fun start_pos end_pos ->
        (* Printf.printf "get_trivia %n %n\n" start_pos end_pos; *)
        (* look back consuming all whitespace to (and including) a newline *)
        let leading_trivia =
          Js.String2.slice str ~from:!str_pos ~to_:start_pos
        in

        (* look forward consuming all whitespace up to a newline *)
        str_pos := end_pos;
        let continue = ref true in
        while !continue do
          let got_space, got_newline = match Js.String2.charAt str !str_pos with
            | " "  -> true,  false
            | "\n" -> false, true
            | _    -> false, false
          in
          continue := !str_pos < str_len && got_space;
          if !continue then str_pos := !str_pos + 1;
        done;

        let trailing_trivia =
          Js.String2.slice str ~from:end_pos ~to_:!str_pos
        in

        (* Printf.printf "get_trivia -> \"%s\", \"%s\"\n" leading_trivia trailing_trivia; *)
        leading_trivia, trailing_trivia
    in

    let rec go_nt : string -> LrParsing.parse_result -> tree
      = fun nt_name { production; children } ->
        let prod_num = match production with
          | Left _ -> failwith
            "invariant violation: go_nt received a terminal production"
          | Right prod_num -> prod_num
        in

        (* Printf.printf "go_nt %s (production_num %n)\n" nt_name prod_num; *)

        let sort_name = production_sort_name
          Lr0.production_nonterminal_map
          nonterminal_nums
          prod_num
        in

        (* Printf.printf "sort_name: %s\n" sort_name; *)
        let { operator_match_pattern } =
          Belt.MutableMap.Int.getExn production_rule_map prod_num
        in

        let m_ctor_name = match operator_match_pattern with
          | OperatorPattern (op_name, _) ->
              (* Printf.printf "ctor_name: %s\n" op_name; *)
              Some op_name
          | ParenthesizingPattern _ -> None
        in

        (* Printf.printf "nt_name: %s\n" nt_name; *)
        let node_type = match m_ctor_name with
          | None -> Parenthesizing
          | Some "var" -> Var (* TODO this seems wrong *)
          | Some ctor_name -> Operator ctor_name
        in

        let err = "tree_of_parse_result: failed to get " ^ nt_name in

        let tokens : nonterminal_token list
          = match sort_rules |. MS.get nt_name |> get_option' err with
            | SortRule { operator_rules } ->
              let maybe_op_rule = operator_rules
              |. BL.flatten
              |. BL.getBy (fun (OperatorMatch { operator_match_pattern }) ->
                match m_ctor_name, operator_match_pattern with
                  | None, ParenthesizingPattern _ -> true
                  | Some ctor_name, OperatorPattern (op_name, _) ->
                      ctor_name = op_name
                  | _, _ -> false
              )
              in
              match maybe_op_rule with
                | Some (OperatorMatch { tokens }) -> tokens
                | None -> (match m_ctor_name with
                   | None -> failwith
                     "error: unable to find parenthesizing pattern"
                   | Some ctor_name -> failwith
                     ("error: unable to find operator " ^ ctor_name)
                )
        in

        let tokens_no_space = tokens
          |. Belt.List.keep (function
            | Underscore _ -> false
            | _ -> true
          )
        in

        { sort_name; node_type;
          children = children
            |. BL.zip tokens_no_space
            |. BL.toArray
            |> Util.array_map_keep (function (parse_result, token) -> match token with
              | TerminalName tn -> Some (TerminalCapture (go_t parse_result))
              | NonterminalName ntn -> Some (NonterminalCapture (go_nt ntn parse_result))
              (* TODO: trivia *)
              | Underscore n -> None
            )
        }

    and go_t : LrParsing.parse_result -> terminal_capture
      = fun { start_pos; end_pos } ->
        let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
        let content = Js.String.slice str ~from:start_pos ~to_:end_pos in
        { leading_trivia; content; trailing_trivia }

    in

    go_nt root_name root

let lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer
  = fun { terminal_rules } ->
    let lex_items = terminal_rules
      |. Belt.Array.map (fun (tok_name, re) -> Regex.to_string re, tok_name)
      |. Belt.List.fromArray
    in ({|[ \n\r\t]+|}, "SPACE") :: lex_items

let parse desc root_name str =
  try
    let grammar, production_rule_map = to_grammar desc in
    let module Lr0' = LrParsing.Lr0(struct
      let grammar = grammar
    end) in
    let lexer = lexer_of_desc desc in

    (* TODO: come up with better idea where to do this *)
    let augmented_sort_rules = MS.set desc.sort_rules "root"
      (SortRule (
        { sort_name = "root";
          operator_rules = [[]];
          variable = Some
            (* XXX tm vs expr *)
            { tokens = [NonterminalName "tm"]; var_capture = 1 };
        }
      ))
    in

    match Lr0'.lex_and_parse lexer str with
      | Ok root
      ->
        (* Printf.printf "parse result: %s\n" (LrParsing.parse_result_to_string root); *)
        Belt.Result.Ok (tree_of_parse_result
        (module Lr0')
        production_rule_map
        (MS.fromArray grammar.nonterminal_nums)
        augmented_sort_rules
        root_name
        str
        root)
      | Error (Either.Left { start_pos; end_pos; message })
      -> Error (Printf.sprintf
        "lexical error at characters %n - %n (%s):\n%s"
        start_pos
        end_pos
        (Js.String2.slice str ~from:start_pos ~to_:end_pos)
        message
      )
      | Error (Either.Right (char_no, message)) -> Error (Printf.sprintf
        "parser error at character %n:\n%s"
        char_no
        message
      )

  with
    | NonMatchingFixities (sort_name, token_names) -> Error
      ("In sort " ^ sort_name ^ ": all fixities in a precedence level must be the same fixity (this is a limitation of Bison-style parsers (Jison in particular). The operators identified by [" ^ String.concat ", " token_names ^ "] must all share the same fixity.")
    | MixedFixities (b, l) -> Error ("Found a mix of fixities -- all must be uniform " ^ string_of_bool b ^ " " ^ string_of_int l)

let make_concrete_description
  (terminal_rules: pre_terminal_rule list)
  (sort_rules : sort_rule list) =
  let module Parse_regex = Parsing.Incremental(Parsing.Parseable_regex) in
  { terminal_rules = terminal_rules
    |> List.map (fun (PreTerminalRule (name, str_or_re_str)) ->
      match str_or_re_str with
        | Left re_str -> (match Parse_regex.parse re_str with
          | Ok re -> name, re
          | Error msg -> failwith ("failed to parse regex: " ^ msg)
        )
        | Right str -> name, Regex.ReString str
    )
    |> Belt.List.toArray;
  sort_rules = sort_rules
    |> List.map (fun ((SortRule { sort_name }) as rule) -> sort_name, rule)
    |> Belt.List.toArray
    |> Belt.Map.String.fromArray;
  }
