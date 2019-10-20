module Result = Belt.Result
module BL = Belt.List
module BA = Belt.Array
module Nominal = Binding.Nominal
open Types
open Types.ConcreteSyntaxDescription
module AA = Util.ArrayApplicative(struct type t = string end)
open AA
module MS = Belt.Map.String
module MI = Belt.Map.Int
let (find, get_option', invariant_violation, traverse_list_result) =
  Util.(find, get_option', invariant_violation, traverse_list_result)
module MMI = Belt.MutableMap.Int
module MSI = Belt.MutableSet.Int
module SI = Belt.Set.Int

type prim_ty =
  | Integer
  | String

type node_type =
  | Operator  of string
  | Var
  | Sequence
  | Primitive of prim_ty

type terminal_capture =
  { content         : string;
    leading_trivia  : string;
    trailing_trivia : string;
  }

type nonterminal_capture = tree

and capture =
  | TerminalCapture    of terminal_capture
  | NonterminalCapture of nonterminal_capture
  | SpaceCapture       of string

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
    children        : capture array;
  }

(* tree equality mod trivia *)
let rec equivalent t1 t2 =
  t1.sort = t2.sort &&
  t1.node_type = t2.node_type &&
  BA.(every (zipBy t1.children t2.children equivalent')) (fun b -> b)

and equivalent' child1 child2 = match child1, child2 with
  | TerminalCapture     tc1, TerminalCapture     tc2 -> tc1 = tc2
  | NonterminalCapture ntc1, NonterminalCapture ntc2 -> equivalent ntc1 ntc2
  | SpaceCapture       str1, SpaceCapture       str2 -> str1 = str2
  |                       _,                       _ -> false

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
(* Escape special characters: TODO more explanation *)
let rec regex_piece_to_string : regex_piece -> string = function
  | ReString str   -> Js.String.(str
    |> replaceByRe [%re "/\\//g"] "\\/"
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
  : term_pattern -> tokens_info
  = function
  | TermPattern (_, scope_patterns) -> scope_patterns
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
  : nonterminal_token list -> term_pattern
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
  let terminal_rules' = M.fromArray terminal_rules in

  try
    sort_rules
      |. M.map (fun (SortRule { operator_rules }) ->
        let operator_maches = BL.flatten operator_rules in
        BL.map operator_maches (fun (OperatorMatch { tokens; term_pattern }) ->
          let non_existent_tokens, duplicate_captures, uncaptured_tokens =
            check_operator_match_validity tokens term_pattern
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
          BL.map uncaptured_tokens (fun (i, tok) -> match tok with
            | NonterminalName name -> raise (CheckValidExn
              (InvalidGrammar ("uncaptured nonterminal: " ^ name)))
            | TerminalName nt_name -> (match M.get terminal_rules' nt_name with
              | None -> raise (CheckValidExn (InvalidGrammar
                ("Named terminal " ^ nt_name ^ " does not exist")))
              | Some regex -> if Util.is_none (regex_is_literal regex)
                (* TODO: print it *)
                then raise (CheckValidExn (InvalidGrammar
                  "Uncaptured regex which is not a string literal"))
            )
            | Underscore -> ()
          )
        );
      );
    terminal_rules' |. M.map (fun regex ->
      if Types.ConcreteSyntaxDescription.accepts_empty regex then
        raise (CheckValidExn (InvalidGrammar
          (* TODO: print it *)
          "Regex accepts empty strings"
        ))
    );
    None
  with
    CheckValidExn err -> Some err

let mk_tree sort node_type children =
  { sort;
    node_type;
    leading_trivia  = "";
    trailing_trivia = "";
    children;
  }

(* Helper for use in of_ast *)
let mk_terminal_capture content =
  TerminalCapture { leading_trivia = ""; content; trailing_trivia = "" }

(* TODO: handle non-happy cases *)
(* Raises: InvariantViolation *)
let rec of_ast
  (Language sorts as lang)
  ({ terminal_rules; sort_rules } as rules)
  current_sort
  tm
  = let terminal_rules' = M.fromArray terminal_rules in
    match current_sort, tm with
  | SortAp (sort_name, _), Nominal.Operator (op_name, scopes) ->
    let SortRule { operator_rules } = get_option'
      ("of_ast: failed to get sort " ^ sort_name)
      (M.get sort_rules sort_name)
    in
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
        -> let SortDef (_, operator_defs) = get_option'
             ("of_ast: failed to get sort" ^ sort) @@
             M.get sorts sort
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

        | FoundBinder binder_name, TerminalName _name
        -> mk_terminal_capture binder_name

        (* if the current token is a terminal, and we didn't capture a binder
         * or term, we just emit the contents of the token *)
        | NotFound, TerminalName name
        -> let terminal_rule = get_option'
              ("of_ast: failed to get terminal rule " ^ name) @@
              M.get terminal_rules' name
           in
           (match regex_is_literal terminal_rule with
             | Some re_str -> mk_terminal_capture re_str
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
  -> mk_tree current_sort Var [| mk_terminal_capture name |]

  | SortAp ("sequence", [|sort|]), Nominal.Sequence tms ->
    let children = tms
      |> List.map (fun tm -> NonterminalCapture (of_ast lang rules sort tm))
      |> BL.toArray
    in
    mk_tree current_sort Sequence children

  | SortAp ("string", [||]), Nominal.Primitive (PrimString str) ->
    mk_tree current_sort (Primitive String) [| mk_terminal_capture str |]

  | SortAp ("integer", [||]), Nominal.Primitive (PrimInteger i) ->
    let str = Bigint.to_string i in
    mk_tree current_sort (Primitive Integer) [| mk_terminal_capture str |]

  | _, _ -> raise (BadSortTerm (current_sort, tm))

let rec to_string { leading_trivia; children; trailing_trivia } =
  let children_str = children
    |> Array.map
      (function
      | TerminalCapture { leading_trivia; content; trailing_trivia }
      -> leading_trivia ^ content ^ trailing_trivia
      | NonterminalCapture nonterminal_capture
      -> to_string nonterminal_capture
      | SpaceCapture str -> str
      )
    |> BL.fromArray
    |> String.concat ""
  in leading_trivia ^ children_str ^ trailing_trivia

let rec remove_spaces : tree -> tree
  = fun { sort; node_type; leading_trivia; trailing_trivia; children }
    -> let children' = children
         |> Util.array_map_keep (function
           | SpaceCapture _ -> None
           | TerminalCapture tc -> Some (TerminalCapture tc)
           | NonterminalCapture ntc
           -> Some (NonterminalCapture (remove_spaces ntc))
         )
       in { sort; node_type; leading_trivia; trailing_trivia; children = children' }

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec to_ast lang tree
  : (Nominal.term, string) Result.t
  = let { node_type; children; } = remove_spaces tree in
    match node_type, children with
    | Var, [| TerminalCapture { content = name } |]
    -> Result.Ok (Nominal.Var name)
    | Sequence, _ -> Result.map
      (traverse_array_result
        (function
          | SpaceCapture _
          -> failwith "invariant violation: space found in to_ast sequence"
          | TerminalCapture _ -> Error "TODO: message"
          | NonterminalCapture child -> to_ast lang child
        )
        children)
      (fun children' -> Nominal.Sequence (BL.fromArray children'))
    | Primitive prim_ty, [| TerminalCapture { content = str } |]
    -> (match prim_ty with
      | String  -> Ok (Nominal.Primitive (PrimString str))
      | Integer ->
        try
          Ok (Nominal.Primitive (PrimInteger (Bigint.of_string str)))
        with
          _ -> Error "failed to read integer literal"
      )

    (* TODO: check validity *)
    | Operator op_name, _ ->
      let children' = children
        |. BA.keepMap (function
          | TerminalCapture { content } -> None
          | SpaceCapture _ -> failwith "invariant violation: space found in to_ast"
          | NonterminalCapture child -> Some (scope_to_ast lang child)
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
          | SpaceCapture _
          -> failwith "invariant violation: space found in to_ast"
          | TerminalCapture { content = binder_name }
          -> Ok binder_name
          | NonterminalCapture _nonterminal_capture
          -> Error "expected binder name, got TODO"
        )
      |. Result.map
        (fun binders' -> Nominal.Scope (List.rev binders', body'))
    | Error err -> Error err
  )
  | [] -> Error "scope_to_ast called on no children"

exception NonMatchingFixities of string * string list
exception MixedFixities of bool * int

(* Produce an augmented grammar *)
let to_grammar ({terminal_rules; sort_rules}: ConcreteSyntaxDescription.t)
  : LrParsing.grammar
      (* TODO: how to do string -> int mapping? *)
  = let terminal_key_arr = BA.map terminal_rules (fun (k, _) -> k) in
    let nonterminal_key_arr = MS.keysToArray sort_rules in
    let terminal_names = terminal_key_arr
        |. BA.mapWithIndex (fun i name -> name, i)
        |. MS.fromArray
    in
    let nonterminal_names = sort_rules
        |. MS.keysToArray
        |. BA.mapWithIndex (fun i name -> name, i)
        |. MS.fromArray
        |. MS.set "root" 0
    in

    let nonterminals = sort_rules
      |. MS.valuesToArray
      |. BA.mapWithIndex (fun i (SortRule { operator_rules; variable }) ->
        let productions = operator_rules
          |. BL.map (fun operator_level -> operator_level |.
            BL.map (fun (OperatorMatch { tokens }) ->
              tokens |. BL.map (function
                | TerminalName tn
                -> LrParsing.Terminal (terminal_names
                  |. MS.get tn
                  |> get_option'
                    ("to_grammar: failed to get " ^ tn)
                )
                | NonterminalName ntn
                -> Nonterminal (nonterminal_names
                  |. MS.get ntn
                  |> get_option'
                    ("to_grammar: failed to get " ^ ntn)
                )
                | Underscore
                -> Terminal (terminal_names
                  (* XXX name consistency -- is it SPACE or WHITESPACE? *)
                  |. MS.get "SPACE"
                  |> get_option'
                    ("to_grammar: failed to get SPACE")
                )
              )
            )
          )
          (* TODO: temporary? *)
          |. BL.flatten
        in
        i + 1, { LrParsing.productions = productions }
      )
      |. MI.fromArray
      |. MI.set 0 { productions = [ [ Nonterminal 1 ] ] }
    in

    {
      nonterminals; terminal_names; nonterminal_names;
      num_terminals = BA.length terminal_rules;
    }

let production_info
  : LrParsing.nonterminal_num MMI.t
  -> LrParsing.nonterminal_num MS.t
  -> LrParsing.production_num
  -> node_type * sort
  = fun nt_map nonterminal_names prod_num ->
    let nt_num = nt_map
      |. MMI.get prod_num
      |> get_option'
        ("production_info: failed to get " ^ string_of_int prod_num)
    in
    let f _ nt_num' = nt_num' = nt_num in
    let sort_name = match nonterminal_names |. MS.findFirstBy f with
      | None -> failwith "production_info: invariant violation: sort not found"
      | Some (name, _) -> name
    in
    (* XXX sort name not applied to anything *)
    Operator sort_name, SortAp (sort_name, [||])

let tree_of_parse_result
  (module Lr0 : LrParsing.LR0)
  : LrParsing.nonterminal_num MS.t
  -> ConcreteSyntaxDescription.sort_rules
  -> string
  -> LrParsing.parse_result
  -> tree
  = fun nonterminal_names sort_rules str root ->
    let str_pos = ref 0 in
    let str_len = Js.String2.length str in

    let get_trivia : int -> int -> string * string
      = fun start_pos end_pos ->
        (* look back consuming all whitespace to (and including) a newline *)
        let leading_trivia =
          str |. Js.String2.slice ~from:!str_pos ~to_:start_pos
        in

        (* look forward consuming all whitespace up to a newline *)
        str_pos := end_pos;
        let continue = ref true in
        while !continue do
          let got_space, got_newline = match Js.String2.charAt str !str_pos with
            | " "  -> true,  false
            | "\ " -> true,  false
            | "\n" -> false, true
            | _    -> false, false
          in
          continue := !str_pos < str_len && got_space;
          if !continue then str_pos := !str_pos + 1;
        done;

        let trailing_trivia =
          str |. Js.String2.slice ~from:end_pos ~to_:!str_pos
        in

        leading_trivia, trailing_trivia
    in

    let rec go_nt : string -> LrParsing.parse_result -> tree
      = fun nt_name { production; children; start_pos; end_pos } ->
        let prod_num = match production with
          | Left _ -> failwith
            "invariant violation: go_nt received a terminal production"
          | Right prod_num -> prod_num
        in
        let node_type, sort = production_info
          (Lr0.production_nonterminal_map)
          nonterminal_names
          prod_num
        in
        let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in

        let ctor_name = match node_type with
          | Operator ctor_name -> ctor_name
          | _ -> failwith
            "invariant violation: got non-operator when handling a nonterminal"
        in

        let err = "tree_of_parse_result: failed to get " ^ nt_name in

        let tokens : nonterminal_token list
          = match sort_rules |. MS.get nt_name |> get_option' err with
            | SortRule { operator_rules } ->
              let maybe_op_rule = operator_rules
              |. BL.flatten
              |. BL.getBy (fun (OperatorMatch { term_pattern }) ->
                match term_pattern with
                  | TermPattern (op_name, _) -> ctor_name = op_name
                  | ParenthesizingPattern _ -> true
              )
              in
              match maybe_op_rule with
                | None -> failwith "error: unable to find operator"
                | Some (OperatorMatch { tokens }) -> tokens
        in

        { sort; node_type; leading_trivia; trailing_trivia;
          children = children
            |. BL.zip tokens
            |. BL.map (function (parse_result, token) -> match token with
              | TerminalName tn -> TerminalCapture (go_t tn parse_result)
              | NonterminalName ntn -> NonterminalCapture (go_nt ntn parse_result)
              (* Attach whitespace to its neighbor *)
              | Underscore -> SpaceCapture " " (* TODO *)
              (* (terminal_capture, nonterminal_capture) Either.t *)
            )
            |. BL.toArray
        }

    and go_t : string -> LrParsing.parse_result -> terminal_capture
      = fun t_name { start_pos; end_pos } ->
        let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
        let content = Js.String.slice str ~from:start_pos ~to_:end_pos in
        { leading_trivia; content; trailing_trivia }

    in

    go_nt "root" (* XXX assumption: root is called root *) root

let lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer
  = fun { terminal_rules } -> terminal_rules
    |. BA.map (fun (tok_name, re) -> regex_to_string re, tok_name)
    |. BL.fromArray

let parse desc str =
  try
    let grammar = to_grammar desc in
    let module Lr0' = LrParsing.Lr0(struct
      let grammar = grammar
    end) in
    let lexer = lexer_of_desc desc in

    match Lr0'.lex_and_parse lexer str with
      | Result.Ok result
      -> Result.Ok (tree_of_parse_result
        (module Lr0')
        grammar.nonterminal_names
        desc.sort_rules
        str
        result)
      | Result.Error (Either.Left { start_pos; end_pos; message })
      -> Error (Printf.sprintf
        "lexical error at characters %n - %n (%s):\n%s"
        start_pos
        end_pos
        (Js.String2.slice str ~from:start_pos ~to_:end_pos)
        message
      )
      | Result.Error (Either.Right (char_no, message)) -> Error (Printf.sprintf
        "parser error at character %n:\n%s"
        char_no
        message
      )

  with
    | NonMatchingFixities (sort_name, token_names) -> Result.Error
      ("In sort " ^ sort_name ^ ": all fixities in a precedence level must be the same fixity (this is a limitation of Bison-style parsers (Jison in particular). The operators identified by [" ^ String.concat ", " token_names ^ "] must all share the same fixity.")
    | MixedFixities (b, l) -> Error ("Found a mix of fixities -- all must be uniform " ^ string_of_bool b ^ " " ^ string_of_int l)
