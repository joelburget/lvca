module BA = Belt.Array
module Nominal = Binding.Nominal
module MS = Belt.Map.String
module MMI = Belt.MutableMap.Int
module MSI = Belt.MutableSet.Int
module SI = Belt.Set.Int
module Lexer = ConcreteSyntax_Lexer
module Parser = ConcreteSyntax_Parser
module ParseErrors = ConcreteSyntax_ParseErrors

open Types
open ConcreteSyntaxDescription
include ConcreteSyntax_Private

let find, get_option' = Util.(find, get_option')
let of_ast = ConcreteSyntax_OfAst.of_ast

type invalid_grammar = InvalidGrammar of string

exception CheckValidExn of invalid_grammar

(* Used to analyze token usage (see `token_usage`). We use this to check if
   there are any tokens not captured (a problem in some circumstances), or if
   there are tokens captured twice (always a problem). *)
type tokens_info =
  { captured_tokens : SI.t
  ; repeated_tokens : SI.t
  }

let accumulate_tokens
      { captured_tokens = seen_toks; repeated_tokens = repeated_toks }
      { captured_tokens = seen_toks'; repeated_tokens = repeated_toks' }
  =
  let isect = SI.intersect seen_toks seen_toks' in
  { captured_tokens = SI.diff (SI.union seen_toks seen_toks') isect
  ; repeated_tokens = SI.union isect (SI.union repeated_toks repeated_toks')
  }
;;

let empty_tokens_info = { captured_tokens = SI.empty; repeated_tokens = SI.empty }

let rec token_usage : operator_match_pattern -> tokens_info = function
  | OperatorPattern (_, scope_patterns) ->
    scope_patterns
    |. Belt.List.reduce empty_tokens_info (fun accum scope_pat ->
      accumulate_tokens accum (scope_token_usage scope_pat))
  | SingleCapturePattern cap_num ->
    { captured_tokens = SI.fromArray [| cap_num |]; repeated_tokens = SI.empty }

and scope_token_usage : numbered_scope_pattern -> tokens_info
  = fun (NumberedScopePattern (binder_captures, body_capture)) ->
  let x = token_usage body_capture in
  let y = binder_captures
    |. Belt.List.reduce empty_tokens_info (fun accum tok ->
      accumulate_tokens
        accum
        { captured_tokens = SI.fromArray [| tok |]; repeated_tokens = SI.empty })
  in accumulate_tokens x y
;;

let check_operator_match_validity
  :  nonterminal_token list -> operator_match_pattern
    -> MSI.t * SI.t * (int * nonterminal_token) list
  =
  fun token_list term_pat ->
  let numbered_toks =
    token_list
    |. Belt.List.toArray
    |. BA.mapWithIndex (fun i tok -> i, tok)
    |. MMI.fromArray
  in
  let { captured_tokens; repeated_tokens } = token_usage term_pat in
  let non_existent_tokens = MSI.make () in
  SI.forEach captured_tokens (fun tok_num ->
    if MMI.has numbered_toks tok_num
    then MMI.remove numbered_toks tok_num
    else MSI.add non_existent_tokens tok_num);
  non_existent_tokens, repeated_tokens, MMI.toList numbered_toks
;;

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
  let show_toks toks = toks
    |. BA.map (Printf.sprintf "$%n")
    |. Js.Array2.joinWith ", "
  in
  try
    sort_rules
    |. Belt.Map.String.forEach (fun _i (SortRule { operator_rules }) ->
      let operator_maches = Belt.List.flatten operator_rules in
      Belt.List.forEach
        operator_maches
        (fun _i (OperatorMatch { tokens; operator_match_pattern }) ->
           let non_existent_tokens, duplicate_captures, uncaptured_tokens =
             check_operator_match_validity tokens operator_match_pattern
           in
           if not (SI.isEmpty duplicate_captures)
           then (
             let tok_names = duplicate_captures |. SI.toArray |. show_toks in
             raise
               (CheckValidExn
                  (InvalidGrammar ("tokens captured more than once: " ^ tok_names))));
           if not (MSI.isEmpty non_existent_tokens)
           then (
             let tok_names = non_existent_tokens |. MSI.toArray |. show_toks in
             raise
               (CheckValidExn
                  (InvalidGrammar ("non-existent tokens mentioned: " ^ tok_names))));
           Belt.List.map uncaptured_tokens (fun (_i, tok) ->
             match tok with
             | NonterminalName name ->
               raise
                 (CheckValidExn
                    (InvalidGrammar ("uncaptured nonterminal: " ^ name)))
             | TerminalName nt_name ->
               (match MS.get terminal_rules' nt_name with
                | None ->
                  raise
                    (CheckValidExn
                       (InvalidGrammar
                          ("Named terminal " ^ nt_name ^ " does not exist")))
                | Some regex ->
                  if Util.is_none (Regex.is_literal regex)
                  then raise (CheckValidExn (InvalidGrammar
                  ("Uncaptured regex which is not a string literal: " ^
                  Regex.to_string regex)))
                  )
             | Underscore _n -> ())));
    Belt.Map.String.forEach terminal_rules' (fun _i regex ->
      if Regex.accepts_empty regex
      then
        raise
          (CheckValidExn
             (InvalidGrammar ("Regex accepts empty strings: " ^ Regex.to_string regex)))
    );
    None
  with
  | CheckValidExn err -> Some err
;;

let rec to_string : formatted_tree -> string
  = fun { children } -> children
  |> Array.map (function
    | TerminalCapture { leading_trivia; content; trailing_trivia } ->
      leading_trivia ^ content ^ trailing_trivia
    | NonterminalCapture nonterminal_capture -> to_string nonterminal_capture)
  |> Belt.List.fromArray
  |> String.concat ""
;;

let rec string_of_sort : sort -> string
  = let rec go = fun needs_parens (SortAp (name, args)) ->
      let args' = Belt.Array.map args (go true) in
      match args' with
        | [||] -> name
        | _ ->
          let pre_result = name ^ " " ^ Js.Array2.joinWith args' " " in
          if needs_parens then "(" ^ pre_result ^ ")" else pre_result
    in go false
;;

let string_of_tree_info : tree_info -> string
  = function
    | SortConstruction (sort, _) -> string_of_sort sort
    | Sequence -> "sequence"
    | Primitive Integer -> "integer"
    | Primitive String -> "string"
;;

let rec to_debug_string : formatted_tree -> string
  = fun { children; tree_info } -> children
  |> Array.map (function
    | TerminalCapture { leading_trivia; content; trailing_trivia } ->
      "t:" ^ leading_trivia ^ content ^ trailing_trivia
    | NonterminalCapture nonterminal_capture
    -> "nt:" ^ to_debug_string nonterminal_capture)
  |> Belt.List.fromArray
  |> String.concat ""
  |> Printf.sprintf "%s(%s)" (string_of_tree_info tree_info)
;;

let rec remove_spaces : formatted_tree -> formatted_tree =
  fun { tree_info; children } ->
  let children' = Belt.Array.map children (function
    | TerminalCapture { content } ->
      TerminalCapture { content; leading_trivia = ""; trailing_trivia = "" }
    | NonterminalCapture ntc -> NonterminalCapture (remove_spaces ntc))
  in
  { tree_info; children = children' }
;;

exception ToAstError of string

let prim_to_ast : prim_ty -> string -> primitive =
  fun prim_ty str ->
  match prim_ty with
  | String -> PrimString str
  | Integer ->
    (try PrimInteger (Bigint.of_string str) with
     | _ -> raise (ToAstError "failed to read integer literal"))
;;

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec tree_to_ast
  :  language
  -> ConcreteSyntaxDescription.t
  -> string
  -> formatted_tree
  -> Nominal.term
  = fun (Language sorts as lang) ({ sort_rules } as rules) sort_name tree ->
  Js.log2 "tree_to_ast" (to_debug_string tree);
  match tree.tree_info, tree.children with
  | Sequence, children ->
    let children' =
      children
      |. Belt.Array.map (function
        | TerminalCapture _ ->
          raise @@ ToAstError "Unexpected terminal found in a sequence"
        (* XXX update sort *)
        | NonterminalCapture child -> tree_to_ast lang rules sort_name child)
      |. Belt.List.fromArray
    in
    Sequence children'
  | Primitive prim_ty, [| TerminalCapture { content } |] ->
    Primitive (prim_to_ast prim_ty content)
  | SortConstruction (_, Var), [| TerminalCapture { content = name } |]
  -> Var name
  (* TODO: check validity *)
  | SortConstruction (_, Operator op_name), tree_children ->

    let SortRule { operator_rules } = sort_rules
      |. MS.get sort_name
      |> get_option' ("tree_to_ast: failed to get sort " ^ sort_name)
    in
    let language_pointer = failwith "TODO" in
    let tm = failwith "TODO" in
    let operator_match_pattern, _, _ =
      find_operator_match language_pointer operator_rules tm
    in

    (* Note: have to be careful to re-index capture numbers from 1-based to
     * 0-based *)
    let rec go_numbered_scope = fun (NumberedScopePattern (var_caps, body_pat)) ->
      let binders = Belt.List.map var_caps (fun cap_num -> tree_children
        |. Belt.Array.get (cap_num - 1)
        |> get_option' (Printf.sprintf
          "Failed to find capture %n in pattern for sort %s, operator %s"
          (cap_num - 1) sort_name op_name
        )
        |> capture_to_pat
      )
      in

      Nominal.Scope (binders, go_op_match_pat body_pat)

    and go_op_match_pat = function
      | SingleCapturePattern cap_num -> (
        match Belt.Array.get tree_children (cap_num - 1) with
        | Some (NonterminalCapture tree) ->
          tree_to_ast lang rules sort_name (* XXX sort name *) tree
        | Some (TerminalCapture { content }) ->
          failwith
            (Printf.sprintf
               "Unexpected terminal capture (%s) in tree (%s), capture $%n, \
                match pattern %s"
               content
               (to_string tree)
               cap_num
               (string_of_operator_match_pattern operator_match_pattern))
        | None ->
          failwith
            (Printf.sprintf
               "Failed to find capture %n in tree (%s), match pattern %s"
               cap_num
               (to_string tree)
               (string_of_operator_match_pattern operator_match_pattern))
      )

      | OperatorPattern (op_name, op_children) -> Operator
        ( op_name
        , Belt.List.map op_children go_numbered_scope
        )

    in
    go_op_match_pat operator_match_pattern

  | Primitive _, _ -> assert false

and capture_to_pat : formatted_capture -> Pattern.t = function
  | TerminalCapture { content } -> Var content
  (*
     raise @@ ToAstError (Printf.sprintf
     "Unexpected bare terminal capture (%s) in capture_to_pat"
     content
  *)
  | NonterminalCapture { tree_info; children } ->
    (match tree_info with
     | SortConstruction (_, Operator op_name) ->
       let children' =
         children
         |. Belt.Array.keepMap (fun cap ->
           match cap with
           | TerminalCapture { content } -> None
           | NonterminalCapture child -> Some (capture_to_pat cap))
         |. Belt.List.fromArray
       in
       Operator (op_name, children')
     | SortConstruction (_, Var) ->
       (match children with
        | [| TerminalCapture { content = name } |] -> Var name
        | _ ->
          raise
          @@ ToAstError
               "Unexpected children of a var capture (expected a single terminal capture)")
     | Sequence ->
       let children' = children |. Belt.Array.map capture_to_pat |. Belt.List.fromArray in
       Sequence children'
     | Primitive prim_ty ->
       (match children with
        | [| TerminalCapture { content } |] -> Primitive (prim_to_ast prim_ty content)
        | _ -> raise @@ ToAstError "Unexpected primitive capture in capture_to_pat"))
;;

let to_ast
  : language -> ConcreteSyntaxDescription.t -> string -> formatted_tree
    -> (Nominal.term, string) Belt.Result.t
  =
  fun lang rules sort tree ->
  try Ok (tree_to_ast lang rules sort tree) with
  | ToAstError msg -> Error msg
;;

(* All infix operators of the same priority must have the same fixity.
 * TODO: is this a restriction that still makes sense?
*)
exception MixedFixities of bool * int

(* Produce an augmented grammar *)
let to_grammar
  :  ConcreteSyntaxDescription.t
    -> string
    -> LrParsing.grammar
       * (nonterminal_token list * operator_match_pattern option) Belt.MutableMap.Int.t
  =
  fun { terminal_rules; sort_rules } start_nonterminal ->
  let terminal_key_arr = Belt.Array.map terminal_rules (fun (k, _) -> k) in
  let terminal_nums =
    Belt.Array.(
      terminal_key_arr
      (* start other terminals (besides $ and SPACE) at 2 *)
      |. mapWithIndex (fun i name -> name, i + 2)
      |. concat [| "$", 0; "SPACE", 1 |])
  in
  let nonterminal_names_map =
    Belt.Map.String.(
      sort_rules
      |. keysToArray
      (* start other nonterminals (besides root) at 1 *)
      |. Belt.Array.mapWithIndex (fun i name -> name, i + 1)
      |. fromArray
      |. set "root" 0)
  in
  (* Spaces are removed by the parsing step, so we need to remove them from
   * our token list. *)
  let non_space_tokens : nonterminal_token -> LrParsing.symbol option = function
    | TerminalName tn ->
      Some
        (LrParsing.Terminal
           (MS.fromArray terminal_nums
            |. MS.get tn
            |> get_option' ("to_grammar: failed to get terminal " ^ tn)))
    | NonterminalName ntn ->
      Some
        (Nonterminal
           (nonterminal_names_map
            |. MS.get ntn
            |> get_option' ("to_grammar: failed to get nonterminal " ^ ntn)))
    | Underscore _ -> None
  in
  (* We're dealing with a non-augmented grammar here. Start counting from 0.
  *)
  let prod_num = ref 1 in
  let start_nonterminal_num = ref 0 in
  let production_rule_map = Belt.MutableMap.Int.make () in
  let nonterminals =
    sort_rules
    |. MS.toArray
    |. BA.mapWithIndex (fun i (sort_name, SortRule { operator_rules }) ->
      let op_prods : LrParsing.symbol list list =
        operator_rules
        |. Belt.List.map (fun operator_level ->
          operator_level
          |. Belt.List.map (fun (OperatorMatch rule) ->
            Belt.MutableMap.Int.set
              production_rule_map
              !prod_num
              (rule.tokens, Some rule.operator_match_pattern);
            prod_num := !prod_num + 1;
            Belt.List.keepMap rule.tokens non_space_tokens))
        (* TODO: temporary pending precedence parsing *)
        |. Belt.List.flatten
      in
      if sort_name = start_nonterminal then start_nonterminal_num := i + 1;
      i + 1, { LrParsing.productions = op_prods })
    |. Belt.Map.Int.fromArray
    |. Belt.Map.Int.set 0 { productions = [ [ Nonterminal !start_nonterminal_num ] ] }
  in
  let nonterminal_nums = MS.toArray nonterminal_names_map in
  { nonterminals; terminal_nums; nonterminal_nums }, production_rule_map
;;

let production_sort_name
  :  LrParsing.nonterminal_num Belt.MutableMap.Int.t
    -> LrParsing.nonterminal_num Belt.Map.String.t -> LrParsing.production_num -> string
  =
  fun nt_map nonterminal_nums prod_num ->
  let nt_num =
    nt_map
    |. MMI.get prod_num
    |> get_option' ("production_sort_name: failed to get " ^ string_of_int prod_num)
  in
  let f _ nt_num' = nt_num' = nt_num in
  match Belt.Map.String.findFirstBy nonterminal_nums f with
  | None -> failwith "production_sort_name: invariant violation: sort not found"
  | Some (name, _) -> name
;;

let tree_of_parse_result (module Lr0 : LrParsing.LR0)
  :  (nonterminal_token list * operator_match_pattern option) Belt.MutableMap.Int.t
    -> LrParsing.nonterminal_num MS.t -> ConcreteSyntaxDescription.sort_rules
    -> string (* root name *) -> string (* parsed string *) -> LrParsing.parse_result
    -> formatted_tree
  =
  fun production_rule_map nonterminal_nums sort_rules root_name str root ->
  Js.log2 "tree_of_parse_result" (LrParsing.parse_result_to_string root);
  let str_pos = ref 0 in
  let str_len = Js.String2.length str in
  let get_trivia : int -> int -> string * string =
    fun start_pos end_pos ->
      (* look back consuming all whitespace to (and including) a newline *)
      let leading_trivia = Js.String2.slice str ~from:!str_pos ~to_:start_pos in
      (* look forward consuming all whitespace up to a newline *)
      str_pos := end_pos;
      let continue = ref true in
      while !continue do
        (* TODO: need to be aware of other whitespace tokens *)
        let got_space = Js.String2.charAt str !str_pos = " " in
        continue := !str_pos < str_len && got_space;
        if !continue then str_pos := !str_pos + 1
      done;
      let trailing_trivia = Js.String2.slice str ~from:end_pos ~to_:!str_pos in
      leading_trivia, trailing_trivia
  in
  let rec go_nt : string -> LrParsing.parse_result -> formatted_tree =
    fun nt_name { production; children } ->
      let prod_num =
        match production with
        | Left prod -> failwith
          (Printf.sprintf
             "invariant violation: go_nt (nt_name %s) received a terminal production: %s"
             nt_name
             (Lr0.string_of_terminal prod))
        | Right prod_num -> prod_num
      in
      Js.log3 "go_nt" nt_name prod_num;
      let tokens, m_operator_match_pattern =
        match Belt.MutableMap.Int.get production_rule_map prod_num with
        | None -> failwith "TODO: error"
        | Some result -> result
      in
      let tree_info =
        match m_operator_match_pattern with
        | None -> failwith
          (Printf.sprintf
             "invariant violation: go_nt couldn't find production %n"
             prod_num)
        | Some (SingleCapturePattern pat) -> failwith "TODO"
        | Some (OperatorPattern (ctor_name, _))
        -> SortConstruction (failwith "TODO", Operator ctor_name)
      in
      let tokens_no_space =
        tokens
        |. Belt.List.keep (function
          | Underscore _ -> false
          | _ -> true)
      in
      { tree_info
      ; children =
          children
          |. Belt.List.zip tokens_no_space
          |. Belt.List.toArray
          |> Util.array_map_keep (function parse_result, token ->
            (match token with
             | TerminalName _ -> Some (TerminalCapture (go_t parse_result))
             | NonterminalName ntn ->
               Some (NonterminalCapture (go_nt ntn parse_result))
             (* TODO: trivia *)
             | Underscore _ -> None))
      }
  and go_t : LrParsing.parse_result -> formatted_terminal_capture =
    fun { start_pos; end_pos } ->
      let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
      let content = Js.String.slice str ~from:start_pos ~to_:end_pos in
      Js.log2 "go_t" content;
      { leading_trivia; content; trailing_trivia }
  in
  go_nt root_name root
;;

let lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer =
  fun { terminal_rules } ->
  let lex_items =
    terminal_rules
    |. Belt.Array.map (fun (tok_name, re) -> Regex.to_string re, tok_name)
    |. Belt.List.fromArray
  in
  ({|[ \n\r\t]+|}, "SPACE") :: lex_items
;;

let parse desc root_name str =
  try
    let grammar, production_rule_map = to_grammar desc root_name in
    (*
       production_rule_map
       |. Belt.MutableMap.Int.mapWithKey (fun i (tokens, m_operator_match_pattern) ->
       Printf.printf "production rule %n: %s %s\n"
       i
       (string_of_tokens tokens)
       (match m_operator_match_pattern with
       | Some x -> string_of_operator_match_pattern x
       | None -> "(none)")
       );
    *)
    let module Lalr = LalrParsing.Lalr1 (struct
                        let grammar = grammar
                      end)
    in
    let lexer = lexer_of_desc desc in
    (* TODO: come up with better idea where to do this *)
    let augmented_sort_rules =
      MS.set
        desc.sort_rules
        "root"
        (SortRule { sort_name = "root"; operator_rules = [ [] ] })
    in
    match Lalr.lex_and_parse lexer str with
    | Ok root ->
      Belt.Result.Ok
        (tree_of_parse_result
           (module Lalr)
           production_rule_map
           (MS.fromArray grammar.nonterminal_nums)
           augmented_sort_rules
           root_name
           str
           root)
    | Error (Either.Left { start_pos; end_pos; message }) ->
      Error
        (Printf.sprintf
           "lexical error at characters %n - %n (%s):\n%s"
           start_pos
           end_pos
           (Js.String2.slice str ~from:start_pos ~to_:end_pos)
           message)
    | Error (Either.Right (char_no, message)) ->
      Error (Printf.sprintf "parser error at character %n:\n%s" char_no message)
  with
  | MixedFixities (b, l) ->
    Error (Printf.sprintf
      "Found a mix of fixities -- all must be uniform: %b / %n" b l)
;;

let make_concrete_description
      (terminal_rules : pre_terminal_rule list)
      (sort_rules : sort_rule list)
  =
  let module Parse_regex = Parsing.Incremental (Parsing.Parseable_regex) in
  { terminal_rules =
      terminal_rules
      |. Belt.List.map (fun (PreTerminalRule (name, str_or_re_str)) ->
        match str_or_re_str with
        | Left re_str ->
          (match Parse_regex.parse re_str with
           | Ok re -> name, re
           | Error msg -> failwith ("failed to parse regex: " ^ msg))
        | Right str -> name, Regex.ReString str)
      |. Belt.List.toArray
  ; sort_rules =
      sort_rules
      |. Belt.List.map (fun (SortRule { sort_name } as rule) -> sort_name, rule)
      |. Belt.List.toArray
      |. Belt.Map.String.fromArray
  }
;;
