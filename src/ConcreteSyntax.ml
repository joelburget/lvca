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

let get_option, get_option' = Util.(get_option, get_option')
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
 * 5. Boxes are matching: There are the same number of '[' and ']' tokens, each
 *    box is opened before it's closed.
 * 6. Only binary operators (`tm OP tm`) can have a left or right
 *    associativity.
 * 7. All operators of the same priority must have the same fixity.
 *
 * Examples:
 * * FOO bar BAZ { op($1; $2; $3) } valid
 * * FOO bar BAZ { op($1; $3) } invalid (uncaptured nonterminal)
 * * FOO bar BAZ { op($1; $2) } possibly invalid (if BAZ isn't a string literal)
 * * FOO bar BAZ { op($2; $2) } invalid (repeated token)
 * * FOO bar BAZ { op($2; $4) } invalid (non-existent token)
*)
let check_description_validity { terminal_rules; nonterminal_rules } =
  let raise_invalid str = raise (CheckValidExn (InvalidGrammar str)) in
  let terminal_rules' = Belt.Map.String.fromArray terminal_rules in
  let show_toks toks = toks
    |. BA.map (Printf.sprintf "$%n")
    |. Js.Array2.joinWith ", "
  in
  let open_depth = ref 0 in
  try
    nonterminal_rules
    |. Belt.Map.String.forEach (fun _i (NonterminalRule { operator_rules }) ->
      operator_rules
        |. Belt.List.forEach (fun level ->
          let OperatorMatch { fixity } = level
            |. Belt.List.head
            |> get_option' "each level is guaranteed to have at least one rule"
          in
          let okay = Belt.List.every level
            (fun (OperatorMatch op_match) -> op_match.fixity = fixity)
          in
          if not okay then raise_invalid "Every operator in a precedence level \
            must have the same fixity"
        );

      operator_rules
        |. Belt.List.flatten
        |. Belt.List.forEach
        (fun _i (OperatorMatch { tokens; operator_match_pattern; fixity }) ->
           let non_existent_tokens, duplicate_captures, uncaptured_tokens =
             check_operator_match_validity tokens operator_match_pattern
           in

           if not (SI.isEmpty duplicate_captures)
           then (
             let tok_names = duplicate_captures |. SI.toArray |. show_toks in
             raise_invalid
               ("tokens captured more than once: " ^ tok_names));

           if not (MSI.isEmpty non_existent_tokens)
           then (
             let tok_names = non_existent_tokens |. MSI.toArray |. show_toks in
             raise_invalid ("non-existent tokens mentioned: " ^ tok_names));

           let tokens' = tokens
             |. Belt.List.keep (function
               | TerminalName _ | NonterminalName _ -> true
               | _ -> false
             )
           in
           (match tokens' with
             | [ NonterminalName _; TerminalName _; NonterminalName _ ] -> ()
             | _
             -> if fixity == Infixl || fixity == Infixr
                then raise_invalid "left or right fixity can only be applied \
                  to a binary operator (eg `tm OP tm`)");

           Belt.List.map uncaptured_tokens (fun (_i, tok) ->
             match tok with
             | NonterminalName name ->
               raise_invalid ("uncaptured nonterminal: " ^ name)
             | TerminalName nt_name ->
               (match MS.get terminal_rules' nt_name with
                | None ->
                  raise_invalid ("Named terminal " ^ nt_name ^ " does not exist")
                | Some regex ->
                  if Util.is_none (Regex.is_literal regex)
                  then raise_invalid
                    ("Uncaptured regex which is not a string literal: " ^
                    Regex.to_string regex)
                  )
             | OpenBox _ -> incr open_depth;
             | CloseBox -> (
               if !open_depth <= 0
               then raise_invalid "Invalid box structure (saw a close box \
                 marker (']') before its opening marker ('['))!";
               decr open_depth;
             )
             | Underscore _ -> ())
        ));
    if !open_depth != 0
    then raise_invalid "At least one group is not closed (there are more open \
      box markers ('[') than close box markers (']'))";
    Belt.Map.String.forEach terminal_rules' (fun _i regex ->
      if Regex.accepts_empty regex
      then raise_invalid ("Regex accepts empty strings: " ^ Regex.to_string regex)
    );
    None
  with
    CheckValidExn err -> Some err
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

let prim_to_ast
  : string -> formatted_tree -> primitive
  = fun prim_ty tree -> match tree.children with
    | [| TerminalCapture { content } |] -> (match prim_ty with
      | "string" -> PrimString content
      | "integer" -> (
        try
          PrimInteger (Bigint.of_string content)
        with
           _ -> raise (ToAstError "failed to read integer literal")
      )
      | _ -> raise (Util.InvariantViolation (Printf.sprintf
        "prim_to_ast can only be called with string or integer (called with %s)"
        prim_ty
      ))
    )
    | _ -> raise @@ ToAstError "TODO: message"
;;

let get_operator_match
  : ConcreteSyntaxDescription.t -> formatted_tree -> operator_match_pattern
  = fun rules tree ->
  let nt_name, nt_prod_no = tree.tree_info in
  let OperatorMatch { operator_match_pattern } = rules.nonterminal_rules
    |. Belt.Map.String.get nt_name
    |> get_option (ToAstError "TODO: message")
    |> fun (NonterminalRule { operator_rules }) -> operator_rules
    |. Belt.List.flatten (* TODO: should we use 2d indexing? *)
    |. Belt.List.get nt_prod_no
    |> get_option' "TODO: message"
  in operator_match_pattern
;;

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec tree_to_ast
  :  ConcreteSyntaxDescription.t
  -> formatted_tree
  -> Nominal.term
  = fun rules tree ->
  (* Js.log2 "tree_to_ast" (to_debug_string tree); *)
  let nt_name, _ = tree.tree_info in
  match nt_name with
    | "list" -> failwith "TODO"
    | "string"
    | "integer" -> Primitive (prim_to_ast nt_name tree)
    | _ -> tree
      |> get_operator_match rules
      |> go_op_match_term rules tree.children

and go_op_match_term
  :  ConcreteSyntaxDescription.t
  -> formatted_capture array
  -> operator_match_pattern
  -> Nominal.term
  = fun rules children op_match_pat -> match op_match_pat with
    | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern n)])
    -> (match children.(n - 1) with
      | NonterminalCapture _ -> failwith "TODO: error"
      | TerminalCapture { content } -> Var content
    )
    | OperatorPattern ("var", _) -> failwith "TODO: error"
    | OperatorPattern (name, scope_pats)
    -> Operator
      ( name
      , Belt.List.map scope_pats (go_numbered_scope_term rules children)
      )
    | SingleCapturePattern n
    -> (match children.(n - 1) with
      | NonterminalCapture tree -> tree_to_ast rules tree
      | TerminalCapture { content } -> failwith (Printf.sprintf (* TODO: error *)
        "go_op_match_term: Unexpectedly received a terminal when a nonterminal child was expected: %n -> %s" n content
      )
    )

and go_numbered_scope_term
  :  ConcreteSyntaxDescription.t
  -> formatted_capture array
  -> numbered_scope_pattern
  -> Nominal.scope
  = fun rules children (NumberedScopePattern (cap_nums, op_match_pat)) -> Scope
    ( cap_nums
      |. Belt.List.map (fun n -> match children.(n - 1) with
        | NonterminalCapture tree -> tree_to_pattern rules tree
        | TerminalCapture { content } -> failwith (Printf.sprintf (* TODO: error *)
        "go_numbered_scope_term: Unexpectedly received a terminal when a nonterminal child was expected: %n -> %s" n content
      )
      )
    , go_op_match_term rules children op_match_pat
    )

and tree_to_pattern
  :  ConcreteSyntaxDescription.t
  -> formatted_tree
  -> Pattern.t
  = fun rules tree ->
  let nt_name, _ = tree.tree_info in
  match nt_name with
    | "list" -> failwith "TODO"
    | "string"
    | "integer" -> Primitive (prim_to_ast nt_name tree)
    | _ -> tree
      |> get_operator_match rules
      |> go_op_match_pattern rules tree.children

and go_op_match_pattern
  :  ConcreteSyntaxDescription.t
  -> formatted_capture array
  -> operator_match_pattern
  -> Pattern.t
  = fun rules children op_match_pat -> match op_match_pat with
    | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern n)])
    -> (match children.(n - 1) with
      | NonterminalCapture _ -> failwith "TODO: error"
      | TerminalCapture { content } -> Var content
    )
    | OperatorPattern ("var", _) -> failwith "TODO: error"
    | OperatorPattern (name, scope_pats)
    -> Operator
      ( name
      , Belt.List.map scope_pats (go_numbered_scope_pattern rules children)
      )
    | SingleCapturePattern n
    -> (match children.(n - 1) with
      | NonterminalCapture tree -> tree_to_pattern rules tree
      | TerminalCapture { content } -> failwith (Printf.sprintf (* TODO: error *)
        "go_op_match_pattern: Unexpectedly received a terminal when a nonterminal child was expected: %n -> %s" n content
      )
    )

and go_numbered_scope_pattern
  :  ConcreteSyntaxDescription.t
  -> formatted_capture array
  -> numbered_scope_pattern
  -> Pattern.t
  = fun rules children (NumberedScopePattern (cap_nums, op_match_pat)) ->
    if Belt.List.length cap_nums > 0 then raise @@ ToAstError "TODO: message";
    go_op_match_pattern rules children op_match_pat
;;

let to_ast
  :  ConcreteSyntaxDescription.t
  -> formatted_tree
  -> (Nominal.term, string) Belt.Result.t
  = fun rules tree ->
    try
      Ok (tree_to_ast rules tree)
    with
      ToAstError msg -> Error msg
;;

(* Produce an augmented grammar *)
let to_grammar
  :  ConcreteSyntaxDescription.t
  -> string
  -> LrParsing.grammar
    * (tree_info * nonterminal_token list * operator_match_pattern option)
        Belt.MutableMap.Int.t
  =
  fun { terminal_rules; nonterminal_rules } start_nonterminal ->
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
      nonterminal_rules
      |. keysToArray
      (* start other nonterminals (besides root) at 1 *)
      |. Belt.Array.mapWithIndex (fun i name -> name, (0 (* XXX *), i + 1))
      |. fromArray
      |. set "root" (0, (* XXX *) 0))
  in
  (* Spaces are removed by the parsing step, so we need to remove them from
   * our token list. *)
  let non_space : nonterminal_token -> bool = function
    | OpenBox _ | CloseBox | Underscore _ -> false
    | _ -> true
  in

  let non_space_tokens : nonterminal_token -> LrParsing.symbol option = function
    | TerminalName tn ->
      Some
        (LrParsing.Terminal
           (terminal_nums
            |. MS.fromArray
            |. MS.get tn
            |> get_option' ("to_grammar: failed to get terminal " ^ tn)))
    | NonterminalName ntn ->
      Some
        (Nonterminal
           (nonterminal_names_map
            |. MS.get ntn
            |> get_option' ("to_grammar: failed to get nonterminal " ^ ntn)
            |> fun (prec, ntn) -> ntn))
    | OpenBox _ | CloseBox | Underscore _ -> None
  in

  (* We're dealing with a non-augmented grammar here. [prod_num] starts
   * counting productions from 1. We'll add the starting production at 0 at the
   * end. *)
  let prod_num = ref 1 in
  (* [nt_num] tracks the number of the nonterminal we're currently on *)
  let nt_num = ref 1 in
  (* [start_nonterminal_num] will be set to the number of the start nonterminal
   * when we find it. *)
  let start_nonterminal_num = ref 0 in
  let production_rule_map = Belt.MutableMap.Int.make () in

  (* Translate a nonterminal into n -- one for eachof its precedence levels *)
  let nonterminals = nonterminal_rules
    |. MS.toArray
    |. BA.map (fun (nonterminal_name, NonterminalRule { operator_rules }) ->
      if nonterminal_name = start_nonterminal
      then start_nonterminal_num := !nt_num;

      (* How many distinct precedence levels does this nonterminal include *)
      let num_levels = Belt.List.length operator_rules in

      (* track the operator number within (all levels of) this nonterminal *)
      let nt_operator_index = ref 0 in

      (* We desugar each precedence level to its own nonterminal. *)
      operator_rules
        |. Belt.List.mapWithIndex (fun level_ix operator_level ->
          let level_productions = operator_level
            |. Belt.List.map (fun (OperatorMatch rule) ->
              Belt.MutableMap.Int.set
                production_rule_map
                !prod_num
                ( (nonterminal_name, !nt_operator_index)
                , rule.tokens
                , Some rule.operator_match_pattern
                );
              incr prod_num;
              incr nt_operator_index;
              (* XXX rewrite to point nonterminals at correct level *)
              Belt.List.keepMap rule.tokens non_space_tokens
            )
          in

          (* If this is not the last level, then there is a lower-precedence
           * level to fall back to. *)
          let level_productions' =
            if level_ix + 1 < num_levels
            (* XXX need level info *)
            then Util.snoc level_productions [ Nonterminal !nt_num ]
            else level_productions
          in

          let result = !nt_num, { LrParsing.productions = level_productions' } in
          incr nt_num;
          result
        )
        |. Belt.List.toArray
        |. Belt.Map.Int.fromArray
    )
    |. Belt.List.fromArray (* TODO: don't convert to list *)
    |. Util.int_map_unions
    |. Belt.Map.Int.set 0 { productions = [ [ Nonterminal !start_nonterminal_num ] ] }
  in
  let nonterminal_nums = nonterminal_names_map
    |. MS.toArray
    |. Belt.Array.map (fun (name, (prec, num)) -> name, prec, num)
  in
  { nonterminals; terminal_nums; nonterminal_nums }, production_rule_map
;;

(*
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
*)

let tree_of_parse_result (module Lr0 : LrParsing.LR0)
  :  (tree_info * nonterminal_token list * operator_match_pattern option)
       Belt.MutableMap.Int.t
  -> LrParsing.nonterminal_num MS.t
  -> ConcreteSyntaxDescription.nonterminal_rules
  -> string (* root name *)
  -> string (* parsed string *)
  -> LrParsing.parse_result
  -> formatted_tree
  =
  fun production_rule_map nonterminal_nums nonterminal_rules root_name str root ->
  (* Js.log2 "tree_of_parse_result" (LrParsing.parse_result_to_string root); *)

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
        if !continue then incr str_pos;
      done;
      let trailing_trivia = Js.String2.slice str ~from:end_pos ~to_:!str_pos in
      leading_trivia, trailing_trivia
  in

  let rec go_nt : string -> LrParsing.parse_result -> formatted_tree =
    fun nt_name { production; children } ->
      let prod_num = match production with
        | Left prod -> invariant_violation (Printf.sprintf
           "go_nt (nt_name %s) received a terminal production: %s"
           nt_name
           (Lr0.string_of_terminal prod))
        | Right prod_num -> prod_num
      in
      (* Js.log3 "go_nt" nt_name prod_num; *)
      let tree_info, tokens, _ =
        match Belt.MutableMap.Int.get production_rule_map prod_num with
        | None -> invariant_violation (Printf.sprintf
          "tree_of_parse_result: couldn't find nonterminal %n in \
          production_rule_map"
          prod_num
        )
        | Some result -> result
      in
      let tokens_no_space = tokens
        |. Belt.List.keep (function Underscore _ -> false | _ -> true)
      in
      { tree_info
      ; children = children
        |. Belt.List.zip tokens_no_space
        |. Belt.List.toArray
        |> Util.array_map_keep (function parse_result, token ->
          (match token with
           | TerminalName _ -> Some (TerminalCapture (go_t parse_result))
           | NonterminalName ntn ->
             Some (NonterminalCapture (go_nt ntn parse_result))
           (* TODO: trivia *)
           | OpenBox _ | CloseBox | Underscore _ -> None))
      }

  and go_t : LrParsing.parse_result -> formatted_terminal_capture =
    fun { start_pos; end_pos } ->
      let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
      let content = Js.String.slice str ~from:start_pos ~to_:end_pos in
      (* Js.log2 "go_t" content; *)
      { leading_trivia; content; trailing_trivia }
  in
  go_nt root_name root
;;

let lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer =
  fun { terminal_rules } ->
  let lex_items = terminal_rules
    |. Belt.Array.map (fun (tok_name, re) -> Regex.to_string re, tok_name)
    |. Belt.List.fromArray
  in
  ({|[ \n\r\t]+|}, "SPACE") :: lex_items
;;

let parse desc root_name str =
  let grammar, production_rule_map = to_grammar desc root_name in
  (*
     production_rule_map
     |. Belt.MutableMap.Int.mapWithKey
       (fun i (tree_info, tokens, m_operator_match_pattern) ->
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
  (* TODO: come up with better idea where to do this augmentation *)
  let augmented_nonterminal_rules =
    MS.set
      desc.nonterminal_rules
      "root"
      (NonterminalRule
        { nonterminal_name = "root"
        ; nonterminal_type = NonterminalType ([], SortAp ("root", [||]))
        ; operator_rules = [ [] ]
        })
  in
  match Lalr.lex_and_parse lexer str with
  | Ok root ->
    Belt.Result.Ok
      (tree_of_parse_result
         (module Lalr)
         production_rule_map
         (grammar.nonterminal_nums
           |. Belt.Array.map (fun (name, prec, num) -> name, num)
           |. MS.fromArray
         )
         augmented_nonterminal_rules
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
;;

let make_concrete_description
      (terminal_rules : pre_terminal_rule list)
      (nonterminal_rules : nonterminal_rule list)
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
  ; nonterminal_rules =
      nonterminal_rules
      |. Belt.List.map (fun (NonterminalRule { nonterminal_name } as rule) ->
        nonterminal_name, rule)
      |. Belt.List.toArray
      |. Belt.Map.String.fromArray
  }
;;
