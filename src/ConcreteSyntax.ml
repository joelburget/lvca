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

(* The parser doesn't need formatting tokens *)
let is_formatting_token
  : nonterminal_token -> bool
  = fun token -> match token with
    | TerminalName _ | NonterminalName _ -> false
    | _ -> true

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
            |> get_option' (fun () ->
              "each level is guaranteed to have at least one rule")
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
             |. Belt.List.keep (fun tok -> not (is_formatting_token tok))
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
    |> get_option' (fun () -> "TODO: message")
  in operator_match_pattern
;;

let array_get : string -> 'a array -> int -> 'a
  = fun msg arr i -> arr
    |. Belt.Array.get i
    |> get_option' (fun () -> Printf.sprintf
      "failed array get in %s: index %n, length %n"
      msg
      i
      (Belt.Array.length arr)
    )

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec tree_to_ast
  :  ConcreteSyntaxDescription.t
  -> formatted_tree
  -> Nominal.term
  = fun rules tree ->
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
  = fun rules children op_match_pat ->
    match op_match_pat with
    | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern n)])
    -> Printf.printf "go_op_match_term var(%n)\n" n;
      (match array_get "go_op_match_term 1" children (n - 1) with
      | NonterminalCapture _ -> failwith "TODO: error"
      | TerminalCapture { content } -> Var content
    )
    | OperatorPattern ("integer", [NumberedScopePattern([], SingleCapturePattern n)])
    -> (match array_get "go_op_match_term 2" children (n - 1) with
      | NonterminalCapture _ -> failwith "TODO: error"
      | TerminalCapture { content }
      -> Primitive (PrimInteger (Bigint.of_string content))
    )
    | OperatorPattern ("var", _)
    | OperatorPattern ("integer", _)
    -> failwith "TODO: error"
    | OperatorPattern (name, scope_pats)
    -> Operator
      ( name
      , Belt.List.map scope_pats (go_numbered_scope_term rules children)
      )
    | SingleCapturePattern n
    -> (match array_get "go_op_match_term 3" children (n - 1) with
      | NonterminalCapture tree -> tree_to_ast rules tree
      | TerminalCapture { content } ->
        Printf.printf "children: [%s]\n" (children
          |. Belt.Array.map string_of_formatted_capture
          |. Js.Array2.joinWith "; "
        );
        failwith (Printf.sprintf (* TODO: error *)
        "go_op_match_term: Single capture pattern unexpectedly received a \
        terminal when a nonterminal child was expected: child %n -> \"%s\""
        n content
      )
    )

and go_numbered_scope_term
  :  ConcreteSyntaxDescription.t
  -> formatted_capture array
  -> numbered_scope_pattern
  -> Nominal.scope
  = fun rules children (NumberedScopePattern (cap_nums, op_match_pat)) -> Scope
    ( cap_nums
      |. Belt.List.map (fun n ->
        match array_get "go_numbered_scope_term" children (n - 1) with
        | NonterminalCapture tree -> tree_to_pattern rules tree
        | TerminalCapture { content } -> failwith (Printf.sprintf (* TODO: error *)
        "go_numbered_scope_term: Unexpectedly received a terminal when a \
        nonterminal child was expected: child %n -> \"%s\"" n content
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
    -> (match array_get "go_op_match_pattern 1" children (n - 1) with
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
    -> (match array_get "go_op_match_pattern 2" children (n - 1) with
      | NonterminalCapture tree -> tree_to_pattern rules tree
      | TerminalCapture { content } -> failwith (Printf.sprintf (* TODO: error *)
        "go_op_match_pattern: Unexpectedly received a terminal when a \
        nonterminal child was expected: child %n -> \"%s\"" n content
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

(** raises: [InvariantViolation] *)
let convert_token
  :  LrParsing.terminal_num MS.t
  -> int Belt.Map.String.t
  -> nonterminal_token
  -> LrParsing.symbol
  = fun terminal_nums nonterminal_entry -> function
  | TerminalName tn -> Terminal
    (terminal_nums
     |. MS.get tn
     |> get_option' (fun () -> "to_grammar: failed to get terminal " ^ tn))
  | NonterminalName nt_name
  -> Nonterminal (nonterminal_entry
    |. Belt.Map.String.get nt_name
    |> get_option' (fun () -> Printf.sprintf
      "convert_token: couldn't find nonterminal %s in names: %s"
      nt_name
      (nonterminal_entry
        |. Belt.Map.String.keysToArray
        |. Js.Array2.joinWith ", ")
    )
  )
  | OpenBox _ | CloseBox | Underscore _
  -> invariant_violation
    "all formatting tokens should be filtered (see is_formatting_token)"

(* Invariants assumed:
 *   %left, %right can't occur at lowest precedence level
 *   Anywhere %left, %right occur has same nonterminal on both sides, parent
 *)
let rewrite_tokens
  : string -> int -> operator_match -> operator_match
  = fun
    nt_name
    level
    (OperatorMatch { tokens; operator_match_pattern; fixity }) ->

    let lowered_nt_name = Printf.sprintf "%s_%n" nt_name (level - 1) in
    Printf.printf "lowered_nt_name: %s\n" lowered_nt_name;
    let tokens' = Belt.List.keep tokens
      (fun tok -> not (is_formatting_token tok))
    in

    let tokens'' = match tokens' with
      | [ NonterminalName name1; TerminalName _ as t; NonterminalName name2 ]
        when name1 = nt_name && name2 = nt_name
        -> (match fixity with
        | Infixl
        -> (* Lower the precedence of the right child by moving to the following
            * (desugared) nonterminal, which corresponds to the next precedence
            * level in the nonterminal we're currently transforming. *)
           [ NonterminalName nt_name;
             t;
             NonterminalName lowered_nt_name;
           ]

        | Infixr
        -> [ NonterminalName lowered_nt_name;
             t;
             NonterminalName nt_name;
           ]

        (* Don't adjust nofix *)
        | Nofix -> tokens'
        )

      (* juxtaposition *)
      | [ NonterminalName name1; NonterminalName name2 ]
      when name1 = nt_name && name2 = nt_name
      (* TODO: adjust! *)
      -> tokens'

      | _ -> tokens'
    in

    (OperatorMatch { tokens = tokens''; operator_match_pattern; fixity = Nofix })

(** An operator match that just defers to a nonterminal / precedence *)
let operator_match_nt_precedence : string -> int -> operator_match
  = fun nonterminal_name precedence -> OperatorMatch
    { tokens = [ NonterminalName
        (Printf.sprintf "%s_%n" nonterminal_name precedence)
      ]
    ; operator_match_pattern = SingleCapturePattern 1
    ; fixity = Nofix
    }

(** Slimmed-down version of [nonterminal_rules], without the name and type
 * information from [NonterminalRule]. Also, limited to a single level, and the
 * [int] is the number of the attached operator rule.
 *)
type nonterminal_operators = (int option * operator_match) list Belt.Map.String.t

let string_of_nonterminal_operators : nonterminal_operators -> string
  = fun nonterminal_operators -> nonterminal_operators
    |. Belt.Map.String.toArray
    |. Belt.Array.map (fun (name, matches) ->
      let rhs = matches
        |. Belt.List.toArray
        |. Belt.Array.map (fun (ix_opt, op_match) ->
            let ix_str = match ix_opt with
              | None -> "_"
              | Some ix -> string_of_int ix
            in
            Printf.sprintf "  %s -> %s" ix_str (string_of_operator_match op_match)
        )
        |. Js.Array2.joinWith "\n"
      in
      Printf.sprintf "%s:\n%s" name rhs
    )
    |. Js.Array2.joinWith "\n"

(** Desugar a nonterminal into one nonterminal per precedence level.
 *
 * raises: [UserError]
 *)
let desugar_nonterminal
  :  Belt.Set.String.t
  -> nonterminal_rule
  -> nonterminal_operators * string option Belt.Map.String.t
  = fun
      taken_names
      (NonterminalRule { nonterminal_name; operator_rules }) ->
    let num_levels = Belt.List.length operator_rules in
    if num_levels = 1
    then
      let operators = operator_rules
        |. Belt.List.headExn
        |. Belt.List.mapWithIndex (fun i op_match -> Some i, op_match)
      in
      Belt.Map.String.fromArray [| nonterminal_name, operators |],
      Belt.Map.String.fromArray [| nonterminal_name, Some nonterminal_name |]
    else

      (* Used to number each operator across all precedence levels *)
      let operator_index = ref 0 in

      let level_nts = Belt.List.mapWithIndex operator_rules (fun i level ->
        (* precedences 1..num_levels *)
        let prec_num = num_levels - i in
        let generated_name = Printf.sprintf "%s_%n" nonterminal_name prec_num in

        (* Our renaming is not hygienic, so we need to prevent name collisions.
         * Also, this would be just confusing to the user. *)
        (if Belt.Set.String.has taken_names generated_name
        then raise (UserError (Printf.sprintf "Desugaring of nonterminal %s \
          generated name %s, which conflicts with an existing nonterminal."
          nonterminal_name generated_name
        )));

        let generated_rules = Belt.List.map level (fun op_match ->
          let ix = !operator_index in
          incr operator_index;
          Some ix, rewrite_tokens nonterminal_name prec_num op_match
        )
        in

        (* If this is not the lowest-precedence level, then there is a
         * lower-precedence level to fall back to. *)
        let generated_rules' = if prec_num = 1
          then generated_rules
          else Util.snoc generated_rules
            (None, operator_match_nt_precedence nonterminal_name (prec_num - 1))
        in

        generated_name, generated_rules'
      )
      in

      (* A rule pointing from the original name to the rewritten highest
       * precedence, so we don't have to redirect all rules already pointing to
       * this nonterminal *)
      let indirect_nt =
        [ None, operator_match_nt_precedence nonterminal_name num_levels ]
      in

      level_nts
        |. Belt.List.add (nonterminal_name, indirect_nt)
        |. Belt.List.toArray
        |. Belt.Map.String.fromArray,

      (* Point from every generated name to the original nonterminal name,
       * mark the original name as an indirection to be removed. *)
      level_nts
        |. Belt.List.toArray
        |. Belt.Array.map (fun (name, _rule) -> name, Some nonterminal_name)
        |. Belt.Map.String.fromArray
        |. Belt.Map.String.set nonterminal_name None

(** Produce an augmented grammar *)
let to_grammar
  :  ConcreteSyntaxDescription.t
  -> string
  -> LrParsing.grammar
    * (tree_info * nonterminal_token list * operator_match_pattern option)
        Belt.MutableMap.Int.t
    * string option Belt.Map.String.t
  =
  fun { terminal_rules; nonterminal_rules } start_nonterminal ->
  let terminal_nums =
    Belt.Array.(
      terminal_rules
      (* start other terminals (besides $ and SPACE) at 2 *)
      |. mapWithIndex (fun i (name, _) -> name, i + 2)
      |. concat [| "$", 0; "SPACE", 1 |])
  in

  (* mapping from terminal to its number *)
  let terminal_num_map : int Belt.Map.String.t
    = MS.fromArray terminal_nums
  in

  let taken_names : Belt.Set.String.t
    = nonterminal_rules
      |. Belt.Map.String.keysToArray
      |. Belt.Set.String.fromArray
  in

  let nonterminal_rules_desugared, nonterminal_renamings = nonterminal_rules
    |. Belt.Map.String.valuesToArray
    |. Belt.Array.map (desugar_nonterminal taken_names)
    |. Belt.Array.unzip
  in

  let desugared_nts : nonterminal_operators
    = Util.array_map_unions nonterminal_rules_desugared
  in

  let nonterminal_renamings : string option Belt.Map.String.t
    = Util.array_map_unions nonterminal_renamings
  in

  Printf.printf "desugared:\n\n%s\n" (nonterminal_rules_desugared
    |. Belt.Array.map string_of_nonterminal_operators
    |. Js.Array2.joinWith "\n\n"
  );

  let nonterminal_num = ref 0 in

  (* Number every desugared nonterminal *)
  let nonterminal_num_map : int Belt.Map.String.t
    = Belt.Map.String.map desugared_nts (fun _ ->
        incr nonterminal_num;
        !nonterminal_num
      )
  in

  (* [nonterminal_nums]: name, level, nonterminal num. mutated below. *)
  let nonterminal_nums = Belt.Map.String.toArray nonterminal_num_map in
  let _ = Js.Array2.unshift nonterminal_nums ("root", 0) in

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

  (* Translate a nonterminal into n -- one for each of its precedence levels *)
  let nonterminals = desugared_nts
    |. Belt.Map.String.toArray
    |. Belt.Array.map (fun (nonterminal_name, productions) ->
      if nonterminal_name = start_nonterminal
      then start_nonterminal_num := !nt_num;

      let productions' = Belt.List.map productions
        (fun (op_index_opt, OperatorMatch rule) ->
          let op_index = match op_index_opt with
            | Some ix -> ix
            | None -> -1 (* TODO: is this okay? *)
          in

          Belt.MutableMap.Int.set
            production_rule_map
            !prod_num
            ( (nonterminal_name, op_index)
            , rule.tokens
            , Some rule.operator_match_pattern
            );
          incr prod_num;
          rule.tokens
            |. Belt.List.keep (fun tok -> not (is_formatting_token tok))
            |. Belt.List.map (convert_token terminal_num_map nonterminal_num_map)
          )
      in

      let result = !nt_num, { LrParsing.productions = productions' } in
      incr nt_num;
      result
    )
    |. Belt.Map.Int.fromArray
    |. Belt.Map.Int.set 0
      { productions = [ [ Nonterminal !start_nonterminal_num ] ] }
  in
  { nonterminals; terminal_nums; nonterminal_nums },
  production_rule_map,
  nonterminal_renamings
;;

(* assumption: the language we're operating on is the derived language:
 * 1. It's augmented with a root nonterminal, 0
 * 2. Precedence / associativity has been desugared
 * 3. All formatting tokens have been removed
 *)
let tree_of_parse_result (module Lr0 : LrParsing.LR0)
  :  (tree_info * nonterminal_token list * operator_match_pattern option)
       Belt.MutableMap.Int.t
  -> string option Belt.Map.String.t
  -> LrParsing.nonterminal_num Belt.Map.String.t
  -> ConcreteSyntaxDescription.nonterminal_rules
  -> string (* root name *)
  -> string (* parsed string *)
  -> LrParsing.parse_result
  -> formatted_tree
  =
  fun production_rule_map nonterminal_renamings nonterminal_nums
    nonterminal_rules root_name str root ->

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

      let tree_info, tokens, _ = production_rule_map
        |. Belt.MutableMap.Int.get prod_num
        |> get_option' (fun () -> Printf.sprintf
          "tree_of_parse_result: couldn't find nonterminal %n in \
          production_rule_map"
          prod_num
        )
      in

      let tokens' = Belt.List.keep tokens
        (fun tok -> not (is_formatting_token tok))
      in

      let children' = children
        |. Belt.List.zip tokens'
        |. Belt.List.toArray
        |. Belt.Array.map (function parse_result, token ->
          match token with
           | TerminalName _ -> TerminalCapture (go_t parse_result)
           | NonterminalName ntn ->
             NonterminalCapture (go_nt ntn parse_result)
           (* TODO: trivia *)
           | OpenBox _ | CloseBox | Underscore _
           -> invariant_violation "formatting tokens must be filtered beforehand"
        )
      in

      let renaming = nonterminal_renamings
        |. Belt.Map.String.get nt_name
        |> get_option' (fun () -> Printf.sprintf
         "tree_of_parse_result: unable to find renaming for %s" nt_name
        )
      in

      match renaming with
        | None -> (match children' with
          | [| NonterminalCapture child |]
          -> (*
             Printf.printf "tree_of_parse_result: unwrapping child %s\n" nt_name;
             Printf.printf "child tree_info: %s, %n\n"
               (fst child.tree_info) (snd child.tree_info);
               *)
            child
          | _ -> invariant_violation (Printf.sprintf "When translating a \
          synthetic nonterminal (%s) parse result to a tree, expected one \
          nonterminal child, but encountered %n children"
          nt_name (Belt.Array.length children'))
        )
        | Some nt_name' ->
          let _old_name, op_no = tree_info in
            (*
          Printf.printf "renaming %s -> %s (%n)\n" old_name nt_name' op_no;
          *)
          (match op_no, children' with
            | -1, [| NonterminalCapture child |] -> child
            | -1, _ -> failwith "TODO: error"
            | _, _ -> { tree_info = nt_name', op_no; children = children' })

  and go_t : LrParsing.parse_result -> formatted_terminal_capture =
    fun { start_pos; end_pos } ->
      let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
      let content = Js.String.slice str ~from:start_pos ~to_:end_pos in
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
  let grammar, production_rule_map, nonterminal_renamings = to_grammar desc root_name in
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
        { nonterminal_name = "root" (* TODO: root_name? *)
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
         nonterminal_renamings
         (MS.fromArray grammar.nonterminal_nums)
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
