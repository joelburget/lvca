open Core_kernel
module Nominal = Binding.Nominal
module MMI = Int.Table
module MSI = Util.MutableSet.Int
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

let root_name = "_root"

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
  { captured_tokens : Int.Set.t
  ; repeated_tokens : Int.Set.t
  }

let accumulate_tokens
      { captured_tokens = seen_toks; repeated_tokens = repeated_toks }
      { captured_tokens = seen_toks'; repeated_tokens = repeated_toks' }
  =
  let isect = Int.Set.inter seen_toks seen_toks' in
  { captured_tokens = Int.Set.diff (Int.Set.union seen_toks seen_toks') isect
  ; repeated_tokens = Int.Set.union isect (Int.Set.union repeated_toks repeated_toks')
  }
;;

let empty_tokens_info = { captured_tokens = Int.Set.empty; repeated_tokens = Int.Set.empty }

let rec token_usage : operator_match_pattern -> tokens_info = function
  | OperatorPattern (_, scope_patterns) ->
    scope_patterns
    |> List.fold_left
      ~init:empty_tokens_info
      ~f:(fun accum scope_pat ->
        accumulate_tokens accum (scope_token_usage scope_pat))
  | SingleCapturePattern cap_num ->
    { captured_tokens = Int.Set.of_list [ cap_num ]; repeated_tokens = Int.Set.empty }

and scope_token_usage : numbered_scope_pattern -> tokens_info
  = fun (NumberedScopePattern (binder_captures, body_capture)) ->
  let x = token_usage body_capture in
  let y = List.fold_left binder_captures
    ~init:empty_tokens_info
    ~f:(fun accum capture ->
      let tok = match capture with
        | VarCapture n -> n
        | PatternCapture n -> n
      in
      accumulate_tokens
        accum
        { captured_tokens = Int.Set.of_list [ tok ]
        ; repeated_tokens = Int.Set.empty
        })
  in accumulate_tokens x y
;;

let check_operator_match_validity
  :  nonterminal_token list -> operator_match_pattern
    -> MSI.t * Int.Set.t * (int * nonterminal_token) list
  =
  fun token_list term_pat ->
  let numbered_toks =
    token_list
    |> List.mapi ~f:(fun i tok -> i, tok)
    |> MMI.of_alist_exn
  in
  let { captured_tokens; repeated_tokens } = token_usage term_pat in
  let non_existent_tokens = MSI.create () in
  Int.Set.iter captured_tokens ~f:(fun tok_num ->
    if MMI.mem numbered_toks tok_num
    then MMI.remove numbered_toks tok_num
    else MSI.add non_existent_tokens tok_num);
  non_existent_tokens, repeated_tokens, MMI.to_alist numbered_toks
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
 * 8. TODO: There can be no fixity at the highest precedence. It's impossible
 *    to raise the precedence of one side, we need some allowance for
 *    parentheses.
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
  let terminal_rules' = String.Map.of_alist_exn terminal_rules in
  let show_toks toks = toks
    |> Array.map ~f:(Printf.sprintf "$%n")
    |> String.concat_array ~sep:", "
  in
  let open_depth = ref 0 in
  try
    nonterminal_rules
    |> String.Map.iter ~f:(fun (NonterminalRule { operator_rules; _ }) ->
      operator_rules
        |> List.iter ~f:(fun level ->
          let OperatorMatch { fixity; _ } = level
            |> List.hd
            |> get_option' (fun () ->
              "each level is guaranteed to have at least one rule")
          in
          let okay = List.for_all level
            ~f:(fun (OperatorMatch op_match) -> Caml.(op_match.fixity = fixity))
          in
          if not okay then raise_invalid "Every operator in a precedence level \
            must have the same fixity"
        );

      operator_rules
        |> List.join
        |> List.iter
        ~f:(fun (OperatorMatch { tokens; operator_match_pattern; fixity }) ->
           let non_existent_tokens, duplicate_captures, uncaptured_tokens =
             check_operator_match_validity tokens operator_match_pattern
           in

           if not (Int.Set.is_empty duplicate_captures)
           then (
             let tok_names = duplicate_captures
               |> Int.Set.to_array
               |> show_toks
             in
             raise_invalid
               ("tokens captured more than once: " ^ tok_names));

           if not (MSI.is_empty non_existent_tokens)
           then (
             let tok_names = non_existent_tokens |> MSI.to_array |> show_toks in
             raise_invalid ("non-existent tokens mentioned: " ^ tok_names));

           let tokens' = tokens
             |> List.filter ~f:(fun tok -> not (is_formatting_token tok))
           in
           (match tokens' with
             | [ NonterminalName _; TerminalName _; NonterminalName _ ] -> ()
             | _
             -> if Caml.(fixity <> Infixl || fixity <> Infixr)
                then raise_invalid "left or right fixity can only be applied \
                  to a binary operator (eg `tm OP tm`)");

           List.iter uncaptured_tokens ~f:(fun (_i, tok) ->
             match tok with
             | NonterminalName name ->
               raise_invalid ("uncaptured nonterminal: " ^ name)
             | TerminalName nt_name ->
               (match String.Map.find terminal_rules' nt_name with
                | None ->
                  raise_invalid ("Named terminal " ^ nt_name ^ " does not exist")
                | Some regex ->
                  if is_none (Regex.is_literal regex)
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
    if !open_depth <> 0
    then raise_invalid "At least one group is not closed (there are more open \
      box markers ('[') than close box markers (']'))";
    String.Map.iter terminal_rules' ~f:(fun regex ->
      if Regex.accepts_empty regex
      then raise_invalid ("Regex accepts empty strings: " ^ Regex.to_string regex)
    );
    None
  with
    CheckValidExn err -> Some err
;;

let rec remove_spaces : formatted_tree -> formatted_tree =
  fun { tree_info; children } ->
  let children' = Array.map children ~f:(function
   | TerminalCapture { content; _ } ->
      TerminalCapture { content; leading_trivia = ""; trailing_trivia = "" }
    | NonterminalCapture ntc -> NonterminalCapture (remove_spaces ntc))
  in
  { tree_info; children = children' }
;;

exception ToAstError of string

let prim_to_ast
  : string -> formatted_tree -> primitive
  = fun prim_ty tree -> match tree.children with
    | [| TerminalCapture { content; _ } |] -> (match prim_ty with
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
  let OperatorMatch { operator_match_pattern; _ } = rules.nonterminal_rules
    |> Fn.flip String.Map.find nt_name
    |> get_option (ToAstError "TODO: message")
    |> fun (NonterminalRule { operator_rules; _ }) -> operator_rules
    |> List.join (* TODO: should we use 2d indexing? *)
    |> Fn.flip List.nth nt_prod_no
    |> get_option' (fun () -> "TODO: message")
  in operator_match_pattern
;;

let array_get : string -> 'a array -> int -> 'a
  = fun _msg arr i -> Array.get arr i
    (*
    |> get_option' (fun () -> Printf.sprintf
      "failed array get in %s: index %n, length %n"
      msg
      i
      (Array.length arr)
    )
   *)

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
      | TerminalCapture { content; _ } -> Var content
    )
    | OperatorPattern ("integer", [NumberedScopePattern([], SingleCapturePattern n)])
    -> (match array_get "go_op_match_term 2" children (n - 1) with
      | NonterminalCapture _ -> failwith "TODO: error"
      | TerminalCapture { content; _ }
      -> Primitive (PrimInteger (Bigint.of_string content))
    )
    | OperatorPattern ("var", _)
    | OperatorPattern ("integer", _)
    -> failwith "TODO: error"
    | OperatorPattern (name, scope_pats)
    -> Operator
      ( name
      , List.map scope_pats ~f:(go_numbered_scope_term rules children)
      )
    | SingleCapturePattern n
    -> (match array_get "go_op_match_term 3" children (n - 1) with
      | NonterminalCapture tree -> tree_to_ast rules tree
      | TerminalCapture { content; _ } ->
        Printf.printf "children: [%s]\n" (children
          |> Array.map ~f:string_of_formatted_capture
          |> String.concat_array ~sep:"; "
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
      |> List.map ~f:(fun capture ->
        let n = match capture with
          | VarCapture n -> n
          | PatternCapture n -> n
        in
        match array_get "go_numbered_scope_term" children (n - 1) with
        | NonterminalCapture tree -> tree_to_pattern rules tree
        | TerminalCapture { content; _ } -> failwith (Printf.sprintf (* TODO: error *)
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
      | TerminalCapture { content; _ } -> Var content
    )
    | OperatorPattern ("var", _) -> failwith "TODO: error"
    | OperatorPattern (name, scope_pats)
    -> Operator
      ( name
      , List.map scope_pats ~f:(go_numbered_scope_pattern rules children)
      )
    | SingleCapturePattern n
    -> (match array_get "go_op_match_pattern 2" children (n - 1) with
      | NonterminalCapture tree -> tree_to_pattern rules tree
      | TerminalCapture { content; _ } -> failwith (Printf.sprintf (* TODO: error *)
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
    if List.length cap_nums > 0 then raise @@ ToAstError "TODO: message";
    go_op_match_pattern rules children op_match_pat
;;

let to_ast
  :  ConcreteSyntaxDescription.t
  -> formatted_tree
  -> (Nominal.term, string) Result.t
  = fun rules tree ->
    try
      Ok (tree_to_ast rules tree)
    with
      ToAstError msg -> Error msg
;;

module TestOperators = struct
  let mul = OperatorMatch
    { tokens = [
        NonterminalName "expr";
        TerminalName "MUL";
        NonterminalName "expr"
      ]
    ; operator_match_pattern = OperatorPattern ("mul", [
        NumberedScopePattern ([], SingleCapturePattern 1);
        NumberedScopePattern ([], SingleCapturePattern 3);
      ])
    ; fixity = Infixl
    }

  let div = OperatorMatch
    { tokens = [
        NonterminalName "expr";
        TerminalName "DIV";
        NonterminalName "expr"
      ]
    ; operator_match_pattern = OperatorPattern ("div", [
        NumberedScopePattern ([], SingleCapturePattern 1);
        NumberedScopePattern ([], SingleCapturePattern 3);
      ])
    ; fixity = Infixl
    }

  let add = OperatorMatch
    { tokens = [
        NonterminalName "expr";
        TerminalName "ADD";
        NonterminalName "expr"
      ]
    ; operator_match_pattern = OperatorPattern ("add", [
        NumberedScopePattern ([], SingleCapturePattern 1);
        NumberedScopePattern ([], SingleCapturePattern 3);
      ])
    ; fixity = Infixl
    }

  let sub = OperatorMatch
    { tokens = [
        NonterminalName "expr";
        TerminalName "SUB";
        NonterminalName "expr"
      ]
    ; operator_match_pattern = OperatorPattern ("sub", [
        NumberedScopePattern ([], SingleCapturePattern 1);
        NumberedScopePattern ([], SingleCapturePattern 3);
      ])
    ; fixity = Infixl
    }

  let var = OperatorMatch
    { tokens = [ TerminalName "ID" ]
    ; operator_match_pattern = SingleCapturePattern 1
    ; fixity = Nofix
    }

  let parens = OperatorMatch
    { tokens = [
        TerminalName "LPAREN";
        NonterminalName "expr";
        TerminalName "RPAREN"
      ]
    ; operator_match_pattern = SingleCapturePattern 2
    ; fixity = Nofix
    }
end

(** raises: [InvariantViolation] *)
let convert_token
  :  LrParsing.terminal_num String.Map.t
  -> int String.Map.t
  -> nonterminal_token
  -> LrParsing.symbol
  = fun terminal_nums nonterminal_entry -> function
  | TerminalName tn -> Terminal
    (String.Map.find terminal_nums tn
     |> get_option' (fun () -> "to_grammar: failed to get terminal " ^ tn))
  | NonterminalName nt_name
  -> Nonterminal (nonterminal_entry
    |> Fn.flip String.Map.find nt_name
    |> get_option' (fun () -> Printf.sprintf
      "convert_token: couldn't find nonterminal %s in names: %s"
      nt_name
      (nonterminal_entry
        |> String.Map.keys
        |> Util.stringify_list Fn.id ", ")
    )
  )
  | OpenBox _ | CloseBox | Underscore _
  -> invariant_violation
    "all formatting tokens should be filtered (see is_formatting_token)"


(** Rewrite the tokens in an operator match to refer to lower precedence levels
 * if there's a fixity / associativity attached.
 *
 * Invariants assumed:
 *   %left, %right can't occur at lowest precedence level
 *   Anywhere %left, %right occur has same nonterminal on both sides, parent
 *)
let rewrite_tokens
  : string -> int -> operator_match -> operator_match
  = fun
    nt_name
    level
    (OperatorMatch { tokens; operator_match_pattern; fixity }) ->

    let raised_nt_name = Printf.sprintf "%s_%n" nt_name (level + 1) in
    let this_level_nt_name = Printf.sprintf "%s_%n" nt_name level in
    (* Printf.printf "raised_nt_name: %s\n" raised_nt_name; *)
    let tokens' = List.filter tokens
      ~f:(fun tok -> not (is_formatting_token tok))
    in

    let tokens'' = match tokens' with
      | [ NonterminalName name1; TerminalName _ as t; NonterminalName name2 ]
        when String.(name1 = nt_name && name2 = nt_name)
        -> (match fixity with
        | Infixl
        -> (* Require a higher precedence left child (operations at this level
              should appear to the right) *)
           [ NonterminalName raised_nt_name;
             t;
             NonterminalName this_level_nt_name;
           ]

        | Infixr
        -> [ NonterminalName this_level_nt_name;
             t;
             NonterminalName raised_nt_name;
           ]

        (* Don't adjust nofix *)
        | Nofix -> tokens'
        )

      (* juxtaposition *)
      | [ NonterminalName name1; NonterminalName name2 ]
      when String.(name1 = nt_name && name2 = nt_name)
      (* TODO: adjust! *)
      -> tokens'

      | _ -> tokens'
    in

    OperatorMatch { tokens = tokens''; operator_match_pattern; fixity = Nofix }

let%test_module "rewrite_tokens" = (module struct
  open TestOperators
  let%expect_test "" =
    let op_match = rewrite_tokens "expr" 1 add in
    print_string (string_of_operator_match op_match);
    [%expect{| expr_2 ADD expr_1 { add($1; $3) } |}]
end)

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
 * [int option] is the number of the attached operator rule. [Some num] if
 * there's a rule from the original grammar, [None] if it's just a
 * fallthrough to a higher level.
 *)
type nonterminal_operators = (int option * operator_match) list String.Map.t

let string_of_nonterminal_operators : nonterminal_operators -> string
  = fun nonterminal_operators -> nonterminal_operators
    |> String.Map.to_alist
    |> List.map ~f:(fun (name, matches) ->
      let rhs = matches
        |> Array.of_list
        |> Array.map ~f:(fun (ix_opt, op_match) ->
            let ix_str = match ix_opt with
              | None -> "_"
              | Some ix -> string_of_int ix
            in
            Printf.sprintf "  %s -> %s" ix_str (string_of_operator_match op_match)
        )
        |> String.concat_array ~sep:"\n"
      in
      Printf.sprintf "%s:\n%s" name rhs
    )
    |> String.concat ~sep:"\n"

(** Desugar a nonterminal into one nonterminal per precedence level.
 *
 * raises: [UserError]
 *)
let desugar_nonterminal
  :  String.Set.t
  -> nonterminal_rule
  -> nonterminal_operators * string option String.Map.t
  = fun
      taken_names
      (NonterminalRule { nonterminal_name; operator_rules; _ }) ->
    let num_levels = List.length operator_rules in
    if num_levels = 1
    then
      let operators = operator_rules
        |> List.hd
        |> get_option'
          (fun () -> "desugar_nonterminal: expected at least one operator")
        |> List.mapi ~f:(fun i op_match -> Some i, op_match)
      in
      String.Map.of_alist_exn [ nonterminal_name, operators ],
      String.Map.of_alist_exn [ nonterminal_name, Some nonterminal_name ]
    else

      (* Used to number each operator across all precedence levels *)
      let operator_index = ref 0 in

      let level_nts = List.mapi operator_rules ~f:(fun i level ->
        (* start at precedence [num_levels], down to 1 *)
        let prec_num = num_levels - i in

        (* create a new nonterminal from just this precedence level of the
         * orginal nonterminal *)
        let generated_name = Printf.sprintf "%s_%n" nonterminal_name prec_num in

        (* Our renaming is not hygienic, so we need to prevent name collisions.
         * Also, this would be just confusing to the user. *)
        (if String.Set.mem taken_names generated_name
         then raise (UserError (Printf.sprintf "Desugaring of nonterminal %s \
           generated name %s, which conflicts with an existing nonterminal."
           nonterminal_name generated_name
         )));

        let generated_rules = List.map level ~f:(fun op_match ->
          let ix = !operator_index in
          incr operator_index;
          Some ix, rewrite_tokens nonterminal_name prec_num op_match
        )
        in

        (* If this is not the highest-precedence level, then there is a
         * higher-precedence level to fall through to. *)
        let generated_rules' = if prec_num = num_levels
          then generated_rules
          else Util.snoc generated_rules
            (None, operator_match_nt_precedence nonterminal_name (prec_num + 1))
        in

        generated_name, generated_rules'
      )
      in

      (* A rule pointing from the original name to the rewritten lowest
       * precedence, so we don't have to redirect all rules already pointing to
       * this nonterminal *)
      let indirect_nt =
        [ None, operator_match_nt_precedence nonterminal_name 1 ]
      in

      level_nts
        |> List.cons (nonterminal_name, indirect_nt)
        |> String.Map.of_alist_exn,

      (* Point from every generated name to the original nonterminal name,
       * mark the original name as an indirection to be removed. *)
      level_nts
        |> List.map ~f:(fun (name, _rule) -> name, Some nonterminal_name)
        |> String.Map.of_alist_exn
        |> String.Map.set ~key:nonterminal_name ~data:None

let%test_module "desugar_nonterminal" = (module struct
  open TestOperators

  let%expect_test "[parens] [add; sub]" =
    let taken_names = String.Set.empty in
    let nonterminal_rule = NonterminalRule
      { nonterminal_name = "expr"
      ; nonterminal_type = NonterminalType ([], SortAp ("expr", [||]))
      ; operator_rules = [ [parens]; [add; sub] ]
      }
    in
    let nonterminal_operators, _name_map =
      desugar_nonterminal taken_names nonterminal_rule
    in
    print_string (string_of_nonterminal_operators nonterminal_operators);
    [%expect{|
      expr:
        _ -> expr_1 { $1 }
      expr_1:
        1 -> expr_2 ADD expr_1 { add($1; $3) }
        2 -> expr_2 SUB expr_1 { sub($1; $3) }
        _ -> expr_2 { $1 }
      expr_2:
        0 -> LPAREN expr RPAREN { $2 } |}]

  let%expect_test "[parens]; [mul; div]; [add; sub]; [var]" =
    let taken_names = String.Set.empty in
    let nonterminal_rule = NonterminalRule
      { nonterminal_name = "expr"
      ; nonterminal_type = NonterminalType ([], SortAp ("expr", [||]))
      ; operator_rules = [ [parens]; [mul; div]; [add; sub]; [var] ]
      }
    in
    let nonterminal_operators, _name_map =
      desugar_nonterminal taken_names nonterminal_rule
    in
    print_string (string_of_nonterminal_operators nonterminal_operators);
    [%expect{|
      expr:
        _ -> expr_1 { $1 }
      expr_1:
        5 -> ID { $1 }
        _ -> expr_2 { $1 }
      expr_2:
        3 -> expr_3 ADD expr_2 { add($1; $3) }
        4 -> expr_3 SUB expr_2 { sub($1; $3) }
        _ -> expr_3 { $1 }
      expr_3:
        1 -> expr_4 MUL expr_3 { mul($1; $3) }
        2 -> expr_4 DIV expr_3 { div($1; $3) }
        _ -> expr_4 { $1 }
      expr_4:
        0 -> LPAREN expr RPAREN { $2 } |}]
end)

(** raises: TODO *)
let derived_nonterminal_rules : nonterminal_rules -> nonterminal_operators array
  = fun nonterminal_rules ->
  let taken_names : String.Set.t
    = nonterminal_rules
      |> String.Map.keys
      |> String.Set.of_list
  in
  let nonterminal_rules_desugared, _nonterminal_renamings = nonterminal_rules
    |> String.Map.data
    |> Array.of_list
    |> Array.map ~f:(desugar_nonterminal taken_names)
    |> Array.unzip
  in nonterminal_rules_desugared

let string_of_derived_rules : nonterminal_operators list -> string
  = fun nonterminal_operators -> nonterminal_operators
    |> List.map ~f:string_of_nonterminal_operators
    |> String.concat ~sep:"\n\n"

(** Produce an augmented grammar *)
let to_grammar
  :  ConcreteSyntaxDescription.t
  -> string
  -> LrParsing.augmented_grammar
    * (tree_info * nonterminal_token list * operator_match_pattern option)
        Int.Table.t
    * string option String.Map.t
  =
  fun { terminal_rules; nonterminal_rules } start_nonterminal ->

  let terminal_nums = terminal_rules
      (* start other terminals (besides $ and SPACE) at 2 *)
      |> List.mapi ~f:(fun i (name, _) -> name, i + 2)
      |> List.append [ "$", 0; "SPACE", 1 ]
  in

  (* mapping from terminal to its number *)
  let terminal_num_map : int String.Map.t
    = String.Map.of_alist_exn terminal_nums
  in

  let taken_names : String.Set.t
    = nonterminal_rules
      |> Map.keys
      |> String.Set.of_list
  in

  (* TODO: remove duplication above *)
  let nonterminal_rules_desugared, nonterminal_renamings = nonterminal_rules
    |> Map.data
    |> List.map ~f:(desugar_nonterminal taken_names)
    |> List.unzip
  in

  let desugared_nts : nonterminal_operators
    = Util.list_map_unions nonterminal_rules_desugared
  in

  let nonterminal_renamings : string option String.Map.t
    = Util.list_map_unions nonterminal_renamings
    |> Map.set ~key:root_name ~data:None
  in

  (*
  Printf.printf "desugared:\n\n%s\n"
    (string_of_derived_rules nonterminal_rules_desugared);
    *)

  let nonterminal_num = ref 0 in

  (* Number every desugared nonterminal *)
  let nonterminal_num_map : int String.Map.t
    = String.Map.map desugared_nts ~f:(fun _ ->
        incr nonterminal_num;
        !nonterminal_num
      )
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
  let production_rule_map = Int.Table.create () in

  (* Translate a nonterminal into n -- one for each of its precedence levels *)
  let nonterminals = desugared_nts
    |> String.Map.to_alist
    |> List.map ~f:(fun (nonterminal_name, productions) ->
      if String.(nonterminal_name = start_nonterminal)
      then start_nonterminal_num := !nt_num;

      let productions' = List.map productions
        ~f:(fun (op_index_opt, OperatorMatch rule) ->
          let op_index = match op_index_opt with
            | Some ix -> ix
            | None -> -1 (* TODO: is this okay? *)
          in

          Int.Table.set
            production_rule_map
            ~key:!prod_num
            ~data:( (nonterminal_name, op_index)
                  , rule.tokens
                  , Some rule.operator_match_pattern
                  );
          incr prod_num;
          rule.tokens
            |> List.filter ~f:(fun tok -> not (is_formatting_token tok))
            |> List.map ~f:(convert_token terminal_num_map nonterminal_num_map)
          )
      in

      let result =
        nonterminal_name, !nt_num, { LrParsing.productions = productions' }
      in
      incr nt_num;
      result
    )
    |> List.cons
      (root_name, 0, {
        LrParsing.productions = [ [ Nonterminal !start_nonterminal_num ] ]
      })
    |> Array.of_list
  in

  AugmentedGrammar
    { nonterminals
    ; terminal_nums = Array.of_list terminal_nums
    },
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
       Int.Table.t
  -> string option String.Map.t
  -> LrParsing.nonterminal_num String.Map.t
  -> ConcreteSyntaxDescription.nonterminal_rules
  -> string (* parsed string *)
  -> LrParsing.parse_result
  -> formatted_tree
  =
  fun production_rule_map nonterminal_renamings _nonterminal_nums
    _nonterminal_rules str root ->

  let str_pos = ref 0 in
  let str_len = String.length str in
  let get_trivia : int -> int -> string * string =
    fun start_pos end_pos ->
      (* look back consuming all whitespace to (and including) a newline *)
      let leading_trivia =
        if !str_pos < start_pos
        then String.slice str !str_pos start_pos
        else ""
      in
      (* look forward consuming all whitespace up to a newline *)
      str_pos := end_pos;
      let continue = ref (end_pos < str_len) in
      while !continue do
        (* TODO: need to be aware of other whitespace tokens *)
        let got_space = Char.(String.get str !str_pos = ' ') in
        continue := !str_pos < str_len && got_space;
        if !continue then incr str_pos;
      done;
      let trailing_trivia = String.slice str end_pos !str_pos in
      leading_trivia, trailing_trivia
  in

  let rec go_nt : string -> LrParsing.parse_result -> formatted_tree =
    fun nt_name { production; children; _ } ->
      let prod_num = match production with
        | First prod -> invariant_violation (Printf.sprintf
           "go_nt (nt_name %s) received a terminal production: %s"
           nt_name
           (Lr0.string_of_terminal prod))
        | Second prod_num -> prod_num
      in

      let tree_info, tokens, _ = Int.Table.find production_rule_map prod_num
        |> get_option' (fun () -> Printf.sprintf
          "tree_of_parse_result: couldn't find nonterminal %n in \
          production_rule_map"
          prod_num
        )
      in

      let tokens' = List.filter tokens
        ~f:(fun tok -> not (is_formatting_token tok))
      in

      let children' = List.map2_exn children tokens' ~f:(fun x y -> x, y)
        |> Array.of_list
        |> Array.map ~f:(function parse_result, token ->
          match token with
           | TerminalName _ -> TerminalCapture (go_t parse_result)
           | NonterminalName ntn ->
             NonterminalCapture (go_nt ntn parse_result)
           (* TODO: trivia *)
           | OpenBox _ | CloseBox | Underscore _
           -> invariant_violation "formatting tokens must be filtered beforehand"
        )
      in

      let renaming = String.Map.find nonterminal_renamings nt_name
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
          nt_name (Array.length children'))
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
    fun { start_pos; end_pos; _ } ->
      let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
      let content = String.slice str start_pos end_pos in
      { leading_trivia; content; trailing_trivia }
  in
  go_nt root_name root
;;

let lexer_of_desc : ConcreteSyntaxDescription.t -> Placemat.Lex.lexer =
  fun { terminal_rules; _ } ->
  ("SPACE", Regex.ReClass (PosClass Whitespace)) :: terminal_rules
;;

let parse desc start_nonterminal str =
  let AugmentedGrammar grammar as ag, production_rule_map, nonterminal_renamings =
    to_grammar desc start_nonterminal
  in
  let module Lalr = LalrParsing.Lalr1 (struct
                      let grammar = ag
                    end)
  in
  let lexer = lexer_of_desc desc in

  (* TODO: come up with better idea where to do this augmentation *)
  (* XXX this is also wrong (but not used) *)
  let augmented_nonterminal_rules =
    String.Map.set
      desc.nonterminal_rules
      ~key:root_name
      ~data:(NonterminalRule
        { nonterminal_name = root_name
        ; nonterminal_type = NonterminalType ([], SortAp (root_name, [||]))
        ; operator_rules = [ [] ]
        })
  in

  match Lalr.lex_and_parse lexer str with
  | Ok tree_root -> Ok
    (tree_of_parse_result
       (module Lalr)
       production_rule_map
       nonterminal_renamings
       (grammar.nonterminals
         |> Array.map ~f:(fun (name, num, _nt) -> name, num)
         |> Array.to_list
         |> String.Map.of_alist_exn)
       augmented_nonterminal_rules
       str
       tree_root)
  | Error (Either.First { start_pos; end_pos; message }) ->
    Error
      (Printf.sprintf
         "lexical error at characters %n - %n (%s):\n%s"
         start_pos
         end_pos
         (String.slice str start_pos end_pos)
         message)
  | Error (Either.Second (char_no, message)) ->
    Error (Printf.sprintf "parser error at character %n:\n%s" char_no message)
;;

let make_concrete_description
      (terminal_rules : pre_terminal_rule list)
      (nonterminal_rules : nonterminal_rule list)
  =
  let module Parse_regex = Parsing.Incremental (Parsing.Parseable_regex) in
  { terminal_rules = terminal_rules
    |> List.map ~f:(fun (PreTerminalRule (name, str_or_re_str)) ->
      match str_or_re_str with
      | First re_str -> (match Parse_regex.parse re_str with
        | Ok re -> name, re
        | Error msg -> failwith ("failed to parse regex: " ^ msg))
      | Second str -> name, Regex.ReString str)
  ; nonterminal_rules = nonterminal_rules
    |> List.map ~f:(fun (NonterminalRule { nonterminal_name; _ } as rule) ->
      nonterminal_name, rule)
    |> String.Map.of_alist_exn
  }
;;
