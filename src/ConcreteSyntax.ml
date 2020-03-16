open Core_kernel
module Nominal = Binding.Nominal
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
let raise_invalid str = raise (CheckValidExn (InvalidGrammar str))

let root_name = "_root"

(* The parser doesn't need formatting tokens *)
let is_formatting_token : nonterminal_token -> bool =
 fun token -> match token with TerminalName _ | NonterminalName _ -> false | _ -> true
;;

module IntStringTuple = struct
  include Tuple.Make       (Int) (String)
  include Tuple.Comparable (Int) (String)
end

type int_string_set = (int * string, IntStringTuple.comparator_witness) Set.t

(* Used to analyze token usage (see `token_usage`). We use this to check if there are any
   tokens not captured (a problem in some circumstances), or if there are tokens captured
   twice (always a problem). *)
type tokens_info =
  { captured_tokens : Int.Set.t
  ; repeated_tokens : Int.Set.t
  ; invalid_captured_terminals : int_string_set
  }

let accumulate_tokens
    { captured_tokens = seen_toks
    ; repeated_tokens = repeated_toks
    ; invalid_captured_terminals
    }
    { captured_tokens = seen_toks'
    ; repeated_tokens = repeated_toks'
    ; invalid_captured_terminals = invalid_captured_terminals'
    }
  =
  let isect = Set.inter seen_toks seen_toks' in
  { captured_tokens = Set.diff (Set.union seen_toks seen_toks') isect

  (* Tokens that already occurred repeatedly on either side, plus any tokens
   * they both reported seeing *)
  ; repeated_tokens = Set.union isect (Set.union repeated_toks repeated_toks')
  ; invalid_captured_terminals =
      Set.union invalid_captured_terminals invalid_captured_terminals'
  }
;;

let empty_tokens_info =
  { captured_tokens = Int.Set.empty
  ; repeated_tokens = Int.Set.empty
  ; invalid_captured_terminals = Set.empty (module IntStringTuple)
  }
;;

let special_patterns = String.Set.of_list ["var"; "integer"; "string"]
;;

(** @raise InvalidGrammar *)
let get_terminal_name : nonterminal_token Int.Table.t -> int -> int_string_set
  = fun token_table token_num -> match Hashtbl.find token_table token_num with
    | Some (TerminalName name)
    -> Set.of_list (module IntStringTuple) [ token_num, name ]
    | Some (NonterminalName _)
    -> Set.empty (module IntStringTuple)
    | Some tok
    -> invariant_violation (Printf.sprintf
      "get_terminal_name: structural token %s captured."
      (string_of_token tok))
    | None
    -> raise_invalid
      (Printf.sprintf "Couldn't find captured token $%n" token_num)

let rec token_usage
  : operator_match_pattern -> nonterminal_token Int.Table.t -> tokens_info
  = fun pat numbered_toks -> match pat with
  | OperatorPattern (pat_name, scope_patterns)
  -> (match scope_patterns with
     | [ NumberedScopePattern ([], SingleCapturePattern cap_num) ]
     (* We still want to run this for the side-effect of checking for invalid
      * tokens. *)
     -> let captured_terminals = get_terminal_name numbered_toks cap_num in
        (* If we didn't capture a terminal then it's fine.
         * If the enclosing pattern is [var], [integer], or [string], then it's fine. *)
        if Set.is_empty captured_terminals || Set.mem special_patterns pat_name
        then { empty_tokens_info with
          captured_tokens = Int.Set.singleton cap_num }
        (* ... otherwise, there's an invalid capture *)
        else { captured_tokens = Int.Set.singleton cap_num
             ; repeated_tokens = Int.Set.empty
             ; invalid_captured_terminals = captured_terminals
             }
     | _
     -> scope_patterns
      |> List.fold_left ~init:empty_tokens_info ~f:(fun accum scope_pat ->
             accumulate_tokens accum (scope_token_usage numbered_toks scope_pat))
  )

  | SingleCapturePattern cap_num
  ->
    let invalid_captured_terminals = get_terminal_name numbered_toks cap_num in
    { captured_tokens = Int.Set.of_list [ cap_num ]
    ; repeated_tokens = Int.Set.empty
    ; invalid_captured_terminals
    }

and scope_token_usage
  : nonterminal_token Int.Table.t -> numbered_scope_pattern -> tokens_info =
 fun numbered_toks (NumberedScopePattern (binder_captures, body_capture)) ->
  let body_info = token_usage body_capture numbered_toks in
  let scope_info =
    List.fold_left binder_captures ~init:empty_tokens_info ~f:(fun accum capture ->
        let tok = match capture with VarCapture n -> n | PatternCapture n -> n in
        accumulate_tokens
          accum
          { empty_tokens_info with
            captured_tokens = Int.Set.of_list [ tok ]
          })
  in
  accumulate_tokens body_info scope_info
;;

let check_operator_match_validity
    :  nonterminal_token list -> operator_match_pattern
    -> MSI.t * Int.Set.t * (int * nonterminal_token) list * int_string_set
  =
 fun token_list term_pat ->
  let numbered_toks = token_list
    |> index_tokens
    |> Fqueue.to_list
    |> Int.Table.of_alist_exn
  in
  let { captured_tokens; repeated_tokens; invalid_captured_terminals }
    = token_usage term_pat numbered_toks
  in
  let non_existent_tokens = MSI.create () in

  Set.iter captured_tokens ~f:(fun tok_num ->
      if Hashtbl.mem numbered_toks tok_num
      then Hashtbl.remove numbered_toks tok_num
      else MSI.add non_existent_tokens tok_num);
  non_existent_tokens,
    repeated_tokens,
    Hashtbl.to_alist numbered_toks,
    invalid_captured_terminals
;;

(** Check invariants of concrete syntax descriptions:
 + For all tokens on the LHS (token list),
    if the token is not captured on the RHS (term pattern):
   {ol
     {- If the token refers to a nonterminal, this is an error. }
     {- If the token refers to a terminal, it must be a string literal. }}
 + No token is used twice on the RHS.
 + No token is mentioned on the RHS that doesn't exist on the left
 + No regex admits empty strings (these tokens could be arbitrarily inserted
    everywhere)
 + All non-literal terminals (regexes) must be captured
 + Boxes are matching: There are the same number of '[' and ']' tokens, each
    box is opened before it's closed.
 + Only string, integer, var capture bare terminals
 + There are no duplicate terminal or nonterminal names

 Examples:
 - [FOO bar BAZ { op($2) }] valid
 - [FOO bar BAZ { op($1; $3) }] invalid (uncaptured nonterminal)
 - [FOO bar BAZ { op($1; $2) }] possibly invalid (if BAZ isn't a string literal)
 - [FOO bar BAZ { op($2; $2) }] invalid (repeated token)
 - [FOO bar BAZ { op($2; $4) }] invalid (non-existent token)
 *)
let check_description_validity { terminal_rules; nonterminal_rules } =
  let terminal_names = terminal_rules
    |> List.map ~f:(fun (name, _) -> name)
  in
  let nonterminal_names = nonterminal_rules
    |> Map.to_alist
    |> List.map ~f:(fun (name, _) -> name)
  in

  try
    (match List.find_a_dup ~compare:String.compare terminal_names with
      | None -> ()
      | Some dup -> raise_invalid ("Duplicate terminal definition: " ^ dup));

    (match List.find_a_dup ~compare:String.compare nonterminal_names with
      | None -> ()
      | Some dup -> raise_invalid ("Duplicate nonterminal definition: " ^ dup));

    let terminal_rules' = String.Map.of_alist_exn terminal_rules in
    let show_toks toks =
      toks |> List.map ~f:(Printf.sprintf "$%n") |> String.concat ~sep:", "
    in
    let open_depth = ref 0 in
    nonterminal_rules
    |> Map.iter ~f:(fun (NonterminalRule { operator_rules; _ }) ->
           operator_rules
           |> List.iter
                ~f:(fun (OperatorMatch { tokens; operator_match_pattern }) ->
                  let non_existent_tokens, duplicate_captures, uncaptured_tokens,
                      invalid_captured_terminals =
                    check_operator_match_validity tokens operator_match_pattern
                  in
                  if not (Set.is_empty duplicate_captures)
                  then (
                    let tok_names = duplicate_captures |> Set.to_list |> show_toks in
                    raise_invalid ("tokens captured more than once: " ^ tok_names));
                  if not (MSI.is_empty non_existent_tokens)
                  then (
                    let tok_names = non_existent_tokens |> MSI.to_list |> show_toks in
                    raise_invalid ("non-existent tokens mentioned: " ^ tok_names));
                  if not (Set.is_empty invalid_captured_terminals)
                  then (
                    let tok_names = invalid_captured_terminals
                      |> Set.to_list
                      |> List.map ~f:(fun (num, tok) -> Printf.sprintf
                        "$%n (%s)" num tok
                      )
                      |> String.concat ~sep:", "
                    in
                    raise_invalid ("Terminals can only be captured by `var`, \
                      `integer`, and `string`: " ^ tok_names));
                  List.iter uncaptured_tokens ~f:(fun (_i, tok) ->
                      match tok with
                      | NonterminalName name ->
                        raise_invalid ("uncaptured nonterminal: " ^ name)
                      | TerminalName nt_name ->
                        (match Map.find terminal_rules' nt_name with
                        | None ->
                          raise_invalid ("Named terminal " ^ nt_name ^ " does not exist")
                        | Some regex ->
                          if is_none (Regex.is_literal regex)
                          then
                            raise_invalid
                              (Printf.sprintf
                              "Uncaptured regex which is not a string literal: /%s/"
                              (Regex.to_string regex)))
                      | OpenBox _ -> incr open_depth
                      | CloseBox ->
                        if !open_depth <= 0
                        then
                          raise_invalid
                            "Invalid box structure (saw a close box marker (']') before \
                             its opening marker ('['))!";
                        decr open_depth
                      | Underscore _ -> ())));
    if !open_depth <> 0
    then
      raise_invalid
        "At least one group is not closed (there are more open box markers ('[') than \
         close box markers (']'))";
    Map.iter terminal_rules' ~f:(fun regex ->
        if Regex.accepts_empty regex
        then raise_invalid ("Regex accepts empty strings: " ^ Regex.to_string regex));
    None
  with
  | CheckValidExn err -> Some err
;;

let rec remove_spaces : formatted_tree -> formatted_tree =
 fun { tree_info; children } ->
  let children' =
    Array.map children ~f:(function
        | TerminalCapture { content; _ } ->
          TerminalCapture { content; leading_trivia = ""; trailing_trivia = "" }
        | NonterminalCapture ntc -> NonterminalCapture (remove_spaces ntc))
  in
  { tree_info; children = children' }
;;

exception ToAstError of string

let prim_to_ast : string -> formatted_tree -> primitive =
 fun prim_ty tree ->
  match tree.children with
  | [| TerminalCapture { content; _ } |] ->
    (match prim_ty with
    | "string" -> PrimString content
    | "integer" ->
      (try PrimInteger (Bigint.of_string content) with
      | _ -> raise (ToAstError "failed to read integer literal"))
    | _ ->
      raise
        (Util.InvariantViolation
           (Printf.sprintf
              "prim_to_ast can only be called with string or integer (called with %s)"
              prim_ty)))
  | _ -> raise @@ ToAstError "TODO: message"
;;

let get_operator_match
    : ConcreteSyntaxDescription.t -> formatted_tree -> operator_match_pattern
  =
 fun rules tree ->
  let nt_name, nt_prod_no = tree.tree_info in
  let (OperatorMatch { operator_match_pattern; _ }) =
    String.Map.find rules.nonterminal_rules nt_name
    |> get_option (ToAstError "TODO: message")
    |> fun (NonterminalRule { operator_rules; _ }) ->
    List.nth operator_rules nt_prod_no
    |> get_option' (fun () -> Printf.sprintf
      "Couldn't find nonterminal production %n in operator rules:\n%s\n"
      nt_prod_no
      (operator_rules
        |> List.map ~f:string_of_operator_match
        |> String.concat ~sep:"\n")
    )
  in
  operator_match_pattern
;;

let array_get : string -> 'a array -> int -> 'a =
 fun msg arr i ->
  if i >= 0 && i < Array.length arr
  then arr.(i)
  else
    Util.invariant_violation
      (Printf.sprintf
         "failed array get in %s: index %n, length %n"
         msg
         i
         (Array.length arr))
;;

(* Convert a concrete tree to an AST. We ignore trivia. *)
let rec tree_to_ast : ConcreteSyntaxDescription.t -> formatted_tree -> Nominal.term =
 fun rules tree ->
  let nt_name, _ = tree.tree_info in
  match nt_name with
  | "list" -> failwith "TODO"
  | "string" | "integer" -> Primitive (prim_to_ast nt_name tree)
  | _ -> tree |> get_operator_match rules |> go_op_match_term rules tree.children

and go_op_match_term
    :  ConcreteSyntaxDescription.t -> formatted_capture array -> operator_match_pattern
    -> Nominal.term
  =
 fun rules children op_match_pat ->
  match op_match_pat with
  | OperatorPattern ("var", [ NumberedScopePattern ([], SingleCapturePattern n) ]) ->
    (match array_get "go_op_match_term 1" children (n - 1) with
    | NonterminalCapture _ -> failwith "TODO: error"
    | TerminalCapture { content; _ } -> Var content)
  | OperatorPattern ("integer", [ NumberedScopePattern ([], SingleCapturePattern n) ]) ->
    (match array_get "go_op_match_term 2" children (n - 1) with
    | NonterminalCapture _ -> failwith "TODO: error"
    | TerminalCapture { content; _ } -> Primitive (PrimInteger (Bigint.of_string content)))
  | OperatorPattern ("var", _)
  -> failwith "TODO: go_op_match_term var"
  | OperatorPattern (name, scope_pats) ->
    Operator (name, List.map scope_pats ~f:(go_numbered_scope_term rules children))
  | SingleCapturePattern n ->
    (match array_get "go_op_match_term 3" children (n - 1) with
    | NonterminalCapture tree -> tree_to_ast rules tree
    | TerminalCapture { content; _ } ->
      failwith
        (Printf.sprintf (* TODO: error *)
           "go_op_match_term: Single capture pattern unexpectedly received a terminal \
            when a nonterminal child was expected: child %n -> \"%s\""
           n
           content))

and go_numbered_scope_term
    :  ConcreteSyntaxDescription.t
    -> formatted_capture array
    -> numbered_scope_pattern
    -> Nominal.scope
  =
 fun rules children (NumberedScopePattern (cap_nums, op_match_pat)) ->
  Scope
    ( cap_nums
      |> List.map ~f:(function
             | VarCapture n ->
               (match array_get "go_numbered_scope_term" children (n - 1) with
               | TerminalCapture { content; _ } -> Pattern.Var content
               | NonterminalCapture tree ->
                 failwith
                   (Printf.sprintf (* TODO: error *)
                      "go_numbered_scope_term: Unexpectedly received a nonterminal when \
                       a terminal child was expected (matching a var): child %n -> %s"
                      n
                      (to_string tree)))
             | PatternCapture n ->
               (match array_get "go_numbered_scope_term" children (n - 1) with
               | NonterminalCapture tree -> tree_to_pattern rules tree
               | TerminalCapture { content; _ } ->
                 failwith
                   (Printf.sprintf (* TODO: error *)
                      "go_numbered_scope_term: Unexpectedly received a terminal when a \
                       nonterminal child was expected (matching a pattern): child %n -> \
                       \"%s\""
                      n
                      content)))
    , go_op_match_term rules children op_match_pat )

and tree_to_pattern : ConcreteSyntaxDescription.t -> formatted_tree -> Pattern.t =
 fun rules tree ->
  let nt_name, _ = tree.tree_info in
  match nt_name with
  | "list" -> failwith "TODO"
  | "string" | "integer" -> Primitive (prim_to_ast nt_name tree)
  | _ -> tree |> get_operator_match rules |> go_op_match_pattern rules tree.children

and go_op_match_pattern
    :  ConcreteSyntaxDescription.t -> formatted_capture array -> operator_match_pattern
    -> Pattern.t
  =
 fun rules children op_match_pat ->
  match op_match_pat with
  | OperatorPattern ("var", [ NumberedScopePattern ([], SingleCapturePattern n) ]) ->
    (match array_get "go_op_match_pattern 1" children (n - 1) with
    | NonterminalCapture _ -> failwith "TODO: error"
    | TerminalCapture { content; _ } -> Var content)
  | OperatorPattern ("var", _) -> failwith "TODO: error"
  | OperatorPattern (name, scope_pats) ->
    Operator (name, List.map scope_pats ~f:(go_numbered_scope_pattern rules children))
  | SingleCapturePattern n ->
    (match array_get "go_op_match_pattern 2" children (n - 1) with
    | NonterminalCapture tree -> tree_to_pattern rules tree
    | TerminalCapture { content; _ } ->
      failwith
        (Printf.sprintf (* TODO: error *)
           "go_op_match_pattern: Unexpectedly received a terminal when a nonterminal \
            child was expected: child %n -> \"%s\""
           n
           content))

and go_numbered_scope_pattern
    :  ConcreteSyntaxDescription.t -> formatted_capture array -> numbered_scope_pattern
    -> Pattern.t
  =
 fun rules children (NumberedScopePattern (cap_nums, op_match_pat)) ->
  if List.length cap_nums > 0 then raise @@ ToAstError "TODO: message";
  go_op_match_pattern rules children op_match_pat
;;

let to_ast
    : ConcreteSyntaxDescription.t -> formatted_tree -> (Nominal.term, string) Result.t
  =
 fun rules tree -> try Ok (tree_to_ast rules tree) with ToAstError msg -> Error msg
;;

(*
module TestOperators = struct
  let mul =
    OperatorMatch
      { tokens = [ NonterminalName "expr"; TerminalName "MUL"; NonterminalName "expr" ]
      ; operator_match_pattern =
          OperatorPattern
            ( "mul"
            , [ NumberedScopePattern ([], SingleCapturePattern 1)
              ; NumberedScopePattern ([], SingleCapturePattern 3)
              ] )
      }
  ;;

  let div =
    OperatorMatch
      { tokens = [ NonterminalName "expr"; TerminalName "DIV"; NonterminalName "expr" ]
      ; operator_match_pattern =
          OperatorPattern
            ( "div"
            , [ NumberedScopePattern ([], SingleCapturePattern 1)
              ; NumberedScopePattern ([], SingleCapturePattern 3)
              ] )
      }
  ;;

  let add =
    OperatorMatch
      { tokens = [ NonterminalName "expr"; TerminalName "ADD"; NonterminalName "expr" ]
      ; operator_match_pattern =
          OperatorPattern
            ( "add"
            , [ NumberedScopePattern ([], SingleCapturePattern 1)
              ; NumberedScopePattern ([], SingleCapturePattern 3)
              ] )
      }
  ;;

  let sub =
    OperatorMatch
      { tokens = [ NonterminalName "expr"; TerminalName "SUB"; NonterminalName "expr" ]
      ; operator_match_pattern =
          OperatorPattern
            ( "sub"
            , [ NumberedScopePattern ([], SingleCapturePattern 1)
              ; NumberedScopePattern ([], SingleCapturePattern 3)
              ] )
      }
  ;;

  let var =
    OperatorMatch
      { tokens = [ TerminalName "ID" ]
      ; operator_match_pattern = SingleCapturePattern 1
      }
  ;;

  let parens =
    OperatorMatch
      { tokens = [ TerminalName "LPAREN"; NonterminalName "expr"; TerminalName "RPAREN" ]
      ; operator_match_pattern = SingleCapturePattern 2
      }
  ;;
end
*)

(** @raise [InvariantViolation] *)
let convert_token
    :  LrParsing.terminal_num String.Map.t -> int String.Map.t -> nonterminal_token
    -> LrParsing.symbol
  =
 fun terminal_nums nonterminal_entry -> function
  | TerminalName tn ->
    Terminal
      (String.Map.find terminal_nums tn
      |> get_option' (fun () -> "to_grammar: failed to get terminal " ^ tn))
  | NonterminalName nt_name ->
    Nonterminal
      (String.Map.find nonterminal_entry nt_name
      |> get_option' (fun () ->
             Printf.sprintf
               "convert_token: couldn't find nonterminal %s in names: %s"
               nt_name
               (nonterminal_entry |> String.Map.keys |> Util.stringify_list Fn.id ", ")))
  | OpenBox _ | CloseBox | Underscore _ ->
    invariant_violation
      "all formatting tokens should be filtered (see is_formatting_token)"
;;

(** Slimmed-down version of [nonterminal_rules], without the name and type * information
    from [NonterminalRule]. Also, limited to a single level, and the * [int option] is the
    number of the attached operator rule. [Some num] if * there's a rule from the original
    grammar, [None] if it's just a * fallthrough to a higher level. *)
type nonterminal_operators = (int option * operator_match) list String.Map.t

let string_of_nonterminal_operators : nonterminal_operators -> string =
 fun nonterminal_operators -> nonterminal_operators
  |> String.Map.to_alist
  |> List.map ~f:(fun (name, matches) ->
     let rhs = matches
       |> Array.of_list
       |> Array.map ~f:(fun (ix_opt, op_match) ->
          let ix_str =
            match ix_opt with None -> "_" | Some ix -> string_of_int ix
          in
          Printf.sprintf "  %s -> %s" ix_str (string_of_operator_match op_match))
       |> String.concat_array ~sep:"\n"
     in
     Printf.sprintf "%s:\n%s" name rhs)
  |> String.concat ~sep:"\n"
;;

(** Produce an augmented grammar
 @raise UserError
 *)
let to_grammar
    :  ConcreteSyntaxDescription.t -> string
    -> LrParsing.augmented_grammar
       * (tree_info
         * nonterminal_token list
         * operator_match_pattern option)
         Int.Table.t
  =
 fun { terminal_rules; nonterminal_rules } start_nonterminal ->
  let terminal_nums =
    terminal_rules
    (* start other terminals (besides $ and SPACE) at 2 *)
    |> List.mapi ~f:(fun i (name, _) -> name, i + 2)
    |> List.append [ "$", 0; "SPACE", 1 ]
  in
  (* mapping from terminal to its number *)
  let terminal_num_map : int String.Map.t = String.Map.of_alist_exn terminal_nums in

  let nonterminal_num = ref 0 in
  (* Number every desugared nonterminal *)
  let nonterminal_num_map : int String.Map.t =
    String.Map.map nonterminal_rules ~f:(fun _ ->
        incr nonterminal_num;
        !nonterminal_num)
  in

  (* We're dealing with a non-augmented grammar here. [prod_num] starts
   * counting productions from 1. We'll add the starting production at 0 at the
   * end. *)
  let prod_num = ref 1 in
  (* [start_nonterminal_num] will be set to the number of the start nonterminal
   * when we find it. *)
  let start_nonterminal_num = ref (-1) in
  let production_rule_map = Int.Table.create () in

  (* Translate a nonterminal into n -- one for each of its precedence levels *)
  let nonterminals =
    nonterminal_rules
    |> String.Map.data
    |> List.mapi ~f:(fun
      i (NonterminalRule { nonterminal_name; operator_rules; _ }) ->
         (* Index starting from 1 *)
         let nt_num = i + 1 in
         if String.(nonterminal_name = start_nonterminal)
         then start_nonterminal_num := nt_num;
         let productions = List.mapi operator_rules ~f:(fun op_index (OperatorMatch rule) ->
           Int.Table.set
             production_rule_map
             ~key:!prod_num
             ~data:
               ( (nonterminal_name, op_index)
               , rule.tokens
               , Some rule.operator_match_pattern );
           incr prod_num;
           rule.tokens
           |> List.filter ~f:(fun tok -> not (is_formatting_token tok))
           |> List.map ~f:(convert_token terminal_num_map nonterminal_num_map))
         in
         let result =
           nonterminal_name, nt_num, { LrParsing.productions }
         in
         result)
    |> List.cons
         ( root_name
         , 0
         , { LrParsing.productions = [ [ Nonterminal !start_nonterminal_num ] ] } )
    |> Array.of_list
  in

  if !start_nonterminal_num = -1
  then raise (UserError
    (Printf.sprintf "Couldn't find starting nonterminal %s" start_nonterminal));

  ( AugmentedGrammar { nonterminals; terminal_nums = Array.of_list terminal_nums }
  , production_rule_map )
;;

(* Convert an LR parse result to a [formatted_tree].

 assumption: the language we're operating on is the derived language:
 1. It's augmented with a root nonterminal, 0
 2. All formatting tokens have been removed
 *)
let tree_of_parse_result (module Lr0 : LrParsing.LR0)
    :  (tree_info * nonterminal_token list * operator_match_pattern option) Int.Table.t
    -> LrParsing.nonterminal_num String.Map.t
    -> ConcreteSyntaxDescription.nonterminal_rules -> string (* parsed string *)
    -> LrParsing.parse_result -> formatted_tree
  =
 fun production_rule_map
     _nonterminal_nums
     _nonterminal_rules
     str
     root ->
  let str_pos = ref 0 in
  let str_len = String.length str in
  let get_trivia : int -> int -> string * string =
   fun start_pos end_pos ->
    (* look back consuming all whitespace to (and including) a newline *)
    let leading_trivia =
      if !str_pos < start_pos then String.slice str !str_pos start_pos else ""
    in
    (* look forward consuming all whitespace up to a newline *)
    str_pos := end_pos;
    let continue = ref (end_pos < str_len) in
    while !continue do
      (* TODO: need to be aware of other whitespace tokens *)
      let got_space = Char.(str.[!str_pos] = ' ') in
      continue := !str_pos < str_len && got_space;
      if !continue then incr str_pos
    done;
    let trailing_trivia = String.slice str end_pos !str_pos in
    leading_trivia, trailing_trivia
  in

  let rec go_nt : LrParsing.parse_result -> formatted_tree =
   fun { production; children; _ } ->
    let prod_num =
      match production with
      | First prod ->
        invariant_violation
          (Printf.sprintf
             "go_nt received a terminal production: %s"
             (Lr0.string_of_terminal prod))
      | Second prod_num -> prod_num
    in
    let tree_info, tokens, _ =
      Int.Table.find production_rule_map prod_num
      |> get_option' (fun () ->
             Printf.sprintf
               "tree_of_parse_result: couldn't find nonterminal %n in production_rule_map"
               prod_num)
    in
    let tokens' = List.filter tokens ~f:(fun tok -> not (is_formatting_token tok)) in
    let children' =
      List.map2_exn children tokens' ~f:(fun x y -> x, y)
      |> Array.of_list
      |> Array.map ~f:(function parse_result, token ->
             (match token with
             | TerminalName _ -> TerminalCapture (go_t parse_result)
             | NonterminalName _ -> NonterminalCapture (go_nt parse_result)
             (* TODO: trivia *)
             | OpenBox _ | CloseBox | Underscore _ ->
               invariant_violation "formatting tokens must be filtered beforehand"))
    in

    { tree_info; children = children' }

  and go_t : LrParsing.parse_result -> formatted_terminal_capture =
   fun { start_pos; end_pos; _ } ->
    let leading_trivia, trailing_trivia = get_trivia start_pos end_pos in
    let content = String.slice str start_pos end_pos in
    { leading_trivia; content; trailing_trivia }
  in

  go_nt root
;;

let lexer_of_desc : ConcreteSyntaxDescription.t -> Lex.lexer =
 fun { terminal_rules; _ } ->
  ("SPACE", Regex.ReClass (PosClass Whitespace)) :: terminal_rules
;;

let parse desc start_nonterminal str =
  let (AugmentedGrammar grammar as ag), production_rule_map =
    to_grammar desc start_nonterminal
  in
  let module Lalr =
    LalrParsing.Lalr1 (struct
      let grammar = ag
    end)
  in
  let lexer = lexer_of_desc desc in

  match Lalr.lex_and_parse lexer str with
  | Ok tree_root ->
    Ok
      (tree_of_parse_result
         (module Lalr)
         production_rule_map
         (grammar.nonterminals
         |> Array.map ~f:(fun (name, num, _nt) -> name, num)
         |> Array.to_list
         |> String.Map.of_alist_exn)
         desc.nonterminal_rules
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
  { terminal_rules =
      terminal_rules
      |> List.map ~f:(fun (PreTerminalRule (name, str_or_re_str)) ->
             match str_or_re_str with
             | First re_str ->
               (match Parse_regex.parse re_str with
               | Ok re -> name, re
               | Error msg -> failwith ("failed to parse regex: " ^ msg))
             | Second str -> name, Regex.re_str str)
  ; nonterminal_rules =
      nonterminal_rules
      |> List.map ~f:(fun (NonterminalRule { nonterminal_name; _ } as rule) ->
             nonterminal_name, rule)
      |> String.Map.of_alist_exn
  }
;;
