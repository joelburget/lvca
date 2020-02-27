open Core_kernel
open Types
open ConcreteSyntaxDescription

let invariant_violation = Util.invariant_violation

(** This type lives in [formatted_tree] (ie [formatted_nonterminal_capture]),
 * and holds enough info to produce an AST.
 *
 * We use the nonterminal name and construction number. We use a construction
 * number rather than an operator name, because there could be multiple
 * constructions producing the same operator
 *)
type tree_info = string * int

(** Terminals capture text from the input buffer *)
type formatted_terminal_capture =
  { content : string
  ; leading_trivia : string
  ; trailing_trivia : string
  }

(** Nonterminals capture their children *)
type formatted_nonterminal_capture = formatted_tree

(** Terminals and nonterminals both capture data about why they were
    constructed
*)
and formatted_capture =
  | TerminalCapture of formatted_terminal_capture
  | NonterminalCapture of formatted_nonterminal_capture

(* Inspired by:
 * - https://github.com/apple/swift/tree/master/lib/Syntax
 * - https://github.com/dotnet/roslyn/wiki/Roslyn-Overview#syntax-trees
 *
 * Rules of trivia (same as for swift):
 * - A token owns all of its trailing trivia up to, but not including, the
 *   next newline character.
 * - Looking backward in the text, a token owns all of the leading trivia up
 *   back and including the first newline character.
 *
 * In other words, a contiguous stretch of trivia between two tokens is split
 * on the leftmost newline.
*)
and formatted_tree =
  { tree_info : tree_info
  ; children : formatted_capture array
  }

(** Pretty-printing declarations that have no children.
 *
 * Perhaps "atomic" is a better word than "terminal".
 *)
type terminal_doc =
  | DocText of string
  | DocNest of int * terminal_doc
  | DocBreak of int

type nonterminal_doc = doc list * tree_info

and doc_group = (terminal_doc, nonterminal_doc) Either.t list

(** Pretty-printing declarations with children.
 *)
and doc =
  | TerminalDoc of terminal_doc
  | NonterminalDoc of nonterminal_doc
  | DocGroup of doc_group

(** tree equality mod trivia *)
let rec equivalent : formatted_tree -> formatted_tree -> bool
  = fun t1 t2 ->
  Caml.(t1.tree_info = t2.tree_info)
  && Array.(length t1.children = length t2.children)
  && Array.for_all (Array.map2_exn t1.children t2.children ~f:equivalent')
       ~f:Fn.id

and equivalent' child1 child2 =
  match child1, child2 with
  | TerminalCapture tc1, TerminalCapture tc2
  -> Caml.(tc1.content = tc2.content)
  | NonterminalCapture ntc1, NonterminalCapture ntc2
  -> equivalent ntc1 ntc2
  | _, _
  -> false
;;

let rec to_string : formatted_tree -> string
  = fun { children; _ } -> children
  |> Array.map ~f:(function
    | TerminalCapture { leading_trivia; content; trailing_trivia } ->
      leading_trivia ^ content ^ trailing_trivia
    | NonterminalCapture nonterminal_capture -> to_string nonterminal_capture)
  |> Array.to_list
  |> String.concat
;;

let string_of_tree_info : tree_info -> string
  = fun (name, i) -> Printf.sprintf "%s:%n" name i
;;

let rec to_debug_string : formatted_tree -> string
  = fun { children; tree_info } -> children
  |> Array.map ~f:string_of_formatted_capture
  |> String.concat_array ~sep:"; "
  |> Printf.sprintf "%s(%s)" (string_of_tree_info tree_info)

and string_of_formatted_capture = function
  | TerminalCapture { leading_trivia; content; trailing_trivia }
  -> "\"" ^ leading_trivia ^ content ^ trailing_trivia ^ "\""
  | NonterminalCapture nonterminal_capture
  -> to_debug_string nonterminal_capture
;;

(** Points to the current sort among the whole language.
 *)
type nonterminal_pointer =
  { nonterminals : nonterminal_rules
  ; current_nonterminal : string
  ; bound_sorts : sort String.Map.t
  }

let current_sort : nonterminal_pointer -> sort
  = fun { nonterminals; current_nonterminal; bound_sorts } ->
  match String.Map.find nonterminals current_nonterminal with
    | None -> invariant_violation (Printf.sprintf
      "current_sort: didn't find current nonterminal %s in set of nonterminals"
      current_nonterminal
    )
    | Some (NonterminalRule nonterminal_rule)
    -> let NonterminalType (_, result) = nonterminal_rule.nonterminal_type in
       instantiate_sort bound_sorts result

let current_nonterminal : nonterminal_pointer -> nonterminal_rule
  = fun { nonterminals; current_nonterminal; _ } ->
    match String.Map.find nonterminals current_nonterminal with
      | None -> invariant_violation (Printf.sprintf
        "current_nonterminal: didn't find current nonterminal %s in set of \
         nonterminals"
        current_nonterminal
      )
      | Some nt -> nt

(* TODO: move with binding *)
let move_to : nonterminal_pointer -> string -> nonterminal_pointer
  = fun { nonterminals; _ } nt_name ->
    { nonterminals
    ; current_nonterminal = nt_name
    ; bound_sorts = String.Map.empty
    }

type subpattern_result =
  CapturedPattern of sort * nonterminal_pointer * Pattern.t

type subterm_result =
  | CapturedTerm   of sort * nonterminal_pointer * Binding.Nominal.term
  | CapturedBinder of sort * nonterminal_pointer * Pattern.t

let string_of_subterm_result : subterm_result -> string
  = function
    | CapturedTerm (sort, { current_nonterminal; _ }, tm) ->
      Printf.sprintf "CapturedTerm (%s, { current_nonterminal = %s; _ }, %s)"
      (string_of_sort sort)
      current_nonterminal
      (Binding.Nominal.pp_term' tm)
    | CapturedBinder (sort, { current_nonterminal; _ }, pat) ->
      Printf.sprintf "CapturedBinder (%s, { current_nonterminal = %s; _ }, %s)"
      (string_of_sort sort)
      current_nonterminal
      (Pattern.string_of_pattern pat)

exception UserError of string
exception NoMatch of string

(** Go through every token giving it an index. Underscores and boxes are all
 * indexed 0, terminals and nonterminals are indexed from 1.
 *)
let index_tokens : nonterminal_token list -> (int * nonterminal_token) list
  = fun tokens -> tokens
    |> List.fold_left
      ~init:(1, [])
      ~f:(fun (ix, indexed_toks) tok -> match tok with
        | Underscore _
        | OpenBox _
        | CloseBox
        -> ix, (0, tok) :: indexed_toks
        | _
        -> ix + 1, (ix, tok) :: indexed_toks)
    |> fun (_, lst) -> lst

(** Go through every token giving it an index. Underscores and boxes are not
 * indexed *)
let map_index_tokens
  : nonterminal_token list -> nonterminal_token Int.Map.t
  = fun tokens -> tokens
    |> index_tokens
    |> List.filter ~f:(fun (i, _) -> i <> 0)
    |> Int.Map.of_alist_exn

let string_of_op_match_line
  : nonterminal_token list -> operator_match_pattern -> string
  = fun tokens op_match_pat -> Printf.sprintf "%s { ... %s ... }"
    (string_of_tokens tokens)
    (string_of_operator_match_pattern op_match_pat)

let string_of_op_match_line'
  : nonterminal_token list -> numbered_scope_pattern -> string
  = fun tokens op_match_pat -> Printf.sprintf "%s { ... %s ... }"
    (string_of_tokens tokens)
    (string_of_numbered_scope_pattern op_match_pat)

let rec get_subpatterns
  :  nonterminal_token list
  -> nonterminal_token Int.Map.t
  -> nonterminal_pointer
  -> operator_match_pattern
  -> Pattern.t
  -> subpattern_result Int.Map.t
  = fun tokens tokens_map nonterminal_pointer op_match_pat pat ->
    match op_match_pat, pat with
  | SingleCapturePattern num, _
  -> let tok = Int.Map.find tokens_map num
       (* TODO: change to InvariantViolation if we validate this *)
       |> Util.get_option (UserError (Printf.sprintf
         "Couldn't find token $%n in tokens: %s"
         num
         (string_of_op_match_line tokens op_match_pat)
       ))
     in
     let nt_name = match tok with
       | NonterminalName nt_name -> nt_name
       | TerminalName t_name -> raise (UserError (Printf.sprintf
         "Token $%n captures a terminal (%s) directly, but only \
          nonterminals can be captured and put in an AST: %s"
         num t_name (string_of_op_match_line tokens op_match_pat)
       ))
       | Underscore _ | OpenBox _ | CloseBox
       -> invariant_violation "boxes and underscores are not indexed"
     in
     let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
     let capture = CapturedPattern
       (current_sort nonterminal_pointer, nonterminal_pointer', pat)
     in
     Int.Map.of_alist_exn [ num, capture ]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if Caml.(pat_op_name = op_name) &&
        List.(length body_pats = length body_scopes)
     then
       List.map2_exn body_pats body_scopes
         ~f:(fun (NumberedScopePattern (caps, body_pat)) ->
           if List.length caps > 0 then failwith "TODO: error 6";
           get_subpatterns tokens tokens_map nonterminal_pointer body_pat)
       |> Util.int_map_unions
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")
  | OperatorPattern _, _
  -> raise (NoMatch "operator pattern and value don't match")

(**
 Retrieve all the numbered subterms / binders for a term pattern template (eg
 [foo($1. bar($2); $3)]). Given the term [foo(x. bar(baz()); y)], this returns

 {[
 1 -> x
 2 -> baz()
 3 -> y
 ]}

 Invariants assumed:
 - Scopes and numbered scope patterns mirror each other (are the same length).
 - Each numbering in the pattern is unique
 - The pattern is numbered contiguously from 1 to some n >= 1.

 @raise [NoMatch]
 @raise [UserError]
 @raise [InvariantViolation]
 *)
let rec get_subterms
  :  nonterminal_token list
  -> nonterminal_token Int.Map.t
  -> nonterminal_pointer
  -> operator_match_pattern
  -> Binding.Nominal.term
  -> subterm_result Int.Map.t
  = fun tokens tokens_map nonterminal_pointer op_match_pat tm ->
    match op_match_pat, tm with
  | SingleCapturePattern num, _
  -> let tok = Int.Map.find tokens_map num
       (* TODO: change to InvariantViolation if we validate this *)
       |> Util.get_option (UserError (Printf.sprintf
         "Couldn't find token $%n in tokens: %s"
         num
         (string_of_op_match_line tokens op_match_pat)
       ))
     in
     let nt_name = match tok with
       | NonterminalName nt_name -> nt_name
       | TerminalName t_name -> raise (UserError (Printf.sprintf
         "Token $%n captures a terminal (%s) directly, but only \
          nonterminals can be captured and put in an AST: %s"
         num t_name (string_of_op_match_line tokens op_match_pat)
       ))
       | Underscore _ | OpenBox _ | CloseBox
       -> invariant_violation "boxes and underscores are not indexed"
     in
     let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
     let capture = CapturedTerm
       (current_sort nonterminal_pointer, nonterminal_pointer', tm)
     in
     Int.Map.of_alist_exn [ num, capture ]
  | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern num)])
  , Var name
  -> let capture = CapturedTerm
       (current_sort nonterminal_pointer, nonterminal_pointer, Var name)
     in
     Int.Map.of_alist_exn [ num, capture ]
  | OperatorPattern ("integer", [NumberedScopePattern([], SingleCapturePattern num)])
  , Primitive (PrimInteger _)
  | OperatorPattern ("string", [NumberedScopePattern([], SingleCapturePattern num)])
  , Primitive (PrimString _)
  -> let capture = CapturedTerm
       (current_sort nonterminal_pointer, nonterminal_pointer, tm)
     in
     Int.Map.of_alist_exn [ num, capture ]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if Caml.(pat_op_name = op_name) &&
        List.(length body_pats = length body_scopes)
     then (List.map2_exn body_pats body_scopes
         ~f:(get_scope_subterms tokens tokens_map nonterminal_pointer)
       |> Util.int_map_unions
     )
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")
  | OperatorPattern _, _
  -> raise (NoMatch "operator pattern and value don't match")

(** See [get_subterms].

 @raise [NoMatch]
 @raise [UserError]
 @raise [InvariantViolation]
*)
and get_scope_subterms
  :  nonterminal_token list
  -> nonterminal_token Int.Map.t
  -> nonterminal_pointer
  -> numbered_scope_pattern
  -> Binding.Nominal.scope
  -> subterm_result Int.Map.t
  = fun tokens tokens_map nonterminal_pointer
    (NumberedScopePattern (numbered_patterns, body_pat) as ns_pat)
    (Scope (term_patterns, body)) ->
      (*
    Printf.printf "trying to match %n / %n binders\n"
      (List.length numbered_patterns)
      (List.length term_patterns) ;
      *)
    if List.(length numbered_patterns <> length term_patterns)
    then raise
      (NoMatch "numbered scope pattern and term scope have different arity")
    else
      let pattern_bindings = List.map2_exn numbered_patterns term_patterns
        ~f:(fun binder_capture term_pat -> match binder_capture, term_pat with
          | VarCapture num, Var var_name ->
            num,
            CapturedBinder
              ( current_sort nonterminal_pointer
              , nonterminal_pointer
              , Var var_name
              )
          | VarCapture _, _ -> raise (UserError (Printf.sprintf
            "get_scope_subterms: trying to capture a variable, but found \
             a pattern"
          ))
          | PatternCapture captured_token_num, _ ->
            let tok = Int.Map.find tokens_map captured_token_num
              (* TODO: change to InvariantViolation if we validate this *)
              |> Util.get_option (UserError (Printf.sprintf
                "Couldn't find token $%n in tokens: %s"
                captured_token_num
                (string_of_op_match_line' tokens ns_pat)
              ))
            in
            let nt_name = match tok with
              | NonterminalName nt_name -> nt_name
              | TerminalName t_name -> raise (UserError (Printf.sprintf
                "Matching pattern, token $%n captures a terminal (%s) \
                 directly, but only nonterminals can be captured and put in \
                 an AST: %s"
                captured_token_num t_name (string_of_op_match_line' tokens ns_pat)
              ))
              | Underscore _ | OpenBox _ | CloseBox
              -> invariant_violation "boxes and underscores are not indexed"
            in
            let nonterminal_pointer' = move_to nonterminal_pointer nt_name in
            let capture = CapturedBinder
              (current_sort nonterminal_pointer, nonterminal_pointer', term_pat)
            in
            captured_token_num, capture
        )
        |> Int.Map.of_alist_exn
      in
      (* Printf.printf "trying to match body\n"; *)
      let body_bindings =
        get_subterms tokens tokens_map nonterminal_pointer body_pat body
      in
      (* Printf.printf "matched body\n"; *)
      Int.Map.merge pattern_bindings body_bindings ~f:(fun ~key -> function
        | `Both _ -> raise
          (UserError (Printf.sprintf "duplicate token capture: $%n" key))
        | `Left v | `Right v -> Some v
      )

(** Check that nonterminal mentioned in tokens appears in the subterm mapping.
 *)
let check_tokens subterms tokens : unit = tokens
  |> index_tokens
  |> Array.of_list
  |> Array.iter ~f:(fun (tok_ix, tok) -> match tok with
    | NonterminalName _ ->
      if not (Int.Map.mem subterms tok_ix)
      then (
        let available_keys = subterms
          |> Int.Map.keys
          |> Util.stringify_list string_of_int ", "
        in
        failwith (Printf.sprintf
          "error: key missing in pattern: $%n. available keys: %s"
          tok_ix available_keys)
      )
    | _ -> ()
  )

(**
 Find a matching syntactical description for the given term. This traverses
 the set of possible forms from bottom to top until it finds one that
 matches.

 Invariants assumed:
 - Each pattern is numbered contiguously from 1 to n, where n is the number
 of nonterminal tokens associated with the pattern.

 Example:

 {[
 foo :=
   | STRING ARROW foo { bar($1. $2) }
   | INTEGER { lit(integer($1)) }
   | TRUE { true() }
 ]}

 The term [bar(x. true())] would match the first form, returning:

 {[
 1 -> [CapturedBinder ...]
 2 -> [CapturedTerm ...]
 ]}

 @raise [InvariantViolation]
 @raise [UserError]
*)
let find_operator_match
  :  nonterminal_pointer
  -> operator_match list list
  -> Binding.Nominal.term
  -> int *
     operator_match_pattern *
     nonterminal_token list *
     subterm_result Int.Map.t
  = fun nonterminal_pointer matches tm ->

    matches
      |> List.join
      |> List.mapi ~f:(fun match_ix x -> match_ix, x)
      |> List.rev (* TODO: O(n^2) reverse *)
      |> List.find_map ~f:(fun (match_ix, OperatorMatch op_match) ->
        let { operator_match_pattern = pat; tokens; _ } = op_match in
        try
          let tokens_map = map_index_tokens tokens in
          Some (
            match_ix,
            pat,
            tokens,
            get_subterms tokens tokens_map nonterminal_pointer pat tm
          )
        with
          NoMatch _ -> None
      )
      |> Util.get_option' (fun () ->
        "failed to find a rule matching term " ^ Binding.Nominal.pp_term' tm)
      |> (fun ((_, _, tokens, subterms) as result) ->
        check_tokens subterms tokens;
        result
      )

let%test_module "find_operator_match" = (module struct
  open Binding.Nominal

  let unit_op_match = OperatorMatch
    { tokens = [ TerminalName "UNIT" ]
    ; operator_match_pattern = OperatorPattern ("unit", [])
    ; fixity = Nofix
    }

  let num_op_match = OperatorMatch
    { tokens = [ TerminalName "NUM" ]
    ; operator_match_pattern = OperatorPattern ("num", [
        NumberedScopePattern ([],
          OperatorPattern ("integer", [
            NumberedScopePattern ([], SingleCapturePattern 1)
          ])
        )
      ])
    ; fixity = Nofix
    }

  let str_op_match = OperatorMatch
    { tokens = [ TerminalName "STR" ]
    ; operator_match_pattern = OperatorPattern ("str", [
        NumberedScopePattern ([],
          OperatorPattern ("string", [
            NumberedScopePattern ([], SingleCapturePattern 1)
          ])
        )
      ])
    ; fixity = Nofix
    }

  let add_op_match = OperatorMatch
    { tokens = [
        NonterminalName "expr";
        TerminalName "ADD";
        NonterminalName "expr";
      ]
    ; operator_match_pattern = OperatorPattern ("add", [
        NumberedScopePattern ([], SingleCapturePattern 1);
        NumberedScopePattern ([], SingleCapturePattern 3);
      ])
    ; fixity = Infixl
    }

  let paren_op_match = OperatorMatch
    { tokens = [
        TerminalName "LPAREN";
        NonterminalName "expr";
        TerminalName "RPAREN";
      ]
    ; operator_match_pattern = SingleCapturePattern 2
    ; fixity = Nofix
    }

  let lam_op_match = OperatorMatch
    { tokens = [
        TerminalName "FUN";
        TerminalName "NAME";
        TerminalName "ARROW";
        NonterminalName "expr";
      ]
    ; operator_match_pattern = OperatorPattern ("lam", [
      NumberedScopePattern ([VarCapture 2], SingleCapturePattern 4)
      ])
    ; fixity = Nofix
    }

  let match_line_match = OperatorMatch
    { tokens = [
        NonterminalName "expr";
        TerminalName "ARROW";
        NonterminalName "expr";
      ]
    ; operator_match_pattern = OperatorPattern ("match_line", [
      NumberedScopePattern ([PatternCapture 1], SingleCapturePattern 3)
      ])
    ; fixity = Nofix
    }

  let operator_rules = [
    [ paren_op_match ];
    [ lam_op_match ];
    [ unit_op_match; num_op_match; str_op_match ];
    [ add_op_match ];
    [ match_line_match ];
  ]

  let nonterminals = String.Map.of_alist_exn
    [ "expr", NonterminalRule
      { nonterminal_name = "expr"
      ; nonterminal_type = NonterminalType ([], SortAp ("expr", [||]))
      ; operator_rules
      }
    ]


  let print_match_result = fun tm ->
    let nonterminal_pointer =
      { nonterminals
      ; current_nonterminal = "expr"
      ; bound_sorts = String.Map.empty
      }
    in

    let match_ix, pat, tokens, subterms =
      find_operator_match nonterminal_pointer operator_rules tm
    in
    Printf.printf "%n\n%s\n%s\n"
      match_ix
      (string_of_operator_match_pattern pat)
      (string_of_tokens tokens);

    subterms
      |> Map.iteri ~f:(fun ~key ~data -> Printf.printf "%n -> %s\n"
        key (string_of_subterm_result data)
      )

  let%expect_test "find_operator_match unit()" =
    print_match_result (Operator ("unit", []));

    [%expect{|
      2
      unit()
      UNIT |}]

  let%expect_test "find_operator_match add(x; y)" =
    print_match_result (Operator ("add", [
      Scope ([], Var "x");
      Scope ([], Var "y");
    ]));

    [%expect{|
      5
      add($1; $3)
      expr ADD expr
      1 -> CapturedTerm (expr, { current_nonterminal = expr; _ }, x)
      3 -> CapturedTerm (expr, { current_nonterminal = expr; _ }, y) |}]

  let%expect_test "find_operator_match lam(x. x)" =
    print_match_result (Operator ("lam", [
      Scope ([Pattern.Var "x"], Var "x")
    ]));

    [%expect{|
      1
      lam(var($2). $4)
      FUN NAME ARROW expr
      2 -> CapturedBinder (expr, { current_nonterminal = expr; _ }, x)
      4 -> CapturedTerm (expr, { current_nonterminal = expr; _ }, x) |}]

  let%expect_test "find_operator_match match_line(add(x; y). add(x; y))" =
    print_match_result (Operator ("match_line", [
      Scope
        ( [Pattern.Operator ("add", [Var "x"; Var "y"])]
        , Operator ("add", [Scope ([], Var "x"); Scope ([], Var "y")])
        )
    ]));

    [%expect{|
      6
      match_line($1. $3)
      expr ARROW expr
      1 -> CapturedBinder (expr, { current_nonterminal = expr; _ }, add(x; y))
      3 -> CapturedTerm (expr, { current_nonterminal = expr; _ }, add(x, y)) |}]

  let%expect_test "find_operator_match num(5)" =
    print_match_result (Operator ("num", [
      Scope ([], Primitive (PrimInteger (Bigint.of_int 5)))
    ]));

    [%expect{|
      3
      num(integer($1))
      NUM
      1 -> CapturedTerm (expr, { current_nonterminal = expr; _ }, 5) |}]

  let%expect_test {|find_operator_match str("foo")|} =
    print_match_result (Operator ("str", [
      Scope ([], Primitive (PrimString "foo"))
    ]));

    [%expect{|
      4
      str(string($1))
      STR
      1 -> CapturedTerm (expr, { current_nonterminal = expr; _ }, "foo") |}]

end)

(** See [find_operator_match].

 @raise [InvariantViolation]
 @raise [UserError]
*)
let find_pat_operator_match
  :  nonterminal_pointer
  -> operator_match list list
  -> Pattern.t
  -> int
   * operator_match_pattern * nonterminal_token list
   * subpattern_result Int.Map.t
  = fun nonterminal_pointer matches pattern -> matches
    |> List.join
    |> List.mapi ~f:(fun match_ix x -> match_ix, x)
    |> List.find_map ~f:(fun (match_ix, OperatorMatch op_match) ->
      let { operator_match_pattern = pat; tokens; _ } = op_match in
      let tokens_map = map_index_tokens tokens in
      try
        Some (
          match_ix,
          pat,
          tokens,
          get_subpatterns tokens tokens_map nonterminal_pointer pat pattern
        )
      with
        NoMatch _ -> None
    )
    |> Util.get_option' (fun () ->
      "failed to find a rule matching pattern " ^ Pattern.string_of_pattern pattern)
    |> (fun ((_, _, tokens, subterms) as result) ->
      check_tokens subterms tokens;
      result
    )
;;
