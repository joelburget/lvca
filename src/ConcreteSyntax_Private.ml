open Core_kernel
open Types
open ConcreteSyntaxDescription
module Parse_abstract = Parsing.Incremental (Parsing.Parseable_abstract_syntax)

let get_option', invariant_violation = Util.(get_option', invariant_violation)

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
  | DocBreak of int

type box_info =
  | HBox of int
  (** A horizontal printing box.

   [HBox h] means the objects of the box are positioned on the same line
   separated by [h] spaces.
   *)
  | VBox of int * int
  (** A vertical printing box.

   [VBox i v] means each object is placed on a separate line, separated by [v]
   blank lines, and the second and later objects are indented by [i]
   characters.
   *)
  | HovBox of int * int * int
  (** A horizontal-or-vertical printing box.

   [Hovbox h i v] means either all objects should go on one line, or they
   should all be formatted vertically.
   *)
  | HvBox of int * int * int
  (** A horizontal/vertical printing box.

   [HvBox h i v] means as many objects as possible should be placed on the same
   line, and a new line begun when there is no more space, with indentation
   [i].
   *)

(** How much does this box cause new lines to indent? *)
let box_indentation : box_info -> int
  = function
    | HBox _ -> 0
    | VBox (i, _)
    | HovBox (_, i, _)
    | HvBox (_, i, _) -> i

let mk_box_info : box_type -> int list -> (box_info, string) Result.t
  = fun ty args ->
  let args_str = args
    |> List.map ~f:string_of_int
    |> String.concat ~sep:", "
  in

  match ty with
  | HBox -> (match args with
    | [] -> Ok (HBox 1)
    | [h] -> Ok (HBox h)
    | _ -> Error (Printf.sprintf
      "Unexpected number of args to hbox (expected 0 or 1): %s"
      args_str))
  | VBox -> (match args with
    | [] -> Ok (VBox (2, 0))
    | [i] -> Ok (VBox (i, 0))
    | [i; v] -> Ok (VBox (i, v))
    | _ -> Error (Printf.sprintf
      "Unexpected number of args to vbox (expected 0-2): %s"
      args_str))
  | HovBox -> (match args with
    | [] -> Ok (HovBox (1, 2, 0))
    | [h] -> Ok (HovBox (h, 2, 0))
    | [h; i] -> Ok (HovBox (h, i, 0))
    | [h; i; v] -> Ok (HovBox (h, i, v))
    | _ -> Error (Printf.sprintf
      "Unexpected number of args to hovbox (expected 0-3): %s"
      args_str))
  | HvBox -> (match args with
    | [] -> Ok (HvBox (1, 2, 0))
    | [h] -> Ok (HvBox (h, 2, 0))
    | [h; i] -> Ok (HvBox (h, i, 0))
    | [h; i; v] -> Ok (HvBox (h, i, v))
    | _ -> Error (Printf.sprintf
      "Unexpected number of args to hvbox (expected 0-3): %s"
      args_str))

type nonterminal_doc = doc list * tree_info

(** Pretty-printing declarations with children.
 *)
and doc =
  | TerminalDoc of terminal_doc
  | NonterminalDoc of nonterminal_doc
  | DocGroup of doc list * box_info

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
  |> Array.map ~f:formatted_capture_to_string
  |> Array.to_list
  |> String.concat

and formatted_capture_to_string : formatted_capture -> string
  = function
    | TerminalCapture { leading_trivia; content; trailing_trivia } ->
      leading_trivia ^ content ^ trailing_trivia
    | NonterminalCapture nonterminal_capture -> to_string nonterminal_capture
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

type subterm_result =
  | CapturedTerm   of sort * Binding.Nominal.term
  | CapturedBinder of sort * Pattern.t

let string_of_subterm_result : subterm_result -> string
  = function
    | CapturedTerm (sort, tm) ->
      Printf.sprintf "CapturedTerm (%s, %s)"
      (string_of_sort sort)
      (Binding.Nominal.pp_term' tm)
    | CapturedBinder (sort, pat) ->
      Printf.sprintf "CapturedBinder (%s, %s)"
      (string_of_sort sort)
      (Pattern.string_of_pattern pat)

exception UserError of string
exception NoMatch of string

(** Go through every token giving it an index. Underscores and boxes are all
 indexed 0, terminals and nonterminals are indexed from 1.
 *)
let index_tokens : nonterminal_token list -> (int * nonterminal_token) Fqueue.t
  = fun tokens -> tokens
    |> List.fold_left
      ~init:(1, Fqueue.empty)
      ~f:(fun (ix, indexed_toks) tok -> match tok with
        | Underscore _
        | OpenBox _
        | CloseBox
        -> ix, Fqueue.enqueue indexed_toks (0, tok)
        | _
        -> ix + 1, Fqueue.enqueue indexed_toks (ix, tok))
    |> snd

(** Go through every token giving it an index. Underscores and boxes are not
 * indexed *)
let map_index_tokens
  : nonterminal_token list -> nonterminal_token Int.Map.t
  = fun tokens -> tokens
    |> index_tokens
    |> Fqueue.to_list
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

let rec concretize_sort : sort String.Map.t -> sort -> sort
  = fun env -> function
    | SortAp (name, sub_sorts) -> SortAp
      ( name
      , Array.map sub_sorts ~f:(concretize_sort env)
      )
    | SortVar name
    -> Map.find env name
      |> get_option' (fun () -> "concretize_sort: unknown variable " ^ name)

let concretize_valence : sort String.Map.t -> valence -> valence
  = fun env -> function
    | FixedValence (binding_sorts, body_sort) -> FixedValence
      ( List.map binding_sorts ~f:(concretize_sort env)
      , concretize_sort env body_sort
      )
    | VariableValence (bound_sort, body_sort)
    -> VariableValence
      (concretize_sort env bound_sort, concretize_sort env body_sort)

let concretize_arity : sort String.Map.t -> arity -> arity
  = fun env (Arity (indices, valences)) -> Arity
    ( indices
    , List.map valences ~f:(concretize_valence env)
    )

let get_valences : sort_defs -> string * string -> string list * arity
  = fun (SortDefs def_map) (sort_name, op_name) ->

  let SortDef (var_names, op_defs) = Map.find def_map sort_name
    |> get_option'
      (fun () -> "get_valences: unable to find child sort " ^ sort_name)
  in

  let OperatorDef (_, arity) = op_defs
    |> List.find ~f:(fun (OperatorDef (name, _)) -> String.(name = op_name))
    |> get_option' (fun () -> Printf.sprintf
      "get_valences: unable to find operator %s in sort %s"
      op_name
      sort_name)
  in

  var_names, arity

let rec get_subpatterns
  :  sort_defs
  -> sort
  -> operator_match_pattern
  -> Pattern.t
  -> subterm_result Int.Map.t
  = fun sort_defs current_sort op_match_pat pat ->
    match op_match_pat, pat with
  | SingleCapturePattern num, _
  | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern num)])
  , Var _
  | OperatorPattern ("integer", [NumberedScopePattern([], SingleCapturePattern num)])
  , Primitive (PrimInteger _)
  | OperatorPattern ("string", [NumberedScopePattern([], SingleCapturePattern num)])
  , Primitive (PrimString _)
  -> Int.Map.of_alist_exn [ num, CapturedBinder (current_sort, pat) ]
  | OperatorPattern (l_op_name, l_body_pats), Operator (r_op_name, r_body_pats)
  -> if String.(l_op_name = r_op_name) &&
        List.(length l_body_pats = length r_body_pats)
        (* TODO check valences length? *)
     then
       let sort_name, sort_args = match current_sort with
         | SortAp (sort_name, args) -> sort_name, Array.to_list args
         | _ -> invariant_violation "get_subterms: passed non-concrete sort"
       in
       let op_sort_vars, arity = get_valences sort_defs (sort_name, l_op_name) in
       (* TODO check right amount of args *)

       let sort_arg_map = List.zip_exn op_sort_vars sort_args
         |> String.Map.of_alist_exn
       in

       let arity' = concretize_arity sort_arg_map arity in
       let Arity (_, valences) = arity' in

       List.map3_exn l_body_pats r_body_pats valences
         ~f:(fun (NumberedScopePattern (caps, l_body_pat)) r_body_pat valence ->
           if List.length caps > 0 then failwith "TODO: get_subpatterns error";
           match valence with
             | FixedValence ([], body_sort)
             -> get_subpatterns sort_defs body_sort l_body_pat r_body_pat
             | _ -> failwith "TODO: error")
       |> Util.int_map_unions
     else raise (NoMatch
       "pattern and operator don't match, either in operator name or subterms")
  | OperatorPattern _, _
  -> raise (NoMatch "operator pattern and value don't match")

(**
 Retrieve all the numbered subterms / binders for a term pattern template (eg
 [foo(var($1). bar($2); $3)]). Given the term [foo(x. bar(baz()); y)], this
 returns

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
  :  sort_defs
  -> sort
  -> operator_match_pattern
  -> Binding.Nominal.term
  -> subterm_result Int.Map.t
  = fun sort_defs current_sort op_match_pat tm ->
    match op_match_pat, tm with
  | SingleCapturePattern num, _
  | OperatorPattern ("var", [NumberedScopePattern([], SingleCapturePattern num)])
  , Var _
  | OperatorPattern ("integer", [NumberedScopePattern([], SingleCapturePattern num)])
  , Primitive (PrimInteger _)
  | OperatorPattern ("string", [NumberedScopePattern([], SingleCapturePattern num)])
  , Primitive (PrimString _)
  -> Int.Map.of_alist_exn [ num, CapturedTerm (current_sort, tm) ]
  | OperatorPattern (pat_op_name, body_pats), Operator (op_name, body_scopes)
  -> if String.(pat_op_name = op_name) &&
        List.(length body_pats = length body_scopes)
        (* TODO: check valences length? *)
     then
       let sort_name, sort_args = match current_sort with
         | SortAp (sort_name, args) -> sort_name, Array.to_list args
         | _ -> invariant_violation "get_subterms: passed non-concrete sort"
       in
       let op_sort_vars, arity = get_valences sort_defs (sort_name, op_name) in

       (if List.(length op_sort_vars <> length sort_args)
       then invariant_violation (Printf.sprintf
         "get_subterms zipping different lengths: [%s] / [%s]"
         (String.concat op_sort_vars ~sep:", ")
         (sort_args
           |> List.map ~f:string_of_sort
           |> String.concat ~sep:", ")));

       let sort_arg_map = List.zip_exn op_sort_vars sort_args
         |> String.Map.of_alist_exn
       in

       let arity' = concretize_arity sort_arg_map arity in
       let Arity (_, valences) = arity' in
       (List.map3_exn body_pats body_scopes valences
         ~f:(get_scope_subterms sort_defs)
       |> Util.int_map_unions
     )
     else raise (NoMatch (Printf.sprintf
       "pattern %s and operator %s don't match, in either operator name or \
       subterms"
       (string_of_operator_match_pattern op_match_pat)
       (Binding.Nominal.pp_term' tm)
       ))
  | OperatorPattern _, _
  -> raise (NoMatch (Printf.sprintf
    "operator pattern %s and value %s don't match"
    (string_of_operator_match_pattern op_match_pat)
    (Binding.Nominal.pp_term' tm)
    ))

(** See [get_subterms].

 @raise [NoMatch]
 @raise [UserError]
 @raise [InvariantViolation]
*)
and get_scope_subterms
  :  sort_defs
  -> numbered_scope_pattern
  -> Binding.Nominal.scope
  -> valence
  -> subterm_result Int.Map.t
  = fun
    sort_defs
    (NumberedScopePattern (numbered_patterns, body_pat))
    (Scope (term_patterns, body))
    valence ->

    if List.(length numbered_patterns <> length term_patterns)
    then raise
      (NoMatch "numbered scope pattern and term scope have different arity");

    let pattern_bindings, body_bindings = match valence with
      | VariableValence (bound_sort, body_sort)
      -> let op_match_pat = match numbered_patterns with
           | [PatternCapture cap_num] -> SingleCapturePattern cap_num
           | _ -> failwith "TODO: error"
         in
         let pat = match term_patterns with
           | [pat] -> pat
           | _ -> failwith "TODO: error"
         in
         let pattern_bindings = get_subpatterns sort_defs bound_sort
           op_match_pat pat
         in
         let body_bindings = get_subterms sort_defs body_sort body_pat body in
         pattern_bindings, body_bindings

      | FixedValence (scope_sorts, body_sort) ->
        let pattern_bindings = List.map3_exn numbered_patterns term_patterns scope_sorts
          ~f:(fun binder_capture term_pat binder_sort ->
            let capture = CapturedBinder (binder_sort, term_pat) in
            match binder_capture, term_pat with
            | VarCapture num, Var _
            | VarCapture num, Ignored _
            -> num, capture
            | VarCapture _, _
            -> raise (UserError (Printf.sprintf
              "get_scope_subterms: trying to capture a variable, but found \
               a pattern"
            ))
            (* TODO: should this be allowed here? this implies variable valence, no? *)
            | PatternCapture captured_token_num, _ ->
              captured_token_num, capture
          )
          |> Int.Map.of_alist_exn
        in

        let body_bindings = get_subterms sort_defs body_sort body_pat body in
        pattern_bindings, body_bindings
    in

    Int.Map.merge pattern_bindings body_bindings ~f:(fun ~key -> function
      | `Both _ -> raise
        (UserError (Printf.sprintf "duplicate token capture: $%n" key))
      | `Left v | `Right v -> Some v
    )

(** Check that nonterminal mentioned in tokens appears in the subterm mapping.
 *)
let check_tokens subterms tokens : unit = tokens
  |> index_tokens
  |> Fqueue.iter ~f:(fun (tok_ix, tok) -> match tok with
    | NonterminalName _ ->
      if not (Map.mem subterms tok_ix)
      then (
        let available_keys = subterms
          |> Map.keys
          |> Util.stringify_list string_of_int ", "
        in
        failwith (Printf.sprintf
          "error: key missing in pattern: $%n. available keys: %s"
          tok_ix available_keys)
      )
    | _ -> ()
  )

(** The result of a call to [find_operator_match]. *)
type found_operator_match =
  { match_number: int
  (** The productions in a nonterminal are numbered from 0. This is the number
   of the first production that matched this term *)

  ; pattern: operator_match_pattern
  (** The pattern that matched this term *)

  ; tokens: nonterminal_token list
  (** The tokens associated with the matching operator *)

  ; subterms: subterm_result Int.Map.t
  (** A match against pattern [foo($1. $2; $3)] will produce a map of subterms
   with entries for 1, 2, and 3 *)
  }

(**
 Find a matching syntactical description for the given term. This traverses
 the set of possible forms from low precedence to high (bottom to top) until it
 finds one that matches.

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
  :  sort_defs
  -> sort
  -> nonterminal_rule
  -> Binding.Nominal.term
  -> found_operator_match
  = fun
      sort_defs
      current_sort
      (NonterminalRule { operator_rules; _ })
      tm ->

    operator_rules
      |> List.find_mapi ~f:(fun match_ix (OperatorMatch op_match) ->
        let { operator_match_pattern = pattern; tokens; _ } = op_match in
        try
          Some
            { match_number = match_ix
            ; pattern
            ; tokens
            ; subterms = get_subterms sort_defs current_sort pattern tm
            }
        with
          NoMatch _ -> None)
      |> Util.get_option' (fun () ->
        "failed to find a rule matching term " ^ Binding.Nominal.pp_term' tm)
      |> (fun result ->
        check_tokens result.subterms result.tokens;
        result
      )

let%test_module "find_operator_match" = (module struct
  open Binding.Nominal

  let unit_op_match = OperatorMatch
    { tokens = [ TerminalName "UNIT" ]
    ; operator_match_pattern = OperatorPattern ("unit", [])
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
    }

  let paren_op_match = OperatorMatch
    { tokens = [
        TerminalName "LPAREN";
        NonterminalName "expr";
        TerminalName "RPAREN";
      ]
    ; operator_match_pattern = SingleCapturePattern 2
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
    }

  let expr_operator_rules = [
    lam_op_match;
    unit_op_match;
    num_op_match;
    str_op_match;
    add_op_match;
    match_line_match;
    paren_op_match;
  ]

  let list_operator_rules = [
    OperatorMatch
      { tokens = [
          TerminalName "NIL";
        ]
      ; operator_match_pattern = OperatorPattern ("nil", [])
      };

    OperatorMatch
      { tokens = [
          NonterminalName "nonempty_list"
        ]
      ; operator_match_pattern = SingleCapturePattern 1
      }
  ]

  let nonempty_list_operator_rules = [
    OperatorMatch
      { tokens = [
          NonterminalName "integer";
          TerminalName "::";
          NonterminalName "list";
        ]
      ; operator_match_pattern = OperatorPattern ("cons", [
          NumberedScopePattern([], SingleCapturePattern 1);
          NumberedScopePattern([], SingleCapturePattern 3);
        ])
      }
  ]

  let expr = SortAp ("expr", [||])
  let integer = SortAp ("integer", [||])
  let list = SortAp ("list", [| integer |])
  let nonempty_list = SortAp ("nonempty_list", [| integer |])

  let abstract_description =
    {|
    expr :=
      | lam(expr(). expr())
      | unit()
      | num(integer())
      | str(string())
      | add(expr(); expr())
      | match_line(expr()*. expr())

    list a :=
      | nil()
      | nonempty_list(a)

    nonempty_list b := cons(b; list(b))
    |}
  ;;

  let { Types.sort_defs; _ } = match Parse_abstract.parse abstract_description with
    | Error msg -> failwith msg
    | Ok lang -> lang
  ;;

  let expr_nonterminal_rule = NonterminalRule
    { nonterminal_name = "expr"
    ; result_sort = None
    ; operator_rules = expr_operator_rules
    }

  let list_nonterminal_rule = NonterminalRule
    { nonterminal_name = "list"
    ; result_sort = Some (SortAp ("list", [| integer |]))
    ; operator_rules = list_operator_rules
    }

  let nonempty_list_nonterminal_rule = NonterminalRule
    { nonterminal_name = "nonempty_list"
    ; result_sort = Some (SortAp ("nonempty_list", [| integer |]))
    ; operator_rules = nonempty_list_operator_rules
    }

  let print_subterms = Map.iteri
    ~f:(fun ~key ~data -> Printf.printf "%n -> %s\n"
      key (string_of_subterm_result data)
    )

  let print_match_result = fun sort nonterminal_rule tm ->
    let { match_number; pattern; tokens; subterms } =
      find_operator_match sort_defs sort nonterminal_rule tm
    in
    Printf.printf "%n\n%s\n%s\n"
      match_number
      (string_of_operator_match_pattern pattern)
      (string_of_tokens tokens);
    print_subterms subterms

  let print_get_subterms = fun (OperatorMatch { operator_match_pattern; _ }) tm ->
    try
      print_subterms @@ get_subterms sort_defs expr operator_match_pattern tm
    with
      NoMatch msg -> print_string msg

  let%expect_test "find_operator_match unit()" =
    print_match_result expr expr_nonterminal_rule (Operator ("unit", []));

    [%expect{|
      1
      unit()
      UNIT |}]

  let%expect_test "find_operator_match add(x; y)" =
    print_match_result expr expr_nonterminal_rule (Operator ("add", [
      Scope ([], Var "x");
      Scope ([], Var "y");
    ]));

    [%expect{|
      4
      add($1; $3)
      expr ADD expr
      1 -> CapturedTerm (expr(), x)
      3 -> CapturedTerm (expr(), y) |}]

  let%expect_test "find_operator_match lam(x. x)" =
    print_match_result expr expr_nonterminal_rule (Operator ("lam", [
      Scope ([Pattern.Var "x"], Var "x")
    ]));

    [%expect{|
      0
      lam(var($2). $4)
      FUN NAME ARROW expr
      2 -> CapturedBinder (expr(), x)
      4 -> CapturedTerm (expr(), x) |}]

  let%expect_test "find_operator_match lam(_. x)" =
    print_match_result expr expr_nonterminal_rule (Operator ("lam", [
      Scope ([Ignored ""], Var "x")
    ]));

    [%expect{|
      0
      lam(var($2). $4)
      FUN NAME ARROW expr
      2 -> CapturedBinder (expr(), _)
      4 -> CapturedTerm (expr(), x) |}]

  let%expect_test "find_operator_match lam(_x. x)" =
    print_match_result expr expr_nonterminal_rule (Operator ("lam", [
      Scope ([Ignored "x"], Var "x")
    ]));

    [%expect{|
      0
      lam(var($2). $4)
      FUN NAME ARROW expr
      2 -> CapturedBinder (expr(), _x)
      4 -> CapturedTerm (expr(), x) |}]

  let%expect_test "find_operator_match match_line(add(x; y). add(x; y))" =
    print_match_result expr expr_nonterminal_rule (Operator ("match_line", [
      Scope
        ( [Pattern.Operator ("add", [Var "x"; Var "y"])]
        , Operator ("add", [Scope ([], Var "x"); Scope ([], Var "y")])
        )
    ]));

    [%expect{|
      5
      match_line($1. $3)
      expr ARROW expr
      1 -> CapturedBinder (expr(), add(x; y))
      3 -> CapturedTerm (expr(), add(x; y)) |}]

  let num_tm = (Operator ("num", [
      Scope ([], Primitive (PrimInteger (Bigint.of_int 5)))
    ]))

  let%expect_test "find_operator_match num(5)" =
    print_match_result expr expr_nonterminal_rule num_tm;

    [%expect{|
      2
      num(integer($1))
      NUM
      1 -> CapturedTerm (integer(), 5) |}]

  let%expect_test "get_subterms num(5)" =
    print_get_subterms lam_op_match num_tm;
    [%expect{| pattern lam(var($2). $4) and operator num(5) don't match, in either operator name or subterms |}]

  let%expect_test "get_subterms num(5)" =
    print_get_subterms num_op_match num_tm;
    [%expect{| 1 -> CapturedTerm (integer(), 5) |}]

  let%expect_test "get_subterms num(5)" =
    print_get_subterms paren_op_match num_tm;
    [%expect{| 2 -> CapturedTerm (expr(), num(5)) |}]

  let%expect_test {|find_operator_match str("foo")|} =
    print_match_result expr expr_nonterminal_rule (Operator ("str", [
      Scope ([], Primitive (PrimString "foo"))
    ]));

    [%expect{|
      3
      str(string($1))
      STR
      1 -> CapturedTerm (string(), "foo") |}]

  let nil = (Operator ("nil", []))

  let%expect_test {|find_operator_match nil()|} =
    print_match_result list list_nonterminal_rule nil;
    [%expect{|
      0
      nil()
      NIL |}]

  let%expect_test {|find_operator_match cons(integer(5); nil())|} =
    print_match_result nonempty_list nonempty_list_nonterminal_rule
      (Operator ("cons", [
        Scope ([], Primitive (PrimInteger (Bigint.of_int 5)));
        Scope ([], nil);
      ]));
    [%expect{|
      0
      cons($1; $3)
      integer :: list
      1 -> CapturedTerm (integer(), 5)
      3 -> CapturedTerm (list(integer()), nil()) |}]

end)
