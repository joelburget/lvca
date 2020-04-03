open Core_kernel
open AbstractSyntax

type terminal_id = string
type pre_terminal_rule = PreTerminalRule of terminal_id * (string, string) Either.t
type terminal_rule = TerminalRule of terminal_id * Regex.t
type terminal_rules = (string * Regex.t) list

type box_type =
  | HBox
  | VBox
  | HovBox
  | HvBox

type bindable_token =
  { binding_name : string option
  ; token_name : string
  }

type nonterminal_token =
  | TerminalName of bindable_token
  | NonterminalName of bindable_token
  | Underscore of int
  | OpenBox of (box_type * int list) option
  | CloseBox

let nonterminal_name : string option -> string -> nonterminal_token
  = fun binding_name token_name -> NonterminalName { binding_name; token_name }

let terminal_name : string option -> string -> nonterminal_token
  = fun binding_name token_name -> TerminalName { binding_name; token_name }

(** A binder pattern can match either a variable `var(name)` (fixed valence) or a
 pattern `name` (variable valence) *)
type binder_capture =
  | VarCapture of string
  | PatternCapture of string

(** An operator match pattern appears in the right-hand-side of a concrete syntax
    declaration, to show how to parse and pretty-print operators. They either match an
    operator, eg [ add($1; $3) ] (for tokens [expr PLUS expr]) or are "single capture",
    eg [{ $2 }] (for tokens [LPAREN expr RPAREN]). *)
type operator_match_pattern =
  | OperatorPattern of string * scope_pattern list
  | SingleCapturePattern of string

(** A term pattern with numbered holes in binder patterns and subterms, eg `$2. $4` (for
    tokens `FUN name ARR expr`) *)
and scope_pattern = NamedScopePattern of binder_capture list * operator_match_pattern

let string_of_binder_capture : binder_capture -> string = function
  | VarCapture name -> Printf.sprintf "var(%s)" name
  | PatternCapture name -> Printf.sprintf "%s" name
;;

let rec string_of_operator_match_pattern : operator_match_pattern -> string
  = function
  | OperatorPattern (name, scope_pats) ->
    Printf.sprintf
      "%s(%s)"
      name
      (Util.stringify_list string_of_scope_pattern "; " scope_pats)
  | SingleCapturePattern name -> name

and string_of_scope_pattern : scope_pattern -> string =
  fun (NamedScopePattern (patterns, body)) ->
  patterns
  |> Array.of_list
  |> Array.map ~f:string_of_binder_capture
  |> Fn.flip Array.append [| string_of_operator_match_pattern body |]
  |> String.concat_array ~sep:". "
;;

type operator_match' =
  { tokens : nonterminal_token list
  ; operator_match_pattern : operator_match_pattern
  }

type operator_match = OperatorMatch of operator_match'

let string_of_token : nonterminal_token -> string = function
  | TerminalName bindable_token
  | NonterminalName bindable_token
  -> let { binding_name; token_name } = bindable_token in
     (match binding_name with
       | None -> token_name
       | Some b_name -> b_name ^ " = " ^ token_name)
  | Underscore i -> "_" ^ if i = 1 then "" else string_of_int i
  | OpenBox None -> "["
  | OpenBox (Some (box_type, params)) ->
    let box_type_str =
      match box_type with
      | HBox -> "h"
      | VBox -> "v"
      | HovBox -> "hov"
      | HvBox -> "hv"
    in
    (match params with
      | [] -> Printf.sprintf "[<%s>" box_type_str
      | _ ->
        let params_str = params
          |> List.map ~f:string_of_int
          |> String.concat ~sep:","
        in
        Printf.sprintf "[<%s %s>" box_type_str params_str)
  | CloseBox -> "]"
;;

let string_of_tokens : nonterminal_token list -> string =
  fun tokens -> tokens
    |> List.map ~f:string_of_token
    |> String.concat ~sep:" "
;;

type variable_rule =
  { tokens : nonterminal_token list
  ; var_capture : string
  }

type nonterminal_rule' =
  { nonterminal_name : string
  ; result_sort : sort option
  ; operator_rules : operator_match list
  }

(** A nonterminal rule shows how to parse / pretty-print a nonterminal *)
type nonterminal_rule = NonterminalRule of nonterminal_rule'

let get_result_sort : nonterminal_rule -> sort
  = fun (NonterminalRule { nonterminal_name; result_sort; _ }) ->
    match result_sort with
    | Some sort -> sort
    | None -> SortAp (nonterminal_name, [||])

(** Mapping from nonterminal names to nonterminal rules *)
type nonterminal_rules = nonterminal_rule String.Map.t

(** A description of the concrete syntax for a language *)
type t =
  { terminal_rules : terminal_rules
  ; nonterminal_rules : nonterminal_rules
  }

type pre_t = pre_terminal_rule list * nonterminal_rule list

let string_of_terminal_rules : terminal_rules -> string =
 fun terminal_rules ->
  terminal_rules
  |> List.map ~f:(fun (name, regex) ->
         Printf.sprintf "%s := %s" name (Regex.to_string regex))
  |> String.concat ~sep:"\n"
;;

let string_of_operator_match : operator_match -> string =
 fun (OperatorMatch { tokens; operator_match_pattern }) ->
  Printf.sprintf
    "%s { %s }"
    (string_of_tokens tokens)
    (string_of_operator_match_pattern operator_match_pattern)
;;

let string_of_operator_rules : operator_match list -> string =
 fun rules ->
  rules
    |> List.map ~f:(fun operator_match ->
      Printf.sprintf "  | %s" (string_of_operator_match operator_match))
    |> String.concat ~sep:"\n"
;;

let string_of_arg : string * sort -> string
  = fun (name, sort) -> Printf.sprintf "(%s : %s)" name (string_of_sort sort)

let string_of_nonterminal_rule : nonterminal_rule -> string =
  fun (NonterminalRule { nonterminal_name; result_sort; operator_rules }) ->
  Printf.sprintf
    "%s%s :=\n%s"
    nonterminal_name
    (match result_sort with
      | None -> ""
      | Some sort -> " " ^ string_of_sort sort
    )
    (string_of_operator_rules operator_rules)
;;

let string_of_nonterminal_rules : nonterminal_rules -> string =
 fun nonterminal_rules ->
  nonterminal_rules
  |> String.Map.to_alist
  |> Array.of_list
  |> Array.map ~f:(fun (_, rule) -> string_of_nonterminal_rule rule)
  |> String.concat_array ~sep:"\n\n"
;;

let string_of_t : t -> string =
 fun { terminal_rules; nonterminal_rules } ->
  string_of_terminal_rules terminal_rules
  ^ "\n\n"
  ^ string_of_nonterminal_rules nonterminal_rules
;;

(* conversion to term *)

let term_of_option : ('a -> NonBinding.term) -> 'a option -> NonBinding.term
  = fun f -> function
    | None -> Operator ("none", [])
    | Some a -> Operator ("some", [f a])

let terminal_rules_to_term : terminal_rules -> NonBinding.term
  = fun rules -> Sequence (rules
    |> List.map ~f:(fun (name, re) -> NonBinding.Operator ("pair",
      [ Primitive (PrimString name)
      ; Regex.to_term re
      ]))
  )

let term_of_nonterminal_token : nonterminal_token -> NonBinding.term
  = function
    | TerminalName { binding_name; token_name }
    -> Operator ("terminal_name",
      [ term_of_option (fun binding_name' -> Primitive (PrimString binding_name'))
        binding_name
      ; Primitive (PrimString token_name)
      ])
    | NonterminalName { binding_name; token_name }
    -> Operator ("nonterminal_name",
      [ term_of_option (fun binding_name' -> Primitive (PrimString binding_name'))
        binding_name
      ; Primitive (PrimString token_name)
      ])
    | _ -> failwith "TODO" (* TODO: should these even be included in the core language? *)

let term_of_binder_capture : binder_capture -> NonBinding.term
  = function
    | VarCapture name -> Operator ("var_capture", [Primitive (PrimString name)])
    | PatternCapture name -> Operator ("pattern_capture", [Primitive (PrimString name)])

let rec term_of_operator_match_pattern : operator_match_pattern -> NonBinding.term
  = function
    | OperatorPattern (name, scope_patterns) -> Operator ("operator_pattern",
      [ Primitive (PrimString name)
      ; Sequence (scope_patterns |> List.map ~f:term_of_scope_pattern)
      ])
    | SingleCapturePattern name
    -> Operator ("single_capture_pattern", [ Primitive (PrimString name)])

and term_of_scope_pattern : scope_pattern -> NonBinding.term
  = fun (NamedScopePattern (binder_caps, op_match_pat)) -> Operator
    ( "scope_pattern"
    , [ Sequence (binder_caps |> List.map ~f:term_of_binder_capture)
      ; term_of_operator_match_pattern op_match_pat
      ])

let term_of_operator_match : operator_match -> NonBinding.term
  = fun (OperatorMatch { tokens; operator_match_pattern }) -> Operator ("operator_match",
    [ Sequence (tokens |> List.map ~f:term_of_nonterminal_token)
    ; term_of_operator_match_pattern operator_match_pattern
    ])

let nonterminal_rule_to_term : nonterminal_rule -> NonBinding.term
  = fun (NonterminalRule { nonterminal_name; result_sort; operator_rules }) ->
    Operator ("nonterminal_rule",
      [ Primitive (PrimString nonterminal_name)
      ; term_of_option term_of_sort result_sort
      ; Sequence (operator_rules |> List.map ~f:term_of_operator_match)
      ])

let nonterminal_rules_to_term : nonterminal_rules -> NonBinding.term
  = fun rules -> Sequence (rules
    |> Map.to_alist
    |> List.map ~f:(fun (name, nonterminal_rule) -> NonBinding.Operator ("pair",
      [ Primitive (PrimString name)
      ; nonterminal_rule_to_term nonterminal_rule
      ])))

let to_term : t -> NonBinding.term
  = function { terminal_rules; nonterminal_rules } -> Operator ("concrete_syntax",
    [ terminal_rules_to_term terminal_rules
    ; nonterminal_rules_to_term nonterminal_rules
    ])

(* conversion from term *)

let option_of_term : (NonBinding.term -> 'a) -> NonBinding.term -> 'a option
  = fun f -> function
    | Operator ("none", []) -> None
    | Operator ("some", [tm]) -> Some (f tm)
    | tm -> raise (OfTermFailure ("option_of_term", tm))

(* TODO: is this okay? *)
let regex_of_term : NonBinding.term -> Regex.t
  = function
    | Operator ("regex", [Primitive (PrimString re_str)])
    (* TODO: does this throw? *)
    -> Regex_Parser.regex Regex_Lexer.read (Lexing.from_string re_str)
    | tm -> raise (OfTermFailure ("regex_of_term", tm))

let terminal_rules_of_term : NonBinding.term -> terminal_rules
  = function
    | Sequence rules -> rules
      |> List.map ~f:(function
      | Operator ("pair", [ Primitive (PrimString name); re_term ])
      -> (name, regex_of_term re_term)
      | tm -> raise (OfTermFailure ("term_of_terminal_rules (inner)", tm))
      )
    | tm -> raise (OfTermFailure ("term_of_terminal_rules (outer)", tm))

let nonterminal_token_of_term : NonBinding.term -> nonterminal_token
  = function
  | Operator ("terminal_name",
      [ maybe_binding_name
      ; Primitive (PrimString token_name)
      ])
  -> TerminalName
    { binding_name = option_of_term (function
      | Primitive (PrimString binding_name) -> binding_name
      | tm -> raise (OfTermFailure ("nonterminal_token_of_term (inner)", tm)))
      maybe_binding_name
    ; token_name
    }
  | Operator ("nonterminal_name",
      [ maybe_binding_name
      ; Primitive (PrimString token_name)
      ])
  -> NonterminalName
    { binding_name = option_of_term (function
      | Primitive (PrimString binding_name) -> binding_name
      | tm -> raise (OfTermFailure ("nonterminal_token_of_term (inner)", tm)))
      maybe_binding_name
    ; token_name
    }
  | tm -> raise (OfTermFailure ("nonterminal_token_of_term (outer)", tm))

let binder_capture_of_term : NonBinding.term -> binder_capture
  = function
    | Operator ("var_capture", [Primitive (PrimString name)])
    -> VarCapture name
    | Operator ("pattern_capture", [Primitive (PrimString name)])
    -> PatternCapture name
    | tm -> raise (OfTermFailure ("binder_capture_of_term", tm))

let rec operator_match_pattern_of_term : NonBinding.term -> operator_match_pattern
  = function
    | Operator ("operator_pattern",
      [ Primitive (PrimString name)
      ; Sequence pats
      ])
      -> OperatorPattern (name, pats |> List.map ~f:scope_pattern_of_term)
    | Operator ("single_capture_pattern", [ Primitive (PrimString name) ])
    -> SingleCapturePattern name
    | tm -> raise (OfTermFailure ("operator_match_pattern_of_term", tm))

and scope_pattern_of_term : NonBinding.term -> scope_pattern
  = function
    | Operator
      ( "scope_pattern"
      , [ Sequence binders
        ; op_match_term
        ])
    -> NamedScopePattern
      ( binders |> List.map ~f:binder_capture_of_term, operator_match_pattern_of_term
      op_match_term)
    | tm -> raise (OfTermFailure ("scope_pattern_of_term", tm))

let operator_match_of_term : NonBinding.term -> operator_match
  = function
    | Operator ("operator_match", [ Sequence tms ; tm ])
    -> OperatorMatch
      { tokens = List.map tms ~f:nonterminal_token_of_term
      ; operator_match_pattern = operator_match_pattern_of_term tm
      }
    | tm -> raise (OfTermFailure ("operator_match_of_term", tm))

let nonterminal_rule_of_term : NonBinding.term -> nonterminal_rule
  = function
    Operator ("nonterminal_rule",
      [ Primitive (PrimString nonterminal_name)
      ; sort_tm
      ; Sequence rules_tm
      ])
  -> NonterminalRule
    { nonterminal_name
    ; result_sort = option_of_term sort_of_term sort_tm
    ; operator_rules = rules_tm |> List.map ~f:operator_match_of_term
    }
    | tm -> raise (OfTermFailure ("nonterminal_rule_of_term", tm))

let nonterminal_rules_of_term : NonBinding.term -> nonterminal_rules
  = fun tm ->
    let f = function
      | NonBinding.Operator ("pair", [ Primitive (PrimString name) ; tm ])
      -> (name, nonterminal_rule_of_term tm)
      | tm -> raise (OfTermFailure ("nonterminal_rules_of_term (inner)", tm))
    in
    match tm with
    | Sequence rules
    -> (match rules |> List.map ~f |> String.Map.of_alist with
      | `Duplicate_key name -> raise
          (OfTermFailure ("nonterminal_rules_of_term (duplicate key: " ^ name ^ ")", tm))
      | `Ok result -> result)
    | _ -> raise (OfTermFailure ("nonterminal_rules_of_term (outer)", tm))

let of_term : NonBinding.term -> t
  = function
    | Operator ("concrete_syntax", [ terminal_tms; nonterminal_tms ])
    ->
      { terminal_rules = terminal_rules_of_term terminal_tms
      ; nonterminal_rules = nonterminal_rules_of_term nonterminal_tms
      }
    | tm -> raise (OfTermFailure ("of_term", tm))
