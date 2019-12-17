type capture_number = int
type terminal_id = string
type pre_terminal_rule = PreTerminalRule of terminal_id * (string, string) Either.t
type terminal_rule = TerminalRule of terminal_id * Regex.t
type terminal_rules = (string * Regex.t) array

type nonterminal_token =
  | TerminalName of string
  | NonterminalName of string
  | Underscore of int

(** A term pattern with numbered holes for binder names and subterms, eg
  `$2. $4` (for tokens `FUN name ARR expr`) *)
type numbered_scope_pattern =
  | NumberedScopePattern of capture_number list * capture_number

let string_of_numbered_scope_pattern : numbered_scope_pattern -> string =
 fun (NumberedScopePattern (binders, body)) ->
  Belt.List.toArray binders
  |. Array.append [| body |]
  |. Belt.Array.map (fun n -> "$" ^ string_of_int n)
  |. Js.Array2.joinWith ". "
;;

(** An operator match pattern appears in the right-hand-side of a concrete
    syntax declaration, to show how to parse and pretty-print operators. They
    either match an operator, eg `{ add($1; $3) }` (for tokens `expr PLUS
    expr`) or are "single capture", eg `{ $2 }` (for tokens `LPAREN expr
    RPAREN`).
 *)
type operator_match_pattern =
  | OperatorPattern of string * numbered_scope_pattern list
  | SingleCapturePattern of capture_number

let string_of_operator_match_pattern : operator_match_pattern -> string =
  let to_string = function
    | OperatorPattern (name, scope_pats) ->
      Printf.sprintf
        "%s(%s)"
        name
        (Util.stringify_list string_of_numbered_scope_pattern "; " scope_pats)
    | SingleCapturePattern num -> "$" ^ string_of_int num
  in
  fun pat -> "{ " ^ to_string pat ^ " }"
;;

type fixity =
  | Infixl
  | Infixr
  | Nofix

let fixity_str = function
  | Infixl -> "left"
  | Infixr -> "right"
  | Nofix -> "nonassoc"
;;

type operator_match' =
  { tokens : nonterminal_token list
  ; operator_match_pattern : operator_match_pattern
  ; fixity : fixity
  }

type operator_match = OperatorMatch of operator_match'

let string_of_tokens : nonterminal_token list -> string =
  Util.stringify_list
    (function
      | TerminalName str -> str
      | NonterminalName str -> str
      | Underscore i -> "_" ^ string_of_int i)
    " "
;;

type variable_rule =
  { tokens : nonterminal_token list
  ; var_capture : capture_number
  }

(* Extract a variable rule, if present. Currently we only recognize it on its
 * own precedence level, which seems like what you usually want, but still
 * arbitrary.
 *
 * By "variable rule", we mean a rule that matches exactly `var($n)`.
 *)
let find_first_single_capture (matches : operator_match list list) : variable_rule option
  =
  Util.find_by matches
  @@ function
  | [ OperatorMatch { tokens; operator_match_pattern = SingleCapturePattern var_capture }
    ] ->
    Some { tokens; var_capture }
  | _ -> None
;;

type sort_rule' =
  { sort_name : string
  ; operator_rules : operator_match list list
  }

(** A sort rule shows how to parse / pretty-print a sort *)
type sort_rule = SortRule of sort_rule'

(** Mapping from sort names to sort rules *)
type sort_rules = sort_rule Belt.Map.String.t

(** A description of the concrete syntax for a language *)
type t =
  { terminal_rules : terminal_rules
  ; sort_rules : sort_rules
  }

type pre_t = pre_terminal_rule list * sort_rule list
