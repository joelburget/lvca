type capture_number = int
type terminal_id = string
type pre_terminal_rule = PreTerminalRule of terminal_id * (string, string) Either.t
type terminal_rule = TerminalRule of terminal_id * Regex.t
type terminal_rules = (string * Regex.t) array

type box_type =
  | HBox
  | VBox
  | HovBox
  | BBox
  | HvBox

type nonterminal_token =
  | TerminalName of string
  | NonterminalName of string
  | Underscore of int
  | OpenBox of (box_type * int list) option
  | CloseBox

(** An operator match pattern appears in the right-hand-side of a concrete
    syntax declaration, to show how to parse and pretty-print operators. They
    either match an operator, eg `{ add($1; $3) }` (for tokens `expr PLUS
    expr`) or are "single capture", eg `{ $2 }` (for tokens `LPAREN expr
    RPAREN`).
*)
type operator_match_pattern =
  | OperatorPattern of string * numbered_scope_pattern list
  | SingleCapturePattern of capture_number

(** A term pattern with numbered holes for binder names and subterms, eg
    `$2. $4` (for tokens `FUN name ARR expr`) *)
and numbered_scope_pattern =
  | NumberedScopePattern of capture_number list * operator_match_pattern

let rec string_of_operator_match_pattern : operator_match_pattern -> string
  = function
  | OperatorPattern (name, scope_pats) ->
    Printf.sprintf
      "%s(%s)"
      name
      (Util.stringify_list string_of_numbered_scope_pattern "; " scope_pats)
  | SingleCapturePattern num -> "$" ^ string_of_int num

and string_of_numbered_scope_pattern : numbered_scope_pattern -> string =
  fun (NumberedScopePattern (binders, body)) ->
  Belt.List.toArray binders
  |. Belt.Array.map (fun n -> "$" ^ string_of_int n)
  |. Array.append [| string_of_operator_match_pattern body |]
  |. Js.Array2.joinWith ". "
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

let string_of_token : nonterminal_token -> string = function
  | TerminalName str -> str
  | NonterminalName str -> str
  | Underscore i -> "_" ^ (if i = 1 then "" else string_of_int i)
  | OpenBox None -> "["
  | OpenBox (Some (box_type, params)) ->
      let box_type_str = match box_type with
        | HBox -> "h"
        | VBox -> "v"
        | HovBox -> "hov"
        | BBox -> "b"
        | HvBox -> "hv"
      in
      let params_str = Util.stringify_list string_of_int "," params in
      Printf.sprintf "[<%s %s>" box_type_str params_str
  | CloseBox -> "]"

let string_of_tokens : nonterminal_token list -> string =
  Util.stringify_list string_of_token " "
;;

type variable_rule =
  { tokens : nonterminal_token list
  ; var_capture : capture_number
  }

(** A nonterminal has 0 or more input sorts, mapping to a result sort. *)
type nonterminal_type = NonterminalType of Types.sort list * Types.sort

type nonterminal_rule' =
  { nonterminal_name : string
  ; nonterminal_type : nonterminal_type
  ; operator_rules : operator_match list list
  }

(** A nonterminal rule shows how to parse / pretty-print a nonterminal *)
type nonterminal_rule = NonterminalRule of nonterminal_rule'

(** Mapping from nonterminal names to nonterminal rules *)
type nonterminal_rules = nonterminal_rule Belt.Map.String.t

(** A description of the concrete syntax for a language *)
type t =
  { terminal_rules : terminal_rules
  ; nonterminal_rules : nonterminal_rules
  }

type pre_t = pre_terminal_rule list * nonterminal_rule list
