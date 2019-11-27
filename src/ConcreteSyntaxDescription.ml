type capture_number = int
type terminal_id    = string

type pre_terminal_rule =
  PreTerminalRule of terminal_id * (string, string) Either.t
type terminal_rule = TerminalRule of terminal_id * Regex.t

type terminal_rules = (string * Regex.t) array

type nonterminal_token =
  | TerminalName    of string
  | NonterminalName of string
  | Underscore      of int

(** A term pattern with numbered holes for binder names and subterms, eg
  `$2. $4` (for tokens `FUN name ARR expr`) *)
type numbered_scope_pattern =
  NumberedScopePattern of capture_number list * capture_number

(** An operator match pattern appears in the right-hand-side of a concrete
    syntax declaration, to show how to parse and pretty-print operators. They
    either match an operator, eg `{ add($1; $3) }` (for tokens `expr PLUS
    expr`) or are "parenthesizing", eg `{ $2 }` (for tokens `LPAREN expr
    RPAREN`).
 *)
type operator_match_pattern =
  | OperatorPattern       of string * numbered_scope_pattern list
  | ParenthesizingPattern of capture_number

type fixity =
  | Infixl
  | Infixr
  | Nofix

let fixity_str = function
  | Infixl -> "left"
  | Infixr -> "right"
  | Nofix  -> "nonassoc"

type operator_match' =
  { tokens                 : nonterminal_token list;
    operator_match_pattern : operator_match_pattern;
    fixity                 : fixity
  }
type operator_match = OperatorMatch of operator_match'

type variable_rule =
  { tokens      : nonterminal_token list;
    var_capture : capture_number;
  }

exception DuplicateVarRules

(* Extract a variable rule, if present. Currently we only recognize it on its
 * own precedence level, which seems like what you usually want, but still
 * arbitrary.
 *
 * By "variable rule", we mean a rule that matches exactly `var($n)`.
 *)
let partition_nonterminal_matches
  (matches: operator_match list list)
  : operator_match list list * variable_rule option
  = Util.fold_right
    (fun (match_, (matches, v_rule)) -> match match_ with
      | [ OperatorMatch
          { tokens;
            operator_match_pattern = OperatorPattern
              ("var", [NumberedScopePattern ([], var_capture)]);
          }
        ]
      -> (match v_rule with
        | Some _ -> raise DuplicateVarRules
        | None   -> matches, Some { tokens; var_capture }
      )
      | _
      -> match_ :: matches, v_rule
    )
    matches ([], None)

type sort_rule' =
  { sort_name      : string;
    operator_rules : operator_match list list;
    variable       : variable_rule option;
  }

(** A sort rule shows how to parse / pretty-print a sort *)
type sort_rule = SortRule of sort_rule'

(** Mapping from sort names to sort rules *)
type sort_rules = sort_rule Belt.Map.String.t

(** A description of the concrete syntax for a language *)
type t = {
  terminal_rules : terminal_rules;
  sort_rules     : sort_rules;
}

type pre_t = pre_terminal_rule list * sort_rule list
