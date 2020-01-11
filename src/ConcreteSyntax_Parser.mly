%token <int> NAT
%token <string> REGEX
%token <string> TERMINAL_ID
%token <string> NONTERMINAL_ID
%token <string> STRING
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token LEFT_BRACE
%token RIGHT_BRACE
%token EOF
%token ASSIGN
%token DOLLAR
%token BAR
%token SEMICOLON
%token LEFT_FIXITY
%token RIGHT_FIXITY
%token GREATER
%token UNDERSCORE

%{ open ConcreteSyntaxDescription %}

%start terminal_rule__test
%start capture_number
%start nonterminal_token__test
%start operator_match__test
%start sort_rule__test
%start language
%type <ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule
%type <ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule__test
%type <ConcreteSyntaxDescription.capture_number> capture_number
%type <ConcreteSyntaxDescription.nonterminal_token> nonterminal_token
%type <ConcreteSyntaxDescription.nonterminal_token> nonterminal_token__test
%type <ConcreteSyntaxDescription.operator_match> operator_match
%type <ConcreteSyntaxDescription.operator_match> operator_match__test
%type <ConcreteSyntaxDescription.operator_match list list> operator_match_list
%type <ConcreteSyntaxDescription.sort_rule> sort_rule
%type <ConcreteSyntaxDescription.sort_rule> sort_rule__test
%type <ConcreteSyntaxDescription.operator_match_pattern> operator_match_pattern
%type <ConcreteSyntaxDescription.pre_terminal_rule list * ConcreteSyntaxDescription.sort_rule list> language
%%

language: terminal_rule+ sort_rule+ EOF { ($1, $2) }

terminal_rule:
  | TERMINAL_ID ASSIGN REGEX
  { PreTerminalRule ($1, Left $3) }
  | TERMINAL_ID ASSIGN STRING
  { PreTerminalRule ($1, Right $3) }

terminal_rule__test: terminal_rule EOF { $1 }

capture_number: DOLLAR NAT { $2 }

sort_rule__test: sort_rule EOF { $1 }

sort_rule:
  | NONTERMINAL_ID ASSIGN BAR? operator_match_list
  { SortRule { sort_name = $1; operator_rules = $4 }
  }

(* The list of operator matches making up a nonterminal. Each operator match is
 * separated by '|' to indicate the same precedence level, or '>' to indicate
 * different precedence levels.
 *)
operator_match_list:
  | operator_match
    { [[ $1 ]] }
  | operator_match BAR     operator_match_list
    { match $3 with
      | []      -> [[ $1 ]]
      | x :: xs -> ($1 :: x) :: xs
    }
  | operator_match GREATER operator_match_list
    { [ $1 ] :: $3 }

fixity:
  | LEFT_FIXITY  { Infixl }
  | RIGHT_FIXITY { Infixr }

operator_match:
  | nonempty_list(nonterminal_token)
    LEFT_BRACE operator_match_pattern RIGHT_BRACE option(fixity)
  { let fixity = match $5 with
    | None   -> Nofix
    | Some f -> f
    in OperatorMatch { tokens = $1; operator_match_pattern = $3; fixity } }

(* TODO: should this id allow uppercase? *)
operator_match_pattern:
  | capture_number
  { SingleCapturePattern $1 }
  | NONTERMINAL_ID
    LEFT_PAREN separated_list(SEMICOLON, term_scope_pattern) RIGHT_PAREN
  { OperatorPattern ($1, $3) }

term_scope_pattern:
  | separated_list(DOT, capture_number) DOT operator_match_pattern
  { NumberedScopePattern ($1, $3) }
  | operator_match_pattern
  { NumberedScopePattern ([], $1) }

operator_match__test: operator_match EOF { $1 }

nonterminal_token:
  | TERMINAL_ID     { TerminalName    $1 }
  | NONTERMINAL_ID  { NonterminalName $1 }
  | UNDERSCORE NAT? { Underscore (Belt.Option.getWithDefault $2 1) }

nonterminal_token__test: nonterminal_token EOF { $1 }
