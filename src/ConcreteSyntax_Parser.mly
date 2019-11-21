%token <int> NAT
%token <string> REGEX
%token <string> TERMINAL_ID
%token <string> NONTERMINAL_ID
%token <string> STRING
%token <string> CHARACTER_SET
%token <string> CHARACTER_CLASS
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token LEFT_BRACE
%token RIGHT_BRACE
%token EOF
%token ASSIGN
%token DOLLAR
%token BAR
%token STAR
%token PLUS
%token QUESTION
%token SEMICOLON
%token LEFT_FIXITY
%token RIGHT_FIXITY
%token GREATER

%{ open Types.ConcreteSyntaxDescription %}

%start terminal_rule__test
%start capture_number
%start nonterminal_token
%start operator_match__test
%start sort_rule__test
%start language
%type <Types.ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule
%type <Types.ConcreteSyntaxDescription.pre_terminal_rule> terminal_rule__test
%type <Types.ConcreteSyntaxDescription.capture_number> capture_number
%type <Types.ConcreteSyntaxDescription.nonterminal_token> nonterminal_token
%type <Types.ConcreteSyntaxDescription.operator_match> operator_match
%type <Types.ConcreteSyntaxDescription.operator_match> operator_match__test
%type <Types.ConcreteSyntaxDescription.operator_match list list> operator_match_list
%type <Types.ConcreteSyntaxDescription.sort_rule> sort_rule
%type <Types.ConcreteSyntaxDescription.sort_rule> sort_rule__test
%type <Types.ConcreteSyntaxDescription.operator_match_pattern> operator_match_pattern
%type <Types.ConcreteSyntaxDescription.t> language
%%

language:
  | terminal_rule+ sort_rule+ EOF
  { Types.ConcreteSyntaxDescription.make $1 $2 }

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
  { let (operator_rules, variable) = partition_nonterminal_matches($4) in
    SortRule { sort_name = $1; operator_rules; variable }
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
  { let fixity = (match $5 with
    | None   -> Nofix
    | Some f -> f
    )
    in OperatorMatch { tokens = $1; operator_match_pattern = $3; fixity } }

(* TODO: should this id allow uppercase? *)
operator_match_pattern:
  | capture_number
  { ParenthesizingPattern $1 }
  | NONTERMINAL_ID
    LEFT_PAREN separated_list(SEMICOLON, term_scope_pattern) RIGHT_PAREN
  { OperatorPattern ($1, $3) }

term_scope_pattern:
  | separated_nonempty_list(DOT, capture_number)
  { let (binds, body) = Util.unsnoc $1 in NumberedScopePattern (binds, body) }

operator_match__test: operator_match EOF { $1 }

nonterminal_token:
  | TERMINAL_ID    { TerminalName    $1 }
  | NONTERMINAL_ID { NonterminalName $1 }

(*
regex: nonempty_list(regex_piece) { $1 }

regex__test: regex EOF { $1 }

regex_piece:
  | STRING               { ReString $1 }
  | CHARACTER_SET        { ReSet    $1 }
  | CHARACTER_CLASS      { ReClass  $1 }
  | regex_piece STAR     { ReStar   $1 }
  | regex_piece PLUS     { RePlus   $1 }
  | regex_piece QUESTION { ReOption $1 }
*)
