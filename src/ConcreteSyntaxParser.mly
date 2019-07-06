%token <int> NAT
%token <string> TERMINAL_ID
%token <string> NONTERMINAL_ID
%token <string> STRING
%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_BRACE
%token RIGHT_BRACE
%token SEMICOLON
%token EOF
%token ASSIGN
%token INFIXL
%token INFIXR
%token INFIX
%token DOLLAR
%token BAR

%{ open Types.ConcreteSyntax %}

%start terminal_rule
%start regex
%start capture_number
%start nonterminal_token
%start operator_match
%start operator_match__test
%start sort_rule
%start sort_rule__test
%type <Types.ConcreteSyntax.terminal_rule> terminal_rule
%type <Types.ConcreteSyntax.regex> regex
%type <Types.ConcreteSyntax.capture_number> capture_number
%type <Types.ConcreteSyntax.nonterminal_token> nonterminal_token
%type <Types.ConcreteSyntax.operator_match> operator_match
%type <Types.ConcreteSyntax.operator_match> operator_match__test
%type <Types.ConcreteSyntax.sort_rule> sort_rule
%type <Types.ConcreteSyntax.sort_rule> sort_rule__test
%type <(string * Types.ConcreteSyntax.term_scope list)> term_pattern
%%

terminal_rule:
  | TERMINAL_ID; ASSIGN; regex
  { TerminalRule ($1, $3) }
  ;

capture_number: DOLLAR; NAT { $2 } ;

sort_rule__test: sort_rule; EOF { $1 }

sort_rule:
  | NONTERMINAL_ID; ASSIGN; BAR?; separated_nonempty_list(BAR, operator_match)
  { let (operator_rules, variable) = partition_nonterminal_matches($4) in
    SortRule { sort_name = $1; operator_rules; variable }
  }
  ;

operator_match:
  | separated_nonempty_list(SEMICOLON, nonterminal_token);
    LEFT_BRACE; term_pattern; RIGHT_BRACE
  { OperatorMatch { tokens = $1; term_pattern = $3 } }
  ;

(* TODO: should this id allow uppercase? *)
term_pattern:
  | NONTERMINAL_ID; LEFT_PAREN; separated_list(SEMICOLON, term_scope_pattern); RIGHT_PAREN
  { ($1, $3) }
  ;

term_scope_pattern:
  | separated_nonempty_list(DOT, capture_number)
  { let (binds, body) = Util.unsnoc $1 in TermScope (binds, body) }
  ;

operator_match__test: | operator_match; EOF { $1 } ;

nonterminal_token:
  | TERMINAL_ID    { TerminalName $1 }
  | NONTERMINAL_ID { NonterminalName $1 }
  ;

regex: STRING { $1 } ;
