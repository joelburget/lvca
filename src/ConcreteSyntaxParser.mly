%token <int> NAT
%token <string> TERMINAL_ID
%token <string> NONTERMINAL_ID
%token <string> STRING
%token DOT
// %token TRUE
// %token FALSE
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
%token ELLIPSIS
%token BAR

%{ open Types.ConcreteSyntax %}

%start terminal_rule
%start regex
%start fixity
%start capture_number
%start nonterminal_token
%start nonterminal_match
%start nonterminal_match__test
%start nonterminal_rule
%start nonterminal_rule__test
%type <Types.ConcreteSyntax.terminal_rule> terminal_rule
%type <Types.ConcreteSyntax.regex> regex
%type <Types.ConcreteSyntax.fixity> fixity
%type <Types.ConcreteSyntax.capture_number> capture_number
%type <Types.ConcreteSyntax.nonterminal_token> nonterminal_token
%type <Types.ConcreteSyntax.nonterminal_match> nonterminal_match
%type <Types.ConcreteSyntax.nonterminal_match> nonterminal_match__test
%type <Types.ConcreteSyntax.nonterminal_rule> nonterminal_rule
%type <Types.ConcreteSyntax.nonterminal_rule> nonterminal_rule__test
%type <(string * Types.ConcreteSyntax.term_scope list)> term_pattern
%%

terminal_rule:
  | TERMINAL_ID; ASSIGN; regex
  { TerminalRule ($1, $3) }
  ;

capture_number: DOLLAR; NAT { $2 } ;

nonterminal_rule__test: nonterminal_rule; EOF { $1 }

nonterminal_rule:
  | NONTERMINAL_ID; ASSIGN; BAR?; separated_nonempty_list(BAR, nonterminal_match)
  { NonterminalRule { sort_name = $1; variants = $4 } }

nonterminal_match:
  | separated_nonempty_list(SEMICOLON, nonterminal_token);
    LEFT_BRACE; term_pattern; RIGHT_BRACE
  { NonterminalMatch { tokens = $1; term_pattern = $3 } }
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

nonterminal_match__test: | nonterminal_match; EOF { $1 } ;

nonterminal_token:
  | TERMINAL_ID    { TerminalName $1 }
  | NONTERMINAL_ID { NonterminalName $1 }
  ;

(* XXX do we want fixity? *)
fixity:
  | INFIXL { Infixl }
  | INFIXR { Infixr }
  | INFIX  { Infix  }
  ;

regex: STRING { $1 } ;
