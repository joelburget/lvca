%token <string> ID
%token LEFT_PAREN
%token RIGHT_PAREN
%token COLON
%token SEMICOLON
%token DOT
%token EOF
%token CTX
%token CTX_SEPARATOR
%token LEFT_D_ARR
%token RIGHT_D_ARR
%token <string option> LINE
%token COMMA

%{
open Statics
%}

%start rules
%start term_top
%type <Statics.term>           term
%type <Statics.term>           term_top
%type <Statics.scope>          scope
%type <Statics.inference_rule> inference_rule
%type <Statics.checking_rule>  checking_rule
%type <Statics.typing_clause>  typing_clause
%type <Statics.hypothesis>     hypothesis
%type <Statics.rule>           rule
%type <Statics.rule list>      rules
%%

term:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID
  { Free $1 }

scope:
  | separated_llist(DOT, ID) DOT term { Scope ($1, $3) }
  | term                              { Scope ([], $1) }

term_top: term EOF { $1 }

inference_rule: term RIGHT_D_ARR term { {tm = $1; ty = $3} }
checking_rule:  term LEFT_D_ARR  term { {tm = $1; ty = $3} }

typing_clause:
  | inference_rule { InferenceRule $1 }
  | checking_rule  { CheckingRule  $1 }

typed_term: ID COLON term { $1, $3 }

context:
  | CTX
  { M.empty }
  | CTX COMMA separated_nonempty_list(COMMA, typed_term)
  { M.fromArray (Belt.List.toArray $3) }

hypothesis: context CTX_SEPARATOR clause = typing_clause { (M.empty, clause) }

rule:
  | hypotheses = list(hypothesis) LINE conclusion = hypothesis
  { { hypotheses; name = $2; conclusion } }

rules: rules = list(rule) EOF { rules }
