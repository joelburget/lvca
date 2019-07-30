%token <string> ID
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token DOT
%token EOF
%token CTX
%token CTX_SEPARATOR
%token LEFT_D_ARR
%token RIGHT_D_ARR
%token LINE

%{
open Types.Statics
%}

%start rules
%start term_top
%type <Types.Statics.term>           term
%type <Types.Statics.term>           term_top
%type <Types.Statics.scope>          scope
%type <Types.Statics.inference_rule> inference_rule
%type <Types.Statics.checking_rule>  checking_rule
%type <Types.Statics.typing_clause>  typing_clause
%type <Types.Statics.hypothesis>     hypothesis
%type <Types.Statics.rule>           rule
%type <Types.Statics.rule list>      rules
%%

term:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Term ($1, $3) }
  | ID
  { Free $1 }

scope:
  | separated_llist(DOT, ID) DOT term { Scope ($1, $3) }
  | term                              { Scope ([], $1) }

term_top: term EOF { $1 }

inference_rule: term RIGHT_D_ARR term { InferenceRule ($1, $3) }
checking_rule:  term LEFT_D_ARR  term { CheckingRule  ($1, $3) }

typing_clause:
  | inference_rule { InferenceRule $1 }
  | checking_rule  { CheckingRule  $1 }

hypothesis: CTX CTX_SEPARATOR clause = typing_clause { (M.empty, clause) }

rule:
  hyps = list(hypothesis) LINE conclusion = hypothesis
  { Rule (hyps, None, conclusion) }

rules: rules = list(rule) EOF { rules }
