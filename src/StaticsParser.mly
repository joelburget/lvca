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
%type <Types.Statics.term>          term
%type <Types.Statics.term>          term_top
%type <Types.Statics.scope>         scope
%type <Types.Statics.inferenceRule> inferenceRule
%type <Types.Statics.checkingRule>  checkingRule
%type <Types.Statics.typingClause>  typingClause
%type <Types.Statics.hypothesis>    hypothesis
%type <Types.Statics.rule>          rule
%type <Types.Statics.rule list>     rules
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

inferenceRule: term RIGHT_D_ARR term { InferenceRule ($1, $3) }
checkingRule:  term LEFT_D_ARR  term { CheckingRule  ($1, $3) }

typingClause:
  | inferenceRule { InferenceRule $1 }
  | checkingRule  { CheckingRule  $1 }

hypothesis: CTX CTX_SEPARATOR clause = typingClause { (M.empty, clause) }

rule:
  hyps = list(hypothesis) LINE conclusion = hypothesis
  { Rule (hyps, None, conclusion) }

rules: rules = list(rule) EOF { rules }
