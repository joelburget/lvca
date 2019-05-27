%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token <string> ID
%token DOT
%token EOF
%token CTX
%token CTX_SEPARATOR
%token <string> RULE_NAME
%token LEFT_D_ARR
%token RIGHT_D_ARR
%token LINE

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
  { Types.Statics.Term ($1, $3) }
  | ID
  { Types.Statics.Free $1 } ;

scope:
  | separated_list(DOT, ID) DOT term { Types.Statics.Scope ($1, $3) }
  | term                             { Types.Statics.Scope ([], $1) } ;

term_top: term EOF { $1 } ;

inferenceRule: term RIGHT_D_ARR term { Types.Statics.InferenceRule ($1, $3) } ;
checkingRule:  term LEFT_D_ARR  term { Types.Statics.CheckingRule  ($1, $3) } ;

typingClause:
  | inferenceRule { InferenceRule $1 }
  | checkingRule  { CheckingRule  $1 } ;

hypothesis: CTX CTX_SEPARATOR clause = typingClause
  { (Types.Statics.M.empty, clause) } ;

rule:
  hyps = list(hypothesis) LINE conclusion = hypothesis
  { Types.Statics.Rule (hyps, None, conclusion) } ;

rules: rules = list(rule) EOF { rules }
