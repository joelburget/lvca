// dynamics
%token <Bigint.t> INT
%token <string>   ID
%token <string>   STRING
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token DOT
%token UNDERSCORE
%token EQ
%token LEFT_OXFORD
%token RIGHT_OXFORD
%token RIGHT_S_ARR
%token EOF
%token APP
%token LAM
%token CASE
%token DEFAULT

%start dynamics
%type <Types.Denotation.pat> pat
%type <Types.Core.core> core
%type <Types.Denotation.pat * Types.Core.core> dynamics_rule
%type <Types.Denotation.chart> dynamics
%%

pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { Types.Denotation.DPatternTm ($1, $3) }
  | APP LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { Types.Denotation.DPatternTm ("app", $3) }
  (* XXX this is a hack *)
  | UNDERSCORE
  { Types.Denotation.DVar None }
  | ID
  { Types.Denotation.DVar (Some $1) };

scope_pat:
  | separated_list(DOT, ID) DOT pat
  { Types.Denotation.DenotationScopePat ($1, $3) }
  | pat
  { Types.Denotation.DenotationScopePat ([], $1) } ;

core:
  | APP LEFT_PAREN core                                                    RIGHT_PAREN
  { Types.Core.CoreApp ($3, []) }
  | APP LEFT_PAREN core SEMICOLON separated_nonempty_list(SEMICOLON, core) RIGHT_PAREN
  { Types.Core.CoreApp ($3, $5) }
  | ID
  { Types.Core.CoreVar $1 }
  | LAM LEFT_PAREN separated_nonempty_list(DOT, ID) DOT core RIGHT_PAREN
  { Types.Core.Lam ($3, $5) }
  | LAM LEFT_PAREN                                      core RIGHT_PAREN
  { Types.Core.Lam ([], $3) }
  | CASE LEFT_PAREN arg = core SEMICOLON cases = separated_list(SEMICOLON, case) RIGHT_PAREN
  { Types.Core.Case (arg, Types.Core.Ty, cases) }
  | LEFT_OXFORD ID RIGHT_OXFORD
  { Types.Core.Metavar $2 }
  ;

case: core_pat RIGHT_S_ARR core { ($1, $3) } ;

core_pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, core_pat) RIGHT_PAREN
  { Types.Core.PatternTerm ($1, $3) }
  | UNDERSCORE
  { Types.Core.PatternVar None }
  | ID
  { Types.Core.PatternVar (Some $1) }
  | prim
  { Types.Core.PatternLit $1 }
  | DEFAULT
  { Types.Core.PatternDefault }
  ;

prim:
  | INT    { PrimInteger $1     }
  | STRING { PrimString  $1     }

dynamics_rule: LEFT_OXFORD pat RIGHT_OXFORD EQ core { ($2, $5) };

dynamics: list(dynamics_rule) EOF { DenotationChart $1 };
