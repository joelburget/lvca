// dynamics

%{
  open Core
%}

%token <Bigint.t> INT
%token <string>   ID
%token <string>   STRING
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token DOT
%token UNDERSCORE
%token EQ
%token LEFT_OXFORD
%token RIGHT_OXFORD
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token RIGHT_S_ARR
%token EOF
%token APP
%token LAM
%token CASE
%token DEFAULT
%token HASH
%token CORE

%start dynamics
%type <denotation_pat> pat
%type <core> core
%type <denotation_pat * core> dynamics_rule
%type <Core.denotation_chart> dynamics
%%

pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { DPatternTm ($1, $3) }
  | APP LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { DPatternTm ("app", $3) }
  (* XXX this is a hack *)
  | UNDERSCORE
  { DVar None }
  | ID
  { DVar (Some $1) };

scope_pat:
  | separated_list(DOT, ID) DOT pat
  { DenotationScopePat ($1, $3) }
  | pat
  { DenotationScopePat ([], $1) } ;

core:
  | APP LEFT_PAREN core SEMICOLON separated_list(SEMICOLON, core) RIGHT_PAREN
  { CoreApp ($3, $5) }
  | ID LEFT_PAREN separated_nonempty_list(SEMICOLON, core) RIGHT_PAREN
  { Operator ($1, $3) }
  | prim
  { Primitive $1 }
  | LAM LEFT_PAREN separated_nonempty_list(DOT, ID) DOT core RIGHT_PAREN
  { Lambda ($3, $5) }
  | LAM LEFT_PAREN                                      core RIGHT_PAREN
  { Lambda ([], $3) }
  (* | core_val *)
  (* { CoreVal $1 } *)
  | ID
  { Var $1 }
  | CASE LEFT_PAREN
    arg = core SEMICOLON
    CORE sort = sort SEMICOLON
    cases = separated_list(SEMICOLON, case)
    RIGHT_PAREN
  { Case (arg, sort, cases) }
  | LEFT_OXFORD ID RIGHT_OXFORD
  { Meaning $2 }
  (* | HASH ID
  { ValPrimop $2 } *)
  (* Lambda? *)
  ;

(* TODO: duplicated from LanguageParser *)
sort:
  | LEFT_PAREN sort RIGHT_PAREN { $2                                }
  | ID list(sort)               { SortAp ($1, Belt.List.toArray $2) }
  ;

case: core_pat RIGHT_S_ARR core { ($1, $3) } ;

core_pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, core_pat) RIGHT_PAREN
  { PatternTerm ($1, $3) }
  | UNDERSCORE
  { PatternVar None }
  | ID
  { PatternVar (Some $1) }
  | LEFT_BRACKET separated_list(COMMA, core_pat) RIGHT_BRACKET
  { PatternSequence $2 }
  | prim
  { PatternPrim $1 }
  | DEFAULT
  { PatternDefault }
  ;

prim:
  | INT    { PrimInteger $1     }
  | STRING { PrimString  $1     }

dynamics_rule: LEFT_OXFORD pat RIGHT_OXFORD EQ core { ($2, $5) };

dynamics: list(dynamics_rule) EOF { DenotationChart $1 };
