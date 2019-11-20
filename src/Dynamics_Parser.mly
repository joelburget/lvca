/* This is basically an extended pattern / term parser:
  * Here we parser multiple lines of rules, where each side is a
    "[[ pattern ]] = term" pair.
  * Terms can also have embedded oxford brackets, "[[ var ]]".
*/

%{ open Core %}

%token <Bigint.t> INT
%token <string>   STRING
%token <string>   ID
%token LEFT_OXFORD
%token RIGHT_OXFORD
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token DOT
%token EQ
%token EOF

%start dynamics
%type <Core.denotation_pat>                        denotation_pat
%type <Core.denotation_pat_scope>                  denotation_pat_scope
%type <Core.denotation_term>                       denotation_term
(* %type <Core.denotation_scope>                      denotation_scope *)
%type <Core.denotation_pat * Core.denotation_term> dynamics_rule
%type <Core.pre_denotation_chart>                  dynamics
%%

denotation_pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, denotation_pat_scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID
  { Var $1 }

denotation_pat_scope:
  | separated_llist(DOT, ID) DOT denotation_pat
  { Scope ($1, $3) }
  | denotation_pat
  { Scope ([], $1) }

denotation_term_pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, denotation_term_pat) RIGHT_PAREN
  { Pattern.Operator ($1, $3) }
  | ID
  { Var $1 }
  | LEFT_BRACKET separated_list(COMMA, denotation_term_pat) COMMA? RIGHT_BRACKET
  { Sequence $2 }
  | prim
  { Primitive $1 }

denotation_term:
  /*
  | ID LEFT_PAREN separated_list(SEMICOLON, denotation_scope) RIGHT_PAREN
  { Operator ($1, $3) }
  */
  | LEFT_BRACKET separated_list(COMMA, denotation_term) COMMA? RIGHT_BRACKET
  { Sequence $2 }
  | prim
  { Primitive $1 }
  | ID
  { Var $1 }
  | LEFT_OXFORD ID RIGHT_OXFORD
  { Meaning $2 }

  /*
denotation_scope:
  | separated_llist(DOT, ID) DOT denotation_scope
  { Scope ($1 |. Belt.List.map (fun x -> PatVar x), $3) }
  | denotation_scope
  { Scope ([], $1) }
  */

prim:
  | INT    { PrimInteger $1 }
  | STRING { PrimString  $1 }

dynamics_rule:
  LEFT_OXFORD denotation_pat RIGHT_OXFORD EQ denotation_term
  { ($2, $5) }

dynamics: list(dynamics_rule) EOF { DenotationChart $1 }

/*
sort:
  | ID nonempty_list(atomic_sort)
  { Types.SortAp ($1, Belt.List.toArray $2) }
  | atomic_sort
  { $1 }

atomic_sort:
  | LEFT_PAREN sort RIGHT_PAREN
  { $2 }
  | ID
  { Types.SortAp ($1, [||]) }

typed_arg: LEFT_PAREN pattern COLON sort RIGHT_PAREN { ($2, $4) }

branch: pattern ARR raw_core { CoreScope ([$1], $3) }

*/
