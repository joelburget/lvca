/* This is a parser for core terms with embedded ASTs */

%{
open Core

let ast_to_sort : Binding.Nominal.term -> Types.sort
  = failwith "TODO"

let ast_to_core_scope : Binding.Nominal.term -> core_scope
  = failwith "TODO"
(*
  | separated_llist(DOT, pattern) DOT core
  { Scope ($1, $3) }
  | core
  { Scope ([], $1) }
  *)

let ast_to_pattern : Binding.Nominal.term -> Pattern.t
  = failwith "TODO"
%}

%token <Bigint.t> INT
%token <string>   STRING
%token <string>   ID
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token DOT
%token EOF

%start dynamics
%type <Core.core> core
%type <Core.core_scope> core_scope
%type <Types.primitive> primitive
%type <Types.sort> sort
%type <Core.core list> dynamics
%type <Pattern.t> pattern
%type <Binding.Nominal.term> ast_like
%type <Binding.Nominal.scope> ast_like_scope
%%

core:
  | ID LEFT_PAREN separated_list(SEMICOLON, core_scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID
  { Var $1 }
  | LEFT_BRACKET separated_list(COMMA, core) COMMA? RIGHT_BRACKET
  { Sequence $2 }
  | primitive { Primitive $1 }
  (* XXX rest: Lambda CoreApp Case Let Metavar Meaning *)

sort:       ast_like { ast_to_sort       $1 }
core_scope: ast_like { ast_to_core_scope $1 }
pattern:    ast_like { ast_to_pattern    $1 }

ast_like:
  | ID LEFT_PAREN separated_list(SEMICOLON, ast_like_scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID { Var $1 }
  | primitive { Primitive $1 }

ast_like_scope:
  | separated_list(DOT, pattern) DOT ast_like
  { Scope ($1, $3) }
  | ast_like
  { Scope ([], $1) }

primitive:
  | INT    { PrimInteger $1 }
  | STRING { PrimString  $1 }

dynamics: list(core) EOF { $1 }

/*
typed_arg: LEFT_PAREN pattern COLON sort RIGHT_PAREN { ($2, $4) }

branch: pattern ARR raw_core { Scope ([$1], $3) }
*/
