%token <string> ID
%token <string> STRING
%token IMPORT
%token FROM
%token AS
%token ASSIGN
%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token COMMA
%token BAR
%token STAR
%token EOF

%start language_def
%type <AbstractSyntax_Types.abstract_syntax>   language_def
%type <AbstractSyntax_Types.sort>              sort
%type <string * AbstractSyntax_Types.sort_def> sort_def
%type <AbstractSyntax_Types.operator_def>      operator_def
%type <AbstractSyntax_Types.arity>             arity
%type <AbstractSyntax_Types.valence>           valence
%type <string * string option>                 import_symbol
%%

/* TODO: duplicated in concrete syntax parser */
sort:
  | ID LEFT_PAREN separated_list(SEMICOLON, sort) RIGHT_PAREN
  { AbstractSyntax_Types.SortAp ($1, $3) }
  | ID
  { AbstractSyntax_Types.SortVar $1 }

valence:
  | sort STAR DOT sort
  { VariableValence ($1, $4) }
  | separated_nonempty_list(DOT, sort)
  { let binds, result = Util.unsnoc $1 in
    AbstractSyntax_Types.FixedValence (binds, result)
  }

// TODO: allow trailing semicolon?
valence_list: separated_list(SEMICOLON, valence) { $1 }

arity:
  | LEFT_BRACK separated_list(COMMA, ID) RIGHT_BRACK
    LEFT_PAREN valence_list RIGHT_PAREN
  { Arity ($2, $5) }
  | LEFT_PAREN valence_list RIGHT_PAREN
  { Arity ([], $2) }

operator_def: ID arity { OperatorDef($1, $2) }

sort_def:
  | ID ASSIGN BAR? separated_nonempty_list(BAR, operator_def)
  { ($1, SortDef ([], $4)) }
  | ID LEFT_PAREN separated_list(SEMICOLON, ID) RIGHT_PAREN ASSIGN
    BAR? separated_nonempty_list(BAR, operator_def)
  { ($1, SortDef ($3, $7)) }

import_symbol:
  | name1 = ID               { (name1, None) }
  | name1 = ID AS name2 = ID { (name1, Some name2) }

import:
  IMPORT LEFT_BRACE separated_nonempty_list(COMMA, import_symbol) RIGHT_BRACE
  FROM STRING
  { { AbstractSyntax_Types.imported_symbols = $3; location = $6 } }

language_def:
  | list(import) nonempty_list(sort_def) EOF
  { match Core_kernel.String.Map.of_alist $2 with
    | `Ok sort_def_map ->
      { imports = $1
      ; sort_defs = AbstractSyntax_Types.SortDefs sort_def_map
      }
    | `Duplicate_key _key -> failwith "TODO: raise error"
  }
