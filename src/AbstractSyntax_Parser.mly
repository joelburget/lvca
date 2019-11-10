%token <string> ID
%token <string> STRING
%token IMPORT
%token FROM
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
%token EOF

%start language_def
%type <Types.abstract_syntax>  language_def
%type <Types.sort>             sort
%type <string * Types.sortDef> sort_def
%type <Types.operatorDef>      operator_def
%type <Types.arity>            arity
%type <Types.valence>          valence
%%

/* TODO: duplicated */
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

valence:
  | ID LEFT_BRACK sort RIGHT_BRACK
  { VariableValence ($1, $3) }
  | separated_nonempty_list(DOT, sort)
  { let binds, result = Util.unsnoc $1 in Types.FixedValence (binds, result) }

valence_list: separated_list(SEMICOLON, valence) { $1 }

arity:
  | LEFT_BRACK separated_nonempty_list(COMMA, ID) RIGHT_BRACK
    LEFT_PAREN valence_list RIGHT_PAREN
  { Arity ($2, $5) }
  | LEFT_PAREN valence_list RIGHT_PAREN
  { Arity ([], $2) }

operator_def: ID arity { OperatorDef($1, $2) }

sort_def:
  ID list(ID) ASSIGN BAR? separated_nonempty_list(BAR, operator_def)
  { ($1, SortDef ($2, $5)) }

import:
  IMPORT LEFT_BRACE separated_nonempty_list(COMMA, ID) RIGHT_BRACE FROM STRING
  { { Types.imported_symbols = $3; location = $6 } }

language_def:
  | list(import) nonempty_list(sort_def) EOF
  { let language =
      Types.Language(Belt.Map.String.fromArray (Belt.List.toArray $2))
    in
    { imports = $1; language }
  }
