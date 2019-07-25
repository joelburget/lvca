%token <string> ID
%token ASSIGN
%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token COMMA
%token BAR
%token EOF

%start language_def
%type <Types.language>         language_def
%type <Types.sort>             sort
%type <string * Types.sortDef> sort_def
%type <Types.operatorDef>      operator_def
%type <Types.arity>            arity
%type <Types.valence>          valence
%%

/* TODO: duplicated */
sort:
  ID LEFT_PAREN separated_list(SEMICOLON, sort) RIGHT_PAREN
  { Types.SortAp ($1, Belt.List.toArray $3) }

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

/* TODO: generalize to sorts taking args */
sort_def:
  ID ASSIGN BAR? separated_nonempty_list(BAR, operator_def)
  { ($1, SortDef ([], $4)) }

language_def:
  | nonempty_list(sort_def) EOF
  { Language(Belt.Map.String.fromArray (Belt.List.toArray $1)) }
