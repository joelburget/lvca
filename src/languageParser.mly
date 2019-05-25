%token <string> ID
%token EOL
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

%start languageDef
%type <Types.language>    languageDef
%type <Types.sort>        sort
// %type <Types.sortDef>     string * sortDef
%type <Types.operatorDef> operatorDef
%type <Types.arity>       arity
%type <Types.valence>     valence
%%

valence:
  | ID LEFT_BRACK sort RIGHT_BRACK { VariableValence ($1, $3) } (* sort instead of ID? *)
  | fixedValence                   { $1                       } ;

fixedValence:
  | sort DOT fixedValence
  { match $3 with | Types.FixedValence (binds, result) -> Types.FixedValence ($1 :: binds, result) }
  | sort
  { FixedValence ([], $1) } ;

sort:
  | LEFT_PAREN sort RIGHT_PAREN { $2             }
  | ID                          { SortName $1    }
  | sort sort                   { Types.SortAp($1, $2) } ;

arity:
  | LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
    { Arity ($2, $5) }
  | LEFT_PAREN valenceList RIGHT_PAREN
    { Arity ([], $2) }

nameList: separated_list(COMMA, ID) { $1 } ;

valenceList: separated_list(DOT, valence) { $1 } ;

operatorDef:
  | ID LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
  { OperatorDef($1, Arity($3, $6)) }
  | ID                                 LEFT_PAREN valenceList RIGHT_PAREN
  { OperatorDef($1, Arity([], $3)) } ;

sortDef:
  | ID ASSIGN operatorMultilineBodyDef  { ($1, $3) }
  | ID ASSIGN operatorSingleLineBodyDef { ($1, $3) } ;

operatorMultilineBodyDef:
  | EOL BAR operatorDef operatorMultilineBodyDef { $3 :: $4 }
  | EOL BAR operatorDef                          { [ $3 ] };

operatorSingleLineBodyDef:
  | separated_list(BAR, operatorDef) { $1 } ;

languageDef:
  | list(sortDef) { Language(Belt.Map.String.from_list $1) } ;
