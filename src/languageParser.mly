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

%start language
%type <Demo.language> language
%type <Demo.sortDef>  sortDef
%type <Demo.operator> operator
%type <Demo.arity>    arity
%type <Demo.valence>  valence
%%

valence:
  | ID LEFT_BRACK ID RIGHT_BRACK { VariableValence ($1, $3) }
  | fixedValence                 { FixedValence    $1       } ;

fixedValence:
  | sort DOT fixedValence
  { match $3 with | FixedValence (binds, result) -> FixedValence ($1 :: binds, result) }
  | sort
  { FixedValence ([], $1) } ;

sort:
  | LEFT_PAREN sort RIGHT_PAREN { $2             }
  | ID                          { SortName $1    }
  | sort sort                   { SortAp($1, $2) } ;

arity:
  | LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
    { Arity ($2, $5) }
  | LEFT_PAREN valenceList RIGHT_PAREN
    { Arity ([], $2) }

nameList:
  | ID COMMA nameList { $1 :: $3 }
  | ID                { [$1]     } ;

valenceList:
  | valence DOT valenceList { $1 :: $3 }
  | valence                 { [$1]     } ;

operatorDef:
  | ID LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
  { OperatorDef($1, Arity(nameList, valenceList)) }
  | ID                                 LEFT_PAREN valenceList RIGHT_PAREN
  { OperatorDef($1, Arity([], valenceList)) } ;

languageDef:
  | ID ASSIGN operatorMultilineBodyDef  { OperatorDef ($1, $3) }
  | ID ASSIGN operatorSingleLineBodyDef { OperatorDef ($1, $3) } ;

operatorMultilineBodyDef:
  | EOL BAR operatorDef operatorMultilineBodyDef { $3 :: $4 }
  | EOL BAR operatorDef                          { [ $3 ] };

operatorSingleLineBodyDef:
  | operatorDef BAR operatorSingleLineBodyDef { $1 :: $3 }
  | operatorDef                               { [ $1 ] };
