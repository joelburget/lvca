%token <string> ID
%token EOL
%token ASSIGN
%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN
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
  | sort DOT fixedValence { match $3 with | FixedValence (binds, result) -> FixedValence ($1 :: binds, result) }
  | sort                  { FixedValence ([], $1) } ;

arity:
  | LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
    { Arity ($2, $5) }
  | LEFT_PAREN valenceList RIGHT_PAREN
    { Arity ([], $2) }

nameList:
  | STRING COMMA nameList { $1 :: $3 }
  | STRING                { [$1]     } ;

valenceList:
  | valence DOT valenceList { $1 :: $3 }
  | valence                 { [$1]     } ;

operatorDef:
  | STRING LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
  { OperatorDef($1, Arity(nameList, valenceList)) }
  | STRING                                 LEFT_PAREN valenceList RIGHT_PAREN
  { OperatorDef($1, Arity([], valenceList)) } ;

languageDef:
  | STRING ASSIGN operatorMultilineBodyDef  { OperatorDef ($1, $4) }
  | STRING ASSIGN operatorSingleLineBodyDef { OperatorDef ($1, $3) } ;

operatorMultilineBodyDef:
  | EOL BAR operatorDef operatorMultilineBodyDef { $3 :: $4 }
  | EOL BAR operatorDef                          { [ $3 ] };

operatorSingleLineBodyDef:
  | operatorDef BAR operatorSingleLineBodyDef { $1 :: $3 }
  | operatorDef                               { [ $1 ] };
