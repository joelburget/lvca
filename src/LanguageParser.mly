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

%start languageDef
%type <Types.language>    languageDef
%type <Types.sort>        sort
%type <string * Types.sortDef>     sortDef
%type <Types.operatorDef> operatorDef
%type <Types.operatorDef list> operatorDefs
%type <Types.arity>       arity
%type <Types.valence>     valence
%%

sort:
  | LEFT_PAREN sort RIGHT_PAREN { $2             }
  | ID                          { SortName $1    }
  | sort sort                   { SortAp($1, $2) } ;

fixedValence:
  | sort DOT fixedValence
  { match $3 with Types.FixedValence (binds, result) -> FixedValence ($1 :: binds, result) }
  | sort
  { FixedValence ([], $1) } ;

valence:
  | ID LEFT_BRACK sort RIGHT_BRACK { VariableValence ($1, $3) } (* sort instead of ID? *)
  | fixedValence                   { $1                       } ;

valenceList: separated_list(SEMICOLON, valence) { $1 } ;

nameList: separated_list(COMMA, ID) { $1 } ;

arity:
  | LEFT_BRACK nameList RIGHT_BRACK LEFT_PAREN valenceList RIGHT_PAREN
  { Arity ($2, $5) }
  | LEFT_PAREN valenceList = valenceList RIGHT_PAREN
  { Arity ([], valenceList) } ;

operatorDef: ID arity { OperatorDef($1, $2) } ;

operatorDefs: BAR? lst = separated_list(BAR, operatorDef) { lst } ;

/* TODO: generalize to sorts taking args */
sortDef: sortName = ID ASSIGN defs = operatorDefs { (sortName, SortDef ([], defs)) } ;

languageDef:
  | list(sortDef) EOF { Language(Belt.Map.String.fromArray (Belt.List.toArray $1)) } ;
