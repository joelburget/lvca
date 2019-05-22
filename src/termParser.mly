%token <Bigint.t> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token DOT
%token TRUE
%token FALSE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token COMMA
%token EOF

%start term
%type <Types.Abt.term> term
%type <Types.Abt.scope list> subterms
%type <Types.Abt.term list> terms
%type <Types.primitive> primitive
%%

term:
  | ID; LEFT_PAREN;           RIGHT_PAREN { Term ($1, []) }
  | ID; LEFT_PAREN; subterms; RIGHT_PAREN { Term ($1, $3) }
  | ID;                                   { Var $1        }
  | LEFT_BRACK;     terms;    RIGHT_BRACK { Sequence $2   }
  | primitive                             { Primitive $1  } ;

subterms:
  | scope SEMICOLON subterms { $1 :: $3 }
  | scope                    { [$1]     } ;

scope:
  | ID; DOT; scope { match $3 with | Scope (scope, tm) -> Scope ($1 :: scope, tm) }
  | term           { Scope ([], $1) }

terms:
  | term SEMICOLON terms { $1 :: $3 }
  | term                 { [$1]     } ;

primitive:
  | INT    { PrimInteger $1    }
  | STRING { PrimString  $1    }
  | TRUE   { PrimBool    true  }
  | FALSE  { PrimBool    false } ;
  /* TODO: Float? */
