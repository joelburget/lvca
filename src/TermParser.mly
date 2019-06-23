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
%type <Types.Ast.term> term
%type <Types.primitive> primitive
%%

term:
  | ID; LEFT_PAREN; RIGHT_PAREN
  { Term ($1, []) }
  | ID; LEFT_PAREN; separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Term ($1, $3) }
  | ID;
  { Var $1        }
  | LEFT_BRACK; separated_list(SEMICOLON, term) RIGHT_BRACK
  { Sequence $2   }
  | primitive
  { Primitive $1  } ;

scope:
  | ID; DOT; scope { match $3 with | Scope (scope, tm) -> Scope ($1 :: scope, tm) }
  | term           { Scope ([], $1) }

primitive:
  | INT    { PrimInteger $1    }
  | STRING { PrimString  $1    }
  | TRUE   { PrimBool    true  }
  | FALSE  { PrimBool    false } ;
  /* TODO: Float? */
