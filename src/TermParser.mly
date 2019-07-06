%token <Bigint.t> INT
%token <string> ID
%token <string> STRING
%token DOT
// %token TRUE
// %token FALSE
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token EOF

%start top_term
%type <Binding.Nominal.term> top_term
%type <Binding.Nominal.term> term
%type <Types.primitive> primitive
%%

top_term:
  | term; EOF
  { $1 } ;

term:
  | ID; LEFT_PAREN; separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID;
  { Var $1        }
  | LEFT_BRACK; separated_list(SEMICOLON, term) RIGHT_BRACK
  { Sequence $2   }
  | primitive
  { Primitive $1  } ;

scope:
  | ID; DOT; scope
  { match $3 with | Binding.Nominal.Scope (scope, tm) -> Scope ($1 :: scope, tm) }
  | term
  { Scope ([], $1) } ;

primitive:
  | INT    { PrimInteger $1    }
  | STRING { PrimString  $1    } ;
  /* | TRUE   { PrimBool    true  }
  | FALSE  { PrimBool    false } ;
  TODO: Float? */
