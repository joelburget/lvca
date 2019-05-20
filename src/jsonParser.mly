%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF

%start prog
%type <Json.value option> prog
%%

prog:
  | value { Some $1 }
  | EOF   { None   } ;

value:
  | LEFT_BRACE; obj_fields; RIGHT_BRACE  { `Assoc $2   }
  | LEFT_BRACK; list_fields; RIGHT_BRACK { `List $2    }
  | STRING                               { `String $1  }
  | INT                                  { `Int $1     }
  | FLOAT                                { `Float $1   }
  | TRUE                                 { `Bool true  }
  | FALSE                                { `Bool false }
  | NULL                                 { `Null       } ;

obj_fields:
  | obj_field COMMA obj_fields           { $1 :: $3 }
  | obj_field                            { [$1] } ;

  /* separated_list(COMMA, obj_field)     { $1 } ; */

obj_field:
    STRING; COLON; value                 { ($1, $3) } ;

list_fields:
  | value COMMA list_fields              { $1 :: $3 }
  | value                                { [$1] } ;

  /* separated_list(COMMA, value)         { $1 } ; */
