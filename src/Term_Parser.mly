%token <Bigint.t> INT
%token <string> STRING
%token <string> ID
%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACK
%token RIGHT_BRACK
%token SEMICOLON
%token COMMA
%token EOF

%start top_term
%type <Binding.Nominal.term> top_term
%type <Binding.Nominal.term> term
%type <Primitive.t> primitive
%%

(** @raise ToPatternScopeEncountered *)
top_term: term EOF { $1 }

(** @raise ToPatternScopeEncountered *)
term:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID;
  { Var $1       }
  | LEFT_BRACK separated_list(COMMA, term) COMMA? RIGHT_BRACK
  { Sequence $2  }
  | primitive
  { Primitive $1 }

(** @raise ToPatternScopeEncountered *)
scope:
  separated_nonempty_list(DOT, term)
  { let binders_tm, body = Util.unsnoc $1 in
    let binders_pat = binders_tm
      |> Core_kernel.List.map ~f:Binding.Nominal.to_pattern_exn
    in
    Binding.Nominal.Scope (binders_pat, body)
  }

primitive:
  | INT    { PrimInteger $1 }
  | STRING { PrimString  $1 }
