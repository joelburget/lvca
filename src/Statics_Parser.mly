/* TODO:
  * implement yields keyword (`pattern yields vars`)
    - or perhaps this should just be `vars(pattern)`?
  * should we add `=` judgement?
*/

%token <string> ID
%token LEFT_PAREN
%token RIGHT_PAREN
%token COLON
%token SEMICOLON
%token DOT
%token EOF
%token CTX
%token CTX_SEPARATOR
%token LEFT_D_ARR
%token RIGHT_D_ARR
%token <string option> LINE
%token COMMA

%{
open Statics

let rec term_to_pattern : Statics.term -> Pattern.t
  = function
    | Operator (name, args)
    -> Operator (name, Belt.List.map args scope_to_pattern)
    | Free var -> Var var
    | _ -> failwith
      "bad parse -- can only match operators and variables in a pattern"

and scope_to_pattern = function
  | Scope ([], body) -> term_to_pattern body
  | _ -> failwith "bad parse -- can't match binders in a pattern"
%}

%start rules
%start term_top
%type <Statics.term>           term
%type <Statics.term>           term_top
%type <Statics.scope>          scope
%type <Statics.inference_rule> inference_rule
%type <Statics.checking_rule>  checking_rule
%type <Statics.typing_clause>  typing_clause
%type <Statics.hypothesis>     hypothesis
%type <Statics.rule>           rule
%type <Statics.rule list>      rules
%%

term:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID
  { Free $1 }

scope:
  separated_nonempty_list(DOT, term)
  { let binders_tm, body = Util.unsnoc $1 in
    let binders_pat = Belt.List.map binders_tm term_to_pattern in
    Scope (binders_pat, body)
  }

term_top: term EOF { $1 }

inference_rule: term RIGHT_D_ARR term { {tm = $1; ty = $3} }
checking_rule:  term LEFT_D_ARR  term { {tm = $1; ty = $3} }

typing_clause:
  | inference_rule { InferenceRule $1 }
  | checking_rule  { CheckingRule  $1 }

typed_term: ID COLON term { $1, $3 }

context:
  | CTX
  { M.empty }
  | CTX COMMA separated_nonempty_list(COMMA, typed_term)
  { M.fromArray (Belt.List.toArray $3) }

hypothesis: context CTX_SEPARATOR clause = typing_clause { (M.empty, clause) }

rule:
  | hypotheses = list(hypothesis) LINE conclusion = hypothesis
  { { hypotheses; name = $2; conclusion } }

rules: rules = list(rule) EOF { rules }