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

(* raises unnamed exception *)
let rec term_to_pattern : Statics.term -> Pattern.t
  = function
    | Operator (name, args)
    -> Operator (name, Core_kernel.List.map args ~f:scope_to_pattern)
    | Free var -> Var var
    | _ -> failwith
      "bad parse -- can only match operators and variables in a pattern"

(* raises unnamed exception *)
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
  | name = ID LEFT_PAREN scopes = separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Operator (name, scopes) }
  | name = ID
  { Free name }

scope:
  tms = separated_nonempty_list(DOT, term)
  { let binders_tm, body = Util.unsnoc tms in
    let binders_pat = Core_kernel.List.map binders_tm ~f:term_to_pattern in
    Scope (binders_pat, body)
  }

term_top: tm = term EOF { tm }

inference_rule: tm = term RIGHT_D_ARR ty = term { {tm; ty} }
checking_rule:  tm = term LEFT_D_ARR  ty = term { {tm; ty} }

typing_clause:
  | rule = inference_rule { InferenceRule rule }
  | rule = checking_rule  { CheckingRule  rule }

typed_term: name = ID COLON tm = term { name, tm }

context:
  | CTX
  { Core_kernel.String.Map.empty }
  | CTX COMMA ctx_entries = separated_nonempty_list(COMMA, typed_term)
  { match Core_kernel.String.Map.of_alist ctx_entries with
    | `Ok context -> context
    | `Duplicate_key str
    -> failwith (Printf.sprintf "duplicate name in context: %s" str)
  }

hypothesis:
  | context CTX_SEPARATOR clause = typing_clause
  { (Core_kernel.String.Map.empty, clause) }

rule:
  hypotheses = list(hypothesis) name = LINE conclusion = hypothesis
  { { hypotheses; name; conclusion } }

rules: rules = list(rule) EOF { rules }
