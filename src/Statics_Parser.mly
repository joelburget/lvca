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
open Statics_Types

exception StaticsParseError of string

(** @raise [StaticsParseError] *)
let rec term_to_pattern : Statics_Types.term -> Pattern.t
  = function
    | Operator (name, args)
    -> Operator (name, Base.List.map args ~f:scope_to_pattern)
    | Free var -> Var var
    | tm -> raise (StaticsParseError (Printf.sprintf
      "Can only match operators and variables in a pattern. Found %s."
      (string_of_term tm)))

(** @raise [StaticsParseError] *)
and scope_to_pattern = function
  | Scope ([], body) -> term_to_pattern body
  | scope -> raise (StaticsParseError (Printf.sprintf
    "Can't match binders in a pattern. Found %s." (string_of_scope scope)))
%}

%start rules
%start term_top
%type <Statics_Types.term>           term
%type <Statics_Types.term>           term_top
%type <Statics_Types.scope>          scope
%type <Statics_Types.inference_rule> inference_rule
%type <Statics_Types.checking_rule>  checking_rule
%type <Statics_Types.typing_clause>  typing_clause
%type <Statics_Types.hypothesis>     hypothesis
%type <Statics_Types.rule>           rule
%type <Statics_Types.rule list>      rules
%%

(** @raise [StaticsParseError] *)
(* TODO: sequence, primitive? *)
term:
  | name = ID LEFT_PAREN scopes = separated_list(SEMICOLON, scope) RIGHT_PAREN
  { Operator (name, scopes) }
  | name = ID
  { Free name }

(** @raise [StaticsParseError] *)
scope:
  tms = separated_nonempty_list(DOT, term)
  { let binders_tm, body = Util.List.unsnoc tms in
    let binders_pat = Base.List.map binders_tm ~f:term_to_pattern in
    Scope (binders_pat, body)
  }

(** @raise [StaticsParseError] *)
term_top: tm = term EOF { tm }

(** @raise [StaticsParseError] *)
inference_rule: tm = term RIGHT_D_ARR ty = term { {tm; ty} }
(** @raise [StaticsParseError] *)
checking_rule:  tm = term LEFT_D_ARR  ty = term { {tm; ty} }

(** @raise [StaticsParseError] *)
typing_clause:
  | rule = inference_rule { InferenceRule rule }
  | rule = checking_rule  { CheckingRule  rule }

(** @raise [StaticsParseError] *)
typed_term: name = ID COLON tm = term { name, tm }

(** @raise [StaticsParseError] *)
context:
  | CTX
  { Util.String.Map.empty }
  | CTX COMMA ctx_entries = separated_nonempty_list(COMMA, typed_term)
  { match Util.String.Map.of_alist ctx_entries with
    | `Ok context -> context
    | `Duplicate_key str
    -> raise (StaticsParseError (Printf.sprintf "duplicate name in context: %s" str))
  }

(** @raise [StaticsParseError] *)
hypothesis:
  | context CTX_SEPARATOR clause = typing_clause
  { (Util.String.Map.empty, clause) }

(** @raise [StaticsParseError] *)
rule:
  hypotheses = list(hypothesis) name = LINE conclusion = hypothesis
  { { hypotheses; name; conclusion } }

(** @raise [StaticsParseError] *)
rules: rules = list(rule) EOF { rules }
