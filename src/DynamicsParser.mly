// dynamics

%{
open Core

let rec vars_of_pattern = function
  | PatternVar (Some v)
  -> [v]
  | PatternTerm (_, children)
  -> Belt.List.(flatten (map children vars_of_binding_pattern))
  | PatternSequence children
  -> Belt.List.(flatten (map children vars_of_pattern))
  | _
  -> []

and vars_of_binding_pattern (CoreBindingPat (binders, body)) =
  binders @ vars_of_pattern body

let fix_up_core : core -> core =
  let module Set = Belt.Set.String in
  let rec go (vars : Set.t) tm = match tm with
    (* This is the crux *)
    | Var s -> if Set.has vars s then tm else Metavar s

    | Operator (name, scopes) -> Operator (name, List.map (go_scope vars) scopes)
    | Sequence tms -> Sequence (List.map (go vars) tms)
    | Lambda scope -> Lambda (go_scope vars scope)
    | CoreApp (func, args) -> CoreApp (go vars func, List.map (go vars) args)
    | Case (scrutinee, ty, branches) -> Case
      ( go vars scrutinee
      , ty
      , List.map (fun (pat, scope) -> (pat, go_scope vars scope)) branches
      )
    | Primitive _ | Metavar _ | Meaning _ -> tm

  and go_scope vars (CoreScope (binders, body)) = CoreScope
        ( binders
        , go (Set.union vars (Set.fromArray (Belt.List.toArray binders))) body
        )
  in go Set.empty
%}

%token <Bigint.t> INT
%token <string>   ID
%token <string>   STRING
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token COLON
%token DOT
%token UNDERSCORE
%token EQ
%token LEFT_OXFORD
%token RIGHT_OXFORD
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token RIGHT_S_ARR
%token EOF
%token APP
%token LAM
%token CASE
%token DEFAULT
%token HASH

%start dynamics
%type <denotation_pat> pat
%type <core> core
%type <denotation_pat * core> dynamics_rule
%type <Core.denotation_chart> dynamics
%%

pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { DPatternTm ($1, $3) }
  | APP LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { DPatternTm ("app", $3) }
  (* XXX this is a hack *)
  | UNDERSCORE
  { DVar None }
  | ID
  { DVar (Some $1) };

scope_pat:
  | separated_list(DOT, ID) DOT pat
  { DenotationScopePat ($1, $3) }
  | pat
  { DenotationScopePat ([], $1) } ;

core_scope:
  | separated_list(DOT, ID) DOT raw_core
  { CoreScope ($1, $3) }
  | raw_core
  { CoreScope ([], $1) }
  ;

(* We parse a raw core term, which has both metavars and vars represented as
 vars, then fix it up, by changing all vars that weren't bound to metavars *)
core: raw_core { fix_up_core $1 } ;

raw_core:
  | APP LEFT_PAREN raw_core SEMICOLON separated_list(SEMICOLON, raw_core) RIGHT_PAREN
  { CoreApp ($3, $5) }
  | ID LEFT_PAREN separated_nonempty_list(SEMICOLON, core_scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | ID LEFT_PAREN RIGHT_PAREN
  { Operator ($1, []) }
  | prim
  { Primitive $1 }
  | LAM LEFT_PAREN separated_nonempty_list(DOT, ID) DOT raw_core RIGHT_PAREN
  { Lambda (CoreScope ($3, $5)) }
  | LAM LEFT_PAREN                                      raw_core RIGHT_PAREN
  { Lambda (CoreScope ([], $3)) }
  (* | core_val *)
  (* { CoreVal $1 } *)
  | ID
  { Var $1 }
  | CASE LEFT_PAREN
    arg = raw_core COLON
    sort = sort SEMICOLON
    cases = separated_list(SEMICOLON, case)
    RIGHT_PAREN
  { Case (arg, sort, cases) }
  | LEFT_OXFORD ID RIGHT_OXFORD
  { Meaning $2 }
  (* | HASH ID
  { ValPrimop $2 } *)
  (* Lambda? *)
  ;

(* TODO: duplicated from LanguageParser *)
sort:
  | LEFT_PAREN sort RIGHT_PAREN { $2                                }
  | ID list(sort)               { SortAp ($1, Belt.List.toArray $2) }
  ;

case: core_pat RIGHT_S_ARR raw_core { ($1, CoreScope (vars_of_pattern $1, $3)) } ;

core_pat:
  | ID LEFT_PAREN separated_list(SEMICOLON, core_binding_pat) RIGHT_PAREN
  { PatternTerm ($1, $3) }
  | UNDERSCORE
  { PatternVar None }
  | ID
  { PatternVar (Some $1) }
  | LEFT_BRACKET separated_list(COMMA, core_pat) RIGHT_BRACKET
  { PatternSequence $2 }
  | prim
  { PatternPrim $1 }
  | DEFAULT
  { PatternDefault }
  ;

core_binding_pat:
  | separated_list(DOT, ID) DOT core_pat
  { CoreBindingPat ($1, $3) }
  | core_pat
  { CoreBindingPat ([], $1) }
  ;

prim:
  | INT    { PrimInteger $1     }
  | STRING { PrimString  $1     }

dynamics_rule: LEFT_OXFORD pat RIGHT_OXFORD EQ core { ($2, $5) };

dynamics: list(dynamics_rule) EOF { DenotationChart $1 };
