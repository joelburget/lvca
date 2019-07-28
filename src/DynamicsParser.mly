%{
open Core
let (flatten, unzip, toArray) = Belt.List.(flatten, unzip, toArray)
let map = List.map

let rec vars_of_pattern = function
  | PatternVar "_"
  -> []
  | PatternVar v
  -> [v]
  | PatternTerm (_, children)
  -> flatten (map vars_of_binding_pattern children)
  | PatternSequence children
  -> flatten (map vars_of_pattern children)
  | _
  -> []

and vars_of_binding_pattern (CoreBindingPat (binders, body)) =
  binders @ vars_of_pattern body

let fix_up_core : core -> core =
  let module Set = Belt.Set.String in
  let rec go (vars : Set.t) tm = match tm with
    (* This is the crux *)
    | Var s -> if Set.has vars s then tm else Metavar s

    | Operator (name, scopes) -> Operator (name, map (go_scope vars) scopes)
    | Sequence tms -> Sequence (map (go vars) tms)
    | Lambda (tys, scope) -> Lambda (tys, go_scope vars scope)
    | CoreApp (func, args) -> CoreApp (go vars func, map (go vars) args)
    | Case (scrutinee, branches) -> Case
      ( go vars scrutinee
      , map (fun (pat, scope) -> (pat, go_scope vars scope)) branches
      )
    | Primitive _ | Metavar _ | Meaning _ -> tm

  and go_scope vars (CoreScope (binders, body)) = CoreScope
        ( binders
        , go (Set.union vars (Set.fromArray (toArray binders))) body
        )
  in go Set.empty
%}

%token <Bigint.t> INT
%token <string>   STRING
%token <string>   ID
%token LEFT_OXFORD
%token RIGHT_OXFORD
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token COLON
%token DOT
%token EQ
%token ARR
%token BACKSLASH
%token BAR
%token EOF
%token APP
%token CASE
%token OF

%start dynamics
%type <denotation_pat> pat
%type <core> core
%type <denotation_pat * core> dynamics_rule
%type <Core.denotation_chart> dynamics
%%

/* TODO: duplicated */
sort:
  ID LEFT_PAREN separated_list(SEMICOLON, sort) RIGHT_PAREN
  { Types.SortAp ($1, Belt.List.toArray $3) }

pat:
  | pat_id LEFT_PAREN separated_list(SEMICOLON, scope_pat) RIGHT_PAREN
  { DPatternTm ($1, $3) }
  | pat_id
  { DVar $1 }

scope_pat:
  | separated_llist(DOT, pat_id) DOT pat
  { DenotationScopePat ($1, $3) }
  | pat
  { DenotationScopePat ([], $1) }

core_scope:
  | separated_llist(DOT, ID) DOT raw_core
  { CoreScope ($1, $3) }
  | raw_core
  { CoreScope ([], $1) }

core_binding_pat:
  | separated_llist(DOT, ID) DOT core_pat
  { CoreBindingPat ($1, $3) }
  | core_pat
  { CoreBindingPat ([], $1) }

(* We parse a raw core term, which has both metavars and vars represented as
 vars, then fix it up, by changing all vars that weren't bound to metavars *)
core: raw_core { fix_up_core $1 }

raw_core:
  | APP LEFT_PAREN raw_core SEMICOLON separated_list(SEMICOLON, raw_core) RIGHT_PAREN
  { CoreApp ($3, $5) }
  | ID LEFT_PAREN separated_list(SEMICOLON, core_scope) RIGHT_PAREN
  { Operator ($1, $3) }
  | LEFT_BRACKET separated_list(COMMA, raw_core) RIGHT_BRACKET
  { Sequence $2 }
  | prim
  { Primitive $1 }
  | BACKSLASH list(typed_arg) ARR raw_core
  { let names, tys = unzip $2 in Lambda (tys, CoreScope (names, $4)) }
  | ID
  { Var $1 }
  | CASE arg = raw_core OF LEFT_BRACE
    BAR? branches = separated_list(BAR, branch)
    RIGHT_BRACE
  { Case (arg, branches) }
  | LEFT_OXFORD ID RIGHT_OXFORD
  { Meaning $2 }

typed_arg: LEFT_PAREN ID COLON sort RIGHT_PAREN { ($2, $4) }

branch: core_pat ARR raw_core { ($1, CoreScope (vars_of_pattern $1, $3)) }

pat_id:
  | ID   { $1     }
  | APP  { "app"  }
  | CASE { "case" }
  | OF   { "of"   }

core_pat:
  | pat_id LEFT_PAREN separated_list(SEMICOLON, core_binding_pat) RIGHT_PAREN
  { PatternTerm ($1, $3) }
  | pat_id
  { PatternVar $1 }
  | LEFT_BRACKET separated_list(COMMA, core_pat) RIGHT_BRACKET
  { PatternSequence $2 }
  | prim
  { PatternPrim $1 }

prim:
  | INT    { PrimInteger $1 }
  | STRING { PrimString  $1 }

dynamics_rule: LEFT_OXFORD pat RIGHT_OXFORD EQ core { ($2, $5) }

dynamics: list(dynamics_rule) EOF { DenotationChart $1 }
