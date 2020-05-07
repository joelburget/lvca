/* This is a parser for core terms with embedded ASTs */

%{
open Dynamics_Core
module Array = Core_kernel.Array
module List = Core_kernel.List

exception InvalidSort

(** @raise InvalidSort *)
let rec ast_to_sort' : NonBinding.term -> AbstractSyntax.sort
  = function
      | Operator (name, subtms)
      -> SortAp (name, subtms
        |> List.map ~f:ast_to_sort'
        |> Array.of_list
      )
      | Sequence _ | Primitive _
      -> raise InvalidSort

(** @raise ScopeEncountered, InvalidSort *)
let ast_to_sort : Binding.Nominal.term -> AbstractSyntax.sort
  = fun term -> term |> NonBinding.from_nominal' |> ast_to_sort'

let rec ast_to_core : Binding.Nominal.term -> core
  = function
  | Var v -> Var v
  | Operator (name, subtms)
  -> Operator (name, List.map subtms ~f:ast_to_core_scope)
  | Sequence subtms -> Sequence (List.map subtms ~f:ast_to_core)
  | Primitive p -> Primitive p

and ast_to_core_scope : Binding.Nominal.scope -> core_scope
  = fun (Scope (binders, body)) -> Scope (binders, ast_to_core body)
%}

%token <Bigint.t> INT
%token <string>   STRING
%token <string>   OPERATOR_ID
%token <string>   VAR
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token COMMA
%token DOT
%token END

%token BACKSLASH
%token COLON

%token MATCH
%token WITH
%token LEFT_BRACE
%token RIGHT_BRACE
%token BAR
%token ARROW

%token LET
%token REC
%token EQ
%token IN

%start dynamics
%start core_top
%type <Dynamics_Core.core> core
%type <Dynamics_Core.core> core_top
%type <Dynamics_Core.core> ast_like_core
%type <Primitive.t> primitive
%type <AbstractSyntax.sort> sort
%type <Pattern.t> pattern
%type <Binding.Nominal.term> ast_like
%type <Binding.Nominal.scope> ast_like_scope
%type <string * Dynamics_Core.core> definition
%type <Dynamics_Core.denotation_chart> dynamics
%type <AbstractSyntax.sort * Pattern.t> typed_arg
%%

(** A core term. See [atomic_core].

 @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort
 *)
core:
  nonempty_list(atomic_core)
  { match $1 with
      | [] -> Util.invariant_violation "must be a nonempty list"
      | [x] -> x
      | f :: args -> CoreApp (f, args)
  }
  | BACKSLASH nonempty_list(typed_arg) ARROW core
  {
    let sorts, args = List.unzip $2 in
    Lambda (sorts, Scope (args, $4))
  }
  | LET REC? var_name = VAR EQ lhs = core IN body = core
  { let is_rec = match $2 with
      | None -> NoRec
      | Some () -> Rec
    in
    Let (is_rec, lhs, Scope([Var var_name], body))
  }

(** A core term with an unambiguous beginning and end.

 Because the arguments in a function application are separated by spaces, some
 terms need to be surrounded by parens to be parseable. atomic terms are those
 that don't need to be surrounded by parens.

 @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort
 *)
atomic_core:
  | ast_like_core
  { $1 }
  | MATCH core WITH LEFT_BRACE option(BAR) separated_nonempty_list(BAR, case_line) RIGHT_BRACE
  { Case ($2, $6) }
  | LEFT_PAREN core RIGHT_PAREN
  { $2 }

(** @raise ScopeEncountered, InvalidSort *)
typed_arg: LEFT_PAREN VAR COLON sort RIGHT_PAREN { ($4, Var $2) }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
case_line: pattern ARROW core { CaseScope ($1, $3) }

(** @raise ScopeEncountered, InvalidSort *)
sort:          ast_like { ast_to_sort $1 }
ast_like_core: ast_like { ast_to_core $1 }
(** @raise ToPatternScopeEncountered *)
pattern:       ast_like { Binding.Nominal.to_pattern_exn $1 }

(** @raise ToPatternScopeEncountered *)
ast_like:
  | name = OPERATOR_ID scopes = separated_list(SEMICOLON, ast_like_scope) RIGHT_PAREN
  { Operator (name, scopes) }
  | VAR
  { Var $1 }
  | LEFT_BRACKET separated_list(COMMA, ast_like) RIGHT_BRACKET
  { Sequence $2 }
  | primitive
  { Primitive $1 }

(** @raise ToPatternScopeEncountered *)
ast_like_scope:
  | separated_nonempty_list(DOT, ast_like)
  { let binders_tm, body = Util.unsnoc $1 in
    let binders_pat = binders_tm
      |> Core_kernel.List.map ~f:Binding.Nominal.to_pattern_exn
    in
    Scope (binders_pat, body)
  }

primitive:
  | INT    { PrimInteger $1 }
  | STRING { PrimString  $1 }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
definition: VAR EQ core SEMICOLON { ($1, $3) }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
dynamics: nonempty_list(definition) END { DenotationChart $1 }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
core_top: core END { $1 }
