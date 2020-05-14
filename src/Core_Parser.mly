(* This is a parser for core terms with embedded ASTs *)

%{
open Core_Types
module Array = Core_kernel.Array
module List = Core_kernel.List

(** Raised when a sequence or primitive is used in a sort. *)
exception InvalidSort

(** @raise InvalidSort *)
let rec ast_to_sort' : NonBinding.term -> AbstractSyntax.sort
  = function
      | Operator (name, subtms)
      -> SortAp (name, List.map subtms ~f:ast_to_sort')
      | Sequence _ | Primitive _
      -> raise InvalidSort

(** @raise ScopeEncountered, InvalidSort *)
let ast_to_sort : Binding.Nominal.term -> AbstractSyntax.sort
  = fun term -> term |> NonBinding.from_nominal' |> ast_to_sort'

let make_apps : term list -> term
  = function
    | [] -> Util.invariant_violation "make_apps: must be a nonempty list"
    | [x] -> x
    | f :: args -> List.fold_left args
      ~init:f
      ~f:(fun f_app arg -> CoreApp (f_app, arg))
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

%token IMPORT
%token AS
%token FROM

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

%start core_module
%start term_top
%type <Core_Types.term> term
%type <Core_Types.term> term_top
%type <Core_Types.term> ast_like_core
%type <Primitive.t> primitive
%type <AbstractSyntax.sort> sort
%type <Pattern.t> pattern
%type <Binding.Nominal.term> ast_like
%type <Binding.Nominal.scope> ast_like_scope
%type <Core_Types.core_defn> definition
%type <Core_Types.core_module> core_module
%type <string * AbstractSyntax.sort> typed_arg
%%

(** A core term. See [atomic_core].

 @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort
 *)
term:
  nonempty_list(atomic_core)
  { make_apps $1 }
  | BACKSLASH arg = typed_arg ARROW body = term
  {
    let name, sort = arg in
    Lambda (sort, Scope (name, body))
  }
  | LET REC? var_name = VAR EQ lhs = term IN body = term
  { let is_rec = match $2 with
      | None -> NoRec
      | Some () -> Rec
    in
    Let (is_rec, lhs, Scope (var_name, body))
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
  (* TODO: should we allow empty pattern matches? (do we allow empty sorts?) *)
  | MATCH tm = term WITH LEFT_BRACE option(BAR) lines = separated_nonempty_list(BAR, case_line) RIGHT_BRACE
  { Case (tm, lines) }
  | LEFT_PAREN tm = term RIGHT_PAREN
  { tm }

(** @raise ScopeEncountered, InvalidSort *)
typed_arg: LEFT_PAREN var = VAR COLON sort = sort RIGHT_PAREN { (var, sort) }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
case_line: pattern ARROW term { CaseScope ($1, $3) }

(** @raise ScopeEncountered, InvalidSort *)
sort:          ast_like { ast_to_sort $1 }
ast_like_core: ast_like { Term $1 }
(** @raise ToPatternScopeEncountered *)
pattern:       ast_like { Binding.Nominal.to_pattern_exn $1 }

(** @raise ToPatternScopeEncountered *)
ast_like:
  (* Note: OPERATOR_ID consumes left paren *)
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
    let binders_pat = List.map binders_tm ~f:Binding.Nominal.to_pattern_exn in
    Scope (binders_pat, body)
  }

import_symbol:
  | name1 = VAR                { (name1, None) }
  | name1 = VAR AS name2 = VAR { (name1, Some name2) }

import:
  IMPORT LEFT_BRACE separated_nonempty_list(COMMA, import_symbol) RIGHT_BRACE
  FROM STRING
  { { AbstractSyntax_Types.imported_symbols = $3; location = $6 } }

primitive:
  | INT    { PrimInteger $1 }
  | STRING { PrimString  $1 }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
definition: name = VAR COLON ty = sort EQ defn = term SEMICOLON { { name; ty; defn } }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
core_module:
  imports = list(import) defns = nonempty_list(definition) END
  { CoreModule (imports, defns) }

(** @raise ToPatternScopeEncountered, ScopeEncountered, InvalidSort *)
term_top: term END { $1 }
