open Tablecloth
let stringify_list = Util.stringify_list

(** Represents the LHS of a denotation rule. Why is this not just `Pattern.t`?
    Because patterns can't match binders. For example, we want to be able to match
    this pattern on the LHS of a denotation rule:

    | lam(x. x) -> ...

    This is not allowed by regular patterns.
*)
type pattern =
  | Operator of string * scope list
  | Var of string
  | Sequence of pattern list
  | Primitive of Types.primitive

(** A scope within the LHS of a denotation rule. Note that it's not currently
    allowed to match on specific patterns -- you can only match on an entire
    slot at once.
*)
and scope = Scope of string list * pattern

type t = pattern

let rec vars
  : pattern -> StrSet.t
  = function
  | Operator (_, pats) -> vars_of_scopes pats
  | Var name -> StrSet.from_list [ name ]
  | Sequence pats -> vars_of_patterns pats
  | Primitive _ -> StrSet.empty

and vars_of_scope (Scope (bound_vars, pat)) = bound_vars
  |> StrSet.from_list
  |> StrSet.union (vars pat)

and vars_of_scopes scopes = scopes
  |. List.map ~f:vars_of_scope
  |. Placemat.List.fold_left ~initial:StrSet.empty ~f:StrSet.union

and vars_of_patterns pats = pats
  |. List.map ~f:vars
  |. Placemat.List.fold_left ~initial:StrSet.empty ~f:StrSet.union
;;

let list_vars : pattern -> string list
  = fun pat -> StrSet.to_list (vars pat)
;;

let rec to_string : pattern -> string = function
  | Operator (name, pats) ->
    Printf.sprintf "%s(%s)" name (stringify_list scope_to_string "; " pats)
  | Var name -> name
  | Sequence pats ->
    Printf.sprintf "[%s]" (stringify_list to_string ", " pats)
  | Primitive prim -> Types.string_of_primitive prim

and scope_to_string : scope -> string
  = fun (Scope (bound_vars, pat)) -> match bound_vars with
    | [] -> to_string pat
    | _ -> Printf.sprintf "%s. %s"
      (stringify_list Util.id ". " bound_vars)
      (to_string pat)
;;

exception BindingAwareScopePatternEncountered

(* raises BindingAwareScopePatternEncountered *)
let rec from_ast : Binding.Nominal.term -> pattern = function
  | Var name -> Var name
  | Operator (name, tms) -> Operator (name, tms |> List.map ~f:from_ast_scope)
  | Sequence tms -> Sequence (List.map tms ~f:from_ast)
  | Primitive prim -> Primitive prim

(* raises BindingAwareScopePatternEncountered *)
and from_ast_scope : Binding.Nominal.scope -> scope
  = fun (Scope (binders, body))
  -> Scope
    ( List.map binders ~f:(function
      | Pattern.Var v -> v
      | _ -> raise BindingAwareScopePatternEncountered
      )
    , from_ast body
    )
;;
