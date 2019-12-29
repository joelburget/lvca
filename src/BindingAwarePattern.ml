let (empty, fromArray, union, toList) =
  Belt.Set.String.(empty, fromArray, union, toList)
let (toArray, map, reduce) = Belt.List.(toArray, map, reduce)
let (stringify_list, id) = Util.(stringify_list, id)

(** Represents the LHS of a denotation rule. Why is this not just `Pattern.t`?
    Because patterns can't match binders. For example, we want to be able to write
    this on the LHS of a denotation rule:

    [[ lam(x. x) ]] = ...

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
  : pattern -> Belt.Set.String.t
  = function
  | Operator (_, pats) -> vars_of_scopes pats
  | Var name -> fromArray [| name |]
  | Sequence pats -> vars_of_patterns pats
  | Primitive _ -> empty

and vars_of_scope (Scope (bound_vars, pat)) = bound_vars
  |. toArray
  |. fromArray
  |. union (vars pat)

and vars_of_scopes scopes = scopes
  |. map vars_of_scope
  |. reduce empty union

and vars_of_patterns pats = pats
  |. map vars
  |. reduce empty union
;;

let list_vars : pattern -> string list
  = fun pat -> toList (vars pat)
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
      (stringify_list id ". " bound_vars)
      (to_string pat)
;;
