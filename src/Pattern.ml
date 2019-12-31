let fromArray, empty, union, toList =
  Belt.Set.String.(fromArray, empty, union, toList)
let stringify_list = Util.stringify_list

type pattern =
  | Operator of string * pattern list
  | Var of string
  | Sequence of pattern list
  | Primitive of Types.primitive

type t = pattern

let rec vars_of_pattern : pattern -> Belt.Set.String.t = function
  | Operator (_, pats) -> vars_of_patterns pats
  | Var name -> fromArray [| name |]
  | Sequence pats -> vars_of_patterns pats
  | Primitive _ -> empty

and vars_of_patterns pats = Belt.List.(pats
  |. map vars_of_pattern
  |. reduce empty union
)
;;

let list_vars_of_pattern : pattern -> string list
  = fun pat -> toList (vars_of_pattern pat)
;;

let rec string_of_pattern : pattern -> string = function
  | Operator (name, pats) ->
    Printf.sprintf "%s(%s)" name (stringify_list string_of_pattern "; " pats)
  | Var name -> name
  | Sequence pats ->
    Printf.sprintf "[%s]" (stringify_list string_of_pattern ", " pats)
  | Primitive prim -> Types.string_of_primitive prim
;;
