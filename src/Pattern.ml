module String = Core_kernel.String
module List = Core_kernel.List
let stringify_list = Util.stringify_list

type pattern =
  | Operator of string * pattern list
  | Var of string
  | Sequence of pattern list
  | Primitive of Types.primitive

type t = pattern

let rec vars_of_pattern : pattern -> String.Set.t = function
  | Operator (_, pats) -> vars_of_patterns pats
  | Var name -> String.Set.of_list [ name ]
  | Sequence pats -> vars_of_patterns pats
  | Primitive _ -> String.Set.empty

and vars_of_patterns pats = pats
  |> List.map ~f:vars_of_pattern
  |> List.fold_right ~init:String.Set.empty ~f:String.Set.union
;;

let list_vars_of_pattern : pattern -> string list
  = fun pat -> String.Set.to_list (vars_of_pattern pat)
;;

let rec string_of_pattern : pattern -> string = function
  | Operator (name, pats) ->
    Printf.sprintf "%s(%s)" name (stringify_list string_of_pattern "; " pats)
  | Var name -> name
  | Sequence pats ->
    Printf.sprintf "[%s]" (stringify_list string_of_pattern ", " pats)
  | Primitive prim -> Types.string_of_primitive prim
;;
