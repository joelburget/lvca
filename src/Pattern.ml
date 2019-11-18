type primitive = Types.primitive

type pattern =
  | Operator  of string * pattern list
  | Var       of string
  | Sequence  of pattern list
  | Primitive of primitive

type t = pattern

let rec vars_of_pattern : pattern -> Belt.Set.String.t
  = function
    | Operator (_, pats) -> vars_of_patterns pats
    | Var name           -> Belt.Set.String.fromArray [| name |]
    | Sequence pats      -> vars_of_patterns pats
    | Primitive _        -> Belt.Set.String.empty

and vars_of_patterns pats = pats
  |. Belt.List.map vars_of_pattern
  |. Belt.List.reduce Belt.Set.String.empty Belt.Set.String.union

let list_vars_of_pattern : pattern -> string list
  = function pat -> Belt.Set.String.toList (vars_of_pattern pat)

let rec string_of_pattern : pattern -> string
  = function
    | Operator (name, pats)
    -> Printf.sprintf "%s(%s)" name
       (Util.stringify_list string_of_pattern "; " pats)
    | Var name -> name
    | Sequence pats
    -> Printf.sprintf "[%s]" (Util.stringify_list string_of_pattern ", " pats)
    | Primitive prim -> Types.string_of_primitive prim
