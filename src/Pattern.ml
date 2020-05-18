module String = Util.String
module List = Base.List

type pattern =
  | Operator of string * pattern list
  | Primitive of Primitive.t
  | Var of string
  | Ignored of string

type t = pattern

let rec vars_of_pattern : pattern -> String.Set.t = function
  | Operator (_, pats) -> vars_of_patterns pats
  | Primitive _ -> String.Set.empty
  | Var name -> String.Set.of_list [ name ]
  | Ignored _ -> String.Set.empty

and vars_of_patterns pats =
  pats
  |> List.map ~f:vars_of_pattern
  |> List.fold_right ~init:String.Set.empty ~f:String.Set.union
;;

let list_vars_of_pattern : pattern -> string list =
 fun pat -> String.Set.to_list (vars_of_pattern pat)
;;

let rec to_string : pattern -> string = function
  | Operator (name, pats) ->
    Printf.sprintf "%s(%s)" name
    (pats |> List.map ~f:to_string |> String.concat ~sep:"; ")
  | Primitive prim -> Primitive.to_string prim
  | Var name -> name
  | Ignored name -> "_" ^ name
;;

let rec pp : Format.formatter -> pattern -> unit
  = fun ppf -> Format.(function
  | Operator (name, pats)
  -> fprintf ppf "%s(%a)" name (pp_pattern_list ";") pats
  | Primitive prim
  -> Primitive.pp ppf prim
  | Var name
  -> fprintf ppf "%s" name
  | Ignored name
  -> fprintf ppf "_%s" name
  )

and pp_pattern_list sep ppf = function
  | []
  -> ()
  | [ pat ]
  -> pp ppf pat
  | pat :: pats
  -> Format.fprintf ppf "%a%s %a" pp pat sep (pp_pattern_list sep) pats
