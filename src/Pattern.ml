open Core_kernel

type pattern =
  | Operator of string * pattern list
  | Sequence of pattern list
  | Primitive of Primitive.t
  | Var of string
  | Ignored of string
  [@@deriving sexp]

type t = pattern
  [@@deriving sexp]

let rec vars_of_pattern : pattern -> String.Set.t = function
  | Operator (_, pats) -> vars_of_patterns pats
  | Sequence pats -> vars_of_patterns pats
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

let rec string_of_pattern : pattern -> string = function
  | Operator (name, pats) ->
    Printf.sprintf "%s(%s)" name
    (pats |> List.map ~f:string_of_pattern |> String.concat ~sep:"; ")
  | Sequence pats ->
    Printf.sprintf "[%s]"
    (pats |> List.map ~f:string_of_pattern |> String.concat ~sep:", ")
  | Primitive prim -> Primitive.to_string prim
  | Var name -> name
  | Ignored name -> "_" ^ name
;;

let rec pp : Format.formatter -> pattern -> unit
  = fun ppf -> Format.(function
  | Operator (name, pats)
  -> fprintf ppf "%s(%a)" name (pp_pattern_list ";") pats
  | Sequence pats
  -> fprintf ppf "[%a]" (pp_pattern_list ",") pats
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
