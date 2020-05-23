module List = Base.List
module Option = Base.Option
module Set = Base.Set
module String = Util.String

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
  |> List.fold_right ~init:String.Set.empty ~f:Set.union
;;

let list_vars_of_pattern : pattern -> string list =
 fun pat -> Base.Set.to_list (vars_of_pattern pat)
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

let rec jsonify pat =
  Util.Json.(
    match pat with
    | Operator (tag, tms) -> array [|
      string "o";
      string tag;
      tms |> List.map ~f:jsonify |> Array.of_list |> array
    |]
    | Primitive p -> array [| string "p"; Primitive.jsonify p |]
    | Var name -> array [| string "v"; string name |]
    | Ignored name -> array [| string "_"; string name |])

let rec unjsonify =
  let open Option.Let_syntax in
  Util.Json.(function
  | Array [| String "o"; String tag; Array subtms |] ->
    let%map subtms' = subtms |> Array.to_list |> List.map ~f:unjsonify |> Option.all in
    Operator (tag, subtms')
  | Array [| String "p"; prim |] ->
    let%map prim' = Primitive.unjsonify prim in
    Primitive prim'
  | Array [| String "v"; String name |]
  -> Some (Var name)
  | Array [| String "_"; String name |]
  -> Some (Ignored name)
  | _ -> None
  )

module Properties = struct
  let round_trip1 : t -> bool
    = fun t -> match t |> jsonify |> unjsonify with
      | None -> false
      | Some t' -> t = t'

  let round_trip2 : Util.Json.t -> bool
    = fun json -> match json |> unjsonify with
      | None -> false
      | Some t -> Util.Json.(jsonify t = json)
end
