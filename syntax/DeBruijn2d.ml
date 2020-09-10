open Base
module String = Lvca_util.String

type 'loc term =
  | Operator of 'loc * string * 'loc scope list
  | Var of 'loc * int * int
  | Primitive of 'loc * Primitive.t

and 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

let rec to_nominal' ctx = function
  | Var (loc, ix1, ix2) ->
    List.nth ctx ix1
    |> Option.bind ~f:(Fn.flip List.nth ix2)
    |> Option.map ~f:(fun name -> Nominal.Var (loc, name))
  | Operator (loc, tag, subtms) ->
    subtms
    |> List.map ~f:(scope_to_nominal ctx)
    |> Option.all
    |> Option.map ~f:(fun subtms' -> Nominal.Operator (loc, tag, subtms'))
  | Primitive (loc, prim) -> Some (Nominal.Primitive (loc, prim))

and scope_to_nominal ctx (Scope (binders, body)) =
  let ctx' = binders
    |> List.map ~f:(fun pat -> pat
      |> Pattern.list_vars_of_pattern
      |> List.map ~f:snd)
    |> List.append ctx
  in
  body
    |> List.map ~f:(to_nominal' ctx')
    |> Option.all
    |> Option.map ~f:(fun body' -> Nominal.Scope (binders, body'))
;;

let to_nominal tm = to_nominal' [] tm

exception FailedFromNominal of string

let rec of_nominal_with_bindings_exn env = function
  | Nominal.Operator (loc, tag, subtms) ->
    Operator (loc, tag, List.map subtms ~f:(scope_of_nominal' env))
  | Var (loc, name) ->
    (match Map.find env name with
    | None -> raise (FailedFromNominal ("couldn't find variable " ^ name))
    | Some (i, j) -> Var (loc, i, j))
  | Primitive (loc, prim) -> Primitive (loc, prim)

and scope_of_nominal' env (Nominal.Scope (pats, body)) =
  let n = List.length pats in
  let var_nums : (string * (int * int)) list =
    pats
    |> List.mapi ~f:(fun i pat -> pat
      |> Pattern.list_vars_of_pattern
      |> List.mapi ~f:(fun j (_, var) -> var, (i, j)))
    |> List.join
  in
  match String.Map.of_alist var_nums with
  | `Ok var_map ->
    let env' : (int * int) String.Map.t = env
      |> Map.map ~f:(fun (i, j) -> i + n, j)
      |> Lvca_util.Map.union_right_biased var_map
    in
    let body' = List.map body ~f:(of_nominal_with_bindings_exn env') in
    Scope (pats, body')
  | `Duplicate_key _key -> failwith "TODO: raise error"
;;

let of_nominal_with_bindings bindings tm =
  try Ok (of_nominal_with_bindings_exn bindings tm) with
  | FailedFromNominal msg -> Error msg
;;

let of_nominal tm = of_nominal_with_bindings String.Map.empty tm

let rec alpha_equivalent = fun t1 t2 ->
  match t1, t2 with
    | Operator (_, h1, subtms1), Operator (_, h2, subtms2)
    -> String.(h1 = h2) && (match List.zip subtms1 subtms2 with
      | Ok zipped -> List.for_all zipped ~f:(fun (Scope (_, body1), Scope (_, body2)) ->
          match List.zip body1 body2 with
            | Ok bodies -> List.for_all
              ~f:(fun (b1, b2) -> alpha_equivalent b1 b2)
              bodies
            | _ -> false)
      | Unequal_lengths -> false
    )
    | Var (_, i1, j1), Var (_, i2, j2)
    -> i1 = i2 && j1 = j2
    | Primitive (_, p1), Primitive (_, p2)
    -> Primitive.(p1 = p2)
    | _, _
    -> false
