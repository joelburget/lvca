open Base
module String = Lvca_util.String

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) scope list
  | BoundVar of 'loc * int * int
  | FreeVar of 'loc * string
  | Primitive of 'loc * 'prim

and ('loc, 'prim) scope =
  | Scope of ('loc, 'prim) Pattern.t list * ('loc, 'prim) term list

let rec to_nominal' ctx = function
  | BoundVar (loc, ix1, ix2) ->
    List.nth ctx ix1
    |> Option.bind ~f:(Fn.flip List.nth ix2)
    |> Option.map ~f:(fun name -> Nominal.Var (loc, name))
  | Operator (loc, tag, subtms) ->
    subtms
    |> List.map ~f:(scope_to_nominal ctx)
    |> Option.all
    |> Option.map ~f:(fun subtms' -> Nominal.Operator (loc, tag, subtms'))
  | FreeVar (loc, name) -> Some (Var (loc, name))
  | Primitive (loc, prim) -> Some (Nominal.Primitive (loc, prim))

and scope_to_nominal ctx (Scope (binders, body)) =
  let ctx' =
    binders
    |> List.map ~f:(fun pat -> pat |> Pattern.list_vars_of_pattern |> List.map ~f:snd)
    |> List.append ctx
  in
  body
  |> List.map ~f:(to_nominal' ctx')
  |> Option.all
  |> Option.map ~f:(fun body' -> Nominal.Scope (binders, body'))
;;

let to_nominal tm = to_nominal' [] tm

let rec of_nominal_with_bindings env = function
  | Nominal.Operator (loc, tag, subtms) ->
    let open Result.Let_syntax in
    let%map subtms' = subtms |> List.map ~f:(scope_of_nominal env) |> Result.all in
    Operator (loc, tag, subtms')
  | Var (loc, name) ->
    Ok
      (match Map.find env name with
      | None -> FreeVar (loc, name)
      | Some (i, j) -> BoundVar (loc, i, j))
  | Primitive (loc, prim) -> Ok (Primitive (loc, prim))

and scope_of_nominal env (Nominal.Scope (pats, body) as scope) =
  let open Result.Let_syntax in
  let n = List.length pats in
  let var_nums : (string * (int * int)) list =
    pats
    |> List.mapi ~f:(fun i pat ->
           pat
           |> Pattern.list_vars_of_pattern
           |> List.mapi ~f:(fun j (_, var) -> var, (i, j)))
    |> List.join
  in
  match String.Map.of_alist var_nums with
  | `Ok var_map ->
    let env' : (int * int) String.Map.t =
      env
      |> Map.map ~f:(fun (i, j) -> i + n, j)
      |> Lvca_util.Map.union_right_biased var_map
    in
    let%map body' = body |> List.map ~f:(of_nominal_with_bindings env') |> Result.all in
    Scope (pats, body')
  | `Duplicate_key _key -> Error scope
;;

let of_nominal tm = of_nominal_with_bindings String.Map.empty tm

let rec alpha_equivalent prim_equivalent t1 t2 =
  match t1, t2 with
  | Operator (_, h1, subtms1), Operator (_, h2, subtms2) ->
    String.(h1 = h2)
    &&
    (match List.zip subtms1 subtms2 with
    | Ok zipped ->
      List.for_all zipped ~f:(fun (Scope (_, body1), Scope (_, body2)) ->
          match List.zip body1 body2 with
          | Ok bodies ->
            List.for_all
              ~f:(fun (b1, b2) -> alpha_equivalent prim_equivalent b1 b2)
              bodies
          | _ -> false)
    | Unequal_lengths -> false)
  | BoundVar (_, i1, j1), BoundVar (_, i2, j2) -> i1 = i2 && j1 = j2
  | FreeVar (_, name1), FreeVar (_, name2) -> String.(name1 = name2)
  | Primitive (_, p1), Primitive (_, p2) -> prim_equivalent p1 p2
  | _, _ -> false
;;

let rec select_path ~path tm = match path with
  | [] -> Ok tm
  | (i, j)::path -> match tm with
    | BoundVar _ | FreeVar _ | Primitive _ -> Error "TODO: message"
    | Operator (_, _, scopes) ->
      let open Option.Let_syntax in
      let tm =
        let%bind (Scope (_pats, tms)) = List.nth scopes i in
        List.nth tms j
      in
      match tm with
        | None -> Error "TODO: message"
        | Some tm -> select_path ~path tm
