open Base
module String = Lvca_util.String

type 'loc term =
  | Operator of 'loc * string * ('loc scope, 'loc term list) Either.t list
  | BoundVar of 'loc * int
  | FreeVar of 'loc * string
  | Primitive of 'loc * Primitive.t

and 'loc scope = Scope of 'loc * string * 'loc term list

let rec to_nominal' ctx = function
  | BoundVar (loc, ix) -> ix
    |> List.nth ctx
    |> Option.map ~f:(fun name -> Nominal.Var (loc, name))
  | FreeVar (loc, name) -> Some (Var (loc, name))
  | Operator (loc, tag, subtms) ->
    subtms
    |> List.map ~f:(scope_or_term_to_nominal ctx)
    |> Option.all
    |> Option.map ~f:(fun subtms' -> Nominal.Operator (loc, tag, subtms'))
  | Primitive (loc, prim) -> Some (Nominal.Primitive (loc, prim))

and scope_to_nominal ctx (Scope (loc, name, body)) =
  let ctx' = List.cons name ctx in
  body
    |> List.map ~f:(to_nominal' ctx')
    |> Option.all
    |> Option.map
      ~f:(fun body' -> Nominal.Scope ([Var (loc, name)], body'))

and scope_or_term_to_nominal ctx = function
  | Either.First scope -> scope_to_nominal ctx scope
  | Second tms -> tms
    |> List.map ~f:(to_nominal' ctx)
    |> Option.all
    |> Option.map ~f:(fun body' -> Nominal.Scope ([], body'))
;;

let to_nominal tm = to_nominal' [] tm

exception FailedFromNominal of string

let rec of_nominal_with_bindings_exn env = function
  | Nominal.Operator (loc, tag, subtms) ->
    Operator (loc, tag, List.map subtms ~f:(scope_of_nominal' env))
  | Var (loc, name) ->
    (match Map.find env name with
    | None -> FreeVar (loc, name)
    | Some i -> BoundVar (loc, i))
  | Primitive (loc, prim) -> Primitive (loc, prim)

and scope_of_nominal' env (Scope (pats, body) as scope) = match pats with
  | [] ->
    let body' = List.map body ~f:(of_nominal_with_bindings_exn env) in
    Second body'
  | [Var (pos, name)] ->
    let env' : int String.Map.t = env
      |> Map.map ~f:(fun i -> i + 1)
      |> Map.set ~key:name ~data:0
    in
    let body' = List.map body ~f:(of_nominal_with_bindings_exn env') in
    First (Scope (pos, name, body'))
  | _ -> raise (FailedFromNominal (Printf.sprintf
    "Expected zero-or-one variable binding, found %s" (Nominal.pp_scope_str scope)))
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
      | Ok zipped ->
        let match_bodies body1 body2 = match List.zip body1 body2 with
          | Ok bodies -> List.for_all bodies
            ~f:(fun (b1, b2) -> alpha_equivalent b1 b2)
          | _ -> false
        in
        let f = function
          | Either.First (Scope (_, _, body1)), Either.First (Scope (_, _, body2))
          -> match_bodies body1 body2
          | Second tms1, Second tms2 -> match_bodies tms1 tms2
          | _, _ -> false
        in
        List.for_all zipped ~f
      | Unequal_lengths -> false
    )
    | BoundVar (_, i1), BoundVar (_, i2)
    -> i1 = i2
    | FreeVar (_, name1), FreeVar (_, name2)
    -> String.(name1 = name2)
    | Primitive (_, p1), Primitive (_, p2)
    -> Primitive.(p1 = p2)
    | _, _
    -> false
