open Base
module String = Lvca_util.String
open Option.Let_syntax

type ('info, 'prim) term =
  | Operator of
      'info * string * (('info, 'prim) scope, ('info, 'prim) term) Base.Either.t list
  | BoundVar of 'info * int
  | FreeVar of 'info * string
  | Primitive of 'info * 'prim

and ('info, 'prim) scope = Scope of 'info * string * ('info, 'prim) term

let rec open_term target_ix subst_tm tm =
  match tm with
  | Operator (loc, tag, subtms) ->
    let subtms' =
      subtms
      |> List.map
           ~f:
             (Either.map
                ~first:(fun (Scope (loc, name, tm)) ->
                  Scope (loc, name, open_term (target_ix + 1) subst_tm tm))
                ~second:(open_term target_ix subst_tm))
    in
    Operator (loc, tag, subtms')
  | BoundVar (_, i) -> if i = target_ix then subst_tm else tm
  | FreeVar _ | Primitive _ -> tm
;;

let open_scope subst_tm (Scope (_, _, tms)) = open_term 0 subst_tm tms

let rec to_nominal' ctx = function
  | BoundVar (loc, ix) ->
    ix |> List.nth ctx |> Option.map ~f:(fun name -> Nominal.Var (loc, name))
  | FreeVar (loc, name) -> Some (Var (loc, name))
  | Operator (loc, tag, subtms) ->
    subtms
    |> List.map ~f:(scope_or_term_to_nominal ctx)
    |> Option.all
    |> Option.map ~f:(fun subtms' -> Nominal.Operator (loc, tag, subtms'))
  | Primitive (loc, prim) -> Some (Nominal.Primitive (loc, prim))

and scope_to_nominal ctx (Scope (loc, name, body)) =
  let ctx = List.cons name ctx in
  let%map body = to_nominal' ctx body in
  Nominal.Scope ([ Var (loc, name) ], body)

and scope_or_term_to_nominal ctx = function
  | Either.First scope -> scope_to_nominal ctx scope
  | Second body ->
    let%map body = to_nominal' ctx body in
    Nominal.Scope ([], body)
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
      | Some i -> BoundVar (loc, i))
  | Primitive (loc, prim) -> Ok (Primitive (loc, prim))

and scope_of_nominal env (Scope (pats, body) as scope) =
  let open Result.Let_syntax in
  match pats with
  | [] ->
    let%map body = of_nominal_with_bindings env body in
    Either.Second body
  | [ Var (pos, name) ] ->
    let env' : int String.Map.t =
      env |> Map.map ~f:(fun i -> i + 1) |> Map.set ~key:name ~data:0
    in
    let%map body = of_nominal_with_bindings env' body in
    Either.First (Scope (pos, name, body))
  | _ -> Error scope
;;

(* (Printf.sprintf "Expected zero-or-one variable binding, found %s" (Nominal.pp_scope_str
   Primitive.pp scope))) *)

let of_nominal tm = of_nominal_with_bindings String.Map.empty tm

let rec alpha_equivalent prim_equivalent t1 t2 =
  match t1, t2 with
  | Operator (_, h1, subtms1), Operator (_, h2, subtms2) ->
    String.(h1 = h2)
    &&
    (match List.zip subtms1 subtms2 with
    | Ok zipped ->
      let f = function
        | Either.First (Scope (_, _, body1)), Either.First (Scope (_, _, body2)) ->
          alpha_equivalent prim_equivalent body1 body2
        | Second tm1, Second tm2 -> alpha_equivalent prim_equivalent tm1 tm2
        | _, _ -> false
      in
      List.for_all zipped ~f
    | Unequal_lengths -> false)
  | BoundVar (_, i1), BoundVar (_, i2) -> i1 = i2
  | FreeVar (_, name1), FreeVar (_, name2) -> String.(name1 = name2)
  | Primitive (_, p1), Primitive (_, p2) -> prim_equivalent p1 p2
  | _, _ -> false
;;
