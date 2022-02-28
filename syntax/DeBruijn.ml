open Base
module String = Lvca_util.String
open Option.Let_syntax

module Kernel = struct
  type term =
    | Operator of Provenance.t * string * (scope, term) Either.t list
    | Bound_var of Provenance.t * int
    | Free_var of Provenance.t * string
    | Primitive of Primitive.All.t

  and scope = Scope of Provenance.t * string * term

  let rec equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
    match t1, t2 with
    | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
      info_eq i1 i2
      && String.(name1 = name2)
      && List.equal
           (Either.equal (scope_eq ~info_eq) (equivalent ~info_eq))
           scopes1
           scopes2
    | Free_var (i1, name1), Free_var (i2, name2) ->
      info_eq i1 i2 && String.(name1 = name2)
    | Bound_var (p1, i1), Bound_var (p2, i2) -> info_eq p1 p2 && Int.(i1 = i2)
    | Primitive p1, Primitive p2 -> Primitive_impl.All.equivalent ~info_eq p1 p2
    | _, _ -> false

  and scope_eq
      ?(info_eq = fun _ _ -> true)
      (Scope (i1, name1, tm1))
      (Scope (i2, name2, tm2))
    =
    info_eq i1 i2 && String.(name1 = name2) && equivalent ~info_eq tm1 tm2
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let rec open_term target_ix subst_tm tm =
    match tm with
    | Operator (info, tag, subtms) ->
      let subtms' =
        subtms
        |> List.map
             ~f:
               (Either.map
                  ~first:(fun (Scope (info, name, tm)) ->
                    Scope (info, name, open_term (target_ix + 1) subst_tm tm))
                  ~second:(open_term target_ix subst_tm))
      in
      Operator (info, tag, subtms')
    | Bound_var (_, i) -> if Int.(i = target_ix) then subst_tm else tm
    | Free_var _ | Primitive _ -> tm
  ;;

  let open_scope subst_tm (Scope (_, _, tms)) = open_term 0 subst_tm tms

  let rec to_nominal' ctx = function
    | Bound_var (info, ix) ->
      ix |> List.nth ctx |> Option.map ~f:(fun name -> Nominal.Term.Var (info, name))
    | Free_var (info, name) -> Some (Var (info, name))
    | Operator (info, tag, subtms) ->
      subtms
      |> List.map ~f:(scope_or_term_to_nominal ctx)
      |> Option.all
      |> Option.map ~f:(fun subtms' -> Nominal.Term.Operator (info, tag, subtms'))
    | Primitive prim -> Some (Nominal.Term.Primitive prim)

  and scope_to_nominal ctx (Scope (info, name, body)) =
    let ctx = List.cons name ctx in
    let%map body = to_nominal' ctx body in
    Nominal.Scope.Scope ([ Var (info, name) ], body)

  and scope_or_term_to_nominal ctx = function
    | Either.First scope -> scope_to_nominal ctx scope
    | Second body ->
      let%map body = to_nominal' ctx body in
      Nominal.Scope.Scope ([], body)
  ;;

  let to_nominal tm = to_nominal' [] tm

  let rec of_nominal_with_bindings ~env = function
    | Nominal.Term.Operator (info, tag, subtms) ->
      let open Result.Let_syntax in
      let%map subtms' = subtms |> List.map ~f:(scope_of_nominal env) |> Result.all in
      Operator (info, tag, subtms')
    | Var (info, name) ->
      Ok
        (match Map.find env name with
        | None -> Free_var (info, name)
        | Some i -> Bound_var (info, i))
    | Primitive prim -> Ok (Primitive prim)

  and scope_of_nominal env (Scope (pats, body) as scope) =
    let open Result.Let_syntax in
    match pats with
    | [] ->
      let%map body = of_nominal_with_bindings ~env body in
      Either.Second body
    | [ Var (pos, name) ] ->
      let env = env |> Map.map ~f:Int.succ |> Map.set ~key:name ~data:0 in
      let%map body = of_nominal_with_bindings ~env body in
      Either.First (Scope (pos, name, body))
    | _ -> Error scope
  ;;

  let of_nominal tm = of_nominal_with_bindings ~env:String.Map.empty tm

  let rec alpha_equivalent t1 t2 =
    match t1, t2 with
    | Operator (_, h1, subtms1), Operator (_, h2, subtms2) ->
      String.(h1 = h2)
      &&
      (match List.zip subtms1 subtms2 with
      | Ok zipped ->
        let f = function
          | Either.First (Scope (_, _, body1)), Either.First (Scope (_, _, body2)) ->
            alpha_equivalent body1 body2
          | Second tm1, Second tm2 -> alpha_equivalent tm1 tm2
          | _, _ -> false
        in
        List.for_all zipped ~f
      | Unequal_lengths -> false)
    | Bound_var (_, i1), Bound_var (_, i2) -> Int.(i1 = i2)
    | Free_var (_, name1), Free_var (_, name2) -> String.(name1 = name2)
    | Primitive p1, Primitive p2 -> Primitive.All.equivalent p1 p2
    | _, _ -> false
  ;;

  let pp ppf tm =
    match to_nominal tm with
    | None -> Fmt.pf ppf "DeBruijn.pp: failed to convert de Bruijn term to nominal"
    | Some tm -> Nominal.Term.pp ppf tm
  ;;

  let parse =
    let open Lvca_parsing.Parser in
    let open Construction in
    let+ tm = Nominal.Term.parse' in
    match of_nominal tm with
    | Ok tm -> tm
    | Error _scope ->
      failwith (* TODO: raise *) "DeBruijn.parse: failed to convert from nominal"
  ;;
end

include Kernel
(* module Properties = LanguageObject.Mk (Kernel) *)
