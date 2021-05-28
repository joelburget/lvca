open Base
module String = Lvca_util.String
open Option.Let_syntax

module Kernel = struct
  type 'info term =
    | Operator of 'info * string * ('info scope, 'info term) Either.t list
    | BoundVar of 'info * int
    | FreeVar of 'info * string
    | Primitive of 'info Primitive.t

  and 'info scope = Scope of 'info * string * 'info term

  let rec map_info ~f = function
    | Operator (i, name, subtms) ->
      let subtms =
        subtms
        |> List.map ~f:(function
               | Either.First scope -> Either.First (scope_map_info ~f scope)
               | Second tm -> Second (map_info ~f tm))
      in
      Operator (f i, name, subtms)
    | BoundVar (i, n) -> BoundVar (f i, n)
    | FreeVar (i, n) -> FreeVar (f i, n)
    | Primitive p -> Primitive (Primitive.map_info ~f p)

  and scope_map_info ~f (Scope (i, name, tm)) = Scope (f i, name, map_info ~f tm)

  let erase tm = map_info ~f:(fun _ -> ()) tm

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
    | BoundVar (_, i) -> if i = target_ix then subst_tm else tm
    | FreeVar _ | Primitive _ -> tm
  ;;

  let open_scope subst_tm (Scope (_, _, tms)) = open_term 0 subst_tm tms

  let rec to_nominal' ctx = function
    | BoundVar (info, ix) ->
      ix |> List.nth ctx |> Option.map ~f:(fun name -> Nominal.Term.Var (info, name))
    | FreeVar (info, name) -> Some (Var (info, name))
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
        | None -> FreeVar (info, name)
        | Some i -> BoundVar (info, i))
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
    | BoundVar (_, i1), BoundVar (_, i2) -> i1 = i2
    | FreeVar (_, name1), FreeVar (_, name2) -> String.(name1 = name2)
    | Primitive p1, Primitive p2 ->
      Primitive.equal ~info_eq:Unit.( = ) (Primitive.erase p1) (Primitive.erase p2)
    | _, _ -> false
  ;;

  let pp_generic ~open_loc ~close_loc ppf tm =
    match to_nominal tm with
    | None -> Fmt.pf ppf "DeBruijn.pp: failed to convert de Bruijn term to nominal"
    | Some tm -> Nominal.Term.pp_generic ~open_loc ~close_loc ppf tm
  ;;

  module Parse = struct
    open Lvca_parsing

    let t =
      Nominal.Term.Parse.t
      >>= fun tm ->
      match of_nominal tm with
      | Ok tm -> return tm
      | Error _scope -> fail "DeBruijn.Parse.t: failed to convert from nominal"
    ;;

    let whitespace_t = whitespace *> t
  end
end

include Kernel
(* module Properties = LanguageObject.Mk (Kernel) *)
