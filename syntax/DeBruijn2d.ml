open Base
module String = Lvca_util.String
open Option.Let_syntax
module PatternF = Pattern

module Make (Prim : LanguageObject_intf.S) : DeBruijn2d_intf.S with module Prim = Prim =
struct
  module Prim = Prim

  module Pattern : Pattern_intf.S with type 'info prim = 'info Prim.t =
    PatternF.Make (Prim)

  module Nominal : Nominal_intf.S with module Prim = Prim and module Pattern = Pattern =
    Nominal.Make (Prim)

  type 'info term =
    | Operator of 'info * string * 'info scope list
    | BoundVar of 'info * int * int
    | FreeVar of 'info * string
    | Primitive of 'info Prim.t

  and 'info scope = Scope of 'info Pattern.t list * 'info term

  let rec to_nominal' ctx = function
    | BoundVar (info, ix1, ix2) ->
      List.nth ctx ix1
      |> Option.bind ~f:(Fn.flip List.nth ix2)
      |> Option.map ~f:(fun name -> Nominal.Term.Var (info, name))
    | Operator (info, tag, subtms) ->
      subtms
      |> List.map ~f:(scope_to_nominal ctx)
      |> Option.all
      |> Option.map ~f:(fun subtms' -> Nominal.Term.Operator (info, tag, subtms'))
    | FreeVar (info, name) -> Some (Var (info, name))
    | Primitive prim -> Some (Nominal.Term.Primitive prim)

  and scope_to_nominal ctx (Scope (binders, body)) =
    let ctx =
      binders
      |> List.map ~f:(fun pat -> pat |> Pattern.list_vars_of_pattern |> List.map ~f:snd)
      |> List.append ctx
    in
    let%map body = to_nominal' ctx body in
    Nominal.Scope.Scope (binders, body)
  ;;

  let to_nominal tm = to_nominal' [] tm

  let rec of_nominal_with_bindings env = function
    | Nominal.Term.Operator (info, tag, subtms) ->
      let open Result.Let_syntax in
      let%map subtms' = subtms |> List.map ~f:(scope_of_nominal env) |> Result.all in
      Operator (info, tag, subtms')
    | Var (info, name) ->
      Ok
        (match Map.find env name with
        | None -> FreeVar (info, name)
        | Some (i, j) -> BoundVar (info, i, j))
    | Primitive prim -> Ok (Primitive prim)

  and scope_of_nominal env (Nominal.Scope.Scope (pats, body) as scope) =
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
      let%map body' = of_nominal_with_bindings env' body in
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
            alpha_equivalent prim_equivalent body1 body2)
      | Unequal_lengths -> false)
    | BoundVar (_, i1, j1), BoundVar (_, i2, j2) -> i1 = i2 && j1 = j2
    | FreeVar (_, name1), FreeVar (_, name2) -> String.(name1 = name2)
    | Primitive p1, Primitive p2 -> prim_equivalent p1 p2
    | _, _ -> false
  ;;

  let rec select_path ~path tm =
    match path with
    | [] -> Ok tm
    | i :: path ->
      (match tm with
      | BoundVar _ | FreeVar _ | Primitive _ -> Error "TODO: message"
      | Operator (_, _, scopes) ->
        (match List.nth scopes i with
        | None -> Error "TODO: message"
        | Some (Scope (_pats, tm)) -> select_path ~path tm))
  ;;
end
