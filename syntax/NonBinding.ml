open Base
open Result.Let_syntax

type ('info, 'prim) term =
  | Operator of 'info * string * ('info, 'prim) term list
  | Primitive of 'info * 'prim

let info = function Operator (i, _, _) | Primitive (i, _) -> i

let rec map_info ~f = function
  | Operator (i, name, tms) -> Operator (f i, name, List.map tms ~f:(map_info ~f))
  | Primitive (i, prim) -> Primitive (f i, prim)
;;

let erase tm = map_info ~f:(Fn.const ()) tm

type ('info, 'prim) de_bruijn_conversion_error =
  | ScopeEncountered of ('info, 'prim) DeBruijn.scope
  | VarEncountered of ('info, 'prim) DeBruijn.term

let rec of_de_bruijn tm =
  match tm with
  | DeBruijn.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_de_bruijn_scope |> Result.all in
    Operator (a, tag, scopes')
  | BoundVar _ | FreeVar _ -> Error (VarEncountered tm)
  | Primitive (a, p) -> Ok (Primitive (a, p))

and of_de_bruijn_scope = function
  | First scope -> Error (ScopeEncountered scope)
  | Second tm -> of_de_bruijn tm
;;

let rec to_de_bruijn tm =
  match tm with
  | Operator (info, tag, tms) ->
    DeBruijn.Operator
      (info, tag, List.map tms ~f:(fun tm -> Either.Second (to_de_bruijn tm)))
  | Primitive (info, p) -> Primitive (info, p)
;;

type ('info, 'prim) nominal_conversion_error =
  | ScopeEncountered of ('info, 'prim) Nominal.scope
  | VarEncountered of ('info, 'prim) Nominal.term

let rec of_nominal tm =
  match tm with
  | Nominal.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_nominal_scope |> Result.all in
    Operator (a, tag, scopes')
  | Var _ -> Error (VarEncountered tm)
  | Primitive (a, p) -> Ok (Primitive (a, p))

and of_nominal_scope = function
  | Nominal.Scope ([], tm) -> of_nominal tm
  | scope -> Error (ScopeEncountered scope)
;;

let rec to_nominal tm =
  match tm with
  | Operator (info, tag, tms) ->
    Nominal.Operator
      (info, tag, List.map tms ~f:(fun tm -> Nominal.Scope ([], to_nominal tm)))
  | Primitive (info, p) -> Primitive (info, p)
;;

let pp pp_prim ppf tm = tm |> to_nominal |> Nominal.pp_term pp_prim ppf
let pp_range pp_prim ppf tm = tm |> to_nominal |> Nominal.pp_term_range pp_prim ppf
let to_string pp_prim tm = tm |> to_nominal |> Nominal.pp_term_str pp_prim
let hash jsonify_prim tm = tm |> to_nominal |> Nominal.hash jsonify_prim
