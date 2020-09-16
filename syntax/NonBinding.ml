open Base

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) term list list
  | Primitive of 'loc * 'prim

let location = function Operator (loc, _, _) | Primitive (loc, _) -> loc

open Result.Let_syntax

type ('loc, 'prim) de_bruijn_conversion_error =
  | ScopeEncountered of ('loc, 'prim) DeBruijn.scope
  | VarEncountered of ('loc, 'prim) DeBruijn.term

let rec of_de_bruijn tm =
  match tm with
  | DeBruijn.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_de_bruijn_scope |> Result.all in
    Operator (a, tag, scopes')
  | BoundVar _ | FreeVar _ -> Error (VarEncountered tm)
  | Primitive (a, p) -> Ok (Primitive (a, p))

and of_de_bruijn_scope = function
  | First scope -> Error (ScopeEncountered scope)
  | Second tms -> tms |> List.map ~f:of_de_bruijn |> Result.all
;;

let rec to_de_bruijn tm (* : unit DeBruijn.term *) =
  match tm with
  | Operator (loc, tag, tms) ->
    DeBruijn.Operator
      ( loc
      , tag
      , List.map tms ~f:(fun tms -> Either.Second (tms |> List.map ~f:to_de_bruijn)) )
  | Primitive (loc, p) -> Primitive (loc, p)
;;

type ('loc, 'prim) nominal_conversion_error =
  | ScopeEncountered of ('loc, 'prim) Nominal.scope
  | VarEncountered of ('loc, 'prim) Nominal.term

let rec of_nominal tm =
  match tm with
  | Nominal.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_nominal_scope |> Result.all in
    Operator (a, tag, scopes')
  | Var _ -> Error (VarEncountered tm)
  | Primitive (a, p) -> Ok (Primitive (a, p))

and of_nominal_scope = function
  | Nominal.Scope ([], tms) -> tms |> List.map ~f:of_nominal |> Result.all
  | scope -> Error (ScopeEncountered scope)
;;

let rec to_nominal tm =
  match tm with
  | Operator (loc, tag, tms) ->
    Nominal.Operator
      ( loc
      , tag
      , List.map tms ~f:(fun tms' -> Nominal.Scope ([], List.map tms' ~f:to_nominal)) )
  | Primitive (loc, p) -> Primitive (loc, p)
;;

let pp pp_prim ppf tm = tm |> to_nominal |> Nominal.pp_term pp_prim ppf
let pp_range pp_prim ppf tm = tm |> to_nominal |> Nominal.pp_term_range pp_prim ppf
let to_string pp_prim tm = tm |> to_nominal |> Nominal.pp_term_str pp_prim
let hash jsonify_prim tm = tm |> to_nominal |> Nominal.hash jsonify_prim

let rec erase = function
  | Operator (_, tag, subtms) ->
    Operator ((), tag, subtms |> List.map ~f:(List.map ~f:erase))
  | Primitive (_, prim) -> Primitive ((), prim)
;;
