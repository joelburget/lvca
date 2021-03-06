open Base
open Result.Let_syntax

type ('info, 'prim) term =
  | Operator of 'info * string * ('info, 'prim) term list
  | Primitive of 'info * 'prim

let rec equal info_eq prim_eq t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (equal info_eq prim_eq) scopes1 scopes2
  | Primitive (i1, p1), Primitive (i2, p2) -> info_eq i1 i2 && prim_eq p1 p2
  | _, _ -> false
;;

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

let rec select_path ~path tm =
  match path with
  | [] -> Ok tm
  | i :: path ->
    (match tm with
    | Primitive _ -> Error "select_path: hit primitive but path not finished"
    | Operator (_, _, tms) ->
      (match List.nth tms i with
      | None ->
        Error
          (Printf.sprintf
             "select_path: path index %n too high (only %n tms)"
             i
             (List.length tms))
      | Some tm -> select_path ~path tm))
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  let term : 'prim ParseUtil.t -> (OptRange.t, 'prim) term ParseUtil.t =
   fun parse_prim ->
    let open Parsers in
    fix (fun term ->
        choice
          [ (parse_prim >>|| fun ~pos prim -> Primitive (pos, prim), pos)
          ; (identifier
            >>== fun ~pos:start ident ->
            parens (sep_end_by (char ';') term)
            >>|| (fun ~pos:finish children ->
                   let pos = OptRange.union start finish in
                   Operator (pos, ident, children), pos)
            <?> "term body")
          ])
    <?> "term"
 ;;

  let whitespace_term prim = Parsers.(junk *> term prim)
end
