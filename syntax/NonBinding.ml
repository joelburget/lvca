open Base
open Result.Let_syntax

type 'info term =
  | Operator of 'info * string * 'info term list
  | Primitive of 'info Primitive.t

let rec equal ~info_eq t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equal ~info_eq) scopes1 scopes2
  | Primitive i1, Primitive i2 -> Primitive.equal ~info_eq i1 i2
  | _, _ -> false
;;

let info = function Operator (i, _, _) -> i | Primitive p -> Primitive.info p

let rec map_info ~f = function
  | Operator (i, name, tms) -> Operator (f i, name, List.map tms ~f:(map_info ~f))
  | Primitive prim -> Primitive (Primitive.map_info ~f prim)
;;

let erase tm = map_info ~f:(Fn.const ()) tm

type 'info de_bruijn_conversion_error =
  | ScopeEncountered of 'info DeBruijn.scope
  | VarEncountered of 'info DeBruijn.term

let rec of_de_bruijn tm =
  match tm with
  | DeBruijn.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_de_bruijn_scope |> Result.all in
    Operator (a, tag, scopes')
  | BoundVar _ | FreeVar _ -> Error (VarEncountered tm)
  | Primitive p -> Ok (Primitive p)

and of_de_bruijn_scope = function
  | First scope -> Error (ScopeEncountered scope)
  | Second tm -> of_de_bruijn tm
;;

let rec to_de_bruijn tm =
  match tm with
  | Operator (info, tag, tms) ->
    DeBruijn.Operator
      (info, tag, List.map tms ~f:(fun tm -> Either.Second (to_de_bruijn tm)))
  | Primitive p -> Primitive p
;;

type 'info nominal_conversion_error =
  | ScopeEncountered of 'info Nominal.Scope.t
  | VarEncountered of 'info Nominal.Term.t

let rec of_nominal tm =
  match tm with
  | Nominal.Term.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_nominal_scope |> Result.all in
    Operator (a, tag, scopes')
  | Var _ -> Error (VarEncountered tm)
  | Primitive p -> Ok (Primitive p)

and of_nominal_scope = function
  | Nominal.Scope.Scope ([], tm) -> of_nominal tm
  | scope -> Error (ScopeEncountered scope)
;;

let rec to_nominal tm =
  match tm with
  | Operator (info, tag, tms) ->
    Nominal.Term.Operator
      (info, tag, List.map tms ~f:(fun tm -> Nominal.Scope.Scope ([], to_nominal tm)))
  | Primitive p -> Primitive p
;;

let pp ppf tm = tm |> to_nominal |> Nominal.Term.pp ppf
let pp_range ppf tm = tm |> to_nominal |> Nominal.Term.pp_range ppf
let hash tm = tm |> to_nominal |> Nominal.Term.hash

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
  module ParsePrim = Primitive.Parse (Comment)

  let term : OptRange.t term ParseUtil.t =
    let open Parsers in
    fix (fun term ->
        choice
          [ (ParsePrim.t >>| fun prim -> Primitive prim)
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

  let whitespace_term = Parsers.(junk *> term)
end
