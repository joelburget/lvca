open Base
open Lvca_provenance
open Lvca_util
open Result.Let_syntax

type 'info term =
  | Operator of 'info * string * 'info term list
  | Primitive of 'info Primitive.All.t

let rec equal ~info_eq t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equal ~info_eq) scopes1 scopes2
  | Primitive i1, Primitive i2 -> Primitive.All.equal ~info_eq i1 i2
  | _, _ -> false
;;

let info = function Operator (i, _, _) -> i | Primitive p -> Primitive.All.info p

let rec map_info ~f = function
  | Operator (i, name, tms) -> Operator (f i, name, List.map tms ~f:(map_info ~f))
  | Primitive prim -> Primitive (Primitive.All.map_info ~f prim)
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
let pp_opt_range ppf tm = tm |> to_nominal |> Nominal.Term.pp_opt_range ppf
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

module Parse = struct
  open Lvca_parsing

  let term : Opt_range.t term Lvca_parsing.t =
    fix (fun term ->
        choice
          ~failure_msg:"looking for a primitive or identifier (for a var or operator)"
          [ (Primitive.All.Parse.t >>| fun prim -> Primitive prim)
          ; (identifier
            >>== fun Parse_result.{ value = ident; range = start; _ } ->
            parens (sep_end_by (char ';') term)
            >>|| (fun Parse_result.{ value = children; range = finish } ->
                   let pos = Opt_range.union start finish in
                   Parse_result.{ value = Operator (pos, ident, children); range = pos })
            <?> "term body")
          ])
    <?> "term"
  ;;
end

module type Convertible_s = sig
  include Language_object_intf.S with type 'info t = 'info term

  val of_nonbinding : 'info term -> ('info t, 'info term) Result.t
  val to_nonbinding : 'info t -> 'info term
end

module Json = struct
  open Option.Let_syntax
  open Json

  let rec jsonify = function
    | Operator (_, tag, tms) -> array [| string "o"; string tag; array_map jsonify tms |]
    | Primitive p -> array [| string "p"; Primitive.All.jsonify p |]
  ;;

  let rec unjsonify = function
    | Array [| String "o"; String tag; Array tms |] ->
      let%map tms = tms |> Array.to_list |> List.map ~f:unjsonify |> Option.all in
      Operator ((), tag, tms)
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive.All.unjsonify prim in
      Primitive prim
    | _ -> None
  ;;
end

include Json
