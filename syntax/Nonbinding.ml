open Base
open Lvca_provenance
open Lvca_util
open Result.Let_syntax

type term =
  | Operator of Provenance.t * string * term list
  | Primitive of Primitive.All.t

let rec equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
  match t1, t2 with
  | Operator (i1, name1, scopes1), Operator (i2, name2, scopes2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (equivalent ~info_eq) scopes1 scopes2
  | Primitive i1, Primitive i2 -> Primitive.All.equivalent ~info_eq i1 i2
  | _, _ -> false
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )
(* let info = function Operator (i, _, _) -> i | Primitive p -> Primitive.All.info p *)

type de_bruijn_conversion_error =
  | Scope_encountered of DeBruijn.scope
  | Var_encountered of DeBruijn.term

let rec of_de_bruijn tm =
  match tm with
  | DeBruijn.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_de_bruijn_scope |> Result.all in
    Operator (a, tag, scopes')
  | Bound_var _ | Free_var _ -> Error (Var_encountered tm)
  | Primitive p -> Ok (Primitive p)

and of_de_bruijn_scope = function
  | First scope -> Error (Scope_encountered scope)
  | Second tm -> of_de_bruijn tm
;;

let rec to_de_bruijn tm =
  match tm with
  | Operator (info, tag, tms) ->
    DeBruijn.Operator
      (info, tag, List.map tms ~f:(fun tm -> Either.Second (to_de_bruijn tm)))
  | Primitive p -> Primitive p
;;

type nominal_conversion_error =
  | Scope_encountered of Nominal.Scope.t
  | Var_encountered of Nominal.Term.t

let rec of_nominal tm =
  match tm with
  | Nominal.Term.Operator (a, tag, scopes) ->
    let%map scopes' = scopes |> List.map ~f:of_nominal_scope |> Result.all in
    Operator (a, tag, scopes')
  | Var _ -> Error (Var_encountered tm)
  | Primitive p -> Ok (Primitive p)

and of_nominal_scope = function
  | Nominal.Scope.Scope ([], tm) -> of_nominal tm
  | scope -> Error (Scope_encountered scope)
;;

let rec to_nominal tm =
  match tm with
  | Operator (info, tag, tms) ->
    Nominal.Term.Operator
      (info, tag, List.map tms ~f:(fun tm -> Nominal.Scope.Scope ([], to_nominal tm)))
  | Primitive p -> Primitive p
;;

let pp ppf tm = tm |> to_nominal |> Nominal.Term.pp ppf
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

let parse =
  let open Lvca_parsing in
  fix (fun term ->
      choice
        ~failure_msg:"looking for a primitive or identifier (for a var or operator)"
        [ (Primitive.All.parse >>| fun prim -> Primitive prim)
        ; (Ws.identifier
          >>== fun Parse_result.{ value = ident; range = start; _ } ->
          Ws.parens (sep_end_by (Ws.char ';') term)
          >>|| (fun { value = children; range = finish } ->
                 let range = Opt_range.union start finish in
                 Parse_result.
                   { value = Operator (Provenance.of_range range, ident, children)
                   ; range
                   })
          <?> "term body")
        ])
  <?> "term"
;;

module type Convertible_s = sig
  include Language_object_intf.S with type t = term

  val of_nonbinding : term -> (t, term) Result.t
  val to_nonbinding : t -> term
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
      Operator (Provenance.of_here [%here], tag, tms)
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive.All.unjsonify prim in
      Primitive prim
    | _ -> None
  ;;
end

include Json
