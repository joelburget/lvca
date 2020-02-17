(** Lots of interesting domains have no binding. At that point they're not
    really languages, just data types. This module gives a tighter
    representation for such types and allows conversion to / from binding types.
*)
open Core_kernel

open Types
open Binding

type term =
  | Operator of string * term list
  | Sequence of term list
  | Primitive of primitive

exception ScopeEncountered

(* TODO: rename to from_de_bruijn_exn *)
(* raises ScopeEncountered *)
let rec from_de_bruijn' = function
  | DeBruijn.Operator (tag, scopes) ->
    Operator (tag, List.map scopes ~f:from_de_bruijn_scope)
  | Var _ -> raise ScopeEncountered
  | Sequence tms -> Sequence (tms |> List.map ~f:from_de_bruijn')
  | Primitive p -> Primitive p

(* raises ScopeEncountered *)
and from_de_bruijn_scope = function
  | DeBruijn.Scope ([], tm) -> from_de_bruijn' tm
  | _ -> raise ScopeEncountered
;;

let from_de_bruijn (tm : DeBruijn.term) : term option =
  try Some (from_de_bruijn' tm) with
  | ScopeEncountered -> None
;;

let rec to_de_bruijn tm : DeBruijn.term =
  match tm with
  | Operator (tag, tms) ->
    DeBruijn.Operator
      (tag, tms |> List.map ~f:(fun tm -> DeBruijn.Scope ([], to_de_bruijn tm)))
  | Sequence tms -> Sequence (tms |> List.map ~f:to_de_bruijn)
  | Primitive p -> Primitive p
;;

(* TODO: rename to from_nominal_exn *)
(* raises ScopeEncountered *)
let rec from_nominal' = function
  | Nominal.Operator (tag, scopes) -> Operator (tag, scopes |> List.map ~f:from_nominal_scope)
  | Var _ -> raise ScopeEncountered
  | Sequence tms -> Sequence (tms |> List.map ~f:from_nominal')
  | Primitive p -> Primitive p

(* raises ScopeEncountered *)
and from_nominal_scope = function
  | Nominal.Scope ([], tm) -> from_nominal' tm
  | _ -> raise ScopeEncountered
;;

let from_nominal (tm : Nominal.term) : term option =
  try Some (from_nominal' tm) with
  | ScopeEncountered -> None
;;

let rec to_nominal tm : Nominal.term =
  match tm with
  | Operator (tag, tms) ->
    Nominal.Operator (tag, tms |> List.map ~f:(fun tm -> Nominal.Scope ([], to_nominal tm)))
  | Sequence tms -> Sequence (tms |> List.map ~f:to_nominal)
  | Primitive p -> Primitive p
;;

let to_string tm : string
  = tm |> to_nominal |> Nominal.pp_term'
