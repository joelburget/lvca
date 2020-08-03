open Base

open Binding

type 'a term =
  | Operator of 'a * string * 'a term list list
  | Primitive of 'a * Primitive.t

exception ScopeEncountered

(** @raise ScopeEncountered *)
let rec from_de_bruijn_exn = function
  | DeBruijn.Operator (a, tag, scopes) ->
    Operator (a, tag, List.map scopes ~f:from_de_bruijn_scope)
  | Var _ -> raise ScopeEncountered
  | Primitive (a, p) -> Primitive (a, p)

(** @raise ScopeEncountered *)
and from_de_bruijn_scope = function
  | DeBruijn.Scope (_, [], tms) -> List.map ~f:from_de_bruijn_exn tms
  | _ -> raise ScopeEncountered
;;

let from_de_bruijn (tm : 'a DeBruijn.term) : 'a term option =
  try Some (from_de_bruijn_exn tm) with ScopeEncountered -> None
;;

let rec to_de_bruijn tm : unit DeBruijn.term =
  match tm with
  | Operator (_, tag, tms) ->
    DeBruijn.Operator
      ( ()
      , tag
      , List.map tms ~f:(fun tms' ->
        DeBruijn.Scope ((), [], List.map tms' ~f:to_de_bruijn))
      )
  | Primitive (_, p) -> Primitive ((), p)
;;

let rec from_nominal_exn = function
  | Nominal.Operator (a, tag, scopes) ->
    Operator (a, tag, List.map scopes ~f:from_nominal_scope)
  | Var _ -> raise ScopeEncountered
  | Primitive (a, p) -> Primitive (a, p)

(** @raise ScopeEncountered *)
and from_nominal_scope = function
  | Nominal.Scope (_, [], tms) -> List.map tms ~f:from_nominal_exn
  | _ -> raise ScopeEncountered
;;

let from_nominal (tm : 'a Nominal.term) : 'a term option =
  try Some (from_nominal_exn tm) with ScopeEncountered -> None
;;

let rec to_nominal tm : unit Nominal.term =
  match tm with
  | Operator (_, tag, tms) ->
    Nominal.Operator
      ( ()
      , tag
      , List.map tms ~f:(fun tms' ->
          Nominal.Scope ((), [], List.map tms' ~f:to_nominal)))
  | Primitive (_, p) -> Primitive ((), p)
;;

let pp ppf tm = tm |> to_nominal |> Nominal.pp_term ppf
let to_string tm = tm |> to_nominal |> Nominal.pp_term_str
