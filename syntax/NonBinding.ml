open Base

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) term list list
  | Primitive of 'loc * 'prim

let location = function Operator (loc, _, _) | Primitive (loc, _) -> loc

exception ScopeEncountered

(** @raise ScopeEncountered *)
let rec of_de_bruijn_exn = function
  | DeBruijn.Operator (a, tag, scopes) ->
    Operator (a, tag, List.map scopes ~f:of_de_bruijn_scope)
  | BoundVar _ | FreeVar _ -> raise ScopeEncountered
  | Primitive (a, p) -> Primitive (a, p)

(** @raise ScopeEncountered *)
and of_de_bruijn_scope = function
  | First _scopt -> raise ScopeEncountered
  | Second tms -> List.map tms ~f:of_de_bruijn_exn
;;

let of_de_bruijn tm (* (tm : 'a DeBruijn.term) : 'a term option *) =
  try Some (of_de_bruijn_exn tm) with ScopeEncountered -> None
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

let rec of_nominal_exn = function
  | Nominal.Operator (a, tag, scopes) ->
    Operator (a, tag, List.map scopes ~f:of_nominal_scope)
  | Var _ -> raise ScopeEncountered
  | Primitive (a, p) -> Primitive (a, p)

(** @raise ScopeEncountered *)
and of_nominal_scope = function
  | Nominal.Scope ([], tms) -> List.map tms ~f:of_nominal_exn
  | _ -> raise ScopeEncountered
;;

let of_nominal (* (tm : ('a, Primitive.t) Nominal.term) : 'a term option *) tm =
  try Some (of_nominal_exn tm) with ScopeEncountered -> None
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
