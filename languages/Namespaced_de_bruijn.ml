(* https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html *)
open Base
open Lvca_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
int32 : *
string : *

syntax :=
  | Variable(string; int32)
  | Lambda(string; syntax)
  | Apply(syntax; syntax)
|}
, { int32 = "Primitive.Int32"; string = "Primitive.String" }]

open Lang

let ( + ), ( - ), ( <= ), one, zero = Int32.(( + ), ( - ), ( <= ), one, zero)

(** Increase the index of all bound variables matching the given variable name *)
let rec shift
    :  int32 (** The amount to shift by *)
    -> string (** The variable name to match (a.k.a. the namespace) *)
    -> int32 (** The minimum bound for which indices to shift *)
    -> Syntax.t (** The expression to shift *) -> Syntax.t
  =
 fun offset namespace min_index syntax ->
  match syntax with
  | Variable (info, (name_info, name), (index_info, index)) ->
    let index =
      if String.(name = namespace) && min_index <= index then index + offset else index
    in
    Variable (info, (name_info, name), (index_info, index))
  | Lambda (info, (name_info, name), body) ->
    let min_index = if String.(name = namespace) then min_index + one else min_index in
    let body = shift offset namespace min_index body in
    Lambda (info, (name_info, name), body)
  | Apply (info, f, argument) ->
    let f = shift offset namespace min_index f in
    let argument = shift offset namespace min_index argument in
    Apply (info, f, argument)
;;

(** Substitute the given variable name and index with an expression *)
let rec substitute
    :  Syntax.t (** The expression to substitute into *)
    -> string (** The name of the variable to replace *)
    -> int32 (** The index of the variable to replace *)
    -> Syntax.t (** The expression to substitute in place of the given variable *)
    -> Syntax.t
  =
 fun expression name index replacement ->
  match expression with
  | Variable (_, (_, name'), (_, index')) ->
    if String.(name' = name) && Int32.(index' = index) then replacement else expression
  | Lambda (info, (name'_info, name'), body) ->
    let index' = if String.(name = name') then Int32.succ index else index in
    let shifted_body = shift one name' zero replacement in
    let body' = substitute body name index' shifted_body in
    Lambda (info, (name'_info, name'), body')
  | Apply (info, f, arg) ->
    Apply
      (info, substitute f name index replacement, substitute arg name index replacement)
;;

(** β-reduce an expression *)
let rec beta_reduce : Syntax.t -> Syntax.t =
 fun syntax ->
  match syntax with
  | Variable _ -> syntax
  | Lambda (info, name, body) -> Lambda (info, name, beta_reduce body)
  | Apply (info, f, arg) ->
    (match beta_reduce f with
    | Syntax.Lambda (_, (_, name), body) ->
      let shifted_arg = shift one name zero arg in
      let substituted_body = substitute body name zero shifted_arg in
      let unshifted_body = shift Int32.(-one) name zero substituted_body in
      beta_reduce unshifted_body
    | f' -> Apply (info, f', beta_reduce arg))
;;

(** α-reduce an expression *)
let rec alpha_reduce : Syntax.t -> Syntax.t =
 fun syntax ->
  match syntax with
  | Variable _ -> syntax
  | Lambda (info, (_, name), body) ->
    let shifted_body = shift one "_" zero body in
    let info = Provenance.calculated_here [%here] [ info ] in
    let substituted_body =
      substitute shifted_body name zero (Variable (info, (info, "_"), (info, zero)))
    in
    let unshifted_body = shift Int32.(-one) name zero substituted_body in
    let body' = alpha_reduce unshifted_body in
    Lambda (info, (info, "_"), body')
  | Apply (info, f, arg) -> Apply (info, alpha_reduce f, alpha_reduce arg)
;;

(** Returns `True` if the two input expressions are α-equivalent *)
let alpha_equivalent : Syntax.t -> Syntax.t -> bool =
 fun l r ->
  Nominal.Term.equivalent
    (l |> alpha_reduce |> Syntax.to_nominal)
    (r |> alpha_reduce |> Syntax.to_nominal)
;;
