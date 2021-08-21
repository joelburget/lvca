(* https://www.haskellforall.com/2021/08/namespaced-de-bruijn-indices.html *)
open Base
open Lvca_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
int32 : *  // module Primitive.Int32
string : *  // module Primitive.String

syntax :=
  | Variable(string; int32)
  | Lambda(string; syntax)
  | Apply(syntax; syntax)
|}]

module Syntax = Lang.Syntax.Plain

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
  | Variable (name, index) ->
    let index =
      if String.(name = namespace) && min_index <= index then index + offset else index
    in
    Variable (name, index)
  | Lambda (name, body) ->
    let min_index = if String.(name = namespace) then min_index + one else min_index in
    let body = shift offset namespace min_index body in
    Lambda (name, body)
  | Apply (f, argument) ->
    let f = shift offset namespace min_index f in
    let argument = shift offset namespace min_index argument in
    Apply (f, argument)
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
  | Variable (name', index') ->
    if String.(name' = name) && Int32.(index' = index) then replacement else expression
  | Lambda (name', body) ->
    let index' = if String.(name = name') then Int32.succ index else index in
    let shifted_body = shift one name' zero replacement in
    let body' = substitute body name index' shifted_body in
    Lambda (name', body')
  | Apply (f, arg) ->
    Apply (substitute f name index replacement, substitute arg name index replacement)
;;

(** β-reduce an expression *)
let rec beta_reduce : Syntax.t -> Syntax.t =
 fun syntax ->
  match syntax with
  | Variable _ -> syntax
  | Lambda (name, body) -> Lambda (name, beta_reduce body)
  | Apply (f, arg) ->
    (match beta_reduce f with
    | Syntax.Lambda (name, body) ->
      let shifted_arg = shift one name zero arg in
      let substituted_body = substitute body name zero shifted_arg in
      let unshifted_body = shift Int32.(-one) name zero substituted_body in
      beta_reduce unshifted_body
    | f' -> Apply (f', beta_reduce arg))
;;

(** α-reduce an expression *)
let rec alpha_reduce : Syntax.t -> Syntax.t =
 fun syntax ->
  match syntax with
  | Variable _ -> syntax
  | Lambda (name, body) ->
    let shifted_body = shift one "_" zero body in
    let substituted_body = substitute shifted_body name zero (Variable ("_", zero)) in
    let unshifted_body = shift Int32.(-one) name zero substituted_body in
    let body' = alpha_reduce unshifted_body in
    Lambda ("_", body')
  | Apply (f, arg) -> Apply (alpha_reduce f, alpha_reduce arg)
;;

(** Returns `True` if the two input expressions are α-equivalent *)
let alpha_equivalent : Syntax.t -> Syntax.t -> bool =
 fun l r -> Syntax.(alpha_reduce l = alpha_reduce r)
;;
