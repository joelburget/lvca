open Base
open Lvca_syntax

include
  [%lvca.abstract_syntax_module
  {|
string : *
primitive : *
list : * -> *

pattern :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)

scope := Scope(list string; pattern)
|}
  , { string = "Primitive.String"; primitive = "Primitive.All"; list = "List_model" }]

let rec into tm =
  let info = Provenance.calculated_here [%here] [ Binding_aware_pattern.info tm ] in
  match tm with
  | Binding_aware_pattern.Operator (_, str, scopes) ->
    let scopes = scopes |> List.map ~f:scope |> List_model.of_list in
    Pattern.Operator (info, (info, str), scopes)
  | Primitive (_, prim) -> Primitive (info, (info, prim))
  | Var (_, str) -> Var (info, (info, str))

and scope (Binding_aware_pattern.Scope (names, pat)) =
  Scope.Scope (Provenance.of_here [%here], List_model.of_list names, into pat)
;;

let rec out tm =
  let info = Provenance.calculated_here [%here] [ Pattern.info tm ] in
  match tm with
  | Pattern.Operator (_, (_, str), scopes) ->
    let scopes = scopes |> List_model.to_list |> List.map ~f:scope in
    Binding_aware_pattern.Operator (info, str, scopes)
  | Primitive (_, (_, prim)) -> Primitive (info, prim)
  | Var (_, (_, str)) -> Var (info, str)

and scope (Scope.Scope (_, names, pat)) =
  Binding_aware_pattern.Scope (List_model.to_list names, out pat)
;;
