open Base
open Lvca_syntax

include
  [%lvca.abstract_syntax_module
  {|
string : *
primitive : *
list : * -> *

pattern :=
  | Operator(string; list pattern)
  | Primitive(primitive)
  | Var(string)
  ;
|}
  , { string = "Primitive.String"; primitive = "Primitive.All"; list = "List_model" }]

let rec into tm =
  let info = Provenance.calculated_here [%here] [ Lvca_syntax.Pattern.info tm ] in
  match tm with
  | Lvca_syntax.Pattern.Operator (_, str, tms) ->
    let tms = tms |> List.map ~f:into |> List_model.of_list in
    Pattern.Operator (info, (info, str), tms)
  | Primitive (_, prim) -> Primitive (info, (info, prim))
  | Var (_, str) -> Var (info, (info, str))
;;

let rec out tm =
  let info = Provenance.calculated_here [%here] [ Pattern.info tm ] in
  match tm with
  | Pattern.Operator (_, (_, str), tms) ->
    let tms = tms |> List_model.to_list |> List.map ~f:out in
    Lvca_syntax.Pattern.Operator (info, str, tms)
  | Primitive (_, (_, prim)) -> Primitive (info, prim)
  | Var (_, (_, str)) -> Var (info, str)
;;
