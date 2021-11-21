open Base
open Lvca_syntax

module Kernel =
[%lvca.abstract_syntax_module
{|
string : *
list : * -> *

sort :=
  | Ap(string; list sort)
  | Name(string)
|}
, { string = "Primitive.String"; list = "List_model.List" }]

let rec into tm =
  let info = Provenance.calculated_here [%here] [ Lvca_syntax.Sort.info tm ] in
  match tm with
  | Lvca_syntax.Sort.Ap (_, name, lst) ->
    let lst = lst |> List.map ~f:into |> List_model.of_list in
    Kernel.Sort.Ap (info, (info, name), lst)
  | Name (_, name) -> Name (info, (info, name))
;;

let rec out tm =
  let info = Provenance.calculated_here [%here] [ Kernel.Sort.info tm ] in
  match tm with
  | Kernel.Sort.Ap (_, (_, name), lst) ->
    let lst = lst |> List_model.to_list |> List.map ~f:out in
    Lvca_syntax.Sort.Ap (info, name, lst)
  | Name (_, (_, name)) -> Name (info, name)
;;

include Kernel.Sort
include Nominal.Convertible.Extend (Kernel.Sort)
