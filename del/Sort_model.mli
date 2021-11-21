open Lvca_syntax

module Kernel : [%lvca.abstract_syntax_module_sig
{|
string : *
list : * -> *

sort :=
  | Ap(string; list sort)
  | Name(string)
|}
, { string = "Primitive.String"; list = "List_model.List" }]

include Nominal.Convertible.Extended_s with type t = Kernel.Sort.t

val mk_Ap
  :  info:Lvca_syntax.Provenance.t
  -> Lvca_syntax.Primitive.String.t
  -> t List_model.List.t
  -> t

val mk_Name : info:Lvca_syntax.Provenance.t -> Lvca_syntax.Primitive.String.t -> t
val into : Lvca_syntax.Sort.t -> t
val out : t -> Lvca_syntax.Sort.t
