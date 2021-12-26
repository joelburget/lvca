open Lvca_syntax

include
  [%lvca.abstract_syntax_module_sig
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

val into : Lvca_syntax.Pattern.t -> Pattern.t
val out : Pattern.t -> Lvca_syntax.Pattern.t
