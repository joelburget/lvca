open Lvca_syntax

include
  [%lvca.abstract_syntax_module_sig
{|
string : *
primitive : *
list : * -> *

pattern :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)
  ;

scope := Scope(list string; pattern);
|}
, { string = "Primitive.String"; primitive = "Primitive.All"; list = "List_model" }]

val into : Lvca_syntax.Binding_aware_pattern.t -> Pattern.t
val out : Pattern.t -> Lvca_syntax.Binding_aware_pattern.t
