open Lvca_syntax
module Option_model = Lvca_del.Option_model
module List_model = Lvca_del.List_model
module Either_model = [%lvca.abstract_syntax_module "either a b := Left(a) | Right(b);"]

module Primitive_model =
[%lvca.abstract_syntax_module
{|
integer : *
int32 : *
string : *
float : *
char : *

primitive :=
  | Integer(integer)
  | Int32(int32)
  | String(string)
  | Float(float)
  | Char(char)
  ;
|}
, { integer = "Primitive.Integer"
  ; int32 = "Primitive.Int32"
  ; string = "Primitive.String"
  ; float = "Primitive.Float"
  ; char = "Primitive.Char"
  }]

module Nonbinding_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *
list : * -> *

term :=
  | Operator(string; list term)
  | Primitive(primitive)
  ;
|}
, { string = "Primitive.String"; primitive = "Primitive.All"; list = "List_model" }]

module Pattern_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *
list : * -> *

pattern :=
  | Operator(string; list pattern)
  | Primitive(primitive)
  | Var(string)
  | Ignored(string)
  ;
|}
, { string = "Primitive.String"; primitive = "Primitive.All"; list = "List_model" }]

module Nominal_model =
[%lvca.abstract_syntax_module
{|
list : * -> *
pattern : *
primitive : *
string : *

term :=
  | Operator(string; list scope)
  | Var(string)
  | Primitive(primitive)
  ;

scope := Scope(list pattern; term);
|}
, { list = "List_model"
  ; pattern = "Pattern_model.Pattern"
  ; primitive = "Primitive.All"
  ; string = "Primitive.String"
  }]

(* TODO: this should fail if we remove the declaration of either. *)
module DeBruijn_model =
[%lvca.abstract_syntax_module
{|
either : * -> * -> *
int32 : *
list : * -> *
primitive : *
string : *

term :=
  | BoundVar(int32)
  | FreeVar(string)
  | Primitive(primitive)
  | Operator(string; list (either scope term))
  ;

scope := Scope(string; term);
|}
, { either = "Either_model.Either"
  ; int32 = "Primitive.Int32"
  ; list = "List_model"
  ; primitive = "Primitive.All"
  ; string = "Primitive.String"
  }]

module DeBruijn_2d_model =
[%lvca.abstract_syntax_module
{|
int32 : *
list : * -> *
pattern : *
primitive : *
string : *

term :=
  | Operator(string; list scope)
  | BoundVar(int32)
  | FreeVar(string)
  | Primitive(primitive)
  ;

scope := Scope(list pattern; term);
|}
, { int32 = "Primitive.Int32"
  ; list = "List_model"
  ; pattern = "Pattern_model.Pattern"
  ; primitive = "Primitive.All"
  ; string = "Primitive.String"
  }]

module Properties (Lang : Nominal.Convertible.S) = struct
  (* TODO: check all generated functions equivalent *)
end
