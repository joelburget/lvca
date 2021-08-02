open Lvca_syntax
module Option_model = Lvca_core.Option_model
module List_model = Lvca_core.List_model
module Either_model = [%lvca.abstract_syntax_module "either a b := Left(a) | Right(b)"]

module Primitive_model =
[%lvca.abstract_syntax_module
{|
integer : *  // module Primitive.Integer
int32 : *  // module Primitive.Int32
string : *  // module Primitive.String
float : *  // module Primitive.Float
char : *  // module Primitive.Char

primitive :=
  | Integer(integer)
  | Int32(int32)
  | String(string)
  | Float(float)
  | Char(char)
|}]

module Nonbinding_model =
[%lvca.abstract_syntax_module
{|
string : *  // module Primitive.String
primitive : *  // module Primitive.All
list : * -> *  // module List_model.List

term :=
  | Operator(string; list term)
  | Primitive(primitive)
|}]

module Pattern_model =
[%lvca.abstract_syntax_module
{|
string : *  // module Primitive.String
primitive : *  // module Primitive.All
list : * -> *  // module List_model.List

pattern :=
  | Operator(string; list pattern)
  | Primitive(primitive)
  | Var(string)
  | Ignored(string)
|}]

module Nominal_model =
[%lvca.abstract_syntax_module
{|
list : * -> *  // module List_model.List
pattern : *  // module Pattern_model.Pattern
primitive : *  // module Primitive.All
string : *  // module Primitive.String

term :=
  | Operator(string; list scope)
  | Var(string)
  | Primitive(primitive)

scope := Scope(list pattern; term)
|}]

(* TODO: this should fail if we remove the declaration of either. *)
module DeBruijn_model =
[%lvca.abstract_syntax_module
{|
either : * -> * -> *  // module Either_model.Either
int32 : *  // module Primitive.Int32
list : * -> *  // module List_model.List
primitive : *  // module Primitive.All
string : *  // module Primitive.String

term :=
  | BoundVar(int32)
  | FreeVar(string)
  | Primitive(primitive)
  | Operator(string; list (either scope term))

scope := Scope(string; term)
|}]

module DeBruijn_2d_model =
[%lvca.abstract_syntax_module
{|
int32 : *  // module Primitive.Int32
list : * -> *  // module List_model.List
pattern : *  // module Pattern_model.Pattern
primitive : *  // module Primitive.All
string : *  // module Primitive.String

term :=
  | Operator(string; list scope)
  | BoundVar(int32)
  | FreeVar(string)
  | Primitive(primitive)

scope := Scope(list pattern; term)
|}]

module Properties (Lang : Nominal.Convertible.S) = struct
  (* TODO: check all generated functions equivalent *)
end
