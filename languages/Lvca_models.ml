open Lvca_syntax
module Maybe_model = [%lvca.abstract_syntax_module "maybe a := Nothing() | Just(a)"]
module List_model = [%lvca.abstract_syntax_module "list a := Nil() | Cons(a; list a)"]
module Either_model = [%lvca.abstract_syntax_module "either a b := Left(a) | Right(b)"]

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
|}]

module Nonbinding_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *
list : * -> *

term :=
  | Operator(string; list term)
  | Primitive(primitive)
|}]

module Pattern_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *

list a :=
  | Nil()
  | Cons(a; list a)

pattern :=
  | Operator(string; list pattern)
  | Primitive(primitive)
  | Var(string)
  | Ignored(string)
|}]

module Nominal_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *
pattern : *
list : * -> *

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
int32 : *
string : *
primitive : *
list : * -> *
either : * -> * -> *

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
int32 : *
string : *
primitive : *
pattern : *
list : * -> *

term :=
  | Operator(string; list scope)
  | BoundVar(int32)
  | FreeVar(string)
  | Primitive(primitive)

scope := Scope(list pattern; term)
|}]

module Core_model =
[%lvca.abstract_syntax_module
{|
nominal_term : *
string : *
binding_aware_pattern : *
is_rec : *
list : * -> *
option : * -> *
sort : *

type :=
  | Arrow(list type)
  | Sort(sort)

term :=
  | Term(nominal_term)
  | Core_app(term; list term)
  | Case(term; list case_scope)
  | Lambda(type; scope)
  | Let(is_rec; term; option type; scope)

scope := Scope(string; term)

case_scope := Case_scope(binding_aware_pattern; term)
|}]

module Properties (Lang : Nominal.Convertible.S) = struct
  (* TODO: check all generated functions equivalent *)
end
