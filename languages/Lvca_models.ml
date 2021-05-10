open Lvca_syntax

module Prelude =
[%abstract_syntax_module
{|
list a :=
  | Nil()
  | Cons(a; list a)

either a b :=
  | Left(a)
  | Right(b)
|}]

module Primitive =
[%abstract_syntax_module
{|
integer : *
string : *
float : *
char : *

primitive :=
  | Integer(integer)
  | String(string)
  | Float(float)
  | Char(char)
|}]

module NonBinding =
[%abstract_syntax_module
{|
string : *
primitive : *
// TODO: list : * -> *

list a :=
  | Nil()
  | Cons(a; list a)

term :=
  | Operator(string; list term)
  | Primitive(primitive)
|}]

module Pattern =
[%abstract_syntax_module
{|
string : *
primitive : *
// TODO: list : * -> *

list a :=
  | Nil()
  | Cons(a; list a)

pattern :=
  | Operator(string; list pattern)
  | Primitive(primitive)
  | Var(string)
  | Ignored(string)
|}]

module BindingAwarePattern =
[%abstract_syntax_module
{|
string : *
primitive : *
// TODO: list : * -> *

list a :=
  | Nil()
  | Cons(a; list a)

t :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)
  | Ignored(string)

scope := Scope(list string; t)
|}]

module Nominal =
[%abstract_syntax_module
{|
string : *
primitive : *
pattern : *
// TODO: list : * -> *

list a :=
  | Nil()
  | Cons(a; list a)

term :=
  | Operator(string; list scope)
  | Var(string)
  | Primitive(primitive)

scope := Scope(list pattern; term)
|}]

module DeBruijn =
[%abstract_syntax_module
{|
int : *
string : *
primitive : *
// TODO: list : * -> *
// TODO: either : * -> *

term :=
  // TODO | Operator(string; list (either scope term))
  | BoundVar(int)
  | FreeVar(string)
  | Primitive(primitive)

scope := Scope(string; term)
|}]

module DeBruijn2d =
[%abstract_syntax_module
{|
int : *
string : *
primitive : *
pattern : *
// TODO list : * -> *

list a :=
  | Nil()
  | Cons(a; list a)

term :=
  | Operator(string; list scope)
  | BoundVar(int)
  | FreeVar(string)
  | Primitive(primitive)

scope := Scope(list pattern; term)
|}]

module Properties (Lang : LanguageObject_intf.S) = struct
  (* check all generated functions equivalent *)
end

(* TODO: core, bidirectional *)
