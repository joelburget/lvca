open Lvca_syntax

module Prelude =
[%lvca.abstract_syntax_module
{|
list a :=
  | Nil()
  | Cons(a; list a)

either a b :=
  | Left(a)
  | Right(b)
|}]

module Primitive_model =
[%lvca.abstract_syntax_module
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

(* TODO: list : * -> * *)
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

(* TODO: list : * -> * *)
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

(* TODO: list : * -> * *)
module Binding_aware_pattern_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *
list : * -> *

t :=
  | Operator(string; list scope)
  | Primitive(primitive)
  | Var(string)
  | Ignored(string)

scope := Scope(list string; t)
|}]

(* TODO: list : * -> * *)
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

(*
TODO: list : * -> *
TODO: either : * -> *
TODO | Operator(string; list (either scope term))
*)
module DeBruijn_model =
[%lvca.abstract_syntax_module
{|
int32 : *
string : *
primitive : *

term :=
  | BoundVar(int32)
  | FreeVar(string)
  | Primitive(primitive)

scope := Scope(string; term)
|}]

(* TODO: list : * -> * *)
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

module Properties (Lang : Language_object_intf.S) = struct
  (* check all generated functions equivalent *)
end

(* TODO: core, bidirectional *)
