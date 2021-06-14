open Lvca_syntax
module Maybe_model = [%lvca.abstract_syntax_module "maybe a := Nothing() | Just(a)"]
module List_model = [%lvca.abstract_syntax_module "list a := Nil() | Cons(a; list a)"]
module Either_model = [%lvca.abstract_syntax_module "either a b := Left(a) | Right(b)"]

module Mk_primitive_model =
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

module Primitive_model =
  Mk_primitive_model (Primitive.Integer) (Primitive.Int32) (Primitive.String)
    (Primitive.Float)
    (Primitive.Char)

module Mk_nonbinding_model =
[%lvca.abstract_syntax_module
{|
string : *
primitive : *
list : * -> *

term :=
  | Operator(string; list term)
  | Primitive(primitive)
|}]

(*
module Nonbinding_model =
  Mk_nonbinding_model (Primitive.String) (Primitive.All) (List_model.List)
*)

module Mk_pattern_model =
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

(* module Pattern_model = Mk_pattern_model (Primitive.String) (Primitive) *)

module Mk_binding_aware_pattern_model =
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

(*
module Binding_aware_pattern_model =
  Mk_binding_aware_pattern_model (Primitive.String) (Primitive) (List)
*)

module Mk_nominal_model =
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
module Nominal_model =
  Mk_nominal_model (Primitive.String) (Primitive) (Pattern) (List_model)
*)

(* TODO: this should fail if we remove the declaration of either. *)
module Mk_DeBruijn_model =
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

(*
module DeBruijn_model =
  Mk_DeBruijn_model (Primitive.Int32) (Primitive.String) (Primitive) (List) (Either)
*)

module Mk_DeBruijn_2d_model =
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

(*
module DeBruijn_2d_model =
  Mk_DeBruijn_2d_model (Primitive.Int32) (Primitive.String) (Primitive) (Pattern) (List)
*)

module Properties (Lang : Language_object_intf.S) = struct
  (* check all generated functions equivalent *)
end

(* TODO: core, bidirectional *)
