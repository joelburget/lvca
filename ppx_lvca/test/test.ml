open Lvca_syntax

let test_nominal = [%lvca.nominal "foo(x. x)"]

(* let test_nonbinding = [%lvca.nonbinding "foo(bar(1))"] *)
let test_pattern = [%lvca.pattern "foo(x)"]

let test_language = [%lvca.abstract_syntax {|
foo := foo(integer)
|}]

module List_model : [%lvca.abstract_syntax_module_sig
"list a := Nil() | Cons(a; list a)"] =
[%lvca.abstract_syntax_module
"list a := Nil() | Cons(a; list a)"]

module Lang =
[%lvca.abstract_syntax_module
{|
integer : *
string : *
maybe : * -> *
list : * -> *

foo :=
  | Foo(integer)
  | Bar(foo[foo]. foo. foo)

nat := Z() | S(nat)

pair a b := Pair(a; b)
pair_plus a b := PairPlus(a; b; foo)

nonempty := Nonempty(string; list string)

term := Operator(list term)

mut_a := Mut_a(mut_b)
mut_b := Mut_b(mut_a)
|}]

module Ifz_lang : [%lvca.abstract_syntax_module_sig "ifz := Ifz(ifz; ifz. ifz; ifz)"] =
[%lvca.abstract_syntax_module
"ifz := Ifz(ifz; ifz. ifz; ifz)"]

module List_lang =
[%lvca.abstract_syntax_module
{|
string : *

predefined := Predefined()
list a := Nil() | Cons(a; list a)
list_external := List_external(list string)
list_predefined := List_predefined(list predefined)

list_list_a a := List_list_a(list (list a))
list_list_string_1 := List_list_string_1(list (list string))
list_list_string_2 := List_list_string_2(list_list_a string)
list_list_predefined_1 := List_list_predefined_1(list (list predefined))
list_list_predefined_2 := List_list_predefined_2(list_list_a predefined)
|}]

module type Is_rec_sig = [%lvca.abstract_syntax_module_sig
{|
sort : *
is_rec := Rec() | No_rec()
ty := Sort(sort) | Arrow(ty; ty)
mut_a := Mut_a(mut_b)
mut_b := Mut_b(mut_a)
|}]

module Option_model : [%lvca.abstract_syntax_module_sig "option a := None() | Some(a)"] =
[%lvca.abstract_syntax_module
"option a := None() | Some(a)"]
