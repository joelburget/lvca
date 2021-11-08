open Lvca_syntax

module Test4 =
[%lvca.abstract_syntax_module
{|
term: *

list := Nil() | Cons(term; list)
|}, { term = "Nominal.Term" }]

(* let test_nonbinding = [%lvca.nonbinding "foo(bar(1))"] *)
let test_pattern = [%lvca.pattern "Foo(x)"]

let test_language = [%lvca.abstract_syntax {|
foo := Foo(integer)
|}]

module List_model : [%lvca.abstract_syntax_module_sig
"list a := Nil() | Cons(a; list a)"] =
[%lvca.abstract_syntax_module
"list a := Nil() | Cons(a; list a)"]

module List = struct
  type 'a t

  let to_nominal _ _ = Nominal.Term.Var (failwith "no provenance", "")
  let of_nominal _ tm = Error (Nominal.Conversion_error.mk_Term tm)
  let equivalent _a ~info_eq:_ _ _ = true
end

module Maybe = List

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
|}
, { integer = "Primitive.Integer"
  ; string = "Primitive.String"
  ; maybe = "Maybe"
  ; list = "List"
  }]

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
|}
, { string = "Nominal.Term" }]

module type Is_rec_sig = [%lvca.abstract_syntax_module_sig
{|
sort : *
is_rec := Rec() | No_rec()
ty := Sort(sort) | Arrow(ty; ty)
mut_a := Mut_a(mut_b)
mut_b := Mut_b(mut_a)
|}
, { sort = "Sort" }]

module Option_model : [%lvca.abstract_syntax_module_sig "option a := None() | Some(a)"] =
[%lvca.abstract_syntax_module
"option a := None() | Some(a)"]

module Empty : [%lvca.abstract_syntax_module_sig "empty :="] =
[%lvca.abstract_syntax_module
"empty :="]

module Empty_as_var =
[%lvca.abstract_syntax_module
{|
list a := Nil() | Cons(a; list a)
empty :=
foo :=
  | Foo(empty. empty)
  | Bar((list empty)[foo]. foo)
|}]
