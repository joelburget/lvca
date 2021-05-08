open Lvca_syntax

let test_nominal = [%lvca_nominal "foo(x. x)"]
let test_nonbinding = [%lvca_nonbinding "foo(bar(1))"]
let test_pattern = [%lvca_pattern "foo(x)"]

let test_language = [%lvca_abstract_syntax {|
foo := foo(integer)
|}]

(* TODO: automate functorizing *)
module Lang =
[%abstract_syntax_module
{|
integer : *
string : *
// TODO: handle / test * -> * externals

// multiple operators, external reference, pattern and var binding
foo :=
  | Foo(integer)
  | Bar(foo[foo]. foo. foo)

nat := Z() | S(nat)

// ignoring sort vars for now.
list a := Nil() | Cons(a; list a)
pair a b := Pair(a; b)
pair_plus a b := PairPlus(a; b; foo)

nonempty := Nonempty(string; list string)

term := Operator(list term)

mut_a := Mut_a(mut_b)
mut_b := Mut_b(mut_a)
|}]
