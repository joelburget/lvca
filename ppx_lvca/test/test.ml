open Lvca_syntax

let test_nominal = [%lvca.nominal "foo(x. x)"]
let test_nonbinding = [%lvca.nonbinding "foo(bar(1))"]
let test_pattern = [%lvca.pattern "foo(x)"]

let test_language = [%lvca.abstract_syntax {|
foo := foo(integer)
|}]

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
ifz := Ifz(ifz; ifz. ifz; ifz)
|}]

module type Is_rec_sig = [%lvca.abstract_syntax_module_sig {|is_rec := Rec() | No_rec()|}]
