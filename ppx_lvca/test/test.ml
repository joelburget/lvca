open Lvca_syntax

let test_nominal = [%lvca_nominal "foo(x. x)"]
let test_nonbinding = [%lvca_nonbinding "foo(bar(1))"]
let test_pattern = [%lvca_pattern "foo(x)"]

let test_language = [%lvca_abstract_syntax {|
foo := foo(integer)
|}]

module Lang =
[%abstract_syntax_module
{|
integer : *
// TODO: handle / test * -> * externals

// multiple operators, external reference, pattern and var binding
foo :=
  | Foo(integer)
  | Bar(foo[foo]. foo. foo)

nat := Z() | S(nat)

list a := Nil() | Cons(a; list(a))

pair a b := Pair(a; b)

// multiple slots, parameters
// TODO: quux a b := Quux(a; b; foo)
// TODO: test mutual definitions
|}]
