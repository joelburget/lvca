open Lvca_syntax

let test_nominal = [%lvca_nominal "foo(x. x)"]
let test_nonbinding = [%lvca_nonbinding "foo(bar(1))"]
let test_pattern = [%lvca_pattern "foo(x)"]

let test_language = [%lvca_abstract_syntax {|
foo := foo(integer)
|}]
