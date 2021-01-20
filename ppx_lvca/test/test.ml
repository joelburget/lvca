open Lvca_syntax

let test_term = [%lvca_term "foo(x. x)"]
let test_pattern = [%lvca_pattern "foo(x)"]

let test_language = [%lvca_abstract_syntax {|
foo := foo(integer)
|}]
