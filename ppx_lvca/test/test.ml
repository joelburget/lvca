open Lvca_syntax

module List_model : [%lvca.abstract_syntax_module_sig
"list a := Nil() | Cons(a; list a)"] =
[%lvca.abstract_syntax_module
"list a := Nil() | Cons(a; list a)"]
