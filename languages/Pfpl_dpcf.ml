open Lvca_syntax

let abstract_syntax =
  [%lvca.abstract_syntax
    {|
exp :=
  | num(integer)
  | zero()
  | succ(exp)
  | ifz(exp; exp. exp; exp)
  | fun(exp. exp)
  | ap(exp; exp)
  | fix(exp. exp)
|}]
;;
