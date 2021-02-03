open Lvca_syntax

let abstract_syntax =
  [%lvca_abstract_syntax
    {|
exp :=
  | num(integer) // harper uses num[n]
  | zero()       // harper uses zero
  | succ(exp)
  | ifz(exp; exp. exp; exp) // harper uses ifz{d_0; x. d_1}(d)
  | fun(exp. exp)
  | ap(exp; exp)
  | fix(exp. exp)
|}]
;;
