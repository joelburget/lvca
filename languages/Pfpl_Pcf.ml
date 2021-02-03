open Lvca_syntax

let abstract_syntax =
  [%lvca_abstract_syntax
    {|
typ := nat() | parr(typ; typ) // harper writes `nat | parr(t_1; t_2)`

exp :=
  | z()
  | s(exp)
  | ifz(exp; exp. exp; exp)
  | lam(typ; exp. exp)
  | ap(exp; exp)
  | fix(typ; exp. exp)
|}]
;;
