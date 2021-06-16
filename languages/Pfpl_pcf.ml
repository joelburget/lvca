let abstract_syntax =
  [%lvca.abstract_syntax
    {|
typ := nat() | parr(typ; typ)

exp :=
  | z()
  | s(exp)
  | ifz(exp; exp. exp; exp)
  | lam(typ; exp. exp)
  | ap(exp; exp)
  | fix(typ; exp. exp)
|}]
;;
