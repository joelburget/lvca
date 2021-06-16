module Lang =
[%lvca.abstract_syntax_module
{|
typ := Nat() | Parr(typ; typ)

exp :=
  | Z()
  | S(exp)
  | Ifz(exp; exp. exp; exp)
  | Lam(typ; exp. exp)
  | Ap(exp; exp)
  | Fix(typ; exp. exp)
|}]
