module Lang =
[%lvca.abstract_syntax_module
{|
exp :=
  | Zero()
  | Succ(exp)
  | Ifz(exp; exp. exp; exp)
  | Fun(exp. exp)
  | Ap(exp; exp)
  | Fix(exp. exp)
|}]
