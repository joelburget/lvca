let abstract_syntax = {|
term :=
  | lam(term. term)
  | app(term; term)
|}

let concrete_syntax =
  {|
LAMBDA := "\\"
ARROW  := "->"
LPAREN := "("
RPAREN := ")"

term :=
  | t1 = term _ t2 = term                      { app(t1; t2) }
  | LAMBDA _0 pat = term _ ARROW _ body = term { lam(pat; body) }
  | LPAREN _0 term _0 RPAREN                   { term }
|}
;;
