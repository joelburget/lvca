let abstractSyntax = {|
term :=
  | lam(term. term)
  | app(term; term)
|}
;;

let concreteSyntax = {|
LAMBDA := "\\"
ARROW  := "->"
LPAREN := "("
RPAREN := ")"

term :=
  | term _ term                   { app($1; $3) }
  | LAMBDA _0 term _ ARROW _ term { lam($2; $6) }
  | LPAREN _0 term _0 RPAREN      { $2 }
|}
;;
