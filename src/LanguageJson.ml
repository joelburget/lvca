let abstractSyntax = {|
arith :=
  | add(arith; arith)
  | sub(arith; arith)
  | mul(arith; arith)
  | div(arith; arith)
  | app(arith; arith)
  | fun(arith. arith)
|}
;;

let concreteSyntax = {|
  ARR    := "->"
  ADD    := "+"
  SUB    := "-"
  MUL    := "*"
  DIV    := "/"
  LPAREN := "("
  RPAREN := ")"
  NAME   := /[a-z][a-zA-Z0-9]*/

  arith :=
    | arith _ ADD _ arith { add($1; $3) } %left
    | arith _ SUB _ arith { sub($1; $3) } %left
    | arith _ MUL _ arith { mul($1; $3) } %left
    | arith _ DIV _ arith { div($1; $3) } %left
    | arith _       arith { app($1; $2) } %right
    | NAME  _ ARR _ arith { fun($1. $3) }
    > NAME                { var($1)     }
    | LPAREN arith RPAREN { $2          }
  |}
;;

let concreteSyntax' = {|
TRUE     := "true"
FALSE    := "false"
NULL     := "null"
// LBRACE   := "{"
// RBRACE   := "}"
// LBRACKET := "["
// RBRACKET := "]"
// COLON    := ":"
// COMMA    := ","
STRING   := /"([^"]+|\\")*"/
NUMBER   := /-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/

json :=
  | NULL { null() }
  | bool { bool($1) }
  | STRING { string($1) }
  | NUMBER { number($1) }
//   | LBRACE _0 kv_pairs _0 RBRACE { object($3) }
//   | LBRACKET _0 list _0 RBRACKET { list($3) }

// kv := STRING _0 COLON _ json { kv($1; $3) }

// kv_pairs :=
//   | kv _0 COMMA _ kv_pairs { cons($1; $3)           }
//   | kv                     { cons($1; empty_list()) }
//
// list :=
//   | json _0 COMMA _ list { cons($1; $3)           }
//   | json                 { cons($1; empty_list()) }

bool :=
  | TRUE  { true()  }
  | FALSE { false() }
|}
;;
