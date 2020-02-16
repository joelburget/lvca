let abstractSyntax = LanguageInteger.abstractSyntax;;
let concreteSyntax = LanguageInteger.concreteSyntax;;

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
