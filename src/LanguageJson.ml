let abstractSyntax, concreteSyntax = LanguageInteger.(abstractSyntax, concreteSyntax)

let concreteSyntax' =
  {|
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
  | x = bool { bool(x) }
  | x = STRING { string(x) }
  | x = NUMBER { number(x) }
//   | LBRACE _0 x = kv_pairs _0 RBRACE { object(x) }
//   | LBRACKET _0 x = list _0 RBRACKET { list(x) }

// kv := k = STRING _0 COLON _ v = json { kv(k; v) }

// kv_pairs :=
//   | kv = kv _0 COMMA _ kvs = kv_pairs { cons(kv; kvs) }
//   |                                   { empty_list() }
//
// list :=
//   | x = json _0 COMMA _ xs = list { cons(x; xs)  }
//   |                               { empty_list() }

bool :=
  | TRUE  { true()  }
  | FALSE { false() }
|}
;;
