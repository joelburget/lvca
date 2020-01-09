let abstractSyntax = {|
import {list, string} from "builtin"

document := document(list block)

block :=
  | header(headerLevel; string)
  | paragraph(inline)

headerLevel :=
  | h1()
  | h2()
  | h3()

inline := inline(list inlineAtom)

inlineAtom :=
  // ideally a set of attributes but sets are harder to model
  | inlineAtom(list attribute; string)

attribute :=
  | bold()
  | italic()
|}
;;

let concreteSyntax = {|
H1 := "#"
H2 := "##"
H3 := "###"
STAR := "*"
UNDERSCORE := "_"
TEXT := /[^*_\n]+/

headerLevel :=
  | H1 { h1() }
  | H2 { h2() }
  | H3 { h3() }

block :=
  | headerLevel STRING { header($1; $2) }
  | paragraph          { paragraph($1)  }

inline :=
  | STAR TEXT STAR             { inlineAtom([bold()]; $2)   }
  | UNDERSCORE TEXT UNDERSCORE { inlineAtom([italic()]; $2) }
  | TEXT                       { inlineAtom([]; ($1))       }
|}
