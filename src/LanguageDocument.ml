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
