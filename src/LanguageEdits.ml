let abstractSyntax = {|
// An edit in some language is either:
edit lang :=
  // A simple, atomic edit
  | atomic(core lang (maybe lang))
  // Or an edit with a message attached
  | labeled(edit; string)
  // Or a sequence of edits.
  | sequence(list edit)
|}
