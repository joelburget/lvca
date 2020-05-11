module Description = struct
  let abstract_syntax =
    {|
  import { sequence } from "builtins";

  maybe(a) :=
    | nothing()
    | some(a)

  // An edit in some language is either:
  edit(lang) :=
    // A simple, atomic edit
    | atomic(core(lang; maybe(lang)))
    // Or an edit with a message attached
    | labeled(edit(); string())
    // Or a sequence of edits.
    | sequence(sequence(edit))
  |}
  ;;

  let concrete_syntax_parser =
    {|

  sequence
    |}
  ;;
end
