open Jest
open Expect

let toBeEquivalent
  : ('a -> string) -> ('a -> 'a -> bool) -> 'a -> [< 'a partial] -> assertion
  = fun show_a equiv a_expected -> function
    | `Just a_actual -> if equiv a_expected a_actual then pass else (
      Printf.printf "not equivalent:\nexpected:\n%s\n\nactual:\n%s\n"
        (show_a a_expected)
        (show_a a_actual);
      fail "not equivalent"
    )
    | `Not  a_actual -> if equiv a_expected a_actual then fail "equivalent" else pass
