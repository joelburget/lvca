open Jest
open Expect

let toBeEquivalent : ('a -> 'a -> bool) -> 'a -> [< 'a partial] -> assertion
  = fun equiv t1 -> function
    | `Just t2 -> if equiv t1 t2 then pass else (
      Js.log (Js.Json.stringifyAny t1);
      Js.log (Js.Json.stringifyAny t2);
      fail "not equivalent"
    )
    | `Not  t2 -> if equiv t1 t2 then fail "equivalent" else pass
