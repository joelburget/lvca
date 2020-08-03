# scratchpad / examples

## hutton:

// is this just abstract syntax or does it carry a concrete syntax with it?
import {integer-literal} from "lvca/integer"

tm :=
  | add(tm(); tm()) // better? add(tm; tm)
  | lit(integer-literal())

---

// TODO: whitespace

import {integer-literal} from "lvca/integer"

// TODO: need better sequence syntax
// TODO: := ?
let parens : parser a -> parser a
  = fun a -> "(" a ")" -> a

fix (fun tm ->
  let atom =
    | parens tm
    | i:integer-literal -> lit(i)
  in

  chainl1 atom ('+'
  )
