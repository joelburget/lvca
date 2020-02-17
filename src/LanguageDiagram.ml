(* Inspiration: haskell's diagrams package, Tikz
 *
 * Questions: What should be primitive? How do we define a standard library?
 *)

let abstract_syntax = {|
// rational?
import {float} from "builtins";

diagram :=
  | atop(diagram; diagram)
  | beside(vector; diagram; diagram)
  | adjustment(list attribute; diagram)

vector := vector(float; float)

adjustment :=
  | scale
  |

shape :=
  | circle
  | line
|}

let standard_library = {|
unitX : vector
  = 1, 0

unitY : vector
  = 0, 1

(===) : diagram -> diagram -> diagram
  = beside (-unitY)

(|||) : diagram -> diagram -> diagram
  = beside unitX
|}
