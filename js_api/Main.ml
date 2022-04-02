open Js_of_ocaml

let () =
  Js.export
    (* Example to be replaced by real API: *)
    "lvca"
    (object%js
       method add x y = x +. y
       method abs x = abs_float x
       val zero = 0.
    end)
;;
