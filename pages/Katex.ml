open Brr

let render : El.t -> string -> unit =
 fun elem str ->
  let katex = Jv.get (Window.to_jv G.window) "katex" in
  let _ : Jv.t = Jv.call katex "render"
    [| Jv.of_string str
     ; El.to_jv elem
     ; Jv.obj [| "throwOnError", Jv.false' |]
    |]
  in
  ()
;;
