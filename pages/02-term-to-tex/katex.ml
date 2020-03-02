open Js_of_ocaml

let render : #Dom_html.element Js.t -> string -> unit =
 fun elem str ->
  Js.Unsafe.(
    fun_call
      global##.katex##.render
      [| inject (Js.string str)
       ; inject elem
       ; obj [| "throwOnError", inject (Js.bool false) |]
      |])
;;
