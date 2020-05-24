open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js

module Change = struct

  type position =
    { ch: int
    ; line: int
    }

  type t =
    { from: position
    ; to_: position
    ; text: string array
    ; removed: string
    (* ; origin *)
    }

    (*
  type js =
    {
    }
  *)

end

(*
class type keypress_event_type = object
  method
end
*)

class type codemirror_type = object
  method on_change : Js.js_string Js.t -> (Change.t -> unit) Js.callback -> unit Js.meth
  method on_keypress
    : Js.js_string Js.t -> (codemirror_type Js.t -> Dom_html.keyboardEvent Js.t -> unit) Js.callback -> unit Js.meth
  method refresh : unit -> unit Js.meth
  method getValue : unit -> Js.js_string Js.meth
end

let codemirror_obj = Js.Unsafe.get Dom_html.window (Js.string "CodeMirror")
let create_fn = Js.Unsafe.get codemirror_obj (Js.string "fromTextArea")

let mk_codemirror
  :  string
  -> ([< Html_types.div_content_fun > `Textarea ] To_dom.elt * codemirror_type Js.t)
  = fun initial_value ->

  let textarea = Html5.(textarea (txt initial_value)) in
  let wrapper = Html5.div [textarea] in

  let editor = Js.Unsafe.fun_call create_fn
    [| textarea |> To_dom.of_textarea |> Js.Unsafe.inject |]
  in

  wrapper, editor
