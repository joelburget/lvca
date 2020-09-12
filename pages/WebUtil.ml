open Base
open Js_of_ocaml

let is_enter key_evt =
  String.equal
    "Enter"
    (key_evt##.code |> Js.Optdef.to_option |> Option.value_exn |> Js.to_string)
;;

let is_meta key_evt = Js.to_bool key_evt##.metaKey
let is_shift key_evt = Js.to_bool key_evt##.shiftKey
let is_ctrl key_evt = Js.to_bool key_evt##.ctrlKey

(** Is this an enter keypress plus meta, shift, or ctrl *)
let is_special_enter key_evt =
  is_enter key_evt && (is_meta key_evt || is_shift key_evt || is_ctrl key_evt)
;;
