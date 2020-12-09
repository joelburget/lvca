open Lvca_syntax
module Ev = Js_of_ocaml_lwt.Lwt_js_events
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js

type term = (OptRange.t, Primitive.t) Nominal.term

module Action : sig
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

val lambda_pretty : (OptRange.t, Primitive.t) Nominal.term Fmt.t

val bind_event : ('a -> ('b -> 'c -> 'd) -> unit Lwt.t) -> 'a -> ('b -> 'd) -> unit
val demo_template :
  Tyxml_js.Xml.mouse_event_handler ->
  [< Html_types.h3_content_fun ] Tyxml_js.To_dom.elt ->
  [< Html_types.div_content_fun > `H3 `PCDATA ] Tyxml_js.To_dom.elt ->
  [< Html_types.h3_content_fun ] Tyxml_js.To_dom.elt ->
  [< Html_types.div_content_fun > `H3 `PCDATA ] Tyxml_js.To_dom.elt ->
  [> Html_types.div ] Tyxml_js.To_dom.elt

type input_event =
  | InputUpdate of string
  | InputSelect of int * int
  | InputUnselect

val mk_multiline_input :
  ?autofocus:bool ->
  ?border:bool ->
  ?rows:int option ->
  ?cols:int ->
  string React.signal ->
  [> Html_types.div ] Tyxml_js.To_dom.elt * input_event React.event

val mk_single_line_input :
  ?autofocus:bool ->
  ?highlights_s:Lvca_syntax.Ranges.t React.signal ->
  string React.signal ->
  [> Html_types.div ] Tyxml_js.To_dom.elt * input_event React.event

val mk_output :
  [< Html_types.div_content_fun ] Tyxml_js.To_dom.elt React.signal ->
  [> Html_types.div ] Tyxml_js.To_dom.elt

type digits_update =
  | SetDigits of int
  | IncrDigits
  | DecrDigits

val mk_digits_entry :
  int React.signal ->
  [> Html_types.input ] Tyxml_js.To_dom.elt *
  digits_update React.event
