val mk :
  ?autofocus:bool ->
  ?border:bool ->
  ?rows:int option ->
  ?cols:int ->
  string React.signal ->
  [> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.To_dom.elt * Common.input_event React.event
