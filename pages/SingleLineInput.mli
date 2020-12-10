val mk :
  ?autofocus:bool ->
  ?highlights_s:Lvca_syntax.Ranges.t React.signal ->
  string React.signal ->
  [> Html_types.div ] Js_of_ocaml_tyxml.Tyxml_js.To_dom.elt * Common.input_event React.event
