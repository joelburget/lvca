module Html = Js_of_ocaml_tyxml.Tyxml_js.Html

module Model = struct
  let initial_model = ()
end

module View = struct
  let view _model = [%html{|<div>TODO</div>|}]
end

let stateless_view = View.view Model.initial_model
