type default_expanded_depth
  = ExpandedTo of int
  (** Start tree expanded to this many levels *)
  | FullyExpanded
  (** Start tree fully expanded *)

val view_tm
  :  ?source_column:bool
  -> ?default_expanded_depth:default_expanded_depth
  -> (Lvca_syntax.SourceRanges.t, Lvca_syntax.Primitive.t) Lvca_syntax.Nominal.term
  -> Html_types.flow5 Js_of_ocaml_tyxml.Tyxml_js.To_dom.elt * Lvca_syntax.SourceRanges.t React.event
