type default_expanded_depth =
  | ExpandedTo of int (** Start tree expanded to this many levels *)
  | FullyExpanded (** Start tree fully expanded *)

(** For the given term, create a DOM node and an event for selection of some range. *)
val view_tm
  :  ?source_column:bool
       (** Render a column displaying the name of the row's source file / buffer? *)
  -> ?range_column:bool
       (** Render a column displaying the row's range within its source file / buffer? *)
  -> ?default_expanded_depth:default_expanded_depth
       (** How many levels of the term to expand and display initially. Fully expanded by
           default. *)
  -> ?highlighted_ranges:Lvca_provenance.Source_ranges.t
  -> Lvca_syntax.Nominal.Term.t
  -> Brr.El.t * Lvca_provenance.Source_ranges.t Note.event
