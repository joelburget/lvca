type default_expanded_depth =
  | ExpandedTo of int (** Start tree expanded to this many levels *)
  | FullyExpanded (** Start tree fully expanded *)

val view_tm
  :  ?source_column:bool
  -> ?range_column:bool
  -> ?default_expanded_depth:default_expanded_depth
  -> Lvca_syntax.SourceRanges.t Lvca_syntax.Nominal.Term.t
  -> Brr.El.t * Lvca_syntax.SourceRanges.t Note.event
