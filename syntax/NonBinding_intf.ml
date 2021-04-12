module type S = sig
  module Prim : LanguageObject_intf.S
  module Nominal : Nominal_intf.S with module Prim = Prim
  module DeBruijn : DeBruijn_intf.S with module Prim = Prim

  type 'info term =
    | Operator of 'info * string * 'info term list
    | Primitive of 'info Prim.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info term -> 'info term -> bool

  (** {1 info} *)

  val info : 'info term -> 'info
  val map_info : f:('a -> 'b) -> 'a term -> 'b term
  val erase : _ term -> unit term

  (** {1 de Bruijn conversion} *)

  type 'info de_bruijn_conversion_error =
    | ScopeEncountered of 'info DeBruijn.scope
    | VarEncountered of 'info DeBruijn.term

  val of_de_bruijn
    :  'info DeBruijn.term
    -> ('info term, 'info de_bruijn_conversion_error) Result.t

  val to_de_bruijn : 'info term -> 'info DeBruijn.term

  (** {1 Nominal conversion} *)

  type 'info nominal_conversion_error =
    | ScopeEncountered of 'info Nominal.Scope.t
    | VarEncountered of 'info Nominal.Term.t

  val of_nominal
    :  'info Nominal.Term.t
    -> ('info term, 'info nominal_conversion_error) Result.t

  val to_nominal : 'info term -> 'info Nominal.Term.t

  (** {1 Printing} *)

  val pp : _ term Fmt.t
  val pp_range : OptRange.t term Fmt.t

  (** {1 Parsing} *)
  module Parse (Comment : ParseUtil.Comment_int) : sig
    val term : OptRange.t term ParseUtil.t
    val whitespace_term : OptRange.t term ParseUtil.t
  end

  (** {1 Misc} *)
  val hash : _ term -> string

  val select_path : path:int list -> 'info term -> ('info term, string) Result.t
end