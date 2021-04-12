(** Patterns for matching binding terms. *)

(** {1 Types} *)

module Make (Prim : LanguageObject_intf.S) : sig
  module Nominal : Nominal_intf.S with module Prim = Prim
  module Pattern = Nominal.Pattern
  module Term = Nominal.Term
  module Scope = Nominal.Scope

  type 'info t =
    | Operator of 'info * string * 'info scope list
    | Primitive of 'info Prim.t
    | Var of 'info * string
    | Ignored of 'info * string

  and 'info scope = Scope of ('info * string) list * 'info t

  type 'info capture_type =
    | BoundVar of 'info Sort.t
    | BoundPattern of 'info AbstractSyntax.PatternSort.t
    | BoundTerm of 'info Sort.t

  type 'info capture =
    | CapturedBinder of 'info Pattern.t
    | CapturedTerm of 'info Term.t

  val capture_eq
    :  info_eq:('info -> 'info -> bool)
    -> 'info capture
    -> 'info capture
    -> bool

  (** {1 Vars} *)

  (** A set of all the variables bound in a pattern. *)
  val vars_of_pattern : _ t -> Lvca_util.String.Set.t

  (** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
      exists? Because in a list we can also include the info for each var (which we can't
      do in a set). *)
  val list_vars_of_pattern : 'info t -> ('info * string) list

  (** {1 Matching} *)

  val match_term
    :  info_eq:('info -> 'info -> bool)
    -> 'info t
    -> 'info Term.t
    -> 'info capture Lvca_util.String.Map.t option

  val match_scope
    :  info_eq:('info -> 'info -> bool)
    -> 'info scope
    -> 'info Scope.t
    -> 'info capture Lvca_util.String.Map.t option

  (** {1 Pretty-printing} *)

  val pp : _ t Fmt.t
  val pp_range : OptRange.t t Fmt.t
  val pp_ranges : SourceRanges.t t Fmt.t
  val pp_scope : _ scope Fmt.t
  val pp_scope_range : OptRange.t scope Fmt.t
  val pp_scope_ranges : SourceRanges.t scope Fmt.t
  val pp_capture : _ capture Fmt.t

  (** {1 Info} *)

  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val erase : _ t -> unit t
  val info : 'info t -> 'info

  (** {1 Misc} *)
  val select_path : path:int list -> 'info t -> ('info t, string) Result.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool

  (** Check that this pattern is valid and return the valence for each variable it binds.

      Checks performed:

      {ol
       {- Primitives: checked by the given primitive checker. }
       {- All used operators are found (in the sort corresponding to the pattern type). }
       {- All
          operators
          have
          the
          correct
          number
          of
          subterms
          for
          their
          arity.

          - Fixed arity patterns must have the exact number of subterms.
          - Variable arity patterns may have any number.
       }
      } *)
  val check
    :  ('info -> 'info Prim.t -> 'info Sort.t -> string option) (** Primitive checker *)
    -> 'info AbstractSyntax.t (** Abstract syntax *)
    -> 'info Sort.t (** Sort to check pattern against *)
    -> 'info t
    -> ( 'info capture_type Lvca_util.String.Map.t
       , ('info, 'info t) CheckFailure.t )
       Result.t

  (** {1 Parsing} *)
  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
    val whitespace_t : OptRange.t t ParseUtil.t
  end

  module Properties : sig
    val string_round_trip1 : unit t -> PropertyResult.t
    val string_round_trip2 : string -> PropertyResult.t
  end
end
