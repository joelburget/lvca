(** Binding-aware patterns allow one to capture binders (variables and patterns). *)

(** {1 Types} *)

type ('info, 'prim) t =
  | Operator of 'info * string * ('info, 'prim) scope list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

and ('info, 'prim) scope = Scope of ('info * string) list * ('info, 'prim) t

type 'info capture_type =
  | BoundVar of 'info Sort.t
  | BoundPattern of 'info AbstractSyntax.pattern_sort
  | BoundTerm of 'info Sort.t

type ('info, 'prim) capture =
  | CapturedBinder of ('info, 'prim) Pattern.t
  | CapturedTerm of ('info, 'prim) Nominal.term

val capture_eq
  :  info_eq:('info -> 'info -> bool)
  -> prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) capture
  -> ('info, 'prim) capture
  -> bool

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : _ t -> Lvca_util.String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : ('info, _) t -> ('info * string) list

(** {1 Matching} *)

val match_term
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) t
  -> ('info, 'prim) Nominal.term
  -> ('info, 'prim) capture Lvca_util.String.Map.t option

val match_scope
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) scope
  -> ('info, 'prim) Nominal.scope
  -> ('info, 'prim) capture Lvca_util.String.Map.t option

(** {1 Pretty-printing} *)

val pp : 'prim Fmt.t -> ('info, 'prim) t Fmt.t
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t
val pp_scope : 'prim Fmt.t -> ('info, 'prim) scope Fmt.t
val pp_scope_range : 'prim Fmt.t -> (OptRange.t, 'prim) scope Fmt.t
val pp_scope_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) scope Fmt.t

(** {1 Info} *)

val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
val erase : (_, 'prim) t -> (unit, 'prim) t
val info : ('info, _) t -> 'info

(** {1 Misc} *)
val select_path : path:int list -> ('info, 'prim) t -> (('info, 'prim) t, string) Result.t

val equal
  :  ('info -> 'info -> bool)
  -> ('prim -> 'prim -> bool)
  -> ('info, 'prim) t
  -> ('info, 'prim) t
  -> bool

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
  :  ('info -> 'prim -> 'info Sort.t -> string option) (** Primitive checker *)
  -> 'info AbstractSyntax.t (** Abstract syntax *)
  -> 'info Sort.t (** Sort to check pattern against *)
  -> ('info, 'prim) t
  -> ( 'info capture_type Lvca_util.String.Map.t
     , ('info, ('info, 'prim) t) CheckFailure.t )
     Result.t

(** {1 Parsing} *)
module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
end

module Properties : sig
  val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end
