(** Patterns for matching non-binding terms. *)

(** {1 Types} *)

type ('info, 'prim) t =
  | Operator of 'info * string * ('info, 'prim) t list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

val equal
  :  info_eq:('info -> 'info -> bool)
  -> prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) t
  -> ('info, 'prim) t
  -> bool

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : _ t -> Lvca_util.String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : ('info, _) t -> ('info * string) list

(** {1 Printing} *)

val pp : 'prim Fmt.t -> (_, 'prim) t Fmt.t
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t

(** {1 Serialization} *)

val jsonify
  :  'prim Lvca_util.Json.serializer
  -> ('info, 'prim) t Lvca_util.Json.serializer

val unjsonify
  :  'prim Lvca_util.Json.deserializer
  -> (unit, 'prim) t Lvca_util.Json.deserializer

(** {1 Info} *)

val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
val erase : (_, 'prim) t -> (unit, 'prim) t
val info : ('info, _) t -> 'info

(** {1 Misc} *)

val select_path : path:int list -> ('info, 'prim) t -> (('info, 'prim) t, string) Result.t

(** Check that this pattern is valid and return the sort for each variable it binds.

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
     {- Patterns can't see valence: they can only bind subterms with some given sort. }
    } *)
val check
  :  'prim Fmt.t
  -> ('info -> 'prim -> 'info Sort.t -> string option) (** Primitive checker *)
  -> 'info AbstractSyntax.t (** Abstract syntax *)
  -> pattern_sort:'info Sort.t (** Sort to check pattern against *)
  -> var_sort:'info Sort.t (** Sort pattern must yield as variables *)
  -> ('info, 'prim) t
  -> ( 'info Sort.t Lvca_util.String.Map.t
     , ('info, ('info, 'prim) t) CheckFailure.t )
     Result.t

(** {1 Parsing} *)

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
end

module Properties : sig
  val json_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end

module Primitive : sig
  (** Hardcoded for the Primitive type *)

  val check
    :  'info AbstractSyntax.t (** Abstract syntax *)
    -> pattern_sort:'info Sort.t (** Sort to check pattern against *)
    -> var_sort:'info Sort.t (** Sort pattern must yield as variables *)
    -> ('info, Primitive.t) t
    -> ( 'info Sort.t Lvca_util.String.Map.t
       , ('info, ('info, Primitive.t) t) CheckFailure.t )
       Result.t
end
