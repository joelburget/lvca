(** Patterns for matching non-binding terms. *)
open Lvca_util

type 'info t =
  | Operator of 'info * string * 'info t list
  | Primitive of 'info Primitive_impl.t
  | Var of 'info * string
  | Ignored of 'info * string

module Plain : sig
  type t =
    | Operator of string * t list
    | Primitive of Primitive_impl.Plain.t
    | Var of string
    | Ignored of string
end

val to_plain : _ t -> Plain.t
val of_plain : Plain.t -> unit t
val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : _ t -> String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : 'info t -> ('info * string) list

(** {1 Printing} *)

val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val pp : _ t Fmt.t
val pp_range : Lvca_provenance.Opt_range.t t Fmt.t
val pp_ranges : Lvca_provenance.Source_ranges.t t Fmt.t

(** {1 Serialization} *)

val jsonify : 'info t Json.serializer
val unjsonify : unit t Json.deserializer

(** {1 Info} *)

val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase : _ t -> unit t
val info : 'info t -> 'info

(** {1 Misc} *)

val select_path : path:int list -> 'info t -> ('info t, string) Result.t

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
  :  'info Abstract_syntax.t (** Abstract syntax *)
  -> pattern_sort:'info Sort.t (** Sort to check pattern against *)
  -> var_sort:'info Sort.t (** Sort pattern must yield as variables *)
  -> 'info t
  -> ('info Sort.t String.Map.t, ('info, 'info t) Check_failure.t) Result.t

(** {1 Parsing} *)

module Parse : sig
  val t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
  val whitespace_t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
end

module Properties : sig
  include Properties_intf.Parse_pretty_s with type 'info t := 'info t
  include Properties_intf.Json_s with type 'info t := 'info t
end
