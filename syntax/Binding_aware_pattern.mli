(** Patterns for matching binding terms. *)
open Lvca_util

(** {1 Types} *)

type t =
  | Operator of Provenance.t * string * scope list
  | Primitive of Primitive.All.t
  | Var of Provenance.t * string

and scope = Scope of (Provenance.t * string) list * t

module Capture_type : sig
  type t =
    | Bound_var of Sort.t
    | Bound_pattern of Abstract_syntax.Pattern_sort.t
    | Bound_term of Sort.t

  val pp : t Fmt.t
end

module Capture : sig
  type t =
    | Binder of Pattern.t
    | Term of Nominal.Term.t

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
end

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : t -> String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : t -> (Provenance.t * string) list

(** {1 Matching} *)

val match_term : t -> Nominal.Term.t -> Capture.t String.Map.t option
val match_scope : scope -> Nominal.Scope.t -> Capture.t String.Map.t option
val match_all : t -> Nominal.Term.t -> Provenance.t list

(** {1 Pretty-printing} *)
val pp : t Fmt.t

(** {1 Info} *)

val info : t -> Provenance.t

(** {1 Misc} *)
val select_path : path:int list -> t -> (t, string) Result.t

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool

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
  :  (Primitive.All.t -> Sort.t -> string option) (** Primitive checker *)
  -> Abstract_syntax.t (** Abstract syntax *)
  -> Sort.t (** Sort to check pattern against *)
  -> t
  -> (Capture_type.t String.Map.t, t Check_failure.t) Result.t

(** {1 Parsing} *)
val parse : t Lvca_parsing.t

module Properties : sig
  val string_round_trip1 : t -> Property_result.t
  val string_round_trip2 : string -> Property_result.t
end
