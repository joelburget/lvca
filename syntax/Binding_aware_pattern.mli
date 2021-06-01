(** Patterns for matching binding terms. *)
open Lvca_provenance

open Lvca_util

(** {1 Types} *)

type 'info t =
  | Operator of 'info * string * 'info scope list
  | Primitive of 'info Primitive.t
  | Var of 'info * string
  | Ignored of 'info * string

and 'info scope = Scope of ('info * string) list * 'info t

type 'info capture_type =
  | Bound_var of 'info Sort.t
  | Bound_pattern of 'info Abstract_syntax.Pattern_sort.t
  | Bound_term of 'info Sort.t

type 'info capture =
  | Captured_binder of 'info Pattern.t
  | Captured_term of 'info Nominal.Term.t

val capture_eq
  :  info_eq:('info -> 'info -> bool)
  -> 'info capture
  -> 'info capture
  -> bool

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : _ t -> String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : 'info t -> ('info * string) list

(** {1 Matching} *)

val match_term
  :  info_eq:('info -> 'info -> bool)
  -> 'info t
  -> 'info Nominal.Term.t
  -> 'info capture String.Map.t option

val match_scope
  :  info_eq:('info -> 'info -> bool)
  -> 'info scope
  -> 'info Nominal.Scope.t
  -> 'info capture String.Map.t option

(** {1 Pretty-printing} *)

val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
val pp_scope_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info scope Fmt.t

val pp_capture_generic
  :  open_loc:'info Fmt.t
  -> close_loc:'info Fmt.t
  -> 'info capture Fmt.t

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
  :  ('info Primitive.t -> 'info Sort.t -> string option) (** Primitive checker *)
  -> 'info Abstract_syntax.t (** Abstract syntax *)
  -> 'info Sort.t (** Sort to check pattern against *)
  -> 'info t
  -> ('info capture_type String.Map.t, ('info, 'info t) Check_failure.t) Result.t

(** {1 Parsing} *)
module Parse : sig
  val t : Opt_range.t t Lvca_parsing.t
  val whitespace_t : Opt_range.t t Lvca_parsing.t
end

module Properties : sig
  val string_round_trip1 : unit t -> Property_result.t
  val string_round_trip2 : string -> Property_result.t
end
