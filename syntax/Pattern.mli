(** Patterns for matching non-binding terms. *)
open Lvca_util

type t =
  | Operator of Provenance.t * string * t list
  | Primitive of Primitive_impl.All.t
  | Var of Provenance.t * string

val mk_Operator : ?provenance:Provenance.t -> string -> t list -> t
val mk_Primitive : Primitive_impl.All.t -> t
val mk_Var : ?provenance:Provenance.t -> string -> t
val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : t -> String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : t -> (Provenance.t * string) list

(** {1 Printing} *)

val pp : t Fmt.t

(** {1 Serialization} *)

val jsonify : t Json.serializer
val unjsonify : t Json.deserializer

(** {1 Info} *)

val info : t -> Provenance.t

(** {1 Misc} *)

val select_path : path:int list -> t -> (t, string) Result.t

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
  :  Abstract_syntax.t (** Abstract syntax *)
  -> pattern_sort:Sort.t (** Sort to check pattern against *)
  -> var_sort:Sort.t (** Sort pattern must yield as variables *)
  -> t
  -> (Sort.t String.Map.t, t Check_failure.t) Result.t

(** {1 Parsing} *)
val parse : t Lvca_parsing.Parser.t

module Properties : sig
  include Properties_intf.Parse_pretty_s with type t := t
  include Properties_intf.Json_s with type t := t
end
