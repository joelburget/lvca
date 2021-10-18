open Lvca_util

(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)
type t =
  | Operator of Provenance.t * string * t list
  | Primitive of Primitive.All.t

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool

(** {1 info} *)

(** {1 de Bruijn conversion} *)

type de_bruijn_conversion_error =
  | Scope_encountered of DeBruijn.scope
  | Var_encountered of DeBruijn.term

val of_de_bruijn : DeBruijn.term -> (t, de_bruijn_conversion_error) Result.t
val to_de_bruijn : t -> DeBruijn.term

(** {1 Nominal conversion} *)

val of_nominal : Nominal.Term.t -> (t, Nominal.Conversion_error.t) Result.t
val to_nominal : t -> Nominal.Term.t

(** {1 Printing} *)

val pp : t Fmt.t

(** {1 Parsing} *)
val parse : t Lvca_parsing.t

(** {1 Misc} *)
val hash : t -> string

val select_path : path:int list -> t -> (t, string) Result.t

(** {1 Serialization} *)
val jsonify : t Json.serializer

val unjsonify : t Json.deserializer

type nonbinding = t

module type Convertible_s = sig
  include Language_object_intf.S with type t = t

  val of_nonbinding : nonbinding -> (t, nonbinding) Result.t
  val to_nonbinding : t -> nonbinding
end
