open Lvca_util

(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)
type term =
  | Operator of Provenance.t * string * term list
  | Primitive of Primitive.All.t

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> term -> term -> bool
val ( = ) : term -> term -> bool

(** {1 info} *)

(** {1 de Bruijn conversion} *)

type de_bruijn_conversion_error =
  | Scope_encountered of DeBruijn.scope
  | Var_encountered of DeBruijn.term

val of_de_bruijn : DeBruijn.term -> (term, de_bruijn_conversion_error) Result.t
val to_de_bruijn : term -> DeBruijn.term

(** {1 Nominal conversion} *)

type nominal_conversion_error =
  | Scope_encountered of Nominal.Scope.t
  | Var_encountered of Nominal.Term.t

val of_nominal : Nominal.Term.t -> (term, nominal_conversion_error) Result.t
val to_nominal : term -> Nominal.Term.t

(** {1 Printing} *)

val pp : term Fmt.t

(** {1 Parsing} *)
val parse : term Lvca_parsing.t

(** {1 Misc} *)
val hash : term -> string

val select_path : path:int list -> term -> (term, string) Result.t

(** {1 Serialization} *)
val jsonify : term Json.serializer

val unjsonify : term Json.deserializer

module type Convertible_s = sig
  include Language_object_intf.S with type t = term

  val of_nonbinding : term -> (t, term) Result.t
  val to_nonbinding : t -> term
end
