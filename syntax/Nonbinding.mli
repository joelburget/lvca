open Lvca_util

(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)
type 'info term =
  | Operator of 'info * string * 'info term list
  | Primitive of 'info Primitive.t

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
val pp_range : Lvca_provenance.Opt_range.t term Fmt.t

(** {1 Parsing} *)
module Parse : sig
  val term : Lvca_provenance.Opt_range.t term Lvca_parsing.t
  val whitespace_term : Lvca_provenance.Opt_range.t term Lvca_parsing.t
end

(** {1 Misc} *)
val hash : _ term -> string

val select_path : path:int list -> 'info term -> ('info term, string) Result.t

(** {1 Serialization} *)
val jsonify : _ term Json.serializer

val unjsonify : unit term Json.deserializer

module type Convertible_s = sig
  include Language_object_intf.S with type 'info t = 'info term

  val of_nonbinding : 'info term -> ('info t, 'info term) Result.t
  val to_nonbinding : 'info t -> 'info term
end

module type Extended_term_s = sig
  include Language_object_intf.Extended_s with type 'info t = 'info term

  (* TODO: to_pattern, of_pattern *)

  val select_path
    :  path:int list
    -> 'info t
    -> ('info t, (string, 'info term) Base.Either.t) Result.t

  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : _ t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : _ t -> string
end

module Extend_term (Object : Convertible_s) : Extended_term_s
