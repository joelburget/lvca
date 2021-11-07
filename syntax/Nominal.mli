(** Representation of terms that simply uses variable names to represent scope. *)
open Lvca_util

module Types : sig
  type term =
    | Operator of Provenance.t * string * scope list
    | Var of Provenance.t * string
    | Primitive of Primitive_impl.All.t

  and scope = Scope of Pattern.t list * term
end

val mk_Operator : ?provenance:Provenance.t -> string -> Types.scope list -> Types.term
val mk_Primitive : Primitive_impl.All.t -> Types.term
val mk_Var : ?provenance:Provenance.t -> string -> Types.term
val mk_Scope : Pattern.t list -> Types.term -> Types.scope

module Conversion_error : sig
  type t =
    | Scope of Provenance.t * Types.scope * string option
    | Term of Provenance.t * Types.term * string option

  val mk_Scope : ?provenance:Provenance.t -> ?message:string -> Types.scope -> t
  val mk_Term : ?provenance:Provenance.t -> ?message:string -> Types.term -> t
  val pp : t Fmt.t
end

module Term : sig
  type t = Types.term =
    | Operator of Provenance.t * string * Types.scope list
    | Var of Provenance.t * string
    | Primitive of Primitive_impl.All.t

  val to_nominal : t -> t
  val of_nominal : t -> (t, Conversion_error.t) Result.t
  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val info : t -> Provenance.t
  val pp : t Fmt.t
  val jsonify : t Json.serializer
  val unjsonify : t Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : t -> string

  (** Attempt to convert a non-binding term to a pattern.

      For example, the term [add(lit(1); a)] is convertible to a pattern, but
      [lambda(a. a)] is not. *)
  val to_pattern : t -> (Pattern.t, Types.scope) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

      For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
  val of_pattern : Pattern.t -> t

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all : t String.Map.t -> t -> t

  (** Substitute a single variable *)
  val subst : name:string -> value:Types.term -> t -> t

  val rename : string -> string -> t -> t
  val select_path : path:int list -> t -> (t, string) Result.t
  val match_pattern : Pattern.t -> t -> t String.Map.t option
  val free_vars : t -> String.Set.t

  (** Check that the given term matches the given sort.

      This recursively checks subterms and patterns.

      Checks performed:

      + All used variables must be bound.
      + Variables must have the correct sort at their use site.
      + Primitives must have the correct sort (string / integer).
      + All mentioned operators must appear in the relevant sort.
      + All operators must have the correct number of subterms.
      + Variable-arity terms can have only non-binding terms as children
      + Fixed-valence terms must have the correct number of binders. All must be
        variables.
      + Variable-valence terms must have one binder, a pattern. *)
  val check
    :  Abstract_syntax.t (** Abstract syntax *)
    -> Sort.t (** Sort to check term against *)
    -> t
    -> (Pattern.t, t) Base.Either.t Check_failure.t option

  val parse : Lvca_util.String.Set.t -> parse_prim:t Lvca_parsing.t -> t Lvca_parsing.t
  val parse' : Lvca_util.String.Set.t -> t Lvca_parsing.t

  module Properties : sig
    include Properties_intf.Parse_pretty_s with type t := t
    include Properties_intf.Json_s with type t := t
  end
end

module Scope : sig
  type t = Types.scope = Scope of Pattern.t list * Types.term

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val jsonify : t Json.serializer
  val unjsonify : t Json.deserializer

  (* TODO?
val to_pattern
  :  (, 'prim) t
  -> ((, 'prim) Pattern.t (unit, 'prim) scope) Result.t

val of_pattern : (, 'prim) Pattern.t -> (, 'prim) t
  *)

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all : Types.term String.Map.t -> t -> t

  (** Substitute a single variable *)
  val subst : name:string -> value:Types.term -> t -> t

  val rename : string -> string -> t -> t

  (* TODO:
  val free_vars : (_, _) t -> String.Set.t

  val parse : parse_prim:Opt_range.t t Lvca_parsing.t -> Opt_range.t t Lvca_parsing.t
  val parse' : Opt_range.t t Lvca_parsing.t

  module Properties : Properties_intf.S with type  t := unit t
  *)
end

module Convertible : sig
  module type S = sig
    include Language_object_intf.S

    val to_nominal : t -> Term.t
    val of_nominal : Term.t -> (t, Conversion_error.t) Result.t
  end

  (** Helpers derivable from [S] *)
  module type Extended_s = sig
    include S

    val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
    val ( = ) : t -> t -> bool
    (* TODO: should they be comparable as well? *)

    (* TODO: to_pattern, of_pattern *)

    (** Substitute all the variables in the context.

        Leaves variables not found in the context free. *)
    val subst_all : t String.Map.t -> t -> (t, Conversion_error.t) Result.t

    (** Substitute a single variable *)
    val subst : name:string -> value:t -> t -> (t, Conversion_error.t) Result.t

    val rename : string -> string -> t -> (t, Conversion_error.t) Result.t

    val select_path
      :  path:int list
      -> t
      -> (t, (string, Conversion_error.t) Base.Either.t) Result.t

    (** {1 Serialization} *)
    include Language_object_intf.Json_convertible with type t := t

    include Language_object_intf.Serializable with type t := t

    (** {1 Printing / Parsing} *)
    val pp : t Fmt.t

    val to_string : t -> string
    val parse : t Lvca_parsing.t
  end

  (** Derive helpers (an extended language object) from the basics. *)
  module Extend (Object : S) : Extended_s with type t = Object.t

  (** Properties of parsing and pretty-printing that should hold for any language object. *)
  module Check_parse_pretty (Object : S) :
    Properties_intf.Parse_pretty_s with type t = Object.t

  (** Properties of json serialization / deserialization that should hold for any language
      object. *)
  module Check_json (Object : S) : Properties_intf.Json_s with type t = Object.t

  (** Check json serialization / deserialization and parsing / pretty-printing properties *)
  module Check_properties (Object : S) : Properties_intf.S with type t = Object.t
end
