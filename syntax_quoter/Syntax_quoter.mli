open Lvca_provenance
open Lvca_syntax

val extract_string : loc:Ppxlib.location -> Ppxlib.expression -> string * Warnings.loc

module Exp : sig
  val str : loc:Ppxlib.location -> string -> Ppxlib.expression
  val opt_range : loc:Ppxlib.location -> Opt_range.t -> Ppxlib.expression
  val commented : loc:Ppxlib.location -> string Commented.t -> Ppxlib.expression

  module Primitive : sig
    val integer
      :  loc:Ppxlib.location
      -> string Commented.t Primitive.Integer.t
      -> Ppxlib.expression

    val int32
      :  loc:Ppxlib.location
      -> string Commented.t Primitive.Int32.t
      -> Ppxlib.expression

    val float
      :  loc:Ppxlib.location
      -> string Commented.t Primitive.Float.t
      -> Ppxlib.expression

    val char
      :  loc:Ppxlib.location
      -> string Commented.t Primitive.Char.t
      -> Ppxlib.expression

    val string
      :  loc:Ppxlib.location
      -> string Commented.t Primitive.String.t
      -> Ppxlib.expression

    val all
      :  loc:Ppxlib.location
      -> string Commented.t Primitive.All.t
      -> Ppxlib.expression
  end

  val sort : loc:Ppxlib.location -> string Commented.t Sort.t -> Ppxlib.expression

  val option
    :  loc:Warnings.loc
    -> (loc:Warnings.loc -> 'a -> Ppxlib.expression)
    -> 'a option
    -> Ppxlib.expression

  val list : loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
  val pattern : loc:Ppxlib.location -> string Commented.t Pattern.t -> Ppxlib.expression

  val nominal
    :  loc:Ppxlib.location
    -> string Commented.t Nominal.Term.t
    -> Ppxlib.expression

  val nonbinding
    :  loc:Ppxlib.location
    -> string Commented.t Nonbinding.term
    -> Ppxlib.expression

  val language
    :  loc:Ppxlib.location
    -> string Commented.t Abstract_syntax.t
    -> Ppxlib.expression
end

module Pat : sig
  val list : loc:Ppxlib.location -> Ppxlib.pattern list -> Ppxlib.pattern
end
