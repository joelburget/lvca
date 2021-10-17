open Lvca_syntax

val extract_string : loc:Ppxlib.location -> Ppxlib.expression -> string * Warnings.loc

module Exp : sig
  val string : loc:Ppxlib.location -> string -> Ppxlib.expression
  val provenance : loc:Ppxlib.location -> Provenance.t -> Ppxlib.expression

  module Primitive : sig
    val integer : loc:Ppxlib.location -> Primitive.Integer.t -> Ppxlib.expression
    val int32 : loc:Ppxlib.location -> Primitive.Int32.t -> Ppxlib.expression
    val float : loc:Ppxlib.location -> Primitive.Float.t -> Ppxlib.expression
    val char : loc:Ppxlib.location -> Primitive.Char.t -> Ppxlib.expression
    val string : loc:Ppxlib.location -> Primitive.String.t -> Ppxlib.expression
    val all : loc:Ppxlib.location -> Primitive.All.t -> Ppxlib.expression
  end

  val sort : loc:Ppxlib.location -> Sort.t -> Ppxlib.expression

  val option
    :  loc:Warnings.loc
    -> (loc:Warnings.loc -> 'a -> Ppxlib.expression)
    -> 'a option
    -> Ppxlib.expression

  val list : loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
  val pattern : loc:Ppxlib.location -> Pattern.t -> Ppxlib.expression
  val nominal : loc:Ppxlib.location -> Nominal.Term.t -> Ppxlib.expression
  val nonbinding : loc:Ppxlib.location -> Nonbinding.term -> Ppxlib.expression
  val language : loc:Ppxlib.location -> Abstract_syntax.t -> Ppxlib.expression
  val single_var : loc:Ppxlib.location -> Single_var.t -> Ppxlib.expression
end

module Pat : sig
  val list : loc:Ppxlib.location -> Ppxlib.pattern list -> Ppxlib.pattern
end
