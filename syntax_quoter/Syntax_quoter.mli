open Lvca_provenance
open Lvca_syntax

val extract_string : loc:Ppxlib.location -> Ppxlib.expression -> string * Warnings.loc

module Exp : sig
  val str : loc:Ppxlib.location -> string -> Ppxlib.expression
  val opt_range : loc:Ppxlib.location -> Opt_range.t -> Ppxlib.expression
  val prim : loc:Ppxlib.location -> Opt_range.t Primitive.All.t -> Ppxlib.expression
  val sort : loc:Ppxlib.location -> Opt_range.t Sort.t -> Ppxlib.expression

  val option
    :  loc:Warnings.loc
    -> (loc:Warnings.loc -> 'a -> Ppxlib.expression)
    -> 'a option
    -> Ppxlib.expression

  val list : loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
  val pattern : loc:Ppxlib.location -> Opt_range.t Pattern.t -> Ppxlib.expression
  val nominal : loc:Ppxlib.location -> Opt_range.t Nominal.Term.t -> Ppxlib.expression
  val nonbinding : loc:Ppxlib.location -> Opt_range.t Nonbinding.term -> Ppxlib.expression
  val language : loc:Ppxlib.location -> Opt_range.t Abstract_syntax.t -> Ppxlib.expression
end

module Pat : sig
  val list : loc:Ppxlib.location -> Ppxlib.pattern list -> Ppxlib.pattern
end
