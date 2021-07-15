open Lvca_provenance
open Lvca_syntax

val extract_string : loc:Ppxlib.location -> Ppxlib.expression -> string * Warnings.loc

type info = Opt_range.t * string option

module Exp : sig
  val str : loc:Ppxlib.location -> string -> Ppxlib.expression
  val opt_range : loc:Ppxlib.location -> Opt_range.t -> Ppxlib.expression
  val info : loc:Ppxlib.location -> info -> Ppxlib.expression
  val prim : loc:Ppxlib.location -> info Primitive.All.t -> Ppxlib.expression
  val sort : loc:Ppxlib.location -> info Sort.t -> Ppxlib.expression

  val option
    :  loc:Warnings.loc
    -> (loc:Warnings.loc -> 'a -> Ppxlib.expression)
    -> 'a option
    -> Ppxlib.expression

  val list : loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
  val pattern : loc:Ppxlib.location -> info Pattern.t -> Ppxlib.expression
  val nominal : loc:Ppxlib.location -> info Nominal.Term.t -> Ppxlib.expression
  val nonbinding : loc:Ppxlib.location -> info Nonbinding.term -> Ppxlib.expression
  val language : loc:Ppxlib.location -> info Abstract_syntax.t -> Ppxlib.expression
end

module Pat : sig
  val list : loc:Ppxlib.location -> Ppxlib.pattern list -> Ppxlib.pattern
end
