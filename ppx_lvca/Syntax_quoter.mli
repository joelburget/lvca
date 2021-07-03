open Lvca_provenance
open Lvca_syntax

module Exp : sig
  val list : loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
  val pattern : loc:Ppxlib.location -> Opt_range.t Pattern.t -> Ppxlib.expression
  val nominal : loc:Ppxlib.location -> Opt_range.t Nominal.Term.t -> Ppxlib.expression
  val nonbinding : loc:Ppxlib.location -> Opt_range.t Nonbinding.term -> Ppxlib.expression
  val language : loc:Ppxlib.location -> Opt_range.t Abstract_syntax.t -> Ppxlib.expression
end

module Pat : sig
  val list : loc:Ppxlib.location -> Ppxlib.pattern list -> Ppxlib.pattern
end
