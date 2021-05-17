open Lvca_provenance
open Lvca_syntax

val mk_pattern : loc:Ppxlib.location -> OptRange.t Pattern.t -> Ppxlib.expression
val mk_nominal : loc:Ppxlib.location -> OptRange.t Nominal.Term.t -> Ppxlib.expression
val mk_nonbinding : loc:Ppxlib.location -> OptRange.t NonBinding.term -> Ppxlib.expression
val mk_language : loc:Ppxlib.location -> OptRange.t AbstractSyntax.t -> Ppxlib.expression
