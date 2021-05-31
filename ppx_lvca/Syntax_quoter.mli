open Lvca_provenance
open Lvca_syntax

val mk_list : loc:Ppxlib.location -> Ppxlib.expression list -> Ppxlib.expression
val mk_pattern : loc:Ppxlib.location -> Opt_range.t Pattern.t -> Ppxlib.expression
val mk_nominal : loc:Ppxlib.location -> Opt_range.t Nominal.Term.t -> Ppxlib.expression

val mk_nonbinding
  :  loc:Ppxlib.location
  -> Opt_range.t Nonbinding.term
  -> Ppxlib.expression

val mk_language
  :  loc:Ppxlib.location
  -> Opt_range.t Abstract_syntax.t
  -> Ppxlib.expression
