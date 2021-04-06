open Lvca_syntax

val mk_pattern
  :  loc:Ppxlib.location
  -> (OptRange.t, OptRange.t Primitive.t) Pattern.t
  -> Ppxlib.expression

val mk_nominal
  :  loc:Ppxlib.location
  -> (OptRange.t, OptRange.t Primitive.t) Nominal.Term.t
  -> Ppxlib.expression

val mk_nonbinding
  :  loc:Ppxlib.location
  -> (OptRange.t, OptRange.t Primitive.t) NonBinding.term
  -> Ppxlib.expression

val mk_language
  :  loc:Ppxlib.location
  -> OptRange.t AbstractSyntax.t
  -> Ppxlib.expression
