val mk_pattern
  :  loc:Ppxlib.location
  -> (Lvca_syntax.OptRange.t, Lvca_syntax.Primitive.t) Lvca_syntax.Pattern.t
  -> Ppxlib.expression

val mk_nominal
  :  loc:Ppxlib.location
  -> (Lvca_syntax.OptRange.t, Lvca_syntax.Primitive.t) Lvca_syntax.Nominal.Term.t
  -> Ppxlib.expression

val mk_nonbinding
  :  loc:Ppxlib.location
  -> (Lvca_syntax.OptRange.t, Lvca_syntax.Primitive.t) Lvca_syntax.NonBinding.term
  -> Ppxlib.expression

val mk_language
  :  loc:Ppxlib.location
  -> Lvca_syntax.OptRange.t Lvca_syntax.AbstractSyntax.t
  -> Ppxlib.expression
