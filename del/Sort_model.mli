open Lvca_syntax

module Kernel : [%lvca.abstract_syntax_module_sig
{|
string : *

sort :=
  | Ap(string; ap_list)
  | Name(string)

ap_list :=
  | Nil()
  | Cons(sort; ap_list)
|}
, { string = "Primitive.String" }]

module Sort : sig
  include Nominal.Convertible.Extended_s with type t = Kernel.Sort.t

  val mk_Ap
    :  info:Lvca_syntax.Provenance.t
    -> Lvca_syntax.Primitive.String.t
    -> Kernel.Ap_list.t
    -> t

  val mk_Name : info:Lvca_syntax.Provenance.t -> Lvca_syntax.Primitive.String.t -> t
  val into : Lvca_syntax.Sort.t -> t
  val out : t -> Lvca_syntax.Sort.t
end

module Ap_list : sig
  include Nominal.Convertible.Extended_s with type t = Kernel.Ap_list.t

  val mk_Nil : info:Lvca_syntax.Provenance.t -> t
  val mk_Cons : info:Lvca_syntax.Provenance.t -> Sort.t -> t -> t
  val into : Lvca_syntax.Sort.ap_list -> t
  val out : t -> Lvca_syntax.Sort.ap_list
end
