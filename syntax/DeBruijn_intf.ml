module type S = sig
  module Prim : LanguageObject_intf.S
  module Nominal : Nominal_intf.S with module Prim = Prim

  type 'info term =
    | Operator of 'info * string * ('info scope, 'info term) Base.Either.t list
    | BoundVar of 'info * int
    | FreeVar of 'info * string
    | Primitive of 'info Prim.t

  and 'info scope = Scope of 'info * string * 'info term

  val map_info : f:('a -> 'b) -> 'a term -> 'b term
  val erase : _ term -> unit term

  (** Open a scope, substituting a term for the variable bound by this scope. *)
  val open_scope : 'info term -> 'info scope -> 'info term

  val to_nominal : 'info term -> 'info Nominal.Term.t option
  val of_nominal : 'info Nominal.Term.t -> ('info term, 'info Nominal.Scope.t) Result.t

  val of_nominal_with_bindings
    :  int Lvca_util.String.Map.t
    -> 'info Nominal.Term.t
    -> ('info term, 'info Nominal.Scope.t) Result.t

  (** Are the two terms equivalent up to variable renaming? *)
  val alpha_equivalent : 'a term -> 'b term -> bool

  (* XXX add properties *)
end
