(** Representation of terms that uses de Bruijn indices to represent scope. *)

type ('info, 'prim) term =
  | Operator of
      'info * string * (('info, 'prim) scope, ('info, 'prim) term) Base.Either.t list
  | BoundVar of 'info * int
  | FreeVar of 'info * string
  | Primitive of 'info * 'prim

and ('info, 'prim) scope = Scope of 'info * string * ('info, 'prim) term

(** Open a scope, substituting a term for the variable bound by this scope. *)
val open_scope : ('info, 'prim) term -> ('info, 'prim) scope -> ('info, 'prim) term

val to_nominal : ('info, 'prim) term -> ('info, 'prim) Nominal.Term.t option

val of_nominal
  :  ('info, 'prim) Nominal.Term.t
  -> (('info, 'prim) term, ('info, 'prim) Nominal.Scope.t) Result.t

val of_nominal_with_bindings
  :  int Lvca_util.String.Map.t
  -> ('info, 'prim) Nominal.Term.t
  -> (('info, 'prim) term, ('info, 'prim) Nominal.Scope.t) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent
  :  ('prim -> 'prim -> bool)
  -> ('a, 'prim) term
  -> ('b, 'prim) term
  -> bool

(* XXX add properties *)
