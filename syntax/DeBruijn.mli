(** Representation of terms that uses de Bruijn indices to represent scope. *)

type 'loc term =
  | Operator of 'loc * string * ('loc scope, 'loc term list) Base.Either.t list
  | BoundVar of 'loc * int
  | FreeVar of 'loc * string
  | Primitive of 'loc * Primitive.t

and 'loc scope = Scope of 'loc * string * 'loc term list

(** Open a scope, substituting a term for the variable bound by this scope. *)
val open_scope : 'loc term -> 'loc scope -> 'loc term list

val to_nominal : 'loc term -> ('loc, Primitive.t) Nominal.term option
val of_nominal : ('loc, Primitive.t) Nominal.term -> ('loc term, string) Result.t

val of_nominal_with_bindings
  :  int Lvca_util.String.Map.t
  -> ('loc, Primitive.t) Nominal.term
  -> ('loc term, string) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent : 'loc term -> 'b term -> bool
