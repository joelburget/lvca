(** Representation of terms that uses 2d de Bruijn indices to represent scope.

    A [BoundVar (_, i, j)] represents a variable bound [i] scopes out, at index [j] in the
    pattern. *)

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) scope list
  | BoundVar of 'loc * int * int
  | FreeVar of 'loc * string
  | Primitive of 'loc * 'prim

and ('loc, 'prim) scope = Scope of ('loc, 'prim) Pattern.t list * ('loc, 'prim) term

val to_nominal : ('loc, 'prim) term -> ('loc, 'prim) Nominal.term option

val of_nominal
  :  ('loc, 'prim) Nominal.term
  -> (('loc, 'prim) term, ('loc, 'prim) Nominal.scope) Result.t

val of_nominal_with_bindings
  :  (int * int) Lvca_util.String.Map.t
  -> ('loc, 'prim) Nominal.term
  -> (('loc, 'prim) term, ('loc, 'prim) Nominal.scope) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent
  :  ('prim -> 'prim -> bool)
  -> ('a, 'prim) term
  -> ('b, 'prim) term
  -> bool

val select_path
  :  path:int list
  -> ('loc, 'prim) term
  -> (('loc, 'prim) term, string) Result.t

(** Open a scope, substituting a term for each variable bound by this scope. *)

(* val open_scope : scope -> term list -> (term, string) Result.t *)
