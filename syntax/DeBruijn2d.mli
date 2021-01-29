(** Representation of terms that uses 2d de Bruijn indices to represent scope.

    A [BoundVar (_, i, j)] represents a variable bound [i] scopes out, at index [j] in the
    pattern. *)

type ('info, 'prim) term =
  | Operator of 'info * string * ('info, 'prim) scope list
  | BoundVar of 'info * int * int
  | FreeVar of 'info * string
  | Primitive of 'info * 'prim

and ('info, 'prim) scope = Scope of ('info, 'prim) Pattern.t list * ('info, 'prim) term

val to_nominal : ('info, 'prim) term -> ('info, 'prim) Nominal.term option

val of_nominal
  :  ('info, 'prim) Nominal.term
  -> (('info, 'prim) term, ('info, 'prim) Nominal.scope) Result.t

val of_nominal_with_bindings
  :  (int * int) Lvca_util.String.Map.t
  -> ('info, 'prim) Nominal.term
  -> (('info, 'prim) term, ('info, 'prim) Nominal.scope) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent
  :  ('prim -> 'prim -> bool)
  -> ('a, 'prim) term
  -> ('b, 'prim) term
  -> bool

val select_path
  :  path:int list
  -> ('info, 'prim) term
  -> (('info, 'prim) term, string) Result.t

(** Open a scope, substituting a term for each variable bound by this scope. *)

(* val open_scope : scope -> term list -> (term, string) Result.t *)
