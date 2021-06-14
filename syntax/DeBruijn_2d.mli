(** Representation of terms that uses 2d de Bruijn indices to represent scope.

    A [BoundVar (_, i, j)] represents a variable bound [i] scopes out, at index [j] in the
    pattern. *)
type 'info term =
  | Operator of 'info * string * 'info scope list
  | BoundVar of 'info * int * int
  | FreeVar of 'info * string
  | Primitive of 'info Primitive.All.t

and 'info scope = Scope of 'info Pattern.t list * 'info term

val to_nominal : 'info term -> 'info Nominal.Term.t option
val of_nominal : 'info Nominal.Term.t -> ('info term, 'info Nominal.Scope.t) Result.t

val of_nominal_with_bindings
  :  (int * int) Lvca_util.String.Map.t
  -> 'info Nominal.Term.t
  -> ('info term, 'info Nominal.Scope.t) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent : 'a term -> 'b term -> bool

val select_path : path:int list -> 'info term -> ('info term, string) Result.t

(** Open a scope, substituting a term for each variable bound by this scope. *)

(* val open_scope : scope -> term list -> (term, string) Result.t *)
