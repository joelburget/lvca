(** Representation of terms that uses de Bruijn indices to represent scope. *)
type term =
  | Operator of Provenance.t * string * (scope, term) Base.Either.t list
  | Bound_var of Provenance.t * int
  | Free_var of Provenance.t * string
  | Primitive of Primitive.All.t

and scope = Scope of Provenance.t * string * term

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> term -> term -> bool
val ( = ) : term -> term -> bool
val pp : term Fmt.t

(** Open a scope, substituting a term for the variable bound by this scope. *)
val open_scope : term -> scope -> term

(** Convert a de Bruijn term to a nominal term. This fails only in the case of invalid
    indices. *)
val to_nominal : term -> Nominal.Term.t option

(** Convert a nominal term to de Bruijn. Fails on scopes that bind patterns or more than
    one variable. *)
val of_nominal : Nominal.Term.t -> (term, Nominal.Scope.t) Result.t

val of_nominal_with_bindings
  :  env:int Lvca_util.String.Map.t
  -> Nominal.Term.t
  -> (term, Nominal.Scope.t) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent : term -> term -> bool

val parse : Lvca_util.String.Set.t -> term Lvca_parsing.t

(* module Properties : Properties_intf.S with type t := term *)
