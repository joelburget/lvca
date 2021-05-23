(** Representation of terms that uses de Bruijn indices to represent scope. *)
type 'info term =
  | Operator of 'info * string * ('info scope, 'info term) Base.Either.t list
  | BoundVar of 'info * int
  | FreeVar of 'info * string
  | Primitive of 'info Primitive.t

and 'info scope = Scope of 'info * string * 'info term

val map_info : f:('a -> 'b) -> 'a term -> 'b term
val erase : _ term -> unit term
val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info term Fmt.t

(** Open a scope, substituting a term for the variable bound by this scope. *)
val open_scope : 'info term -> 'info scope -> 'info term

(** Convert a de Bruijn term to a nominal term. This fails only in the case of invalid
    indices. *)
val to_nominal : 'info term -> 'info Nominal.Term.t option

(** Convert a nominal term to de Bruijn. Fails on scopes that bind patterns or more than
    one variable. *)
val of_nominal : 'info Nominal.Term.t -> ('info term, 'info Nominal.Scope.t) Result.t

val of_nominal_with_bindings
  :  env:int Lvca_util.String.Map.t
  -> 'info Nominal.Term.t
  -> ('info term, 'info Nominal.Scope.t) Result.t

(** Are the two terms equivalent up to variable renaming? *)
val alpha_equivalent : 'a term -> 'b term -> bool

module Parse (Comment : ParseUtil_intf.Comment_s) : sig
  val t : Lvca_provenance.OptRange.t term ParseUtil.t
  val whitespace_t : Lvca_provenance.OptRange.t term ParseUtil.t
end

(* module Properties : Properties_intf.S with type 'info t := 'info term *)
