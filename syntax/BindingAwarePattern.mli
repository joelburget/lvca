type ('info, 'prim) t =
  | Operator of 'info * string * ('info, 'prim) scope list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

and ('info, 'prim) scope = Scope of ('info * string) list * ('info, 'prim) t

(** {1 Vars} *)

(** A set of all the variables bound in a pattern. *)
val vars_of_pattern : _ t -> Lvca_util.String.Set.t

(** A list of all the variables bound in a pattern. Why have this when [vars_of_pattern]
    exists? Because in a list we can also include the info for each var (which we can't do
    in a set). *)
val list_vars_of_pattern : ('info, _) t -> ('info * string) list

(** {1 Pretty-printing} *)

val pp : 'prim Fmt.t -> ('info, 'prim) t Fmt.t
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t
val pp_scope : 'prim Fmt.t -> ('info, 'prim) scope Fmt.t
val pp_scope_range : 'prim Fmt.t -> (OptRange.t, 'prim) scope Fmt.t
val pp_scope_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) scope Fmt.t

(** {1 Info} *)

val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
val erase : (_, 'prim) t -> (unit, 'prim) t
val info : ('info, _) t -> 'info

(** {1 Misc} *)
val select_path : path:int list -> ('info, 'prim) t -> (('info, 'prim) t, string) Result.t

val equal
  :  ('info -> 'info -> bool)
  -> ('prim -> 'prim -> bool)
  -> ('info, 'prim) t
  -> ('info, 'prim) t
  -> bool

(** {1 Parsing} *)
module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
end
