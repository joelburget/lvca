(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)

type ('info, 'prim) term =
  | Operator of 'info * string * ('info, 'prim) term list
  | Primitive of 'info * 'prim

val equal
  :  ('info -> 'info -> bool)
  -> ('prim -> 'prim -> bool)
  -> ('info, 'prim) term
  -> ('info, 'prim) term
  -> bool

(** {1 info} *)

val info : ('info, _) term -> 'info
val map_info : f:('a -> 'b) -> ('a, 'prim) term -> ('b, 'prim) term
val erase : (_, 'prim) term -> (unit, 'prim) term

(** {1 de Bruijn conversion} *)

type ('info, 'prim) de_bruijn_conversion_error =
  | ScopeEncountered of ('info, 'prim) DeBruijn.scope
  | VarEncountered of ('info, 'prim) DeBruijn.term

val of_de_bruijn
  :  ('info, 'prim) DeBruijn.term
  -> (('info, 'prim) term, ('info, 'prim) de_bruijn_conversion_error) Result.t

val to_de_bruijn : ('info, 'prim) term -> ('info, 'prim) DeBruijn.term

(** {1 Nominal conversion} *)

type ('info, 'prim) nominal_conversion_error =
  | ScopeEncountered of ('info, 'prim) Nominal.scope
  | VarEncountered of ('info, 'prim) Nominal.term

val of_nominal
  :  ('info, 'prim) Nominal.term
  -> (('info, 'prim) term, ('info, 'prim) nominal_conversion_error) Result.t

val to_nominal : ('info, 'prim) term -> ('info, 'prim) Nominal.term

(** {1 Printing} *)

val pp : 'prim Fmt.t -> (_, 'prim) term Fmt.t

(* Format.formatter -> ('info, 'prim) term -> unit *)
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) term Fmt.t

(** {1 Parsing} *)
module Parse (Comment : ParseUtil.Comment_int) : sig
  val term : 'prim ParseUtil.t -> (OptRange.t, 'prim) term ParseUtil.t
  val whitespace_term : 'prim ParseUtil.t -> (OptRange.t, 'prim) term ParseUtil.t
end

(** {1 Misc} *)
val hash : 'prim Lvca_util.Json.serializer -> (_, 'prim) term -> string

val select_path
  :  path:int list
  -> ('info, 'prim) term
  -> (('info, 'prim) term, string) Result.t
