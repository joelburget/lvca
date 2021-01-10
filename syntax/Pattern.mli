(** Patterns match terms and bind variables. *)

type ('info, 'prim) pattern =
  | Operator of 'info * string * ('info, 'prim) pattern list list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

type ('info, 'prim) t = ('info, 'prim) pattern

val equal : ('loc -> 'loc -> bool) -> ('prim -> 'prim -> bool) -> ('loc, 'prim) t -> ('loc, 'prim) t ->  bool

val vars_of_pattern : _ t -> Lvca_util.String.Set.t
val list_vars_of_pattern : ('info, _) t -> ('info * string) list
val to_string : 'prim Fmt.t -> ('info, 'prim) t -> string (* TODO: remove? *)

val pp : 'prim Fmt.t -> ('info, 'prim) t Fmt.t
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t
val jsonify : 'prim Lvca_util.Json.serializer -> ('loc, 'prim) t Lvca_util.Json.serializer

val unjsonify
  :  'prim Lvca_util.Json.deserializer
  -> (unit, 'prim) t Lvca_util.Json.deserializer

val map_loc : f:('a -> 'b) -> ('a, 'prim) pattern -> ('b, 'prim) pattern
val erase : (_, 'prim) pattern -> (unit, 'prim) pattern
val location : ('info, _) pattern -> 'info

val select_path
  :  path:((int * int) list)
  -> ('loc, 'prim) pattern
  -> (('loc, 'prim) pattern, string) Result.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
end

module Properties : sig
  val json_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end
