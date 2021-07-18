type ('info, 'a) t =
  | Nil of 'info
  | Cons of 'info * 'a * ('info, 'a) t

module Plain : sig
  type 'a t =
    | Nil
    | Cons of 'a * 'a t
end

val of_plain : 'a Plain.t -> (unit, 'a) t
val to_plain : (_, 'a) t -> 'a Plain.t

val equal
  :  info_eq:('info -> 'info -> bool)
  -> ('a -> 'a -> bool)
  -> ('info, 'a) t
  -> ('info, 'a) t
  -> bool

val info : ('info, _) t -> 'info
val map_info : f:('i1 -> 'i2) -> ('i1, 'a) t -> ('i2, 'a) t
val to_nominal : ('info, 'info Nominal.Term.t) t -> 'info Nominal.Term.t

val of_nominal
  :  'info Nominal.Term.t
  -> (('info, 'info Nominal.Term.t) t, 'info Nominal.Term.t) Result.t

val map : ('info, 'a) t -> f:('a -> 'b) -> ('info, 'b) t
val length : _ t -> int
val fold : ('info, 'a) t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
