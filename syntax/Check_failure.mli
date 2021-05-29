type ('info, 'term) frame =
  { term : 'term
  ; sort : 'info Sort.t
  }

type ('info, 'term) t =
  { message : string
  ; stack : ('info, 'term) frame list
  }

val map_frame_terms : f:('a -> 'b) -> ('info, 'a) t -> ('info, 'b) t
val err : string -> ('info, 'term) t
val pp : ('info, 'term) frame Fmt.t -> Stdlib.Format.formatter -> ('info, 'term) t -> unit
