(** A frame in the typechecking stack. *)
module Frame : sig
  type ('info, 'term) t =
    { term : 'term
    ; sort : 'info Sort.t
    }

  val pp : 'term Fmt.t -> ('info, 'term) t Fmt.t
end

(** A typechecking error carries a message and the context where the error occurred. *)
type ('info, 'term) t =
  { message : string
  ; stack : ('info, 'term) Frame.t list
  }

val map_frame_terms : f:('a -> 'b) -> ('info, 'a) t -> ('info, 'b) t
val err : string -> ('info, 'term) t
val pp : 'term Fmt.t -> Stdlib.Format.formatter -> ('info, 'term) t -> unit
