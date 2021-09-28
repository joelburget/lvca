(** A frame in the typechecking stack. *)
module Frame : sig
  type 'term t =
    { term : 'term
    ; sort : Sort.t
    }

  val pp : 'term Fmt.t -> 'term t Fmt.t
end

(** A typechecking error carries a message and the context where the error occurred. *)
type 'term t =
  { message : string
  ; stack : 'term Frame.t list
  }

val map_frame_terms : f:('a -> 'b) -> 'a t -> 'b t
val err : string -> 'term t
val pp : 'term Fmt.t -> Stdlib.Format.formatter -> 'term t -> unit
