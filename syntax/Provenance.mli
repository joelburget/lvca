open Base
open Lvca_provenance

module Parse_input : sig
  type t =
    | Input_unknown
    | Buffer_name of string
    | String of string

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
end

module Parse_located : sig
  (** A location that was parsed: the string it was parsed from and the range in that
      string *)
  type t =
    { (* parser : Provenance.t *)
      input : Parse_input.t
    ; range : Opt_range.t
    }

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
end

module Located : sig
  (** A location is either an (implementation) source code position or was parsed. *)
  type t =
    | Source_located of Source_code_position.t
    | Parse_located of Parse_located.t

  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
end

(** A term is:

    - written directly
    - computed from others
    - or, indexed (a reference to some external data) *)
type t =
  | Located of Located.t
  | Calculated of Located.t * t list
  | Indexed of int

val unsafe_parse_located : t -> Parse_located.t
val unsafe_range : t -> Opt_range.t
val calculated_here : Source_code_position.t -> t list -> t
val of_here : Source_code_position.t -> t
val of_range : ?input:Parse_input.t -> Opt_range.t -> t
val ( = ) : t -> t -> bool
val pp : t Fmt.t
val open_stag : Stdlib.Format.formatter -> t -> unit
val close_stag : Stdlib.Format.formatter -> t -> unit
val fmt_stag : t -> 'a Fmt.t -> 'a Fmt.t

(** For testing only: used to enable outputting of the [Stag] semantic tag. *)
val stag_functions : Stdlib.Format.formatter_stag_functions

module Test_setup : sig
  type t = Stdlib.Format.formatter_stag_functions

  val setup : unit -> t
  val teardown : t -> unit
end
