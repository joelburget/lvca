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

val calculated_here : Source_code_position.t -> t list -> t
val of_here : Source_code_position.t -> t
val of_range : ?input:Parse_input.t -> Opt_range.t -> t
val ( = ) : t -> t -> bool
val pp : t Fmt.t
val open_stag : Stdlib.Format.formatter -> t -> unit
val close_stag : Stdlib.Format.formatter -> t -> unit

(** For testing only: used to enable outputting of the [Stag] semantic tag. *)
val stag_functions : Stdlib.Format.formatter_stag_functions

val make0
  :  ?input:Parse_input.t
  -> (info:t -> 'b)
  -> _ Lvca_parsing.t
  -> 'b Lvca_parsing.t

val make1
  :  ?input:Parse_input.t
  -> (info:t -> 'a -> 'b)
  -> 'a Lvca_parsing.t
  -> 'b Lvca_parsing.t

val make2
  :  ?input:Parse_input.t
  -> (info:t -> 'a -> 'b -> 'c)
  -> 'a Lvca_parsing.t
  -> 'b Lvca_parsing.t
  -> 'c Lvca_parsing.t

val make3
  :  ?input:Parse_input.t
  -> (info:t -> 'a -> 'b -> 'c -> 'd)
  -> 'a Lvca_parsing.t
  -> 'b Lvca_parsing.t
  -> 'c Lvca_parsing.t
  -> 'd Lvca_parsing.t

val make4
  :  ?input:Parse_input.t
  -> (info:t -> 'a -> 'b -> 'c -> 'd -> 'e)
  -> 'a Lvca_parsing.t
  -> 'b Lvca_parsing.t
  -> 'c Lvca_parsing.t
  -> 'd Lvca_parsing.t
  -> 'e Lvca_parsing.t

val make5
  :  ?input:Parse_input.t
  -> (info:t -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
  -> 'a Lvca_parsing.t
  -> 'b Lvca_parsing.t
  -> 'c Lvca_parsing.t
  -> 'd Lvca_parsing.t
  -> 'e Lvca_parsing.t
  -> 'f Lvca_parsing.t

val make6
  :  ?input:Parse_input.t
  -> (info:t -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
  -> 'a Lvca_parsing.t
  -> 'b Lvca_parsing.t
  -> 'c Lvca_parsing.t
  -> 'd Lvca_parsing.t
  -> 'e Lvca_parsing.t
  -> 'f Lvca_parsing.t
  -> 'g Lvca_parsing.t
