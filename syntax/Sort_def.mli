open Lvca_util

type t =
  | Sort_def of (string * Kind.t option) list * Operator_def.t list * string list
      (** A sort is defined by a set of variables and a set of operators. *)

val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool
val binds_vars : t -> bool
val kind_check : Int.Set.t String.Map.t -> string -> t -> Int.Set.t String.Map.t
val pp : name:string -> t Fmt.t

(** Parse a definition, eg [bool := True() | False()]. *)
val parse : (string * t) Lvca_parsing.Parser.t

(** Parse a definition starting after the name, eg [:= True() | False()]. *)
val parse' : t Lvca_parsing.Parser.t

val find_operator_def : t -> string -> Operator_def.t option
