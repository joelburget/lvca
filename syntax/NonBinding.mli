(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) term list
  | Primitive of 'loc * 'prim

val info : ('loc, _) term -> 'loc

(** {1 de Bruijn conversion} *)

type ('loc, 'prim) de_bruijn_conversion_error =
  | ScopeEncountered of ('loc, 'prim) DeBruijn.scope
  | VarEncountered of ('loc, 'prim) DeBruijn.term

val of_de_bruijn
  :  ('loc, 'prim) DeBruijn.term
  -> (('loc, 'prim) term, ('loc, 'prim) de_bruijn_conversion_error) Result.t

val to_de_bruijn : ('loc, 'prim) term -> ('loc, 'prim) DeBruijn.term

(** {1 Nominal conversion} *)

type ('loc, 'prim) nominal_conversion_error =
  | ScopeEncountered of ('loc, 'prim) Nominal.scope
  | VarEncountered of ('loc, 'prim) Nominal.term

val of_nominal
  :  ('loc, 'prim) Nominal.term
  -> (('loc, 'prim) term, ('loc, 'prim) nominal_conversion_error) Result.t

val to_nominal : ('loc, 'prim) term -> ('loc, 'prim) Nominal.term

(** {1 Printing} *)

val pp : 'prim Fmt.t -> (_, 'prim) term Fmt.t

(* Format.formatter -> ('loc, 'prim) term -> unit *)
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) term Fmt.t

(* Format.formatter -> OptRange.t term -> unit *)
val to_string : 'prim Fmt.t -> (_, 'prim) term -> string
val hash : 'prim Lvca_util.Json.serializer -> (_, 'prim) term -> string
val erase : (_, 'prim) term -> (unit, 'prim) term
