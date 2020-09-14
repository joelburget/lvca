(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) term list list
  | Primitive of 'loc * 'prim

val location : ('loc, _) term -> 'loc

(** Raised by [of_de_bruijn_exn] or [of_nominal_exn] when they encounter a scope. *)
exception ScopeEncountered

(** {1 de Bruijn conversion} *)

(** @raise ScopeEncountered *)
val of_de_bruijn_exn : ('loc, 'prim) DeBruijn.term -> ('loc, 'prim) term

val of_de_bruijn : ('loc, 'prim) DeBruijn.term -> ('loc, 'prim) term option
val to_de_bruijn : ('loc, 'prim) term -> ('loc, 'prim) DeBruijn.term

(** {1 Nominal conversion} *)

(** @raise ScopeEncountered *)
val of_nominal_exn : ('loc, 'prim) Nominal.term -> ('loc, 'prim) term

val of_nominal : ('loc, 'prim) Nominal.term -> ('loc, 'prim) term option
val to_nominal : ('loc, 'prim) term -> ('loc, 'prim) Nominal.term

(** {1 Printing} *)

val pp : 'prim Fmt.t -> (_, 'prim) term Fmt.t

(* Format.formatter -> ('loc, 'prim) term -> unit *)
val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) term Fmt.t

(* Format.formatter -> OptRange.t term -> unit *)
val to_string : 'prim Fmt.t -> (_, 'prim) term -> string
val hash : 'prim Lvca_util.Json.serializer -> (_, 'prim) term -> string
val erase : (_, 'prim) term -> (unit, 'prim) term
