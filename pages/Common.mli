(** Grab-bag of frontend stuff. *)

open Lvca_syntax

type term = (OptRange.t, Primitive.t) Nominal.term

val html_eq : Brr.El.t -> Brr.El.t -> bool
val htmls_eq : Brr.El.t list -> Brr.El.t list -> bool

module Action : sig
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

val lambda_pretty : (OptRange.t, Primitive.t) Nominal.term Fmt.t

val demo_template
  : Brr.El.t -> Brr.El.t -> Brr.El.t -> Brr.El.t
  -> Brr.El.t * Brr.Ev.Mouse.t Brr.Ev.t Note.event

type input_event =
  | InputUpdate of string
  | InputSelect of int * int
  | InputUnselect

val mk_output : Brr.El.t Note.signal -> Brr.El.t

type digits_update =
  | SetDigits of int
  | IncrDigits
  | DecrDigits

val mk_digits_entry : int Note.signal -> Brr.El.t * digits_update Note.event
