(** Grab-bag of frontend stuff. *)

open Lvca_syntax

type term = OptRange.t Nominal.Term.t

module LambdaParse : sig
  val t : (term * OptRange.t) Angstrom.t
end

type lang =
  | Lambda
  | Term

val parser_of : lang -> (term * OptRange.t) Angstrom.t
val term_pretty : OptRange.t Nominal.Term.t Fmt.t
val lambda_pretty : OptRange.t Nominal.Term.t Fmt.t
val lambda_ranges_pretty : SourceRanges.t Nominal.Term.t Fmt.t
val html_eq : Brr.El.t -> Brr.El.t -> bool
val htmls_eq : Brr.El.t list -> Brr.El.t list -> bool

module Action : sig
  type t =
    | Evaluate of string
    | InputSelect of OptRange.t
    | OutputSelect of OptRange.t
    | SwitchInputLang
end

val demo_template
  :  Brr.El.t
  -> Brr.El.t
  -> Brr.El.t
  -> Brr.El.t
  -> Brr.El.t * Brr.Ev.Mouse.t Brr.Ev.t Note.event

type input_event =
  | InputUpdate of string
  | InputSelect of Range.t
  | InputUnselect

val mk_output : Brr.El.t Note.signal -> Brr.El.t
