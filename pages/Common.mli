(** Grab-bag of frontend stuff. *)

open Lvca_provenance
open Lvca_syntax

type term = Opt_range.t Nominal.Term.t

type lang =
  | Lambda
  | Term

val parser_of : lang -> term Lvca_parsing.t
val term_pretty : Opt_range.t Nominal.Term.t Fmt.t
val lambda_pretty : Opt_range.t Nominal.Term.t Fmt.t
val lambda_ranges_pretty : Source_ranges.t Nominal.Term.t Fmt.t
val html_eq : Brr.El.t -> Brr.El.t -> bool
val htmls_eq : Brr.El.t list -> Brr.El.t list -> bool

module Action : sig
  type t =
    | Evaluate of string
    | InputSelect of Opt_range.t
    | OutputSelect of Opt_range.t
    | SwitchInputLang
end

val demo_template
  :  Brr.El.t
  -> Brr.El.t
  -> Brr.El.t
  -> Brr.El.t
  -> Brr.El.t * Brr.Ev.Mouse.t Brr.Ev.t Note.event

type input_event =
  | EvaluateInput of string
  | InputSelect of Range.t
  | InputUnselect

val mk_output : Brr.El.t Note.signal -> Brr.El.t
