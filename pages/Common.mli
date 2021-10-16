(** Grab-bag of frontend stuff. *)

open Lvca_provenance
open Lvca_syntax

type term = Nominal.Term.t

val parse_term : string -> (term, string) Result.t

type lang =
  | Lambda
  | Term

val parser_of : lang -> term Lvca_parsing.t

(* val term_pretty : Nominal.Term.t Fmt.t *)
(* val lambda_pretty : Nominal.Term.t Fmt.t *)
(* val lambda_ranges_pretty : Nominal.Term.t Fmt.t *)
val html_eq : Brr.El.t -> Brr.El.t -> bool
val htmls_eq : Brr.El.t list -> Brr.El.t list -> bool

module Action : sig
  type t =
    | Evaluate of string
    | Input_select of Opt_range.t
    | Output_select of Opt_range.t
    | Switch_input_lang
end

val demo_template
  :  Brr.El.t
  -> Brr.El.t
  -> Brr.El.t
  -> Brr.El.t
  -> Brr.El.t * Brr.Ev.Mouse.t Brr.Ev.t Note.event

type input_event =
  | Evaluate_input of string
  | Input_update of string
  | Input_select of Range.t
  | Input_unselect

val mk_output : Brr.El.t Note.signal -> Brr.El.t
