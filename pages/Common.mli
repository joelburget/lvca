(** Grab-bag of frontend stuff. *)

open Lvca_syntax
(*
module Ev = Js_of_ocaml_lwt.Lwt_js_events
module Tyxml_js = Js_of_ocaml_tyxml.Tyxml_js
*)

type term = (OptRange.t, Primitive.t) Nominal.term

val html_eq : Brr.El.t -> Brr.El.t -> bool
(* val rhtml_eq : 'a Brr.El.t -> 'a Brr.El.t -> bool *)

module Action : sig
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

val lambda_pretty : (OptRange.t, Primitive.t) Nominal.term Fmt.t

(*
val bind_event : ('a -> ('b -> 'c -> 'd) -> unit Lwt.t) -> 'a -> ('b -> 'd) -> unit
*)
val demo_template : _ -> Brr.El.t -> Brr.El.t -> Brr.El.t -> Brr.El.t -> Brr.El.t

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
