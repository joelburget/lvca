open Base
open Brr
open Brr_note
open Lvca_provenance
open Lvca_syntax
open Prelude

type term = Opt_range.t Nominal.Term.t

type lang =
  | Lambda
  | Term

let parser_of = function
  | Lambda -> Lvca_languages.Lambda_calculus.Parse.t
  | Term -> Nominal.Term.Parse.t
;;

let term_pretty = Nominal.Term.pp_range
let lambda_pretty = Lvca_languages.Lambda_calculus.pp_range
let lambda_ranges_pretty = Lvca_languages.Lambda_calculus.pp_ranges
let html_eq = Caml.( = )
let htmls_eq = List.equal Caml.( = )

module Action = struct
  type t =
    | Evaluate of string
    | InputSelect of Opt_range.t
    | OutputSelect of Opt_range.t
    | SwitchInputLang
end

let demo_template input_desc input_elem output_desc output_elem =
  let button, div, h2, h3 = El.(button, div, h2, h3) in
  let txt str = El.txt (Jstr.v str) in
  let button =
    button
      ~at:(classes "p-2 border-2 border-indigo-900 rounded")
      [ txt "switch input languages" ]
  in
  let evt = Evr.on_el Ev.click Fn.id button in
  let elem =
    div
      [ h2 [ txt "Demo" ]
      ; div
          ~at:[ class' "container" ]
          [ div ~at:[ class' "py-4" ] [ h3 [ input_desc ]; input_elem ]
          ; div ~at:[ class' "switch-languages" ] [ button ]
          ; div ~at:[ class' "py-4" ] [ h3 [ output_desc ]; output_elem ]
          ]
      ]
  in
  elem, evt
;;

type input_event =
  | InputUpdate of string
  | InputSelect of Range.t
  | InputUnselect

let mk_output elt_s = mk_reactive' El.div ~at:[ class' "bg-gray-100" ] elt_s
