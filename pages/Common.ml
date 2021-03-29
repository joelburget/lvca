open Base
open Brr
open Brr_note
open Lvca_syntax
open Prelude
module PrimitiveParse = Primitive.Parse (ParseUtil.NoComment)
module TermParse = Nominal.Term.Parse (ParseUtil.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

type term = (OptRange.t, Primitive.t) Nominal.Term.t

type lang =
  | Lambda
  | Term

let parser_of = function Lambda -> LambdaParse.t | Term -> TermParse.t PrimitiveParse.t
let term_pretty = Nominal.Term.pp_range Primitive.pp
let lambda_pretty = Lvca_languages.LambdaCalculus.pp_range
let lambda_ranges_pretty = Lvca_languages.LambdaCalculus.pp_ranges
let html_eq = Caml.( = )
let htmls_eq = List.equal Caml.( = )

module Action = struct
  type t =
    | Evaluate of string
    | InputSelect of OptRange.t
    | OutputSelect of OptRange.t
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
