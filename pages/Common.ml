open Base
open Lvca_syntax

type term = (OptRange.t, Primitive.t) Nominal.term

type lang =
  | Lambda
  | Term

module PrimitiveParse = Primitive.Parse (ParseUtil.NoComment)
module TermParse = Nominal.Parse (ParseUtil.NoComment)
module LambdaParse = Lvca_languages.LambdaCalculus.AngstromParse (ParseUtil.NoComment)

module Action = struct
  type t =
    | Evaluate of string
    | Unselect
    | Select of int * int
    | SwitchInputLang
end

let parser_of = function Lambda -> LambdaParse.t | Term -> TermParse.t PrimitiveParse.t

let term_pretty = Nominal.pp_term_range Primitive.pp (* XXX why used twice? *)
let lambda_pretty = Lvca_languages.LambdaCalculus.pp (* XXX why used twice? *)

let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Js_of_ocaml_lwt.Lwt_js_events.async @@ fun () -> ev elem handler
;;
