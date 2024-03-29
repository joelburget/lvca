open Base
open Brr
open Note
open Lvca_syntax
module Document = Lvca_languages.Document
open Document.Lang
module List_model = Lvca_del.List_model
module Option_model = Lvca_del.Option_model

type term = Doc.t

let parse = Lvca_languages.Document.parse

module Model = struct
  type t =
    { input : string
    ; result : term
    }

  let input =
    {|# document

paragraph

* li 1
* li 2

First Term
: This is the definition of the first term.

Second Term
: This is one definition of the second term.
: This is another definition of the second term.

***

* This is the first list item.
* Here's the second list item.

    > A blockquote would look great below the second list item.

* And here's the third list item.

---

1. Open the file.
2. Find the following code block on line 21:

        <html>
          <head>
            <title>Test</title>
          </head>

3. Update the title to match the name of your website.|}
  ;;

  let initial_model = { input; result = parse input }

  module Doc = Nominal.Convertible.Extend (Doc)

  let pp ppf { input; result } =
    Fmt.pf ppf "{ input = %s; result = %a }" input Doc.pp result
  ;;

  let ( = ) m1 m2 =
    let term_eq x y = Nominal.Term.(Doc.to_nominal x = Doc.to_nominal y) in
    String.(m1.input = m2.input) && term_eq m1.result m2.result
  ;;
end

module Action = struct
  type t = Evaluate of string
end

module Controller = struct
  let update (action : Action.t) _model =
    match action with Evaluate iput -> Model.{ input = iput; result = parse iput }
  ;;
end

module View = struct
  open Prelude

  let div = El.div

  let view model_s =
    let input_s = S.map ~eq:String.( = ) (fun Model.{ input; _ } -> input) model_s in
    let input_elem, input_evt = Multiline_input.mk ~at:[ class' "m-4" ] input_s in
    let enter_input_e =
      input_evt
      |> E.filter_map (function
             | Common.Evaluate_input str -> Some (Action.Evaluate str)
             | _ -> None)
    in
    let output_view =
      model_s |> S.map ~eq:phys_equal (fun model -> Md_viewer.of_doc model.Model.result)
    in
    let elem =
      div
        ~at:(classes "flex flex-row")
        [ input_elem; mk_reactive' ~at:[ class' "m-4" ] div output_view ]
    in
    enter_input_e, elem
  ;;
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
