open Base
open Note
open Prelude
open Lvca_syntax

let parse_lang lang_str =
  Lvca_parsing.(parse_string (whitespace *> Abstract_syntax.parse) lang_str)
;;

let reserved = Lvca_util.String.Set.empty

let parse_term term_str =
  Lvca_parsing.parse_string (Nominal.Term.parse' reserved) term_str
;;

module Model = struct
  type t =
    { language_str : string
    ; language_parsed : (Abstract_syntax.t, string) Result.t
    ; term_str : string
    ; term_parsed : (Nominal.Term.t, string) Result.t
    }

  let ( = ) t1 t2 =
    String.(t1.language_str = t2.language_str)
    && Result.equal
         Abstract_syntax.( = )
         String.( = )
         t1.language_parsed
         t2.language_parsed
    && String.(t1.term_str = t2.term_str)
    && Result.equal Nominal.Term.( = ) String.( = ) t1.term_parsed t2.term_parsed
  ;;

  let language_str =
    {|
list : * -> *
integer : *
string : *

value :=
  | unit()
  | lit_int(integer)
  | lit_str(string)

match_line :=
  | match_line(value[value]. term)

term :=
// TODO(#21): uncomment
//  | lambda(value. term)
  | alt_lambda(term. term)
  | match(list match_line)
  | value(value)
   |}
  ;;

  let term_str = "value(unit())"

  let initial_model =
    { language_str
    ; language_parsed = parse_lang language_str
    ; term_str
    ; term_parsed = parse_term term_str
    }
  ;;
end

module Action = struct
  type t =
    | Update_language of string
    | Update_term of string
end

module Controller = struct
  let update (action : Action.t) model =
    match action with
    | Update_language language_str ->
      { model with Model.language_str; language_parsed = parse_lang language_str }
    | Update_term term_str -> { model with term_str; term_parsed = parse_term term_str }
  ;;
end

module View = struct
  open Brr

  let div, h2, h3, table, td, thead, tr, txt' =
    El.(div, h2, h3, table, td, thead, tr, txt')
  ;;

  let rec view_pat = function
    | Pattern.Var (_, name) -> txt' name
    | Primitive prim -> txt' (Fmt.to_to_string Primitive.All.pp prim)
    | Operator (_, name, pats) ->
      div [ txt' name; div (List.map pats ~f:(fun pat -> div [ view_pat pat ])) ]
  ;;

  let rec view_term = function
    | Nominal.Term.Var (_, name) -> txt' name
    | Primitive prim -> txt' (Fmt.to_to_string Primitive.All.pp prim)
    | Operator (_, name, scopes) -> div [ txt' name; div (List.map scopes ~f:view_scope) ]

  and view_scope (Nominal.Scope.Scope (pats, tm)) =
    div [ pats |> List.map ~f:view_pat |> div; tm |> view_term ]
  ;;

  let rec sort = function
    | Sort.Name (_, name) -> txt' name
    | Ap (_, name, subsorts) -> div [ txt' name; div (ap_list subsorts) ]

  and ap_list = function Sort.Nil _ -> [] | Cons (_, s, ss) -> sort s :: ap_list ss

  let view_check_frame
      : (Pattern.t, Nominal.Term.t) Base.Either.t Check_failure.Frame.t -> El.t
    =
   fun { term; sort = s } ->
    tr
      [ td
          [ (match term with
            | Either.First pat -> view_pat pat
            | Second tm -> view_term tm)
          ]
      ; td [ sort s ]
      ]
 ;;

  let view_check_failure
      : (Pattern.t, Nominal.Term.t) Base.Either.t Check_failure.t -> El.t
    =
   fun { message; stack } ->
    let table_header = thead [ td [ txt' "term" ]; td [ txt' "sort" ] ] in
    let table_rows = stack |> List.map ~f:view_check_frame in
    div [ txt' message; table (table_header :: table_rows) ]
 ;;

  let view model_s =
    let language_input, input_event =
      model_s
      |> S.map (fun Model.{ language_str; _ } -> language_str)
      |> Multiline_input.mk
    in
    let language_evt : Action.t event =
      input_event
      |> E.filter_map (function
             | Common.Evaluate_input str -> Some (Action.Update_language str)
             | _ -> None)
    in
    let term_input, input_event =
      model_s |> S.map (fun Model.{ term_str; _ } -> term_str) |> Single_line_input.mk
    in
    let term_evt : Action.t event =
      input_event
      |> E.filter_map (function
             | Common.Evaluate_input str -> Some (Action.Update_term str)
             | _ -> None)
    in
    let info = Provenance.of_here [%here] in
    let todo_sort = Sort.Ap (info, "term", Nil info) in
    let check_result_s =
      model_s
      |> S.map (fun Model.{ language_parsed; term_parsed; _ } ->
             match language_parsed, term_parsed with
             | Ok language, Ok term -> Some (Nominal.Term.check language todo_sort term)
             | _, _ -> None)
    in
    let result_elem =
      check_result_s
      |> S.map (function
             | None -> txt' "(parse error)"
             | Some None -> txt' "all good!"
             | Some (Some check_failure) -> view_check_failure check_failure)
      |> mk_reactive' div
    in
    let elem =
      div
        [ h2 [ txt' "Term Checking" ]
        ; div ~at:[ class' "container" ] [ language_input; term_input ]
        ; div ~at:[ class' "side" ] [ result_elem ]
        ]
    in
    E.select [ language_evt; term_evt ], elem
  ;;
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)
