open Base
open Brr
open Note
open Prelude
open Lvca_syntax
module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.NoComment)
module NominalParse = Nominal.Parse (ParseUtil.NoComment)
module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

let parse_lang lang_str = ParseUtil.parse_string AbstractSyntaxParse.whitespace_t lang_str

let parse_term term_str =
  ParseUtil.parse_string (NominalParse.t ParsePrimitive.t) term_str
;;

module Model = struct
  type t =
    { language_str : string
    ; language_parsed : (OptRange.t AbstractSyntax.t, string) Result.t
    ; term_str : string
    ; term_parsed : ((OptRange.t, Primitive.t) Nominal.term, string) Result.t
    }

  let language_str =
    {|
value :=
  | unit()
  | lit_int(integer())
  | lit_str(string())

match_line :=
  | match_line(value()*. term())

term :=
  | lambda(value(). term())
  | alt_lambda(term(). term())
  | match(match_line()*)
  | value(value())
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
    | UpdateLanguage of string
    | UpdateTerm of string
end

module Controller = struct
  let update (action : Action.t) model =
    match action with
    | UpdateLanguage language_str ->
      { model with Model.language_str; language_parsed = parse_lang language_str }
    | UpdateTerm term_str -> { model with term_str; term_parsed = parse_term term_str }
  ;;
end

module View = struct
  let div, h2, h3, table, td, thead, tr = El.(div, h2, h3, table, td, thead, tr)

  let rec view_pat = function
    | Pattern.Var (_, name) -> txt name
    | Ignored (_, name) -> txt ("_" ^ name)
    | Primitive (_, prim) -> txt (Fmt.to_to_string Primitive.pp prim)
    | Operator (_, name, pats) ->
      div [ txt name; div (List.map pats ~f:(fun pat -> div [ view_pat pat ])) ]
  ;;

  let rec view_term = function
    | Nominal.Var (_, name) -> txt name
    | Primitive (_, prim) -> txt (Fmt.to_to_string Primitive.pp prim)
    | Operator (_, name, scopes) -> div [ txt name; div (List.map scopes ~f:view_scope) ]

  and view_scope (Nominal.Scope (pats, tm)) =
    div [ pats |> List.map ~f:view_pat |> div; tm |> view_term ]
  ;;

  let rec view_sort = function
    | Sort.Name (_, name) -> txt name
    | Sort.Ap (_, name, subsorts) ->
      div [ txt name; div (subsorts |> List.map ~f:view_sort) ]
  ;;

  let view_check_frame
      :  ( OptRange.t
         , ( ('info, Primitive.t) Pattern.t
           , ('info, Primitive.t) Nominal.term )
           Base.Either.t )
         CheckFailure.frame
      -> El.t
    =
   fun { term; sort } ->
    tr
      [ td
          [ (match term with
            | Either.First pat -> view_pat pat
            | Second tm -> view_term tm)
          ]
      ; td [ view_sort sort ]
      ]
 ;;

  let view_check_failure
      :  ( OptRange.t
         , ( ('info, Primitive.t) Pattern.t
           , ('info, Primitive.t) Nominal.term )
           Base.Either.t )
         CheckFailure.t
      -> El.t
    =
   fun { message; stack } ->
    let table_header = thead [ td [ txt "term" ]; td [ txt "sort" ] ] in
    let table_rows = stack |> List.map ~f:view_check_frame in
    div [ txt message; table (table_header :: table_rows) ]
 ;;

  let view model_s =
    let language_input, input_event =
      model_s
      |> S.map (fun Model.{ language_str; _ } -> language_str)
      |> MultilineInput.mk
    in
    let language_evt : Action.t event =
      input_event
      |> E.filter_map (function
             | Common.InputUpdate str -> Some (Action.UpdateLanguage str)
             | _ -> None)
    in
    let term_input, input_event =
      model_s |> S.map (fun Model.{ term_str; _ } -> term_str) |> SingleLineInput.mk
    in
    let term_evt : Action.t event =
      input_event
      |> E.filter_map (function
             | Common.InputUpdate str -> Some (Action.UpdateTerm str)
             | _ -> None)
    in
    let todo_sort = Sort.Ap (None, "term", []) in
    let check_result_s =
      model_s
      |> S.map (fun Model.{ language_parsed; term_parsed; _ } ->
             match language_parsed, term_parsed with
             | Ok language, Ok term ->
               Some (Nominal.Primitive.check language todo_sort term)
             | _, _ -> None)
    in
    let result_elem =
      check_result_s
      |> S.map (function
             | None -> txt "(parse error)"
             | Some None -> txt "all good!"
             | Some (Some check_failure) -> view_check_failure check_failure)
      |> mk_reactive' div
    in
    let elem =
      div
        [ h2 [ txt "Term Checking" ]
        ; div ~at:[ class' "container" ] [ language_input; term_input ]
        ; div ~at:[ class' "side" ] [ result_elem ]
        ]
    in
    E.select [ language_evt; term_evt ], elem
  ;;
end

let stateless_view () =
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;
