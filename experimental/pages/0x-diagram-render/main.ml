open Base
open Js_of_ocaml
open Lvca

module Language = struct
  module PT = Parsing.NonBindingTerm

  let language =
    {|
  { float_lit }

  float :=
    | lit(float_lit())
    | add(float(); float())
    | sub(float(); float())
    | mul(float(); float())
    | div(float(); float())

  p2 := p2(float(); float())
  size2 := size2(float(); float())
  box2 := box2(p2(); size2())

  path :=
    // subpaths and segments
    | empty()
    | sub(p2(); path())
    | line(p2(); path())
    | qcurve(p2(); p2(); path())
    | ccurve(p2(); p2(); p2(); path())
    | earc(size2(); p2(); path())
    | close(path())

    // derived paths
    | circle(p2(); float(); path())
    | ellipse(p2(); size2(); path())
    | rect(box2(); path())
    | rrect(box2(); size2(); path())

  image :=
    // primitive images
    | const(color()) // TODO: color
    // TODO: axial, radial

    // cutting images
    | cut(path(); image()) // TODO: area

    // blending images
    | blend(image(); image())

    // transforming images
    | move(v2(); image())
    | rot(float(); image())
    | scale(v2(); image())
    // TODO: transform

  |}
  ;;

  let parse : string -> PT.parse_result = PT.parse
end

module Parser = struct
  open Angstrom
  open Util.Angstrom

  let op0 name = NonBinding.Operator (name, [])
  let op1 name a = NonBinding.Operator (name, [ a ])
  let op2 name a b = NonBinding.Operator (name, [ a; b ])
  let op3 name a b c = NonBinding.Operator (name, [ a; b; c ])
  let op4 name a b c d = NonBinding.Operator (name, [ a; b; c; d ])

  let bin_op
      :  char Angstrom.t -> string
      -> (NonBinding.term -> NonBinding.term -> NonBinding.term) Angstrom.t
    =
   fun op_consumer name -> op_consumer *> return (op2 name)
 ;;

  let add = bin_op (char' '+') "add"
  let sub = bin_op (char' '-') "sub"
  let mul = bin_op (char' '*') "mul"
  let div = bin_op (char' '/') "div"

  let float : NonBinding.term Angstrom.t =
    fix (fun float ->
        let float_lit' = float_lit >>| fun f -> NonBinding.Primitive (PrimFloat f) in
        let factor = parens float <|> float_lit' in
        let term = chainl1 factor (mul <|> div) in
        chainl1 term (add <|> sub))
    <?> "float"
  ;;

  let p2 : NonBinding.term Angstrom.t =
    lift4 (fun _ f1 f2 _ -> op2 "p2" f1 f2) (string' "p2<") float float (char' '>')
    <?> "p2"
  ;;

  let size2 : NonBinding.term Angstrom.t =
    lift4 (fun _ f1 f2 _ -> op2 "size2" f1 f2) (string' "size2<") float float (char' '>')
    <?> "size2"
  ;;

  let box2 : NonBinding.term Angstrom.t =
    lift4 (fun _ p2 size2 _ -> op2 "box2" p2 size2) (string' "box2<") p2 size2 (char' '>')
    <?> "box2"
  ;;

  let rparen = char' ')'
  let semi = char' ';'

  let path : NonBinding.term Angstrom.t =
    let term =
      choice
        [ lift3 (fun _ p _ -> "sub", [ p ]) (string' "sub(") p2 rparen
        ; lift3 (fun _ p _ -> "line", [ p ]) (string' "line(") p2 rparen
        ; lift4 (fun _ p1 _ p2 _ -> "qcurve", [ p1; p2 ]) (string' "qcurve(") p2 semi p2
          <*> rparen
        ; lift4
            (fun _ p1 _ p2 _ p3 _ -> "ccurve", [ p1; p2; p3 ])
            (string' "ccurve(")
            p2
            semi
            p2
          <*> semi
          <*> p2
          <*> rparen
        ; lift4 (fun _ e _ p _ -> "qcurve", [ e; p ]) (string' "earc(") size2 semi p2
          <*> rparen
        ; lift (fun _ -> "close", []) (string' "close")
        ; lift4 (fun _ p _ f _ -> "circle", [ p; f ]) (string' "circle(") p2 semi float
          <*> rparen
        ; lift4
            (fun _ p _ size _ -> "ellipse", [ p; size ])
            (string' "ellipse(")
            p2
            semi
            size2
          <*> rparen
        ; lift3 (fun _ box _ -> "rect", [ box ]) (string' "rect(") box2 rparen
        ; lift4
            (fun _ box _ size _ -> "rrect", [ box; size ])
            (string' "rrect(")
            box2
            semi
            size2
          <*> rparen
        ]
      <?> "path"
    in
    sep_by1 semi term >>| fun terms -> NonBinding.Operator ("path", terms)
  ;;

  let color : NonBinding.term Angstrom.t =
    choice
      [ lift4
          (fun _ f1 _ f2 _ f3 _ -> op3 "rgb" f1 f2 f3)
          (string' "rgb(")
          float
          semi
          float
        <*> semi
        <*> float
        <*> rparen
        (* rgbi? *)
      ; lift4
          (fun _ fr _ fg _ fb _ fa _ -> op4 "rgba" fr fg fb fa)
          (string' "rgba(")
          float
          semi
          float
        <*> semi
        <*> float
        <*> semi
        <*> float
        <*> rparen
      ; lift3 (fun _ f _ -> op1 "gray" f) (string' "gray(") float rparen
      ; string' "void" *> return (op0 "void")
      ; string' "black" *> return (op0 "black")
      ; string' "white" *> return (op0 "white")
      ; string' "red" *> return (op0 "red")
      ; string' "green" *> return (op0 "green")
      ; string' "blue" *> return (op0 "blue")
        (* TODO: blend, clamp, with_a, etc *)
      ]
    <?> "color"
  ;;

  let image : NonBinding.term Angstrom.t =
    fix (fun image ->
        choice
          [ lift3 (fun _ color _ -> op1 "const" color) (string' "const(") color rparen
          ; lift4
              (fun _ path _ img _ -> op2 "cut" path img)
              (string' "cut(" <* whitespace)
              path
              semi
              image
            <*> rparen
          ]
        <?> "image")
  ;;
end

module Model = struct
  type t =
    { input : string
    ; result : (NonBinding.term, string) Result.t option
    }

  let initial_model : t =
    let input =
      (* TODO: modules: everything is crammed in one namespace optionals / named arguments *)
      {|cut(circle(p2<0.5 0.5>; 0.4); const(rgba(0.5; 0.2; 0.3; 0.5)))|}
    in
    { input; result = None }
  ;;
end

module Render = struct
  open Result.Let_syntax

  let float_ : NonBinding.term -> (float, string) Result.t = function
    | Primitive (PrimFloat f) -> Ok f
    | _ -> Error "TODO 2"
  ;;

  let color_ : NonBinding.term -> (Gg.color, string) Result.t =
    let open Gg.Color in
    function
    | Operator ("rgb", [ r; g; b ]) ->
      let%bind r' = float_ r in
      let%bind g' = float_ g in
      let%map b' = float_ b in
      v_srgb r' g' b'
    | Operator ("rgba", [ r; g; b; a ]) ->
      let%bind r' = float_ r in
      let%bind g' = float_ g in
      let%bind b' = float_ b in
      let%map a' = float_ a in
      v r' g' b' a'
    | Operator ("gray", [ f ]) ->
      let%map f' = float_ f in
      gray f'
    | Operator ("void", []) -> Ok void
    | Operator ("black", []) -> Ok black
    | Operator ("white", []) -> Ok white
    | Operator ("red", []) -> Ok red
    | Operator ("green", []) -> Ok green
    | Operator ("blue", []) -> Ok blue
    | tm ->
      Error (Printf.sprintf "render: unknown color term: %s" (NonBinding.to_string tm))
  ;;

  let p2_ : NonBinding.term -> (Gg.p2, string) Result.t = function
    | Operator ("p2", [ f1; f2 ]) ->
      let%bind f1' = float_ f1 in
      let%map f2' = float_ f2 in
      Gg.P2.v f1' f2'
    | _ -> Error "TODO 4"
  ;;

  let rec path_ : NonBinding.term -> (Vg.path, string) Result.t = function
    | Operator ("path", components) -> failwith "TODO"
  ;;

  let path_component : Vg.path -> NonBinding.term -> (Vg.path, string) Result.t =
   fun path -> function
    | Operator ("circle", [ p2; float; path ]) ->
      let%bind p2' = p2_ p2 in
      let%bind float' = float_ float in
      let%map path' = path_ path in
      Vg.P.circle p2' float' path'
    | Operator ("empty", []) -> Ok Vg.P.empty
    | Operator (op, _) -> Error (Printf.sprintf "path_: unknown operator %s" op)
    | _ -> Error "TODO 5"
 ;;

  let rec image_ : NonBinding.term -> (Vg.image, string) Result.t = function
    | Operator ("const", [ color ]) ->
      let%map color' = color_ color in
      Vg.I.const color'
    | Operator ("cut", [ path; image ]) ->
      let%bind path' = path_ path in
      let%map image' = image_ image in
      Vg.I.cut path' image'
    | tm -> Error (Printf.sprintf "TODO 6: %s" (NonBinding.to_string tm))
  ;;

  (* let diagram : NonBinding.term -> (Vg.image, string) Result.t = function | Operator
     ("diagram", [img]) -> image_ img | _ -> Error "TODO 7" *)

  let model : Model.t -> (Vg.image, string) Result.t =
   fun { result; _ } ->
    match result with
    | None -> Error "TODO 8"
    | Some (Ok img) -> image_ img
    | Some (Error err) -> Error err
 ;;
end

module Action = struct
  type t =
    | UpdateInput of string
    | Evaluate of string
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module Controller = struct
  let update (action : Action.t) ((r, f) : react_pair) =
    let open Model in
    let { input = _; result } = React.S.value r in
    let new_model =
      match action with
      | Action.UpdateInput str -> { input = str; result }
      | Action.Evaluate str ->
        { input = str; result = Some (ParseUtil.parse_string Parser.image str) }
    in
    f new_model
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler evt _ = handler evt in
    Ev.(async @@ fun () -> ev elem handler)
  ;;

  let mk_input ((signal, _f) as react_pair : react_pair) =
    let input =
      signal
      |> React.S.map (fun m -> m.Model.input)
      |> fun (value : string React.signal) ->
      Html5.(textarea ~a:[ a_rows 25; a_cols 90; a_autofocus () ] (R.Html5.txt value))
    in
    let input_dom = To_dom.of_textarea input in
    (* XXX need to trigger render on Evaluate *)
    bind_event Ev.keydowns input_dom (fun evt ->
        Lwt.return
          (if Lvca_web.Web_util.is_special_enter evt
          then (
            Dom.preventDefault evt;
            Controller.update (Evaluate (Js.to_string input_dom##.value)) react_pair)));
    input
  ;;

  let mk_viewport (signal : (Vg.image, string) Result.t React.signal) =
    signal
    |> React.S.map (function
           | Ok img ->
             let open Gg in
             let open Vg in
             let canvas = R.Html5.canvas ReactiveData.RList.empty in
             let canvas_dom = To_dom.of_canvas canvas in
             let size = Size2.v 100. 100. in
             (* TODO: make configurable *)
             let view = Box2.v P2.o (Size2.v 1. 1.) in
             (* TODO: make configurable *)
             let r = Vgr.create (Vgr_htmlc.target canvas_dom) `Other in
             ignore (Vgr.render r (`Image (size, view, img)) : [ `Ok | `Partial ]);
             ignore (Vgr.render r `End : [ `Ok | `Partial ]);
             canvas
           | Error msg -> Html5.txt msg)
    |> ReactiveData.RList.singleton_s
    |> R.Html5.div
  ;;

  let info signal =
    signal
    |> React.S.map (fun m ->
           match m.Model.result with
           | None -> "(press (ctrl/shift/meta)-enter to evaluate)"
           | Some (Ok _tm) -> ""
           | Some (Error err) -> err)
    |> React.S.map Html5.txt
    |> ReactiveData.RList.singleton_s
    |> R.Html5.div
  ;;

  let view ((signal, _update_fun) as react_pair : react_pair) =
    Html5.(
      div
        [ mk_input react_pair
        ; signal |> React.S.map Render.model |> mk_viewport
        ; info signal
        ])
  ;;
end

let main _ =
  let parent =
    Js.Opt.get
      (Dom_html.document##getElementById (Js.string "app"))
      (fun () -> assert false)
  in
  let m = Model.initial_model in
  let react_pair = React.S.create m in
  Dom.appendChild parent (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view react_pair));
  Lwt.return ()
;;

let (_ : unit Lwt.t) =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main
;;
