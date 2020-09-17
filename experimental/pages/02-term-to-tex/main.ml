open Lvca_web
open Js_of_ocaml
open Lwt.Infix

module Action = struct
  type action = Evaluate of string
end

module Model = struct
  type t = string

  let initial_value =
end

type signal = Model.t React.signal
type update_fun = ?step:React.step -> Model.t -> unit
type react_pair = signal * update_fun

module Controller = struct
  let update (Evaluate str : Action.action) ((_model_signal, set_model) : react_pair) =
    (* let m = React.S.value r in *)
    let m' = str in
    set_model m'
  ;;
end

module View = struct
  open Js_of_ocaml_tyxml.Tyxml_js
  module Ev = Js_of_ocaml_lwt.Lwt_js_events

  let bind_event ev elem handler =
    let handler' evt _ = handler evt in
    Ev.async (fun () -> ev elem handler')
  ;;

  (* let textarea = Html5.(textarea (txt "") ~a:[ a_autofocus (); a_rows 25; a_cols 90; ])
     let textarea_dom = To_dom.of_textarea textarea let set_textarea v =
     textarea_dom##.value := Js.string v let focus_textarea () = textarea_dom##focus *)

  let codemirror_elem, codemirror = Codemirror.mk_codemirror Model.initial_value
  let katex_area = Html5.div []
  let katex_dom = To_dom.of_div katex_area
  let set_katex = Katex.render katex_dom

  (* let key_handler rp : Xml.keyboard_event_handler = fun evt -> *)
  let key_handler rp
      : Codemirror.codemirror_type Js.t -> Dom_html.keyboardEvent Js.t -> bool
    =
   fun cm evt ->
    (* let tgt = Dom_html.CoerceTo.textarea (Dom.eventTarget evt) in *)
    if Web_util.is_special_enter evt then Dom.preventDefault evt;
    let str = Js.to_string (cm##.getValue ()) in
    Printf.printf "eval '%s'\n" str;
    set_katex str;
    Controller.update
      (Evaluate str)
      rp
      (* Js.Opt.case tgt (fun () -> ()) (fun input -> let str = Js.to_string input##.value
         in Printf.printf "eval '%s'\n" str; set_katex str; Controller.update (Evaluate
         str) rp ); *)
      true
 ;;

  (* let textarea' (rp : react_pair) = bind_event Ev.keypresses textarea_dom (fun evt ->
     Lwt.return @@ (let (_ : bool) = key_handler rp evt in ())); textarea *)

  let view (_rp : react_pair) =
    Html5.div [ (* textarea' rp; *) codemirror_elem; katex_area ]
  ;;
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app")) (fun () -> assert false)
  in
  let m = Model.initial_value in
  let react_pair = React.S.create m in
  Dom.appendChild parent (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view react_pair));
  (* View.set_textarea m; *)
  View.codemirror##refresh ();
  View.codemirror##on_change
    (Js.string "change")
    (Js.wrap_callback (fun _ -> Printf.printf "change 2\n"));
  (* View.codemirror##on (Js.string "cursorActivity") (Js.wrap_callback (fun _ ->
     Printf.printf "cursor activity\n")); View.codemirror##on (Js.string "focus")
     (Js.wrap_callback (fun _ -> Printf.printf "focus\n")); View.codemirror##on (Js.string
     "update") (Js.wrap_callback (fun _ -> Printf.printf "update\n")); *)
  View.codemirror##on_keypress
    (Js.string "keypress")
    (Js.wrap_callback (fun cm evt ->
         let (_ : bool) = View.key_handler react_pair cm evt in
         ()));
  Lwt.return ()
;;

let (_ : unit Lwt.t) = Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main
