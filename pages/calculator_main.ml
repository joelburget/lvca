open Js_of_ocaml

let insert_demo elem =
  Dom.appendChild
    elem
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div Calculator.stateless_view);
  Lwt.return ()
;;

let (_ : unit Lwt.t) =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById (Js.string "app")) (fun () -> assert false)
  in
  insert_demo parent
;;
