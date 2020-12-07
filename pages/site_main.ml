open Base
open Js_of_ocaml

let pages = Lvca_util.String.Map.of_alist_exn
  [ "term-and-concrete", TermAndConcrete.stateless_view
  ; "calculator", Calculator.stateless_view
  ; "parser", Parser.stateless_view
  ]

let insert_demo (elem : Dom_html.element Js.t) =
  let classes = elem##.classList in
  (match Js.Optdef.to_option (classes##item(0)) with
    | None -> Caml.Printf.printf "LVCA injected on element without classes\n"
    | Some cls ->
      let cls = Js.to_string cls in
      match Map.find pages cls with
        | None
        -> Caml.Printf.printf "LVCA injected on element with an unknown class (%s)\n" cls
        | Some page -> Dom.appendChild
          elem
          (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (page ())));
  Lwt.return ()
;;

let (_ : unit Lwt.t) =
  let doc = Dom_html.document in
  let parent : Dom_html.element Js.t =
    Js.Opt.get (doc##getElementById (Js.string "app")) (fun () -> assert false)
  in
  insert_demo parent
;;
