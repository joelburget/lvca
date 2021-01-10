open Base
open Brr

let pages =
  Lvca_util.String.Map.of_alist_exn
    [ "term-and-concrete", TermAndConcrete.stateless_view
    ; "calculator", Calculator.stateless_view (* ; "parser", Parser.stateless_view *)
    ; "binding-viewer", ScopeViewer.stateless_view
    ]
;;

let main () =
  (* let open Fut.Syntax in *)
  (* let* _ev = Ev.next Ev.load (Window.as_target G.window) in *)
  match Document.find_el_by_id G.document (Jstr.v "app") with
  | None -> Console.(log [ str "no app found" ])
  | Some app_elem ->
    (match El.at (Jstr.v "class") app_elem with
    | None -> Console.(log [ str "no class attr" ])
    | Some attr ->
      let cls = Jstr.to_string attr in
      (match Map.find pages cls with
      | None -> Console.(log [ str "page not found" ])
      | Some page -> El.set_children app_elem [ page () ]))
;;

(* Fut.return () *)

let () = main () (* ignore (main ()) *)
