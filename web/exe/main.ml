open Bonsai_web
open Core_kernel
open Lvca_web2

let test_md = [%blob "test.md"]

let (_ : _ Start.Handle.t) =

  (* Points from sha256 to term *)
  let term_store = String.Table.create () in

  (* Point from name to sha *)
  let name_store = String.Table.of_alist_exn
    [ "term",
      "0000000000000000000000000000000000000000000000000000000000000001"
    ; "abstract_syntax",
      "0000000000000000000000000000000000000000000000000000000000000002"
    ; "concrete_syntax",
      "0000000000000000000000000000000000000000000000000000000000000003"
    ; "dynamics",
      "0000000000000000000000000000000000000000000000000000000000000004"
    ; "statics",
      "0000000000000000000000000000000000000000000000000000000000000005"
    ]
  in

  let component = test_md
    |> LanguageDocument.evaluate_and_produce_vdom { term_store; name_store }
    |> Bonsai.const
  in

  Start.start_standalone
    ~initial_input:()
    ~initial_model:()
    ~bind_to_element_with_id:"app"
    component
;;
