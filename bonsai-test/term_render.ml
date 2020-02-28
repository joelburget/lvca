(* open Bonsai_web *)
open Core_kernel
open Lvca
open Virtual_dom

module Term_render_component = struct
  module Input = struct
    type t =
      { eval : Binding.Nominal.term -> (Vdom.Node.t, string) Result.t
      ; start_str : string
      }
  end
  module Result = Vdom.Node
  module Model = String
  module Action = struct
    type t = Evaluate of string [@@deriving sexp]
  end

  let apply_action ~inject:_ ~schedule_event:_ () _model (Action.Evaluate str)
    = str

  let compute ~inject { Input.eval; _ } str =
    let module P_term = Parsing.Incremental(Parsing.Parseable_term) in
    let elem = str
      |> P_term.parse
      |> Core_kernel.Result.bind ~f:eval
      |> Fn.flip Util.get_result Vdom.Node.text
    in

    let is_key_ret key =
      String.equal
        "Enter"
        (key##.code
         |> Js_of_ocaml.Js.Optdef.to_option
         |> Option.value_exn
         |> Js_of_ocaml.Js.to_string)
    in

    (*
    let handleKeyDown = event => {
      if (ReactEvent.Keyboard.key(event) == "Enter" &&
          (ReactEvent.Keyboard.ctrlKey(event) ||
           ReactEvent.Keyboard.metaKey(event))) {
        setStr(ReactEvent.Keyboard.target(event)##value);
      }
    };
    *)

    let handle_keydown = Vdom.Attr.on_keydown (fun key ->
      if is_key_ret key
      then
        inject (Action.Evaluate str)
        (*
        Vdom.Event.Many
          [ Input.eval input model.Model.message
          ; inject (Action.Evaluate str)
          ]
          *)
      else Vdom.Event.Ignore
    )
    in

    Vdom.Node.(
      div
       []
       [ textarea (Vdom.Attr.(
           [ string_property "rows" "20";
             string_property "cols" "80";
             handle_keydown;
           ]))
           [];
         div [] [ elem ];
       ])

end
