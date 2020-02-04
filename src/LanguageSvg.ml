(** Simplified model of SVG *)
let abstractSyntax = {|
import {integer, float, string} from "builtins";

viewbox := viewbox(integer; integer; integer; integer)

document := document(viewbox; integer; integer; list styled-element)

point := point(float; float)

// I don't feel like modeling all of css right now
// should include at least fill, stroke, stroke-dasharray?, etc
styled-element := styled-element(string; element)

element :=
  | rect(float; float; float; float; maybe float; maybe float)
  | circle(float; float; float)
  | ellipse(float; float; float; float)
  | line(float; float; float; float)
  | polyline(list point)
  | polygon(list point)
  | path(list line-command)

line-command := line-command(command-type; base-line-command)

command-type := absolute() | relative()

base-line-command :=
  | move(point; point) // M / m
  | line-to(point; point) // L / l
  | horizontal-line(point; point) // H / h
  | vertical-line(point; point) // V / v
  | close-path() // Z / z
  | cubic(point; point; point) // C / c
  | cubics(point; point) // S / s
  | quadratic(point; point) // Q / q
  | quadratics(point) // T / t
  | arc(float; float; float; bool; bool; float; float)
|};;

(* WIP *)
let concreteSyntax = {|
// TODO: this needs some work
path : list line-command := QUOTE list(line-command) QUOTE

line-command :=
  | MU point point { line-command(absolute(); move($2; $3)) }
  | ML point point { line-command(relative(); move($2; $3)) }
  | LU point point { line-command(absolute(); line-to($2; $3)) }
  | LL point point { line-command(relative(); line-to($2; $3)) }
  // etc
|};;

exception EvalSvgException of string

let expected str tm = raise (EvalSvgException (Printf.sprintf
  "svg evaluation: expected %s, got %s" str (NonBinding.to_string tm)
))
;;

let get_option : (NonBinding.term -> 'a) -> NonBinding.term -> 'a option
  = fun f -> function
    | Operator ("nothing", []) -> None
    | Operator ("just", [tm]) -> Some (f tm)

let get_int : NonBinding.term -> Bigint.t
  = function
    | Primitive (PrimInteger x) -> x
    | tm -> expected "integer" tm

let eval_point : NonBinding.term -> Bigint.t * Bigint.t
  = function
    | Operator ("point", [ x; y ]) -> get_int x, get_int y

let eval_element : NonBinding.term -> React.element
  = function
    | Operator ("rect", [ x; y; width; height; rx; ry ])
    -> ReactDOMRe.createDOMElementVariadic
        "rect"
        ~props:(ReactDOMRe.domProps
          ~x:(Bigint.to_string (get_int x))
          ~y:(Bigint.to_string (get_int y))
          ~width:(Bigint.to_string (get_int width))
          ~height:(Bigint.to_string (get_int height))
          ?rx:(get_option (fun x -> x |> get_int |> Bigint.to_string) rx)
          ?ry:(get_option (fun x -> x |> get_int |> Bigint.to_string) ry)
          ())
        [||]
    | Operator ("circle", [ cx; cy; r ])
    -> ReactDOMRe.createDOMElementVariadic
        "circle"
        ~props:(ReactDOMRe.domProps
          ~cx:(Bigint.to_string (get_int cx))
          ~cy:(Bigint.to_string (get_int cy))
          ~r:(Bigint.to_string (get_int r))
          ())
        [||]
    | Operator (op, [ Sequence points ])
    when op = "polyline" || op = "polygon"
    -> ReactDOMRe.createDOMElementVariadic
        op
        ~props:(ReactDOMRe.domProps
          ~points:(points
            |. Belt.List.toArray
            |. Belt.Array.map eval_point
            |. Belt.Array.map (fun (x, y) -> Printf.sprintf "%s,%s"
              (Bigint.to_string x)
              (Bigint.to_string y)
            )
            |. Js.Array2.joinWith " "
          )
          ())
        [||]
    | tm -> expected "element" tm

let eval_styled_element : NonBinding.term -> React.element
  = function
    | Operator ("styled-element", [style; element]) -> eval_element element
    | tm -> expected "styled-element" tm

let eval_viewbox : NonBinding.term -> string
  = function
    | Operator ("viewbox", [minX; minY; width; height])
    -> Printf.sprintf "%s %s %s %s"
      (Bigint.to_string (get_int minX))
      (Bigint.to_string (get_int minY))
      (Bigint.to_string (get_int width))
      (Bigint.to_string (get_int height))
    | tm -> expected "viewbox" tm

let eval : NonBinding.term -> React.element
  = function
    | Operator ("document", [viewbox; width; height; Sequence children])
    -> ReactDOMRe.createDOMElementVariadic
      "svg"
      ~props:(ReactDOMRe.domProps
      ~width:(Bigint.to_string (get_int width))
      ~height:(Bigint.to_string (get_int height))
      ~viewBox:(eval_viewbox viewbox)
      ())
      (children |. Belt.List.toArray |. Belt.Array.map eval_styled_element)
    | tm -> expected "document" tm

let eval_tm : Binding.Nominal.term -> (React.element, string) Belt.Result.t
  = fun tm ->
  match NonBinding.from_nominal tm with
  | None -> Error "failed to convert nominal term to nonbinding (svg)"
  | Some db_tm ->
    try
      Ok (eval db_tm)
    with
      EvalSvgException msg -> Error msg
;;


let coreAbstractSyntax = {|
// Similar to SVG, but the only element type supported is a path
import {integer, float, string} from "builtins";
import {base-line-command, command-type} from "svg";

document := document(integer; integer; list element)

element :=
  | path(list line-command)
  | text(
    float; // x
    float; // y
    maybe float; // dx
    maybe float; // dy
    list float; // rotate
    string // text
  )
|};;