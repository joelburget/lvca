(* https://jyp.github.io/posts/towards-the-prettiest-printer.html

data Doc where
  Line :: Doc
  Nil :: Doc
  (:<>) :: Doc -> Doc -> Doc
  Text :: String -> Doc
  Nest :: Int -> Doc -> Doc
  Align :: Doc -> Doc
  (:<|>) :: Doc -> Doc -> Doc -- ^ Attn: INVARIANT
  Spacing :: String -> Doc
 *)
open Base
open Lvca_syntax

let language =
  [%lvca_abstract_syntax
    {|
doc :=
  | line()
  | nil()
  | cat(doc; doc)
  | text(string)
  | spacing(string)
  | nest(int; doc)
  | align(doc)
  | alt(doc; doc)
|}]
;;

type doc =
  | Line
  | Nil
  | Cat of doc * doc
  | Text of string
  | Spacing of string
  | Nest of int * doc
  | Align of doc
  | Alt of doc * doc

let rec of_term = function
  | NonBinding.Operator (_, "line", []) -> Line
  | Operator (_, "nil", []) -> Nil
  | Operator (_, "cat", [ d1; d2 ]) -> Cat (of_term d1, of_term d2)
  | Operator (_, "text", [ Primitive (_, Primitive.PrimString s) ]) -> Text s
  | Operator (_, "spacing", [ Primitive (_, Primitive.PrimString s) ]) -> Spacing s
  | Operator (_, "nest", [ Primitive (_, Primitive.PrimInteger j); d ]) ->
    Nest (Z.to_int j, of_term d)
  | Operator (_, "align", [ d ]) -> Align (of_term d)
  | Operator (_, "alt", [ d1; d2 ]) -> Alt (of_term d1, of_term d2)
  | Primitive _ -> failwith "TODO: error"
  | Operator _ -> failwith "TODO: error"
;;

type semantics = int -> int -> (string * int) list

let rec eval doc i (* current indentation *) c (* current column *) =
  match doc with
  | Line -> [ String.(of_char '\n' ^ make i ' '), i ]
  | Nil -> [ "", c ]
  | Cat (d1, d2) ->
    eval d1 i c
    |> List.concat_map ~f:(fun (t1, c1) ->
           eval d2 i c1 |> List.map ~f:(fun (t2, c2) -> t1 ^ t2, c2))
  | Text s | Spacing s -> [ s, c + String.length s ]
  | Nest (j, d) -> eval d (i + j) c
  | Align d -> eval d c c
  | Alt (d1, d2) -> eval d1 i c @ eval d2 i c
;;

type docs = (int * doc) list

type process =
  { cur_indent : int
  ; progress : int
  ; tokens : string list
  ; rest : docs
  }

let measure { cur_indent; progress; _ } = cur_indent, -progress

let rec filtering : process list -> process list = function
  | x :: y :: xs ->
    if x.progress >= y.progress then filtering (x :: xs) else x :: filtering (y :: xs)
  | xs -> xs
;;

let render_fast : int -> doc -> string =
 fun w doc ->
  let rec rall : int -> string list -> int -> docs -> (string list, process) Either.t list
    =
   fun p ts k ds0 ->
    if Int.(k > w)
    then []
    else (
      match ds0 with
      | [] -> [ First ts ]
      | (i, d) :: ds ->
        (match d with
        | Nil -> rall p ts k ds
        | Text s -> rall (p + 1) (s :: ts) (k + String.length s) ds
        | Spacing s -> rall p (s :: ts) (k + String.length s) ds
        | Line ->
          [ Either.Second
              { cur_indent = i
              ; progress = p
              ; tokens = String.(of_char '\n' ^ String.make i ' ') :: ts
              ; rest = ds
              }
          ]
        | Cat (x, y) -> rall p ts k ((i, x) :: (i, y) :: ds)
        | Nest (j, x) -> rall p ts k ((i + j, x) :: ds)
        | Alt (x, y) -> rall p ts k ((i, x) :: ds) @ rall p ts k ((i, y) :: ds)
        | Align x -> rall p ts k ((k, x) :: ds)))
  in
  let rec loop : process list -> string list =
   fun ps ->
    let ps =
      ps
      |> List.concat_map ~f:(fun { cur_indent; progress; tokens; rest } ->
             rall progress tokens cur_indent rest)
    in
    let dones, conts = List.partition_map ps ~f:Fn.id in
    match dones with
    | done_ :: _ -> done_
    | [] ->
      (match conts with
      | _ :: _ -> conts |> List.sort ~compare:(failwith "TODO") |> filtering |> loop
      | [] -> failwith "overflow")
  in
  [ { cur_indent = 0; progress = 0; tokens = []; rest = [ 0, doc ] } ]
  |> loop
  |> List.rev
  |> String.concat
;;

let cvt tm =
  match NonBinding.of_nominal tm with
  | Ok tm -> of_term tm
  | Error _ -> failwith "failed to convert from nominal"
;;

let%expect_test _ =
  let tm = [%lvca_term {|cat(text("("); text(")"))|}] in
  Stdio.print_string (render_fast 80 (cvt tm));
  [%expect {| () |}]
;;

(*
let x <+> y = x <> spacing(" ") <> y in
let x </> y = x <> line() <> y in
let sep xs = match xs {
  | [] -> Nil
  | xs -> alt(foldr1 (<+>) xs; align(foldr1 (</>) xs))
}
...
*)
