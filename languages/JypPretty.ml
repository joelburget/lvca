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

TODO: make disjunctionless? https://github.com/jyp/prettiest/pull/10
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

let rec of_nonbinding tm =
  let open Result.Let_syntax in
  match tm with
  | NonBinding.Operator (_, "line", []) -> Ok Line
  | Operator (_, "nil", []) -> Ok Nil
  | Operator (_, "cat", [ d1; d2 ]) ->
    let%bind d1 = of_nonbinding d1 in
    let%map d2 = of_nonbinding d2 in
    Cat (d1, d2)
  | Operator (_, "text", [ Primitive (_, Primitive.PrimString s) ]) -> Ok (Text s)
  | Operator (_, "spacing", [ Primitive (_, Primitive.PrimString s) ]) -> Ok (Spacing s)
  | Operator (_, "nest", [ Primitive (_, Primitive.PrimInteger j); d ]) ->
    let%map d = of_nonbinding d in
    Nest (Z.to_int j, d)
  | Operator (_, "align", [ d ]) ->
    let%map d = of_nonbinding d in
    Align d
  | Operator (_, "alt", [ d1; d2 ]) ->
    let%bind d1 = of_nonbinding d1 in
    let%map d2 = of_nonbinding d2 in
    Alt (d1, d2)
  | Primitive _ | Operator _ -> Error ("Couldn't convert term", tm)
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

let render_fast : int -> doc -> string option =
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
  let rec loop : process list -> string list option =
   fun processes ->
    let dones, conts =
      processes
      |> List.concat_map ~f:(fun { cur_indent; progress; tokens; rest } ->
             rall progress tokens cur_indent rest)
      |> List.partition_map ~f:Fn.id
    in
    match dones with
    | done_ :: _ -> Some done_
    | [] ->
      (match conts with
      | _ :: _ ->
        conts
        |> List.sort ~compare:(fun x y ->
               Lvca_util.Tuple2.compare
                 ~cmp1:Int.compare
                 ~cmp2:Int.compare
                 (measure x)
                 (measure y))
        |> filtering
        |> loop
      | [] -> None)
  in
  [ { cur_indent = 0; progress = 0; tokens = []; rest = [ 0, doc ] } ]
  |> loop
  |> Option.map ~f:(fun strs -> strs |> List.rev |> String.concat)
;;

let test_render width tm =
  match of_nonbinding tm with
  | Ok tm ->
    let str =
      match render_fast width tm with None -> "no valid render" | Some str -> str
    in
    Stdio.print_string str
  | Error (msg, _) -> failwith msg
;;

let%expect_test _ =
  let tm = [%lvca_nonbinding {|cat(text("("); text(")"))|}] in
  test_render 80 tm;
  [%expect {| () |}]
;;

let%expect_test _ =
  let tm =
    [%lvca_nonbinding
      {|alt(
    cat(text("abc"); cat(spacing(" "); text("def")));
    cat(text("abc"); cat(line(); text("def")))
    )|}]
  in
  test_render 7 tm;
  Stdio.print_string "\n";
  test_render 6 tm;
  [%expect {|
    abc def
    abc
    def |}]
;;

let%expect_test _ =
  let tm = [%lvca_nonbinding {|cat(text("abc"); cat(text("def"); text("ghi")))|}] in
  test_render 1 tm;
  Stdio.print_string "\n";
  test_render 9 tm;
  [%expect {|
  no valid render
  abcdefghi |}]
;;

let%expect_test _ =
  let tm =
    let init = [%lvca_nonbinding {|text("9")|}] in
    let space = [%lvca_nonbinding {|spacing(" ")|}] in
    let line = [%lvca_nonbinding {|line()|}] in
    let cat l r = NonBinding.Operator (None, "cat", [ l; r ]) in
    let mk_chain sep =
      List.init 9 ~f:(fun i ->
          NonBinding.Operator
            (None, "text", [ Primitive (None, Primitive.PrimString (Int.to_string i)) ]))
      |> List.fold_right ~init ~f:(fun l r -> cat l (cat sep r))
    in
    NonBinding.Operator (None, "alt", [ mk_chain space; mk_chain line ])
  in
  test_render 19 tm;
  Stdio.print_string "\n";
  test_render 18 tm;
  [%expect {|
  0 1 2 3 4 5 6 7 8 9
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9 |}]
;;
