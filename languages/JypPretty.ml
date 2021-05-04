(* https://jyp.github.io/posts/towards-the-prettiest-printer.html

TODO: make disjunctionless? https://github.com/jyp/prettiest/pull/10
 *)
open Base
open Lvca_syntax

module Lang =
[%abstract_syntax_module
{|
int : *
string : *

doc :=
  | Line() // insert a new line (unconditionally)
  | Nil() // the empty document
  | Cat(doc; doc) // concatenation
  | Text(string) // insert a meaningful piece of text
  | Spacing(string) // non-meaningful text
  | Nest(int; doc) // nest the argument
  | Align(doc) // align the documents in the argument
  | Alt(doc; doc) // disjunction
|}]

module Lang' = Lang (Primitive.Int) (Primitive.String)
module Doc = Lang'.Doc

type doc = Doc.Plain.t

let rec of_nonbinding tm =
  let open Result.Let_syntax in
  match tm with
  | NonBinding.Operator (_, "line", []) -> Ok Doc.Plain.Line
  | Operator (_, "nil", []) -> Ok Nil
  | Operator (_, "cat", [ d1; d2 ]) ->
    let%bind d1 = of_nonbinding d1 in
    let%map d2 = of_nonbinding d2 in
    Doc.Plain.Cat (d1, d2)
  | Operator (_, "text", [ Primitive (_, String s) ]) -> Ok (Text s)
  | Operator (_, "spacing", [ Primitive (_, String s) ]) -> Ok (Spacing s)
  | Operator (_, "nest", [ Primitive (_, Integer j); d ]) ->
    let%map d = of_nonbinding d in
    Doc.Plain.Nest (Z.to_int j, d)
  | Operator (_, "align", [ d ]) ->
    let%map d = of_nonbinding d in
    Doc.Plain.Align d
  | Operator (_, "alt", [ d1; d2 ]) ->
    let%bind d1 = of_nonbinding d1 in
    let%map d2 = of_nonbinding d2 in
    Doc.Plain.Alt (d1, d2)
  | Primitive _ | Operator _ -> Error ("Couldn't convert term", tm)
;;

type semantics = int -> int -> (string * int) list

let rec eval doc i (* current indentation *) c (* current column *) =
  match doc with
  | Doc.Plain.Line -> [ String.(of_char '\n' ^ make i ' '), i ]
  | Nil -> [ "", c ]
  | Cat (d1, d2) ->
    eval d1 i c
    |> List.concat_map ~f:(fun (t1, c1) ->
           eval d2 i c1 |> List.map ~f:(fun (t2, c2) -> t1 ^ t2, c2))
  | Text s | Spacing s -> [ s, c + String.length s ]
  | Nest (j, d) -> eval d Int.(i + j) c
  | Align d -> eval d c c
  | Alt (d1, d2) -> eval d1 i c @ eval d2 i c
;;

type docs = (int * doc) list

type process =
  { cur_indent : int (** current indentation *)
  ; progress : int
  ; tokens : string list (** tokens produced, in reverse order *)
  ; rest : docs (** rest of the input document to process *)
  }

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
      | [] -> [ First ts ] (* done *)
      | (i, d) :: ds ->
        (match d with
        | Nil -> rall p ts k ds
        | Text s -> rall (p + 1) (s :: ts) (k + String.length s) ds
        | Spacing s -> rall p (s :: ts) (k + String.length s) ds
        | Line ->
          [ Either.Second
              { cur_indent = i
              ; progress = p
              ; tokens = ("\n" ^ String.make i ' ') :: ts
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
               let measure { cur_indent; progress; _ } = cur_indent, -progress in
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

let%test_module _ =
  (module struct
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

    let space = [%lvca_nonbinding {|spacing(" ")|}]
    let line = [%lvca_nonbinding {|line()|}]
    let text str = NonBinding.Operator (None, "text", [ Primitive (None, String str) ])
    let cat l r = NonBinding.Operator (None, "cat", [ l; r ])
    let alt l r = NonBinding.Operator (None, "alt", [ l; r ])

    let cats lst =
      let init = List.last_exn lst in
      let lst = List.drop_last_exn lst in
      List.fold_right lst ~init ~f:cat
    ;;

    let parenthesize doc = cats [ text "("; doc; text ")" ]
    let sep_list list ~sep = list |> List.intersperse ~sep |> cats
    let vsep = sep_list ~sep:line
    let hsep = sep_list ~sep:space
    let counting_list n = List.init n ~f:(fun i -> i |> Int.to_string |> text)
    let align docs = NonBinding.Operator (None, "align", [ docs ])

    let%expect_test _ =
      let tm =
        let items = counting_list 5 in
        alt (hsep items) (vsep items)
      in
      test_render 9 tm;
      Stdio.print_string "\n";
      test_render 8 tm;
      [%expect {|
  0 1 2 3 4
  0
  1
  2
  3
  4 |}]
    ;;

    let%expect_test _ =
      let tm =
        cat [%lvca_nonbinding {|text("abc")|}] (3 |> counting_list |> vsep |> align)
      in
      test_render 5 tm;
      [%expect {|
        abc0
           1
           2 |}]
    ;;

    type sexpr =
      | SExpr of sexpr list
      | Atom of string

    let sep = function
      | [] -> [%lvca_nonbinding "nil()"]
      | xs -> alt (hsep xs) (align (vsep xs))
    ;;

    let rec pretty_sexp = function
      | Atom s -> text s
      | SExpr xs -> parenthesize (xs |> List.map ~f:pretty_sexp |> sep)
    ;;

    let%expect_test _ =
      let tm =
        let abcd = SExpr [ Atom "a"; Atom "b"; Atom "c"; Atom "d" ] in
        let abcd4 = SExpr [ abcd; abcd; abcd; abcd ] in
        SExpr [ Atom "axbxcxd"; abcd4 ] |> pretty_sexp
      in
      test_render 20 tm;
      [%expect
        {|
        (axbxcxd
         ((a b c d)
          (a b c d)
          (a b c d)
          (a b c d))) |}]
    ;;

    let%expect_test _ =
      let tm =
        let abcd = SExpr [ Atom "a"; Atom "b"; Atom "c"; Atom "d" ] in
        let abcd4 = SExpr [ abcd; abcd; abcd; abcd ] in
        SExpr [ Atom "axbxcxd"; abcd4 ] |> pretty_sexp
      in
      test_render 21 tm;
      [%expect
        {|
        (axbxcxd ((a b c d)
                  (a b c d)
                  (a b c d)
                  (a b c d))) |}]
    ;;
  end)
;;
