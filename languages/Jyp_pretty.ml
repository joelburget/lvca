(* https://jyp.github.io/posts/towards-the-prettiest-printer.html

TODO: make disjunctionless? https://github.com/jyp/prettiest/pull/10
 *)
open Base
open Lvca_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
int32 : *  // module Primitive.Int32
string : *  // module Primitive.String

doc :=
  | Line()
  | Nil()
  | Cat(doc; doc)
  | Text(string)
  | Spacing(string)
  | Nest(int32; doc)
  | Align(doc)
  | Alt(doc; doc)
|}]

module Doc = Nominal.Convertible.Extend (Lang.Doc)

let of_nonbinding tm =
  tm |> Nonbinding.to_nominal |> Doc.of_nominal |> Result.map ~f:Doc.to_plain
;;

type semantics = int -> int -> (string * int) list

let rec eval doc i (* current indentation *) c (* current column *) =
  match doc with
  | Lang.Plain.Line -> [ String.(of_char '\n' ^ make i ' '), i ]
  | Nil -> [ "", c ]
  | Cat (d1, d2) ->
    eval d1 i c
    |> List.concat_map ~f:(fun (t1, c1) ->
           eval d2 i c1 |> List.map ~f:(fun (t2, c2) -> t1 ^ t2, c2))
  | Text s | Spacing s -> [ s, c + String.length s ]
  | Nest (j, d) -> eval d Int.(i + of_int32_exn j) c
  | Align d -> eval d c c
  | Alt (d1, d2) -> eval d1 i c @ eval d2 i c
;;

(* type docs = (int * doc) list *)
type docs = (int * Doc.Plain.t) list

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

let render_fast : int -> Doc.Plain.t -> string option =
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
        | Lang.Plain.Nil -> rall p ts k ds
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
        | Nest (j, x) -> rall p ts k ((i + Int.of_int32_exn j, x) :: ds)
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
      | Error _tm -> failwith "failed to convert term from nonbinding"
    ;;

    let%expect_test _ =
      let tm = [%lvca.nonbinding {|Cat(Text("("); Text(")"))|}] in
      test_render 80 tm;
      [%expect {| () |}]
    ;;

    let%expect_test _ =
      let tm =
        [%lvca.nonbinding
          {|Alt(
    Cat(Text("abc"); Cat(Spacing(" "); Text("def")));
    Cat(Text("abc"); Cat(Line(); Text("def")))
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
      let tm = [%lvca.nonbinding {|Cat(Text("abc"); Cat(Text("def"); Text("ghi")))|}] in
      test_render 1 tm;
      Stdio.print_string "\n";
      test_render 9 tm;
      [%expect {|
  no valid render
  abcdefghi |}]
    ;;

    let none = Lvca_provenance.Commented.none
    let space = [%lvca.nonbinding {|Spacing(" ")|}]
    let line = [%lvca.nonbinding {|Line()|}]
    let text str = Nonbinding.Operator (none, "Text", [ Primitive (none, String str) ])
    let cat l r = Nonbinding.Operator (none, "Cat", [ l; r ])
    let alt l r = Nonbinding.Operator (none, "Alt", [ l; r ])

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
    let align docs = Nonbinding.Operator (none, "Align", [ docs ])

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
        cat [%lvca.nonbinding {|Text("abc")|}] (3 |> counting_list |> vsep |> align)
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
      | [] -> [%lvca.nonbinding "Nil()"]
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
